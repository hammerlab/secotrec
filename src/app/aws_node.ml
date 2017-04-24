
open Nonstd
open Pvem_lwt_unix.Deferred_result
module String = Sosa.Native_string
let (//) = Filename.concat

let env_exn s = try Sys.getenv s with _ -> ksprintf failwith "Env: %s missing" s

let treat_error f m =
  m >><
  begin function
  | `Ok o -> return o
  | `Error e ->
    fail (f e)
  end

let say s =
  let prefix = "[AWS-NODE] " in
  printf "%s%s\n%!"
    prefix
    (String.split ~on:(`Character '\n') s
     |> String.concat ~sep:("\n" ^ String.make (String.length prefix) ' '))
let sayf fmt = ksprintf say fmt

let exec ?(bin="") argl =
  let command = (bin, Array.of_list argl) in
  let process = Lwt_process.open_process_full command in
  wrap_deferred ~on_exn:(fun e ->
      process#terminate; 
      Lwt.ignore_result process#close; 
      `Process (`Exec (bin, argl), `Exn e))
    Lwt.(fun () ->
        Lwt_list.map_p Lwt_io.read
          [process#stdout; process#stderr; ]
        >>= fun output_2list ->
        process#close >>= fun status ->
        return (status, output_2list))
  >>= fun (ret, output_2list) ->
  let code =
    match ret with
    | Lwt_unix.WEXITED n ->   (`Exited n)
    | Lwt_unix.WSIGNALED n -> (`Signaled n)
    | Lwt_unix.WSTOPPED n ->  (`Stopped n)
  in
  begin match output_2list with
  | [out; err] -> return (out, err, code)
  | _ -> assert false
  end

module Common = struct

  type t = {
    dry_run: bool [@default false];
    (** Whether to actually do things. *)
  } [@@deriving cmdliner,show]

  let authenticated_request ~error ~region a b =
    let access_key =  env_exn "AWS_KEY_ID" in
    let secret_key =  env_exn "AWS_SECRET_KEY" in
    Aws_lwt.Runtime.run_request
      ~region ~access_key ~secret_key a b
    |> treat_error error
  let ec2_authenticated_request ~region a b =
    authenticated_request ~region ~error:(fun e -> `Aws_ec2 e) a b
  let sdb_authenticated_request ~region a b =
    authenticated_request ~region ~error:(fun e -> `Aws_sdb e) a b

  let to_option f m =
    m >>< begin function
    | `Ok o -> return (Some o)
    | `Error e when f e -> return None
    | `Error e -> fail e
    end

  let is_ec2_error error =
    function
    | `Aws_ec2 (Aws.Error.HttpError (_, Aws.Error.AwsError errors))
      when
        List.exists errors ~f:(function
          | (Aws.Error.Understood e, _) when e = error -> true
               (* Aws_ec2.Errors.InvalidGroup_NotFound, _) -> true *)
          | _ -> false) -> true
    | _ -> false

  let is_dry_run ?(allow_errors = []) common e =
    common.dry_run &&
    (is_ec2_error Aws_ec2.Errors.DryRunOperation e
     ||
     List.exists allow_errors ~f:(fun ae -> is_ec2_error ae e))

  let unless_dry_run common f =
    begin match common.dry_run with
    | true -> return ()
    | false -> f ()
    end
end


module Security_group = struct
  type t = {
    description: string;
    region: string;
    name: string [@main];
  }
  [@@deriving make,show]

  let ensure common t =
    begin
      Common.ec2_authenticated_request ~region:t.region
        (module Aws_ec2.DescribeSecurityGroups)
        (Aws_ec2.Types.DescribeSecurityGroupsRequest.make
           ~group_names:[t.name]
           ())
      |> Common.to_option
        (Common.is_ec2_error Aws_ec2.Errors.InvalidGroup_NotFound)
    end
    >>= begin function
    | Some { Aws_ec2.Types.DescribeSecurityGroupsResult.security_groups = [sg] } ->
      let open Aws_ec2.Types.SecurityGroup in
      sayf "Security_group already there: %s (owner: %s)."
        sg.group_id sg.owner_id;
      return sg.group_id
    | Some { Aws_ec2.Types.DescribeSecurityGroupsResult.security_groups } ->
      fail (`Failure (
          let open Aws_ec2.Types.SecurityGroup in
          sprintf "wrong number of security groups returned: [%s]"
            (List.map security_groups (fun sg ->
                 sprintf "%s, %s, %s, %s"
                   sg.owner_id sg.group_name sg.group_id sg.description)
             |> String.concat ~sep:"; "
            )
        ))
    | None ->
      sayf "Security_group %S must be created:\n%s" t.name (show t);
      Common.ec2_authenticated_request ~region:t.region
        (module Aws_ec2.CreateSecurityGroup)
        (Aws_ec2.Types.CreateSecurityGroupRequest.make
           ~dry_run:common.Common.dry_run
           ~group_name:t.name
           ~description:t.description
           ())
      |> Common.(to_option (is_ec2_error Aws_ec2.Errors.DryRunOperation))
      >>= begin function
      | Some {Aws_ec2.Types.CreateSecurityGroupResult.group_id} ->
        sayf "Created group: %s -> %S" t.name group_id;
        return group_id
      | None ->
        sayf "DRY-RUN: Would have created group: %s" t.name;
        ksprintf return "%s-fake-group-id" t.name
      end
    end

  type rule = [
    | `Tcp_port of int
  ] [@@deriving show]

  let authorize_port common t rule =
    begin
      let ip_protocol, from_port, to_port, cidr_ip =
        match rule with
        | `Tcp_port port -> "tcp", port, port, "0.0.0.0/0"
      in
      Common.ec2_authenticated_request ~region:t.region
        (module Aws_ec2.AuthorizeSecurityGroupIngress)
        (Aws_ec2.Types.AuthorizeSecurityGroupIngressRequest.make
           ~dry_run:common.Common.dry_run
           ~group_name:t.name
           ~ip_protocol
           ~from_port
           ~to_port
           ~cidr_ip
           ())
    end
    >>< begin function
    | `Ok _ ->
      sayf "Rule %s authorized in %s."
        (show_rule rule) t.name;
      return ()
    | `Error e when
        Common.is_dry_run common e
          ~allow_errors:[
            Aws_ec2.Errors.InvalidGroup_NotFound;
          ] ->
      sayf "DRY-RUN: rule %s\nwould have been authorized in %s."
        (show_rule rule) t.name;
      return ()
    | `Error e when
        Common.is_ec2_error Aws_ec2.Errors.InvalidPermission_Duplicate e ->
      sayf "Rule %s seems already in %s."
        (show_rule rule) t.name;
      return ()
    | `Error e -> fail e
    end


  let destroy common t =
    Common.ec2_authenticated_request ~region:t.region
      (module Aws_ec2.DeleteSecurityGroup)
      (Aws_ec2.Types.DeleteSecurityGroupRequest.make
         ~dry_run:common.Common.dry_run
         ~group_name:t.name
         ())
    >>< begin function
    | `Ok () ->
      sayf "Sec-group %s deleted." t.name;
      return ()
    | `Error e when
        Common.is_ec2_error Aws_ec2.Errors.InvalidGroup_NotFound e ->
      sayf "%sSec-group %S is already absent."
        (if common.Common.dry_run then "(DRY-RUN) " else "")
        t.name;
      return ()
    | `Error e when Common.is_dry_run common e ->
      sayf "DRY-RUN: sec-group %s would have been destroyed." t.name;
      return ()
    | `Error e -> fail e
    end

end

module Key_pair = struct

  module Imported = struct

    type t = {
      from: [ `Path of string | `Inline of string ];
      region: string;
      name: string [@main];
    } [@@deriving make,show,yojson]

    let to_hex s =
      let b = Buffer.create (String.length s * 2) in
      String.iter s ~f:(fun c -> Buffer.add_string b (sprintf "%x" (Char.code c)));
      Buffer.contents b
    let sha256 s =
      let cstruct = Cstruct.of_string s in
      Nocrypto.Hash.SHA256.digest cstruct
      |> Cstruct.to_string
      |> to_hex

    let base64 s =
      Cstruct.of_string s |> Nocrypto.Base64.encode |> Cstruct.to_string

    let name t = t.name

    let import ({Common.dry_run; _ } as common) t =
      begin match t.from with
      | `Inline pkm -> return pkm
      | `Path p ->
        Pvem_lwt_unix.IO.read_file p
      end
      >>| String.strip
      (*
         Need to base64 on top of the SSH format:
         http://stackoverflow.com/questions/9711903/ec2-api-importkeypair-der-and-openssh-public-key-canot-be-imported
         https://groups.google.com/forum/?fromgroups#!topic/boto-dev/IczrStO9Q8M
      *)
      >>| base64
      >>= fun public_key_material ->
      (* sayf "Goiing to import: %S" public_key_material; *)
      Common.ec2_authenticated_request ~region:t.region
        (module Aws_ec2.ImportKeyPair)
        (Aws_ec2.Types.ImportKeyPairRequest.make
           ~dry_run
           ~key_name:(name t)
           ~public_key_material
           ())
      >>< begin function
      | `Ok {Aws_ec2.Types.ImportKeyPairResult. key_fingerprint; _ } ->
        sayf "Key-pair %s Fingerprint:\n%s."
          (show t) (Option.value ~default:"None" key_fingerprint);
        return ()
      | `Error e when
          Common.is_dry_run common e
        ->
        sayf "DRY-RUN: Key-pair %s\nwould have been created." (show t);
        return ()
      | `Error e when
          Common.is_ec2_error Aws_ec2.Errors.InvalidKeyPair_Duplicate e ->
        sayf "Key %s is already there" (name t);
        return ()
      | `Error e ->
        fail e
      end

    let ensure common t =
      import common t

    (* For some reason this query is already idempotent.
       On the CLI one can keep successfully calling:
       aws ec2 delete-key-pair --key-name <any-random-string>
    *)
    let destroy common t =
      Common.ec2_authenticated_request ~region:t.region
        (module Aws_ec2.DeleteKeyPair)
        (Aws_ec2.Types.DeleteKeyPairRequest.make
           ~dry_run:common.Common.dry_run
           ~key_name:(t.name)
           ())
      >>< begin function
      | `Ok () ->
        sayf "Key-pair %s: deleted." (show t);
        return ()
      | `Error e when Common.is_dry_run common e ->
        sayf "DRY-RUN: Key-pair %s: would have been deleted." (show t);
        return ()
      | `Error e ->
        fail e
      end

  end

  type t = {
    path: string;
    region: string;
    name: string [@main];
  } [@@deriving make,show]

  let name t = t.name
  let show t = sprintf "%S" t.name

  let path t key_fingerprint =
   t.path // key_fingerprint ^ ".pem"


  let chmod ~octal path =
    wrap_deferred
      ~on_exn:(fun e -> `Chmod (path, octal, e))
      (fun () -> Lwt_unix.chmod path octal)

  let file_exists path =
    Pvem_lwt_unix.System.file_info path
    >>= function
    | `Absent -> return false
    | _ -> return true

  let save t ~key_fingerprint ~key_material =
    let kpath = path t key_fingerprint in
    begin
      file_exists kpath
      >>= function
      | true -> chmod ~octal:0o500 kpath
      | false -> Pvem_lwt_unix.System.ensure_directory_path t.path
    end
    >>= fun () ->
    Pvem_lwt_unix.IO.write_file kpath ~content:key_material
    >>= fun () ->
    chmod ~octal:0o400 kpath

  let get_fingerprint common t =
    Common.ec2_authenticated_request ~region:t.region
      (module Aws_ec2.DescribeKeyPairs)
      (Aws_ec2.Types.DescribeKeyPairsRequest.make
         ~key_names:[t.name]
         ())
    >>< begin function
    | `Ok {
        Aws_ec2.Types.DescribeKeyPairsResult.key_pairs =
          [{ Aws_ec2.Types.KeyPairInfo. key_name; key_fingerprint }]
      } ->
      return key_fingerprint
    | `Ok moreorless ->
      fail (`Failure "too many or not enough keys")
    | `Error e when
        Common.is_ec2_error Aws_ec2.Errors.InvalidKeyPair_NotFound e ->
      return None
    | `Error other -> fail other
    end

  let import ({Common.dry_run; _ } as common) t ~pub_key =
    begin match pub_key with
    | `Inline pkm -> return pkm
    | `Path p ->
      Pvem_lwt_unix.IO.read_file p
    end
    >>= fun public_key_material ->
    Common.ec2_authenticated_request ~region:t.region
      (module Aws_ec2.ImportKeyPair)
      (Aws_ec2.Types.ImportKeyPairRequest.make
         ~dry_run
         ~key_name:t.name
         ~public_key_material
         ())
    >>< begin function
    | `Ok {Aws_ec2.Types.ImportKeyPairResult. key_fingerprint; _ } ->
      sayf "Key-pair %s Fingerprint:\n%s."
        (show t) (Option.value ~default:"None" key_fingerprint);
      return ()
    | `Error e when
        Common.is_dry_run common e
          ~allow_errors:[Aws_ec2.Errors.InvalidGroup_NotFound] ->
      sayf "DRY-RUN: Key-pair %s\nwould have been created." (show t);
      return ()
    | `Error e ->
      fail e
    end

  let create ({Common.dry_run; _ } as common) t =
    get_fingerprint common t
    >>= fun kf ->
    sayf "Key-pair %s Fingerprint:\n%s."
      (show t) (Option.value ~default:"None" kf);
    Common.ec2_authenticated_request ~region:t.region
      (module Aws_ec2.CreateKeyPair)
      (Aws_ec2.Types.CreateKeyPairRequest.make
         ~dry_run
         ~key_name:t.name
         ())
    >>< begin function
    | `Ok { Aws_ec2.Types.KeyPair. key_name; key_fingerprint; key_material } ->
      save t ~key_fingerprint ~key_material
      >>= fun () ->
      sayf "Key-pair %s: created\nFingerprint:\n%s\nPath:\n%s."
        (show t) key_fingerprint (path t key_fingerprint);
      return ()
    | `Error e when
        Common.is_dry_run common e
          ~allow_errors:[Aws_ec2.Errors.InvalidGroup_NotFound] ->
      sayf "DRY-RUN: Key-pair %s\nwould have been created." (show t);
      return ()
    | `Error e when
        Common.is_ec2_error Aws_ec2.Errors.InvalidKeyPair_Duplicate e ->
      sayf "Key-pair %s is already there." (show t);
      return ()
    | `Error e ->
      fail e
    end


  (* For some reason this query is already idempotent.
     On the CLI one can keep successfully calling:
     aws ec2 delete-key-pair --key-name <any-random-string>
  *)
  let destroy common t =
    Common.ec2_authenticated_request ~region:t.region
      (module Aws_ec2.DeleteKeyPair)
      (Aws_ec2.Types.DeleteKeyPairRequest.make
         ~dry_run:common.Common.dry_run
         ~key_name:(t.name)
         ())
    >>< begin function
    | `Ok () ->
      sayf "Key-pair %s: deleted." (show t);
      return ()
    | `Error e when Common.is_dry_run common e ->
      sayf "DRY-RUN: Key-pair %s: would have been deleted." (show t);
      return ()
    | `Error e ->
      fail e
    end

end

module Node = struct

  type t = {
    name: string;
    ports: int list;
    save_path: string;
    pub_key: Key_pair.Imported.t option;
    region: string;
    (** The AWS region to run the tests on. *)
  } [@@deriving make,show,yojson]


  let security_group t =
    Security_group.make
      ~region:t.region
      (t.name ^ "-secgrp")
      ~description:(sprintf "Security group for Node %s" t.name)

  let instance_id_path t = t.save_path // "id"

  let get_instance common t =
    let open Pvem_lwt_unix in
    System.file_info
      ~follow_symlink:true
      (instance_id_path t)
    >>= begin function
    | `Absent ->
      sayf "No instance for %s" t.name;
      return None
    | `Regular_file _ ->
      IO.read_file (instance_id_path t)
      >>= fun instance_id ->
      Common.ec2_authenticated_request ~region:t.region
        (module Aws_ec2.DescribeInstances)
        (Aws_ec2.Types.DescribeInstancesRequest.make
           ~instance_ids:[instance_id] ())
      >>< begin function
      | `Ok {Aws_ec2.Types.DescribeInstancesResult.reservations;
             next_token = None} ->
        let l =
          List.map reservations
            ~f:(fun
                 { Aws_ec2.Types.Reservation. reservation_id; instances; _ } ->
                 instances)
          |> List.concat in
        (* sayf "%d instances" (List.length l); *)
        let interesting =
          List.map l ~f:(fun inst ->
              let open Aws_ec2.Types.Instance in
              let {Aws_ec2.Types.InstanceState. code; name} = inst.state in
              let open Aws_ec2.Types.InstanceStateName in
              object
                method id = instance_id
                method ip = inst.public_ip_address
                method dns = inst.public_dns_name
                method state = name
                method show =
                  sprintf "id: %s\nip: %s\ndns: %s\nstate: %s"
                    instance_id
                    (Option.value ~default:"None" inst.public_ip_address)
                    (Option.value ~default:"None" inst.public_dns_name)
                    (List.Assoc.get name t_to_str
                     |> Option.value_exn ~msg:"Aws_node: List.Assoc. failed")
              end)
        in
        begin match interesting with
        | [] -> return None
        | [one] -> return (Some one)
        | more ->
          fail (`Failure "many or too few instances")
        end
      | `Ok other ->
        fail (`Failure "many or too few instances")
      | `Error e ->
        fail e
      end
    | other_file_type ->
      fail (`Failure "instance id is not a file???")
    end

  let wait_for ?(max_attempts = 30) ?(sleep = 3.) common t ~condition =
    let rec f count =
      if count >= max_attempts
      then fail (`Failure "Maximum wait for condition")
      else
        get_instance common t
        >>= function
        | iopt when condition iopt -> return ()
        | None | Some _ ->
          printf ".%!";
          Pvem_lwt_unix.System.sleep sleep
          >>< fun _ ->
          f (count + 1)
    in
    f 0



  let run ~security_group_id common t =
    get_instance common t
    >>= begin function
    | Some instance when
        Aws_ec2.Types.InstanceStateName.(
          instance#state = Pending
          || instance#state = Running
        ) ->
      sayf "Instance already up:\n%s" instance#show;
      return ()
    | Some _
    | None ->
      let key_name =
        Option.map t.pub_key Key_pair.Imported.name in
      Common.ec2_authenticated_request ~region:t.region
        (module Aws_ec2.RunInstances)
        (Aws_ec2.Types.RunInstancesRequest.make
           (* ~client_token:(sprintf "%s-token" t.name) *)
           ~dry_run:common.Common.dry_run
           ~min_count:1 ~max_count:1
           ~image_id:"ami-7a3dd76c"
           ?key_name
           ~security_group_ids:[security_group_id]
           ~instance_type:Aws_ec2.Types.InstanceType.T1_micro
           ())
      >>< begin function
      | `Ok {Aws_ec2.Types.Reservation. reservation_id; instances; _} ->
        begin match instances with
        | [one] ->
          let open Aws_ec2.Types.Instance in
          sayf "Instances reserved: %s\nfor node %s\nId: %s."
            reservation_id (show t) one.instance_id;
          Pvem_lwt_unix.IO.write_file (instance_id_path t)
            ~content:one.instance_id
        | other ->
          fail (`Failure "Too many or zero instances")
        end
        >>= fun () ->
        return ()
      | `Error e when
          Common.is_dry_run common e
            ~allow_errors:[
              Aws_ec2.Errors.InvalidKeyPair_NotFound;
              Aws_ec2.Errors.InvalidParameterValue;
            ] ->
        sayf "DRY-RUN: instance for node %s:\nwould have been created." (show t);
        return ()
      | `Error e ->
        fail e
      end
    end
    >>= fun () ->
    Common.unless_dry_run common begin fun () ->
      sayf "Waiting for IP Address";
      wait_for common t
        ~condition:(function Some i -> i#ip <> None | _ -> false)
    end

  let terminate common t =
    Pvem_lwt_unix.IO.read_file (instance_id_path t)
    >>= fun instance_id ->
    Common.ec2_authenticated_request ~region:t.region
      (module Aws_ec2.TerminateInstances)
      (Aws_ec2.Types.TerminateInstancesRequest.make
         ~dry_run:common.Common.dry_run
         ~instance_ids:[instance_id]
         ())
    >>< begin function
    | `Ok {Aws_ec2.Types.TerminateInstancesResult.terminating_instances} ->
      sayf "Instance terminating...\nfor node %s." (show t);
      return ()
    | `Error e when
        Common.is_dry_run common e
          ~allow_errors:[] ->
      sayf "DRY-RUN: instance termination for node\n%s." (show t);
      return ()
    | `Error e when
        Common.is_ec2_error Aws_ec2.Errors.InvalidInstanceID_NotFound e ->
      sayf "Instance already gone...\nfor node %s." (show t);
      return ()
    | `Error e ->
      fail e
    end


  let up common t =
    Pvem_lwt_unix.System.ensure_directory_path t.save_path
    >>= fun () ->
    Security_group.ensure common (security_group t)
    >>= fun security_group_id ->
    Pvem_lwt_unix.Deferred_list.while_sequential t.ports begin fun port ->
      Security_group.authorize_port common (security_group t) (`Tcp_port port)
    end
    >>= fun _ ->
    begin match t.pub_key with
    | None ->
      sayf "No Pub-Key to setup";
      return ()
    | Some pk ->
      Key_pair.Imported.ensure common pk
    end
    >>= fun () ->
    run ~security_group_id common t
    >>= fun () ->
    return ()


  let down common t =
    terminate common t
    >>= fun () ->
    Common.unless_dry_run common begin fun () ->
      sayf "Waiting for Termination";
      wait_for common t ~condition:Aws_ec2.Types.InstanceStateName.(function
        | Some i -> i#state = Terminated
        | None -> true)
    end
    >>= fun () ->
    begin match t.pub_key with
    | None -> sayf "No pub-key to delete"; return ()
    | Some pk ->
      Key_pair.Imported.destroy common pk
    end
    >>= fun () ->
    Security_group.destroy common (security_group t)
    >>= fun () ->
    return ()

  let ssh_prefix common t =
    get_instance common t
    >>= function
    | None ->
      let err =
        sprintf "Can't find instance; is it up?\n(node: %s)" (show t) in
      fail (`Failure err)
    | Some obj ->
      let addr =
        Option.(
          value obj#dns ~default:(value_exn obj#ip ~msg:"Instance has no IP address")
        ) in
      return (sprintf "ec2-user@%s" addr)

  let ssh_command common t args =
    ssh_prefix common t
    >>= fun sshprefix ->
    let ssh_cmd =
      ["ssh"; sshprefix] in
    return (ssh_cmd @ args)

  let ssh_interactive common t args =
    ssh_command common t args
    >>= fun ssh_cmd ->
    Unix.execv "/usr/bin/ssh" (Array.of_list ssh_cmd)
end



module Configuration = struct

  type t = {
    node: Node.t
  } [@@deriving yojson,show,make]
            
  type input = {
    node_name : string;
    ports : int list;
    pub_key_file : string option;
    pub_key_inline : string option;
    region : string [@default "us-east-1"];
  } [@@deriving show,make,cmdliner]

  module Path = struct
    type t = {
      path: string;
      (** The cofiguration directory. *)
    } [@@deriving show,cmdliner]
  end

  let build_cmd () =
    let open Cmdliner.Term in
    let open Path in
    let term =
      pure (
        fun { node_name; ports; pub_key_file; pub_key_inline; region } {path} ->
          let open Pvem_lwt_unix in
          let pub_key =
            let ma from =
              let name = sprintf "%s-pubkey" node_name in
              Some (Key_pair.Imported.make ~from ~region name) in
            match pub_key_inline, pub_key_file with
            | None, None -> None
            | Some i, None -> ma (`Inline i) 
            | None, Some p -> ma (`Path p) 
            | Some _, Some _ ->
              failwith "Cannot give both --pub-key-file and --pub-key-inline, \
                        It's confusing you know..."
          in
          let node =
            Node.make ~name:node_name ~ports ?pub_key ~region
              ~save_path:(path // node_name) () in
          let c = make ~node in
          let content = to_yojson c |> Yojson.Safe.pretty_to_string ~std:true in
          System.ensure_directory_path path
          >>= fun () ->
          IO.write_file ~content (path // "configuration.json")
      )
      $ input_cmdliner_term ()
      $ cmdliner_term ()
    in
    (term, info "configure" ~doc:"Create a configuration")

  let config_term () =
    Cmdliner.Term.(
      ret @@
      begin
        pure (fun {Path.path} ->
            Yojson.Safe.from_file (path // "configuration.json")
            |> of_yojson
            |> function
            | Ok o -> `Ok o
            | Error s -> `Error (false, s)
          )
        $ Path.cmdliner_term ()
      end
    )

  let show_config () =
    Cmdliner.Term.(pure (fun c -> show c |> sayf "%s"; return ())
                   $ config_term ())

  let node_up_cmd () =
    let open Cmdliner.Term in
    let term =
      pure (fun com conf ->
          Node.up com conf.node)
      $ Common.cmdliner_term ()
      $ config_term ()
    in
    term, info "up"

  let node_down_cmd () =
    let open Cmdliner.Term in
    let term =
      pure (fun com conf ->
          Node.down com conf.node)
      $ Common.cmdliner_term ()
      $ config_term ()
    in
    term, info "down"

  let ssh_cmd () =
    let open Cmdliner.Term in
    let term =
      pure (fun com conf args ->
          Node.ssh_interactive com conf.node args)
      $ Common.cmdliner_term ()
      $ config_term ()
      $ Cmdliner.Arg.(value & pos_all string [] & info [] ~doc:"Additional Arguments")
    in
    term, info "ssh"

end


let () =
  let clires =
    let default name =
      Configuration.show_config (), Cmdliner.Term.info name in
    Cmdliner.Term.(
      eval_choice (default Sys.argv.(0)) [
        default "show-configuration";
        Configuration.build_cmd ();
        Configuration.node_up_cmd ();
        Configuration.node_down_cmd ();
        Configuration.ssh_cmd ();
      ]) in
  begin match clires with
  | `Ok lwt ->
    begin match Lwt_main.run lwt with
    | `Ok () -> exit 0
    | `Error (`Aws_ec2 e) ->
      eprintf "ERROR: %s\n%!"
        (Aws.Error.format Aws_ec2.Errors.to_string e);
      exit 1
    | `Error (`IO _ as io) ->
      eprintf "I/O-ERROR: %s\n%!"
        (Pvem_lwt_unix.IO.error_to_string io);
      exit 3
    | `Error (`Chmod (p,m,e)) ->
      eprintf "Sys-ERROR: chmod %s -> %d: %s\n%!" p m (Printexc.to_string e);
      exit 3
    | `Error (`System _ as sys) ->
      eprintf "System-ERROR: %s\n%!"
        (Pvem_lwt_unix.System.error_to_string sys);
      exit 3
    | `Error (`Process (`Exec (b,l),`Exn e)) ->
      eprintf "Process execution error: %s %s\n%s\n%!"
        b (String.concat ~sep:" " l) (Printexc.to_string e);
      exit 3
    | `Error (`Failure s) ->
      eprintf "ERROR: %s\n%!" s;
      exit 2
    end
  | `Error _
  | `Version
  | `Help -> ()
  end
