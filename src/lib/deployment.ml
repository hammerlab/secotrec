open Common

module Node = struct

  type local_directory = string
  type property = [
    | `Has_directory of string * [`All_rwx]
  ]

  let has_directory ?(access = `All_rwx) path : property =
    `Has_directory (path, access)

  type t = {
    host : [
      | `Localhost
      | `Gcloud of Gcloud_instance.t
      | `Aws_ssh of Aws_instance.Ssh.t
    ] [@main];
    properties: property list;
  } [@@deriving make]

  let localhost ?properties () =
    make `Localhost ?properties
  let gcloud ?properties g = make (`Gcloud g) ?properties
  let aws_ssh ?properties a = make (`Aws_ssh a) ?properties
end

module Extra_nfs_server = struct
  type t = {
    server: Nfs.Fresh.t;
    mount_point: string;
  } [@@deriving make]

  let mount_point t = t.mount_point
  let to_mount {server; mount_point} =
    Nfs.Fresh.mount server ~mount_point

  let parse_sexp ~prefix ~zone str =
    let open Sexplib.Sexp in
    let fail ?sexp ?expect msg =
      ksprintf failwith "Error while parsing NFS server definitions: %s %s%s"
        msg
        (Option.value_map sexp ~default:""
           ~f:(fun s -> sprintf "Got `%s`  " (to_string s)))
        (Option.value_map expect ~default:"" ~f:(sprintf "Expecting `%s` "))
    in
    let parse_one =
      function
      | List [Atom name; List params] ->
        let identity e = e in
        let get_string tag f =
          List.find_map params ~f:(function
            | List [Atom n; Atom t] when n = tag -> Some (f t)
            | _ -> None) in
        let get_int tag f =
          match get_string tag identity |> Option.map ~f:Int.of_string with
          | Some (Some v) -> Some (f v)
          | Some None ->
            fail ~expect:(sprintf "(%s <integer>)" tag) "Size parsing failed"
          | None -> None
        in
        let reuse_data_disk = get_string "reuse-data-disk" identity in
        (* let machine_type = get_string "machine-type" identity in *)
        let size =
          get_int "size" (fun s -> `GB s) |> Option.value ~default:(`GB 5000) in
        let mount_point =
          get_string "mount-point" identity
          |> Option.value ~default:("/" // name) in
        let server =
          Nfs.Fresh.make (sprintf "%s-%s" prefix name)
            ?reuse_data_disk
            ~instance:(
              Gcloud_instance.make (sprintf "%s-%s-%s" prefix name "vm")
                ~zone ~machine_type:(`Google_cloud `Highmem_8))
            ~size in
        make ~server ~mount_point
      | other ->
        fail ~sexp:other ~expect:"(<name> <parameters-list>)"
          "Can't recognize the S-Expression pattern"
    in
    match of_string str with
    | List l ->
      List.map l ~f:parse_one
    | other ->
      fail ~sexp:other ~expect:"(<server-definition-list>)"
        "Can't recognize the S-Expression pattern"

  let sexp_syntax_help =
    "Syntax: (<server-list>) where each server is:\n\
     * (<name> <parameter-list>), parameters can be \n\
    \     * (size <integer>): Set the size of the storage (in GiB) \n\
    \       of the fresh NFS server.\n\
    \     * (reuse-data-disk <gcloud-name>): Use an existing disk\n\
    \       (and don't delete in when taking the deployment down).\n\
    \     * (mount-point <path>): Mount the fresh NFS at that path \n\
    \       on deployed services (default: /<name>).\n\
    "

  let sexp_syntax_example =
    "(\n\
    \   ;; 10 TB fresh NFS server mounted at `/nfs01`:\n\
    \   (nfs01 (\n\
    \      (size 10000)))\n\
    \   ;; Use a GCloud disk for a fresh NFS server:\n\
    \   (nfs02 (\n\
    \      (reuse-data-disk my-gcloud-storage)))\n\
    \   ;; Create a fresh NFS server and specify the mount-point:\n\
    \   (nfs03 (\n\
    \      (size 10000)\n\
    \      (mount-point /custommountpoint)))\n\
     )"
end

type t = {
  node: Node.t;
  (* compose_configuration : Docker_compose.Configuration.t; *)
  gke_cluster : Gke_cluster.t option;
  db : Postgres.t option;
  dns : Gcloud_dns.t option;
  extra_nfs_servers : Extra_nfs_server.t list;
  efs : Aws_efs.t option;
  ketrew : Ketrew_server.t option;
  coclobas : Coclobas.t option;
  letsencrypt : Letsencrypt.t option;
  biokepi_machine : Biokepi_machine_generation.t option;
  proxy_nginx: Nginx.Proxy.t option;
  tlstunnel : Tlstunnel.t option;
  preparation: Data_preparation.t option;
  extra_services: Docker_compose.Configuration.service list;
  name : string [@main ];
} [@@deriving make]

let name t = t.name

let compose_configuration t =
  List.filter_opt [
    Option.map ~f:Nginx.Proxy.to_service t.proxy_nginx;
    Option.map ~f:Coclobas.to_service t.coclobas;
    Option.map ~f:Postgres.to_service t.db;
    Option.map ~f:Ketrew_server.to_service t.ketrew;
    Option.map ~f:Tlstunnel.to_service t.tlstunnel;
  ]
  @ t.extra_services
  |> Docker_compose.Configuration.make

module Run = struct

  let on_node ?(name = "deployment-run") t cmd =
    match t.node.Node.host with
    | `Gcloud node -> Gcloud_instance.run_on ~name node cmd
    | `Localhost ->
      let tmp = Filename.temp_file "secotrec" "script.sh" in
      write_file tmp ~content:(Genspio.Language.to_many_lines cmd);
      Genspio_edsl.exec ["sh"; tmp]
    | `Aws_ssh a ->
      Aws_instance.Ssh.run_on ~name a cmd

  let cp_from_node t file_from file_to =
    match t.node.Node.host with
    | `Gcloud node ->
      Gcloud_instance.copy_file node (`On_node file_from) (`Local file_to)
    | `Localhost ->
      Genspio_edsl.exec ["cp"; file_from; file_to]
    | `Aws_ssh node ->
      let name = sprintf "cp_from_node" in
      let open Genspio_edsl in
      seq_succeeds_or ~clean_up:[fail] ~name (
        Aws_instance.Ssh.copy_file node (`On_node file_from) (`Local file_to)
      )

  let cp_to_node t file_from file_to =
    match t.node.Node.host with
    | `Gcloud node ->
      Gcloud_instance.copy_file node (`Local file_from) (`On_node file_to)
    | `Aws_ssh node ->
      let name = sprintf "cp_to_node" in
      let open Genspio_edsl in
      seq_succeeds_or ~clean_up:[fail] ~name (
        Aws_instance.Ssh.copy_file node (`Local file_from) (`On_node file_to)
      )
    | `Localhost ->
      Genspio_edsl.exec ["cp"; file_from; file_to]

  let ensure_node t =
    let open Node in
    let create_directories =
      let open Genspio_edsl in
      List.map t.node.properties ~f:begin function
      | `Has_directory (path, `All_rwx) ->
        [
          exec ["mkdir"; "-p"; "-m"; "777"; path];
          (* If the dir already exists `mkdir -m` is not enough: *)
          exec ["chmod"; "777"; path];
        ]
      end
      |> List.concat
      |> seq_succeeds_or
        ~clean_up:[fail] ~name:"Creating the node's local-directories"
    in
    match t.node.host with
    | `Gcloud node ->
      run_genspio Genspio_edsl.(seq [
          Gcloud_instance.ensure node;
          on_node t create_directories;
        ])
    | `Localhost ->
      run_genspio Genspio_edsl.(seq [
          create_directories;
        ])
    | `Aws_ssh node ->
      run_genspio Genspio_edsl.(seq [
          on_node t create_directories;
        ])

  let destroy_node t =
    let open Genspio_edsl in
    match t.node.Node.host with
    | `Gcloud node ->
      seq [
        sayf "Shutting down the GCHost...";
        Gcloud_instance.destroy node;
      ]
    | `Localhost -> nop
    | `Aws_ssh _ -> nop


  let only_gcloud_node ~msg t f =
    match t.node.Node.host with
    | `Gcloud node -> f node
    | `Localhost | `Aws_ssh _ ->
      ksprintf failwith
        "The feature “%s” is only available on GCloud instances"
        msg

  let optional opt f =
    match opt with
    | None -> ()
    | Some o -> f o

  let use_sudo t =
    match t.node.Node.host with
    | `Gcloud _ | `Aws_ssh _ -> true
    | `Localhost -> false

  let docker_compose_command ?with_software ?save_output t more =
    let cmd =
      Docker_compose.make_command ?save_output ~use_sudo:(use_sudo t)
        ?with_software ~compose_config:(compose_configuration t)
        ~tmp_dir:(Tmp.in_dir "seco-docker-compose") more in
    cmd

  let docker_compose_get_container_id t ?with_software ~container =
    let open Genspio_edsl in
    let tmp = tmp_file  "container-id" in
    object
      method name = container
      method make =
        seq [
          tmp#set (string "");
          docker_compose_command ?with_software ~save_output:(tmp#path)
            t ["ps"; "-q"; container];
        ]
      method get =
        (tmp#get >> exec ["tr"; "-d"; "\\n"] |> output_as_string)
    end


  let docker_compose t more =
    docker_compose_command t more |> on_node t |> run_genspio

  let docker_command t more =
    let open Genspio_edsl in
    (if use_sudo t then ["sudo"] else [])
    @ ["docker"]
    @ more
    |> exec

  let get_file_from_container t ~container_id
      ~container_path ~node_path ~local_path =
    let open Genspio_edsl in
    seq [
      container_id#make;
      sayl "Getting backup from %s container (`%s`) to host."
        [string container_id#name; container_id#get];
      call (
        (if use_sudo t then [string "sudo"] else [])
        @ [string "docker"]
        @ [string "cp"; string_concat [container_id#get; string ":";
                                       string container_path];
           string node_path]
      );
    ] |> on_node t |> run_genspio ~name:"copy-container-to-node";
    seq [
      sayf "Getting Node:%S to %S" node_path local_path;
      cp_from_node t node_path local_path;
    ] |> run_genspio ~name:"get-file-locally";
    ()

  let up t =
    let open Genspio_edsl in
    ensure_node t;
    optional t.dns begin fun dns ->
      only_gcloud_node ~msg:"Setting GCloud DNS name" t (fun node ->
          Gcloud_dns.setup ~node dns;
        )
    end;
    optional t.letsencrypt begin fun letsencrypt ->
      on_node t (
        letsencrypt#ensure
          ~before_generation:(
            docker_compose_command t  ["kill"; "tlstun"]
          )
      ) |> run_genspio ~name:"letsencrypt-ensure" ~returns:0;
    end;
    run_genspio ~name:"nfs-up" (
      on_node t (
        seq (
          Docker_compose.ensure_software
          :: List.map t.extra_nfs_servers ~f:(fun nfs ->
              exec [ (* We use the `gcoudnfs` script in the coclobas container *)
                "sudo"; "docker"; "run";
                "hammerlab/keredofi:coclobas-gke-biokepi-default";
                "sh"; "-c";
                genspio_to_one_liner ~name:"ensure-nfs"
                  (Nfs.Fresh.ensure nfs.Extra_nfs_server.server);
              ]
            )
        )
      )
    );
    run_genspio ~name:"deploy-up" ~returns:0 (
      let efs_ensure =
        Option.map t.efs ~f:begin fun efs ->
          let aws_cli = Aws_cli.guess () in
          Aws_efs.To_genspio.ensure aws_cli efs
        end in
      on_node t (
        seq [
          Option.value ~default:(sayf "No EFS to do.") efs_ensure;
          docker_compose_command ~with_software:false t ["up"; "-d"]
        ]
      );
    )

  let node_ip_address t =
    match t.node.Node.host with
    | `Gcloud node -> `External (Gcloud_instance.external_ip node)
    | `Aws_ssh node ->
      (* TODO: fix the name of the function or get the actuall IP address: *)
      `External (Aws_instance.Ssh.hostname node |> Genspio_edsl.string)
    | `Localhost -> `Local



  let status t =
    let open Genspio_edsl in
    let docker_compose_status_cmd =
      seq [
        sayf "Getting `docker-compose ps`:";
        docker_compose_command t ["ps"];
        sayf "Getting `docker ps -s`:";
        docker_command t ["ps"; "-s"];
        sayf "Getting `docker stats` snapshot:";
        docker_command t ["stats"; "--no-stream"];
      ] in
    let coclobas_status_cmd =
      Option.value_map t.coclobas ~default:nop ~f:(fun c ->
          let o =
            exec ["curl"; "-s"; "http://127.0.0.1:8082/status"]
            |> output_as_string in
          seq [
            sayf "Getting Coclobas status:";
            call [string "printf"; string "Curl coclobas/status says: %s\n"; o]
          ]) in
    let ketrew_status_cmd =
      Option.value_map t.ketrew ~default:nop ~f:(fun kserver ->
          let curl cmd =
            call ([string "curl"; string "-k"; string "-s"] @ cmd) in
          seq [
            sayf "Getting Ketrew Server Status:";
            begin
              let message =
                Ketrew_pure.Protocol.Up_message.serialize `Get_server_status in
              curl [string "-d"; string message;
                    Ketrew_server.make_url kserver ~service:`Api `Local]
            end;
            sayf "Ketrew GUI:";
            Ketrew_server.make_url kserver ~service:`Gui
              (node_ip_address t) >> exec ["cat"];
            exec ["printf"; "\\n"];
            Option.value_map t.dns ~default:nop ~f:(fun dns ->
                Ketrew_server.make_url kserver ~service:`Gui
                  (`External (string dns.Gcloud_dns.name))
                >> exec ["cat"]);
            exec ["printf"; "\\n"];
          ]) in
    seq [
      on_node t docker_compose_status_cmd;
      on_node t coclobas_status_cmd;
      on_node t ketrew_status_cmd;
      Option.value_map ~default:nop t.efs ~f:begin fun efs ->
        let aws_cli = Aws_cli.guess () in
        on_node t (
          Aws_efs.To_genspio.describe aws_cli efs
        )
      end;
      Option.value_map t.ketrew ~default:nop ~f:(fun kserver ->
          begin
            let cmd =
              List.map (Ketrew_server.all_nfs_witnesses kserver)
                ~f:(fun path -> exec ["ls"; "-l"; path])
              |> seq
            in
            on_node t @@ seq [
              sayf "Checking NFS-mounts on `kserver`:";
              docker_compose_command t
                ["exec"; "kserver"; "sh"; "-c";
                 genspio_to_one_liner ~name:"check-nfs-mount" cmd];
            ]
          end);
    ] |> run_genspio

  let down t =
    let open Genspio_edsl in
    optional t.dns begin fun dns ->
      only_gcloud_node ~msg:"DNS name destruction" t begin fun node ->
        Gcloud_dns.clean_up ~node dns;
      end;
    end;
    run_genspio (seq [
        seq (
          List.map t.extra_nfs_servers ~f:(fun nfs ->
              seq [
                sayf "Destroying the Extra-NFS server...";
                on_node t (
                  let enfs = nfs.Extra_nfs_server.server in
                  if_seq
                    (Gcloud_instance.instance_is_up (Nfs.Fresh.instance enfs))
                    ~t:[
                      exec [
                        "sudo"; "docker"; "run";
                        "hammerlab/keredofi:coclobas-gke-biokepi-default";
                        "sh"; "-c";
                        genspio_to_one_liner ~name:"destroy-nfs"
                          (Nfs.Fresh.destroy enfs);
                      ]
                    ]
                    ~e:[sayf "NFS extra server is already down"]
                );
              ]));
        Option.value_map ~default:nop t.efs ~f:begin fun efs ->
          let aws_cli = Aws_cli.guess () in
          on_node t (
            Aws_efs.To_genspio.destroy aws_cli efs
          )
        end;
        Option.value_map t.gke_cluster ~default:nop ~f:(fun kube ->
            seq [
              sayf "Shutting down the cluster...";
              on_node t
                (seq_succeeds_or
                   ~name:"Destruction-of-the-Kube-cluster"
                   [Gke_cluster.destroy kube]
                   ~clean_up:[
                     sayf "Destruction-of-the-Kube-cluster failed but we keep going \
                           the destruction of the “GCHost”";
                   ]);
            ]);
        begin match t.node.Node.host with
        | `Gcloud _ -> (* destroying the whole node gets rid of everything *)
          destroy_node t
        | `Localhost ->
          docker_compose_command t ["down"]
        | `Aws_ssh node ->
          sayf "Destruction of Aws-ssh nodes is not implemented"
        end;
      ])

  let top t =
    let open Genspio_edsl in
    run_genspio (seq [
        on_node t (
          seq [
            exec ((if use_sudo t then ["sudo"] else []) @ [ "docker"; "stats"])
          ]
        )
      ])

  module Generate = struct

    let biokepi_machine ?with_script_header t ~path =
      begin match t.biokepi_machine with
      | None ->
        eprintf "No biokepi-machine was configured for %S\n%!" t.name;
        failwith "Missing biokepi-machine configuration"
      | Some bm ->
        printf "Puting biokepi-machine in %s:\n```\n%s\n```\n%!" path
          (Biokepi_machine_generation.show bm);
        write_file path
          (Biokepi_machine_generation.to_ocaml ?with_script_header bm)
      end

    let ketrew_configuration ?userinfo t ~path =
      let open Genspio_edsl in
      let ketrew_url =
        let kserver = Option.value_exn t.ketrew
            ~msg:"This deployment does not have a Ketrew server" in
        match t.dns with
        | None ->
          Ketrew_server.make_url ?userinfo kserver ~service:`Gui
            (node_ip_address t)
        | Some dns ->
          Ketrew_server.make_url ?userinfo kserver ~service:`Gui
            (`External (string dns.Gcloud_dns.name))
      in
      run_genspio (seq [
          call [string "ketrew"; string "init";
                string "--configuration-path"; string path;
                string "--just-client"; ketrew_url;];
        ])

    let useful_env ?path t =
      let out, clos =
        Option.value_map path ~f:(fun p -> open_out p, close_out)
          ~default:(stdout, fun _ -> ()) in
      let env k v = fprintf out "export %s=%s\n%!" k v in
      let cmt s = fprintf out "# %s\n%!" s in
      let opt_iter ~f = function None -> () | Some x -> f x in
      opt_iter t.db ~f:begin fun pg ->
        env "PG_SERVICE_NAME" (Postgres.container_name pg);
        env "PG_SERVICE_URI" (Postgres.to_uri pg |> Uri.to_string);
      end;
      opt_iter t.coclobas ~f:begin fun coco ->
        env "COCLOBAS_SERVICE_NAME" (Coclobas.name coco);
        env "COCLOBAS_TMP_DIR" (Coclobas.tmp_dir coco);
        env "COCLOBAS_BASE_URL" (Coclobas.base_url coco);
        env "COCLOBAS_LOCAL_URL" (Coclobas.url_for_local_client coco);
      end;
      opt_iter t.ketrew ~f:begin fun ks ->
        env "KETREW_SERVICE_NAME" (Ketrew_server.name ks);
      end;
      begin
        let out s = env "CURRENT_BIOKEPI_WORK_DIR" s in
        begin match Sys.getenv "BIOKEPI_WORK_DIR" with
        | s -> out s
        | (exception _) -> cmt "BIOKEPI_WORK_DIR is not overridden; \
                                the current is the default."
        end;
        begin match t.biokepi_machine with
        | Some biok ->
          out biok.Biokepi_machine_generation.default_work_dir;
          env "DEFAULT_BIOKEPI_WORK_DIR"
            biok.Biokepi_machine_generation.default_work_dir;
        | None ->
          cmt "Biokepi-machine (hence default-biokepi-work-dir) not defined."
        end;
      end;
      clos out;
      ()

    let docker_compose_configuration t ~path =
      let config = compose_configuration t in
      cmdf ~returns:0 "mkdir -p '%s'" path;
      List.iter (Docker_compose.Configuration.render config) ~f:begin
        function
        | `Configuration conf ->
          let content =
            let d = ODate.Unix.(now () |> Printer.to_iso) in
            sprintf "# Generated by Secotrec deployment `%s` (on %s)\n" t.name d
            ^ conf in
          write_file (path // "docker-compose.json") ~content
        | `File_relative (relp, content) ->
          write_file (path // relp) ~content
      end

  end

  let prepare ?userinfo t =
    let prep =
      Option.value_exn t.preparation
        ~msg:"Deployment does not have a preparation step" in
    let wf = Data_preparation.to_workflow prep in
    let kconfdir = Filename.get_temp_dir_name () // "kconf" in
    Generate.ketrew_configuration ?userinfo t ~path:kconfdir;
    let override_configuration =
      Ketrew.Configuration.load_exn ~and_apply:true (`In_directory kconfdir) in
    Ketrew.Client.submit_workflow wf ~override_configuration

  let create_simple_test_job ?userinfo ~commands ~name t =
    let biomachine =
      Option.value_exn t.biokepi_machine
        ~msg:"missing biokepi-machine definition" in
    let machine_script =
      Biokepi_machine_generation.to_ocaml ~with_script_header:true biomachine in
    let workflow =
      let part_1 =
{ocaml|
let workflow =
   let open Biokepi in
   let open KEDSL in
   let program =
     let open Program in
     let cmd c =
       let shorten s =
         match String.sub s ~index:0 ~length:50 with
         | None -> s | Some s -> s ^ "…" in
       shf "echo \"## Secotrec Test of Biokepi machine: %s\""
         (Filename.quote (shorten c))
       && shf "%s || { echo 'Command failed' ; echo failure >> /tmp/failures ; }" c
     in
     chain [
|ocaml} in
      let part_2 =
        let cmdf fmt = ksprintf (sprintf "cmd %S;") fmt in
        let workdir_to_test =
          try Sys.getenv "BIOKEPI_WORK_DIR" with
          | _ ->
            biomachine.Biokepi_machine_generation.default_work_dir in
        let test_file =
          workdir_to_test // sprintf "secotest-%s.txt"
            ODate.Unix.(now () |> Printer.to_iso) in
        begin match commands with
        | `Custom cmd ->
          cmdf "%s" cmd
        | `Test_workdir ->
          String.concat ~sep:"\n" [
            cmdf "ls -la $HOME";
            cmdf "mkdir -p %s" workdir_to_test;
            cmdf "echo 'Secotrec Biokepi Test' $(date) > %s" test_file;
            cmdf "ls -la %s" workdir_to_test;
            cmdf "cat %s" test_file;
          ]
        end
      in
      let part_3 =
        sprintf
{ocaml|
     ]
     && sh "if [ -f /tmp/failures ] ; then exit 1 ; else exit 0 ; fi"
   in
   let name = %S in
   let make = Machine.run_program ~name biokepi_machine program in
   (* let host = Machine.(as_host biokepi_machine) in *)
   workflow_node ~name without_product ~make
|ocaml} name in
      part_1 ^ part_2 ^ part_3 in
    let kconfdir = Filename.get_temp_dir_name () // "kconf" in
    Generate.ketrew_configuration ?userinfo t ~path:kconfdir;
    let submission =
      sprintf
        {ocaml|
let () =
  let override_configuration =
    Ketrew.Configuration.load_exn ~and_apply:true (`In_directory %S) in
Ketrew.Client.submit_workflow workflow ~override_configuration
|ocaml} kconfdir in
    let script = Tmp.in_dir "script.ml" in
    write_file script
      (sprintf "%s\n\n%s\n\n%s\n"
         machine_script workflow submission);
    cmdf "ocaml %s" script


  let deploy_debug_node ~minutes ?userinfo t =
    let cmd_str = sprintf "sleep %d" (minutes * 60) in (* mins -> secs *)
    let name = sprintf "** DEBUG ME in %d mins **" minutes in
    create_simple_test_job ?userinfo ~commands:(`Custom cmd_str) ~name t


  let test_biokepi_machine ?userinfo t =
    let name = "Test of the biokepi machine" in
    create_simple_test_job ?userinfo ~name t ~commands:`Test_workdir


  let psql t ~args =
    let db, container =
      match t.db, t.coclobas, t.ketrew with
      | None, _, _ ->
        ksprintf failwith "Deployment %S does not have a Postres database." t.name
      | Some db, Some pn, _ -> db, Coclobas.name pn
      | Some db, None, Some k -> db, Ketrew_server.name k
      | _, _, _ ->
        ksprintf failwith "Can't find a container to run `psql` (Deployment %S)"
          t.name
    in
    let open Genspio_edsl in
    let psql_cmd =
      let psqlrc = tmp_file "psqlrc" in
      seq [
        psqlrc#set (string "SET bytea_output = 'escape';");
        setenv (string "PAGER") (string "cat");
        setenv (string "PSQLRC") psqlrc#path;
        exec (
          ["psql"; "--pset=pager=off"]
          @ args
          @ [Postgres.to_uri db |> Uri.to_string]
        );
      ] in
    seq [
      docker_compose_command t ["exec"; container; "bash"; "-c";
                                genspio_to_one_liner ~name:"psql_cmd" psql_cmd]
    ] |> on_node t |> run_genspio

  let coclobas_client t ~args =
    let coclo =
      Option.value_exn ~msg:"No Coclobas server configured" t.coclobas in
    docker_compose t (
      [
        "exec"; Coclobas.name coclo;
        "opam";
        "conf";
        "exec";
        "--";
        "coclobas"; "client"; "--server"; Coclobas.url_for_local_client coclo;
      ]
      @ args)

  let backup_postgres t ~path =
    let db, container =
      match t.db with
      | None ->
        ksprintf failwith "Deployment %S does not have a Postres database." t.name
      | Some db -> db, Postgres.container_name db
    in
    let open Genspio_edsl in
    let container_id =
      docker_compose_get_container_id t ~with_software:false ~container in
    seq [
      sayf "Running `pg_dump`";
      docker_compose_command t
        ["exec"; container; "pg_dump";
         "--clean";
         "--format"; "p";
         "-f"; "/backup.sql";
         Postgres.to_uri db |> Uri.to_string;
        ];
    ]
    |> on_node t |> run_genspio ~name:"backup_postgres";
    get_file_from_container t ~container_id
      ~container_path:"/backup.sql"
      ~node_path:("/tmp/" // Tmp.fresh_name "tmp-backup.sql")
      ~local_path:path;
    ()

  let restore_db_backup t ~path =
    let db, container =
      match t.db with
      | None ->
        ksprintf failwith "Deployment %S does not have a Postres database." t.name
      | Some db -> db, Postgres.container_name db
    in
    let open Genspio_edsl in
    let container_id =
      docker_compose_get_container_id t ~with_software:false ~container in
    let optionally_stop name ~container_name opt =
      begin match opt with
      | None -> sayf "Deployment does not have a %s." name
      | Some cb -> seq [
          sayf "Stopping the %s." name;
          docker_compose_command ~with_software:false t
            ["stop"; container_name cb];
        ]
      end |> run_genspio ~name:(sprintf "stop-%s" name) ~returns:0 in
    optionally_stop "coclobas-server" ~container_name:Coclobas.name t.coclobas;
    optionally_stop "ketrew-server" ~container_name:Ketrew_server.name t.ketrew;
    seq [
      sayf "Getting backup on the node as /tmp/backup.sql";
      cp_to_node t path "/tmp/backup.sql";
    ] |> run_genspio ~name:"push-backup-to-node" ~returns:0;
    seq [
      container_id#make;
      sayl "Getting backup on Postgres container (`%s`)."
        [container_id#get];
      call (
        (if use_sudo t then [string "sudo"] else [])
        @ [string "docker"]
        @ [
          string "cp";
          string "/tmp/backup.sql";
          string_concat [container_id#get; string ":";
                         string "/backup-to-restore.sql"];
        ]
      );
    ]
    |> on_node t |> run_genspio ~name:"put-backup-on-container" ~returns:0;
    seq [
      sayf "Running `pg_restore`";
      seq_succeeds_or ~clean_up:[fail] ~name:"pg-restore" [
        (*
          https://www.opsdash.com/blog/postgresql-backup-restore.html
          https://www.postgresql.org/docs/current/static/app-psql.html
        *)
        docker_compose_command ~with_software:false t
          ["exec"; container; "psql";
           "--no-psqlrc";
           "-v"; "ON_ERROR_STOP=1"; "--pset"; "pager=off";
           "--single-transaction";
           Postgres.to_uri db |> Uri.to_string;
           "-f"; "/backup-to-restore.sql";
          ];
      ];
    ]
    |> on_node t |> run_genspio ~name:"restore_postgres" ~returns:0;
    seq [
      docker_compose_command t ["up"; "-d"]
    ] |> on_node t |> run_genspio ~name:"recall-docker-compose-up" ~returns:0;
    ()

  let get_coclobas_logs t ~path =
    let coclo =
      Option.value_exn t.coclobas
        ~msg:"This deployment does not have a Coclobas server." in
    let open Genspio_edsl in
    let container_path = "/tmp" // Filename.basename path in
    let tar_cmd =
      seq [
        exec ["cd"; Coclobas.logs_path coclo];
        exec ["tar"; "cfz"; container_path; "."];
      ] in
    seq [
      sayf "Running `tar cvfz`";
      seq_succeeds_or ~clean_up:[fail] ~name:"tar-coclo-logs" [
        docker_compose_command ~with_software:false t
          ["exec"; Coclobas.name coclo;
           "bash"; "-c"; genspio_to_one_liner tar_cmd];
      ];
    ] |> on_node t |> run_genspio ~name:"tar-coclo-logs" ~returns:0;
    let container_id =
      docker_compose_get_container_id t ~with_software:false
        ~container:(Coclobas.name coclo) in
    get_file_from_container t ~container_id
      ~container_path
      ~node_path:container_path
      ~local_path:path;
    ()

  let get_ketrew_logs t ~path =
    let kserver =
      Option.value_exn t.ketrew
        ~msg:"This deployment does not have a Ketrew server." in
    let open Genspio_edsl in
    let container_path = "/tmp" // Filename.basename path in
    let tar_cmd =
      seq [
        exec ["cd"; Ketrew_server.logs_path kserver];
        exec ["sudo"; "chmod"; "-R"; "a+rw"; "."];
        exec ["tar"; "cfz"; container_path; "."];
      ] in
    seq [
      sayf "Running `tar cvfz`";
      seq_succeeds_or ~clean_up:[fail] ~name:"tar-kserver-logs" [
        docker_compose_command ~with_software:false t
          ["exec"; Ketrew_server.name kserver;
           "bash"; "-c"; genspio_to_one_liner tar_cmd];
      ];
    ] |> on_node t |> run_genspio ~name:"tar-kserver-logs" ~returns:0;
    let container_id =
      docker_compose_get_container_id t ~with_software:false
        ~container:(Ketrew_server.name kserver) in
    get_file_from_container t ~container_id
      ~container_path
      ~node_path:container_path
      ~local_path:path;
    ()

end

