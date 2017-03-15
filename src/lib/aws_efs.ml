open Common

module Aws_cli = struct
  type t = {
    access_key: string [@env "AWS_KEY_ID"];
    (** AWS Access Key ID (looks like "AKIDEDJEIIDDENJNJDE435F"). *)

    secret_key: string [@env "AWS_SECRET_KEY"];
    (** AWS Secret Access Key (looks like "8fJIDe933900n45GTe9deiDJEIjj/deyRdiO90C"). *)

    default_region: string [@default "us-east-1"];
    (** AWS Configured default region. *)

  } [@@deriving make, cmdliner]

  let configure t =
    let open Genspio.EDSL in
    let set k v =
      exec ["aws"; "configure"; "set"; k; v] in
    seq [
      set "aws_access_key_id" t.access_key;
      set "aws_secret_access_key" t.secret_key;
      set "default.region" t.default_region;
    ]

end

let tr_remove_new_lines = Genspio.EDSL.exec ["tr"; "-d"; "\\n"]

type guess_value = [ `From_metadata | `Value of string ] [@@deriving yojson]

type t = {
  name: string [@main];
  guess_subnet: guess_value [@default `From_metadata];
  guess_secgroup: guess_value [@default `From_metadata];
} [@@deriving yojson, make]

module To_genspio = struct
  open Genspio_edsl

  let saylp t fmt l =
    sayl ("[EFS:%s] " ^ fmt) (string t.name :: l)

  let aws_efs more =
    call ([string "aws"; string "efs"] @ more)

  let aws_efs_strings more =
    aws_efs (List.map more ~f:string)

  (** Get the interesting contents a command returns something like
      ["\"129.32.23.11\"\n"]. *)
  let get_successful_single_string cmd ~or_else =
    output_as_string (cmd |> succeeds |> if_seq ~t:[] ~e:[or_else])
    >> exec ["tr"; "-d"; "\\n\""]
    |> output_as_string

  let aws_get_or_create t name ~get ~create =
    let tmp = tmp_file name in
    object (self)
      method fill =
        tmp#set (get_successful_single_string get ~or_else:fail);
      method fill_or_null =
        tmp#set (get_successful_single_string
                   (with_redirections get [to_file (int 2) (string "/dev/null")])
                   ~or_else:(exec ["printf";"%s\\n"; "null"]))
      method build =
        seq [
          saylp t "Checking %s" [string name];
          self#fill_or_null;
          if_seq (
            (tmp#get =$= string "null")
            ||| (tmp#get =$= string ""))
            ~t:[
              saylp t "Building %s" [string name];
              tmp#set (get_successful_single_string create ~or_else:(seq [
                  saylp t "Building %s failed" [string name];
                  fail
                ]));
              saylp t " -> %s was just created: %s" [string name; tmp#get];
            ]
            ~e:[
              saylp t " -> %s is already there: %s" [string name; tmp#get];
            ];
        ]
      method get = tmp#get
    end

  let get_or_create_file_system_id t =
    let make cmd query =
      aws_efs_strings [cmd; "--creation-token"; t.name; "--query"; query] in
    aws_get_or_create t "file-system-id"
      ~get:(make  "describe-file-systems" "FileSystems[0].FileSystemId")
      ~create:(make "create-file-system" "FileSystemId")

  let get_or_create_mount_target t ~fs_id ~subnet_id ~secgrp_id =
    let get =
      aws_efs [
        string "describe-mount-targets";
        string "--file-system-id"; fs_id;
        string "--output"; string "text";
        string "--query"; string "MountTargets[].MountTargetId";
      ] in
    let create =
      aws_efs [
        string "create-mount-target";
        string "--file-system-id"; fs_id;
        string "--subnet-id"; subnet_id;
        string "--security-groups"; secgrp_id;
        string "--query"; string "MountTargetId";
      ] in
    aws_get_or_create t "mount-target-id" ~get ~create

  let curl_metadata_item path =
    let uri =
      string_concat [string "http://169.254.169.254/latest/meta-data/"; path] in
    let tmp_err =
      let unique = Genspio.Language.to_one_liner path in
      ksprintf tmp_file "curl-error-%s" Digest.(string unique |> to_hex) in
    call [string "curl"; string "--stderr"; tmp_err#path; uri]
    ||> tr_remove_new_lines
    |> output_as_string


  let subnet_id ~aws_cli t =
    match t.guess_subnet with
    | `Value v -> string v
    | `From_metadata ->
      let macs_path = string "network/interfaces/macs/" in
      (* "http://169.254.169.254/latest/meta-data/network/interfaces/macs/" in *)
      let mac = curl_metadata_item macs_path in
      curl_metadata_item
      @@ string_concat [macs_path; mac; string "subnet-id"]

  let security_group ~aws_cli t =
    match t.guess_subnet with
    | `Value v -> string v
    | `From_metadata ->
      let name = curl_metadata_item @@ string "security-groups/" in
      call [
        string "aws"; string "ec2"; string "describe-security-groups";
        string "--group-names"; name;
        string "--query"; string "SecurityGroups[0].GroupId";
      ]
      |> get_successful_single_string ~or_else:fail

  let mount_point t = ksprintf string "/mnt-%s" t.name

  let ensure_nfs_traffic_in_security_group t ~security_group =
    seq [
      saylp t "Authorizing :2049 traffic within group %s" [security_group];
      begin
        let tmp_err = tmp_file "asgi-error" in
        if_seq (
          with_redirections
            (call [
                string "aws"; string "ec2"; string "authorize-security-group-ingress";
                string "--group-id"; security_group;
                string "--protocol"; string "tcp"; string "--port"; string "2049";
                string "--source-group"; security_group;
              ]) [
            to_file (int 2) tmp_err#path;
          ]
          |> succeeds
        )
          ~t:[saylp t " -> NFSv4 traffic authorized" []]
          ~e:[
            if_seq (call [string "grep"; string "InvalidPermission.Duplicate";
                          tmp_err#path] |> succeeds_silently)
              ~t:[saylp t " -> NFSv4 traffic was already authorized" []]
              ~e:[
                saylp t "ERROR while Authorizing NFSv4 traffic:" [];
                call [string "cat"; tmp_err#path];
                fail;
              ]
          ]
      end;
    ]


  let wait_for_mount_target_available t ~mount_target_id =
    seq [
      saylp t "Waiting for mount-target to be really available." [];
      seq_succeeds_or
        ~name:"Waiting-for-mount-target"
        ~silent:false
        ~clean_up:[fail] [
        loop_until_ok
          (
            (aws_efs [
                string "describe-mount-targets";
                string "--mount-target-id"; mount_target_id;
                string "--output"; string "text";
                string "--query"; string "MountTargets[].LifeCycleState";
              ]
             ||> tr_remove_new_lines
             |> output_as_string)
            =$= string "available")
          ~attempts:40
          ~sleep:4;
      ]
    ]

  let mount t ~mount_target_id =
    let mt_ip_address =
      aws_efs [
        string "describe-mount-targets";
        string "--mount-target-id"; mount_target_id;
        string "--output"; string "text";
        string "--query"; string "MountTargets[].IpAddress";
      ]
      ||> tr_remove_new_lines
      |> output_as_string in
    seq [
      (* sayl "IP Address to mount: %s" [mt_ip_address]; *)
      call [string "sudo"; string "mkdir"; string "-p"; mount_point t];
      if_seq (
        exec ["mount"] ||> call [string "grep"; mt_ip_address]
        ||> call [string "grep"; mount_point t]
        |> succeeds_silently
      )
        ~t:[
          saylp t "%s already mounted at %s:" [
            string_concat [mt_ip_address; string ":/"];
            mount_point t;
          ];
          output_markdown_code "" (
            exec ["mount"] ||> call [string "grep"; mt_ip_address]
            ||> call [string "grep"; mount_point t]
          );
        ]
        ~e:[
          saylp t "Mounting %s at %s" [
            string_concat [mt_ip_address; string ":/"];
            mount_point t;
          ];
          call [
            string "sudo"; string "mount"; string "-t"; string "nfs4";
            (* Options from:
               http://docs.aws.amazon.com/efs/latest/ug/mounting-fs-mount-cmd-ip-addr.html *)
            string "-o"; string "nfsvers=4.1,rsize=1048576,wsize=1048576,\
                                 hard,timeo=600,retrans=2";
            string_concat [mt_ip_address; string ":/"];
            mount_point t;
          ];
        ]
    ]


  let ensure ~aws_cli t =
    let file_system_id = get_or_create_file_system_id t in
    let mount_target_id =
      get_or_create_mount_target t ~fs_id:file_system_id#get
        ~subnet_id:(subnet_id ~aws_cli t)
        ~secgrp_id:(security_group ~aws_cli t) in
    seq_succeeds_or ~silent:false ~name:(sprintf "Ensure-EFS-%s" t.name)
      ~clean_up:[fail] [
      Aws_cli.configure aws_cli;
      file_system_id#build;
      (* sayl "EFS-%s: File-system-ID: %s" [string t.name; file_system_id#get]; *)
      saylp t "Using: Subnet: %s, Secgrp: %s" [
        subnet_id ~aws_cli t;
        security_group ~aws_cli t;
      ];
      mount_target_id#build;
      ensure_nfs_traffic_in_security_group t
        ~security_group:(security_group ~aws_cli t);
      wait_for_mount_target_available t ~mount_target_id:mount_target_id#get;
      mount t ~mount_target_id:mount_target_id#get;
    ]

  let describe ~aws_cli t =
    let file_system_id = get_or_create_file_system_id t in
    seq [
      file_system_id#fill;
      if_seq (file_system_id#get =$= string "null")
        ~t:[
          saylp t "File-system-id not available; \
                   list of all visible file-systems:" [];
          output_markdown_code "" begin
            aws_efs [string "describe-file-systems";
                     string "--output"; string "text"];
          end;
        ]
        ~e:[
          saylp t "File-system-id: %s:" [file_system_id#get];
          output_markdown_code "json" begin
            aws_efs [string "describe-file-systems";
                     string "--file-system-id"; file_system_id#get;
                     string "--output"; string "json"];
          end;
          saylp t "Mount-Targets:" [];
          output_markdown_code "" begin
            aws_efs [string "describe-mount-targets";
                     string "--file-system-id"; file_system_id#get;
                     string "--output"; string "text"];
          end;
        ];
      saylp t "Local-mount:" [];
      output_markdown_code "" begin
        exec ["mount"] ||> call [string "grep"; mount_point t];
      end;
      saylp t "Done." [];
    ]

  let destroy ~aws_cli t =
    let file_system_id = get_or_create_file_system_id t in
    let mount_target_id =
      get_or_create_mount_target t ~fs_id:file_system_id#get
        ~subnet_id:(subnet_id ~aws_cli t)
        ~secgrp_id:(security_group ~aws_cli t) in
    let fs_id = file_system_id#get in
    let mt_id = mount_target_id#get in
    let number_of_mount_targets =
      get_successful_single_string
        (aws_efs_strings ["describe-file-systems";
                          "--creation-token"; t.name;
                          "--query"; "FileSystems[0].NumberOfMountTargets"])
        ~or_else:(fail)
    in
    seq [
      file_system_id#fill;
      mount_target_id#fill_or_null;
      saylp t "File-system-ID: %s, Mount-Target-ID: %s" [fs_id; mt_id];
      saylp t "Unmounting `%s`..." [mount_point t];
      output_markdown_code "" begin
        call [string "sudo"; string "umount";
              string "-f"; string "-l"; mount_point t];
      end;
      if_seq ((mt_id <$> string "") &&& (mt_id <$> string "null"))
        ~t:[
          sayf "Deleting mount-target.";
          aws_efs [string "delete-mount-target";
                   string "--mount-target-id"; mt_id];
        ];
      sayl "Waiting for the FS to not be “in use”: `%s` user(s) now..." [
        number_of_mount_targets;
      ];
      seq_succeeds_or
        ~name:"Waiting-for-file-system-to-notice-deletion"
        ~silent:false
        ~clean_up:[fail] [
        loop_until_ok
          ((number_of_mount_targets =$= string "0")
           ||| (number_of_mount_targets =$= string "null"))
          ~attempts:40
          ~sleep:4;
      ];
      if_seq (fs_id <$> string "null")
        ~t:[
          sayl "EFS-%s: Deleting file-system." [string t.name];
          aws_efs [string "delete-file-system";
                   string "--file-system-id"; fs_id];
        ]
        ~e:[
          sayl "EFS-%s: Already deleted." [string t.name];
        ];
    ]
end
