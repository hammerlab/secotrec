open Common

(** One {!Cmdliner} hack found in Opam codebase to create command aliases. *)
let make_command_alias cmd ?(options="") name =
  let open Cmdliner in
  let term, info = cmd in
  let orig = Term.name info in
  let doc = Printf.sprintf "An alias for $(b,%s%s)." orig options in
  let man = [
    `S "DESCRIPTION";
    `P (Printf.sprintf "$(b,$(mname) %s) is an alias for $(b,$(mname) %s%s)."
          name orig options);
    `P (Printf.sprintf "See $(b,$(mname) %s --help) for details."
          orig);
  ] in
  (term, Term.info name ~docs:"SOME COMMAND ALIASES" ~doc ~man)

let sub_command ~info ~term = (term, info)

let version = "0.0.0"

let argless_sub_command cname ~doc f =
  let open Cmdliner in
  sub_command
    ~info:Term.(info cname ~version ~sdocs:"COMMON OPTIONS" ~man:[] ~doc)
    ~term: Term.(pure f $ pure ())

let user_info_term =
  let open Cmdliner in
  let open Term in
  pure (fun user passwd ->
      match user, passwd with
      | None, None -> None
      | u, p ->
        let opt n o =
          Option.value_exn o ~msg:(sprintf "Missing %s" n) in
        Some (sprintf "%s:%s" (opt "user" u) (opt "password" p)))
  $ Arg.(
      value
      @@ opt (some string) None
      @@ info ["username"; "U"]
        ~doc:"HTTP basic-auth username"
    )
  $ Arg.(
      value
      @@ opt (some string) None
      @@ info ["password"; "P"]
        ~doc:"HTTP basic-auth password"
    )

let sub_command_with_user_info cname ~doc f =
  let open Cmdliner in
  sub_command
    ~info:Term.(info cname ~version ~sdocs:"COMMON OPTIONS" ~man:[] ~doc)
    ~term: Term.(
        pure f
        $ user_info_term
      )

let configuration_commands configuration =
  let open Cmdliner in
  let gen_config =
    sub_command
      ~info:Term.(info "generate-configuration"
                    ~version ~sdocs:"COMMON OPTIONS" ~man:[]
                    ~doc:"Generate a documented configuration file template.")
      ~term: Term.(
          pure begin fun path  ->
            Configuration_dot_env.generate configuration ~path
          end
          $ Arg.(required
                 @@ pos 0 (some string) None
                 @@ info [] ~docv:"PATH"
                   ~doc:"Path of the configuration file.")
        ) in
  let print_config =
    argless_sub_command "print-configuration"
      ~doc:"Print the current configuration." begin fun () ->
      Configuration_dot_env.display configuration
    end in
  [
    gen_config;
    make_command_alias gen_config "genconf";
    print_config;
    make_command_alias print_config "pc";
  ]

let deployment_commands (deployment : unit -> Deployment.t) =
  let open Cmdliner in
  let deployment_arg =
    Term.(pure (fun () ->
        match deployment () with
        | d -> d
        | exception e ->
          eprintf "ERROR: Deployment not ready:\n  %s\n\
                   Maybe you should try the `print-configuration` sub-command?\
                   \n\n%!"
            (match e with
            | Failure s -> s
            | other -> Printexc.to_string other);
          failwith "Wrong command line")
          $ pure ())
  in
  let argless_sub_command cname ~doc f =
    let open Cmdliner in
    sub_command
      ~info:Term.(info cname ~version ~sdocs:"COMMON OPTIONS" ~man:[] ~doc)
      ~term: Term.(pure f $ deployment_arg)
  in
  let argall_sub_command cname ~doc f =
    sub_command
      ~info:Term.(info cname ~version ~sdocs:"COMMON OPTIONS" ~man:[] ~doc)
      ~term: Term.(
          pure begin fun deployment more ->
            f deployment more
          end
          $ deployment_arg
          $ Arg.(value
                 @@ pos_all string []
                 @@ info []
                   ~doc:"Command line arguments")
        ) in
  let docker_compose =
    argall_sub_command "docker-compose" ~doc:"Run docker-compose commands."
      Deployment.Run.docker_compose in
  let up =
    argless_sub_command "up" ~doc:"Bring the deployment UP." Deployment.Run.up
  in
  let down =
    argless_sub_command "down" ~doc:"Bring the deployment DOWN."
      Deployment.Run.down in
  let status =
    argless_sub_command "status" ~doc:"Get the status of the deployment."
      Deployment.Run.status in
  let top =
    argless_sub_command "top" ~doc:"Display docker's `top`."
      Deployment.Run.top in
  let psql =
    argall_sub_command "psql" ~doc:"Run `psql` on the deployments DB."
      (fun depl args -> Deployment.Run.psql depl ~args) in
  let coclobas_client =
    argall_sub_command "coclobas-client" ~doc:"Run coclobas client commands."
      (fun depl args ->
         Deployment.Run.coclobas_client depl ~args) in
  let ketrew_configuration =
    sub_command
      ~info:Term.(info "ketrew-configuration"
                    ~version ~sdocs:"COMMON OPTIONS" ~man:[]
                    ~doc:"Generate a Ketrew configuration directory.")
      ~term: Term.(
          pure (fun deployment userinfo path ->
              Deployment.Run.Generate.ketrew_configuration deployment
                ?userinfo ~path)
          $ deployment_arg
          $ user_info_term
          $ Arg.(required
                 @@ pos 0 (some string) None
                 @@ info [] ~docv:"PATH"
                   ~doc:"Path to the configuration directory.")
        ) in
  let biokepi_machine =
    sub_command
      ~info:Term.(info "biokepi-machine"
                    ~version ~sdocs:"COMMON OPTIONS" ~man:[]
                    ~doc:"Generate a `Biokepi.Machine.t` script.")
      ~term: Term.(
          pure (fun deployment with_script_header path ->
              Deployment.Run.Generate.biokepi_machine deployment
                ~with_script_header ~path)
          $ deployment_arg
          $ Arg.(value
                 @@ opt bool true
                 @@ info ["with-script-header"]
                   ~doc:"Generate an ocaml “script” with top-level \
                         directives.")
          $ Arg.(required
                 @@ pos 0 (some string) None
                 @@ info [] ~docv:"PATH"
                   ~doc:"Path to the .ml file.")
        ) in
  let useful_env =
    sub_command
      ~info:Term.(info "environment-varirables"
                    ~version ~sdocs:"COMMON OPTIONS" ~man:[]
                    ~doc:"Generate some useful environment variables.")
      ~term: Term.(
          pure (fun deployment path ->
              Deployment.Run.Generate.useful_env ?path deployment)
          $ deployment_arg
          $ Arg.(value
                 @@ pos 0 (some string) None
                 @@ info [] ~docv:"PATH"
                   ~doc:"Path to the configuration directory.")
        ) in
  let docker_compose_config =
    sub_command
      ~info:Term.(info "compose-configuration"
                    ~version ~sdocs:"COMMON OPTIONS" ~man:[]
                    ~doc:"Generate a confguration file for Docker-compose.")
      ~term: Term.(
          pure (fun deployment path ->
              Deployment.Run.Generate.docker_compose_configuration
                ?path deployment)
          $ deployment_arg
          $ Arg.(value
                 @@ pos 0 (some string) None
                 @@ info [] ~docv:"PATH"
                   ~doc:"Path to the configuration file.")
        ) in
  let preparation_workflow =
    sub_command
      ~info:Term.(info "prepare"
                    ~version ~sdocs:"COMMON OPTIONS" ~man:[]
                    ~doc:"Submit the preparation workflow.")
      ~term: Term.(
          pure (fun deployment userinfo ->
              Deployment.Run.prepare ?userinfo deployment)
          $ deployment_arg
          $ user_info_term
        ) in
  let test_biokepi_machine =
    sub_command
      ~info:Term.(info "test-biokepi-machine"
                    ~version ~sdocs:"COMMON OPTIONS" ~man:[]
                    ~doc:"Submit the test workflow.")
      ~term: Term.(
          pure (fun deployment userinfo ->
              Deployment.Run.test_biokepi_machine ?userinfo deployment)
          $ deployment_arg
          $ user_info_term
        ) in
  let backup_database =
    sub_command
      ~info:Term.(info "backup-database"
                    ~version ~sdocs:"COMMON OPTIONS" ~man:[]
                    ~doc:"Create a backup of the postgresql DB.")
      ~term: Term.(
          pure (fun deployment path ->
              Deployment.Run.backup_postgres deployment ~path)
          $ deployment_arg
          $ Arg.(required
                 @@ pos 0 (some string) None
                 @@ info [] ~docv:"PATH"
                   ~doc:"Path of the backup file to write \
                         (`.sql` extension recommended).")
        ) in
  let restore_database_backup =
    sub_command
      ~info:Term.(info "restore-database-backup"
                    ~version ~sdocs:"COMMON OPTIONS" ~man:[]
                    ~doc:"Restore a given backup into the postgresql DB.")
      ~term: Term.(
          pure (fun deployment path ->
              Deployment.Run.restore_db_backup deployment ~path)
          $ deployment_arg
          $ Arg.(required
                 @@ pos 0 (some string) None
                 @@ info [] ~docv:"PATH"
                   ~doc:"Path to the backup file (should be a `.sql`).")
        ) in
  let coclobas_logs =
    sub_command
      ~info:Term.(info "get-coclobas-logs"
                    ~version ~sdocs:"COMMON OPTIONS" ~man:[]
                    ~doc:"Get a snapshot of the Coclobas server's logs.")
      ~term: Term.(
          pure (fun deployment path ->
              Deployment.Run.get_coclobas_logs deployment ~path)
          $ deployment_arg
          $ Arg.(required
                 @@ pos 0 (some string) None
                 @@ info [] ~docv:"PATH"
                   ~doc:"Path to the tar-gz file.")
        ) in
  let ketrew_logs =
    sub_command
      ~info:Term.(info "get-ketrew-logs"
                    ~version ~sdocs:"COMMON OPTIONS" ~man:[]
                    ~doc:"Get a snapshot of the Ketrew server's logs.")
      ~term: Term.(
          pure (fun deployment path ->
              Deployment.Run.get_ketrew_logs deployment ~path)
          $ deployment_arg
          $ Arg.(required
                 @@ pos 0 (some string) None
                 @@ info [] ~docv:"PATH"
                   ~doc:"Path to the tar-gz file.")
        ) in
  [
    docker_compose; make_command_alias docker_compose "dc";
    up; down; status; top; psql;
    coclobas_client; make_command_alias coclobas_client "cc";
    ketrew_configuration; biokepi_machine; useful_env;
    docker_compose_config;
    preparation_workflow; test_biokepi_machine;
    backup_database; restore_database_backup;
    coclobas_logs; ketrew_logs;
  ]

let run ?configuration (deployment : unit -> Deployment.t) =
  let open Cmdliner in
  let configuration_commands =
    Option.value_map configuration ~f:configuration_commands ~default:[] in
  let deployment_commands = deployment_commands deployment in
  let cmds = configuration_commands @ deployment_commands in
  let default_cmd =
    let doc =
      sprintf "The %S secotrec deployment's commands."
        (try Deployment.name (deployment ()) with _ -> "not-yet-defined") in
    let man = [
      `S "AUTHORS";
      `P "Sebastien Mondet <seb@mondet.org>"; `Noblank;
      `S "BUGS";
      `P "Browse and report new issues at"; `Noblank;
      `P "<https://github.com/hammerlab/secotrec-private>.";
    ] in
    Term.(ret (pure (`Help (`Plain, None)))),
    Term.info Sys.argv.(0) ~version ~doc ~man in
  match Term.eval_choice default_cmd cmds with
  | `Ok f -> f
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0


