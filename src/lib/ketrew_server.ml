
open Common


type t = {
  auth_token: string;
  db: Postgres.t;
  image: string [@default "hammerlab/keredofi:coclobas-gke-biokepi-dev"];
  nfs_mounts: Nfs.Mount.t list;
  port: int [@default 8080 ];
  local_volumes: (string * string) list;
  opam_pin: Opam.Pin.t list;
  ketrew_debug_functions: string list;
  name: string [@main ];
} [@@deriving make]

let name t = t.name

let tmpdir = "/tmp/cocloketrew"

let logs_path (t : t) =
  tmpdir // "logs/"

let to_service t =
  let config =
    let open Ketrew.Configuration in
    let engine =
      let database_parameters = Postgres.to_uri t.db |> Uri.to_string in
      engine ~database_parameters () in
    let prof =
      server
        ~engine
        ~authorized_tokens:[
          authorized_token ~name:"From-env" t.auth_token;
        ]
        ~return_error_messages:true
        ~log_path:(logs_path t)
        ~command_pipe:(tmpdir // "command.pipe")
        (`Tcp 8080)
      |> create
    in
    to_json [profile "default" prof]
  in
  let config_path = tmpdir // "config.json" in
  let shell_cmd =
    let open Genspio_edsl in
    seq [
      exec ["umask"; "0000"]; (* We're going to deal with ≥ 1 users. *)
      exec ["mkdir"; "-p"; tmpdir];
      exec ["sudo"; "chmod"; "-R"; "777"; tmpdir];
      Opam.Pin.list_to_command t.opam_pin;
      Postgres.wait_for t.db;
      seq (List.map t.nfs_mounts ~f:Nfs.Mount.ensure_mounted);
      begin
        string config >> exec ["cat"]
        |> write_output ~stdout:(string config_path)
      end;
      (* setenv (string "KETREW_CONFIGURATION") (string config_path); *)
      (* setenv (string "KETREW_BINARY") *)
      (*   (exec ["which"; "colobas-ketrew"] |> output_as_string); *)
      cat_markdown (string config_path) "json";
      exec ["which"; "coclobas-ketrew"];
      exec ["ls"; "-la"; config_path];
      exec ["ls"; "-la"; "/tmp"];
      exec ["ls"; "-la"; "/tmp/ketrew"];
      exec ["umask"];
      exec ["bash"; "-c";
            "echo '/tmp/core_%e.%p' | sudo tee /proc/sys/kernel/core_pattern"];
      call [
        string "sudo"; string "su"; string "biokepi"; string "-c";
        string_concat [
          string "umask 0000 ; ";
          ksprintf string "debug_log_functions=%s "
            (String.concat ~sep:"," t.ketrew_debug_functions);
          (exec ["which"; "coclobas-ketrew"] |> output_as_string
           >> exec ["tr"; "-d"; "\\n"] |> output_as_string);
          string " start-server -C ";
          string config_path;
        ]
      ];
    ] in
  Docker_compose.Configuration.service t.name
    ~image:t.image
    ~ports:[sprintf "%d:8080" t.port]
    ~command:["dash"; "-c";
              genspio_to_one_liner ~name:"kserver-compose" shell_cmd]
    ~volumes:(List.map t.local_volumes ~f:(fun (a, b) -> sprintf "%s:%s" a b))
    ~environment:[
      (* "DB_URI", Postgres.to_uri t.db |> Uri.to_string; *)
      (* "PORT", "8443"; *)
      (* "AUTH_TOKEN", t.auth_token; *)
    ]
    ~privileged:true (* Necessary to mount NFS volumes *)
(* ~raw_json:[ *)
(*   "privileged", `Bool true; *)
(* ] *)

let all_nfs_witnesses t =
  List.map t.nfs_mounts ~f:(fun t ->
      Nfs.Mount.(mount_point t // witness t))

(** This function assumes that with [`External address] we have a TLS
    tunnel on port 443 in fron of Ketrew's server. *)
let make_url ?userinfo t ~service how =
  let open Genspio_edsl in
  let user_at = Option.value_map userinfo ~default:"" ~f:(sprintf "%s@") in
  let domain =
    match how with
    | `Local ->
      [ksprintf string "http://%s127.0.0.1:%d" user_at t.port]
    | `External address ->
      [ksprintf string "https://%s" user_at; address]
  in
  let query =
    match service with
    | `Hello -> [string "/hello"]
    | `Api ->
      [ksprintf string "/api?token=%s" t.auth_token]
    | `Gui ->
      [ksprintf string "/gui?token=%s" t.auth_token]
  in
  string_concat (domain @ query)
