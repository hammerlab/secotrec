open Common

module Configuration : sig
  type t
  type service
  val service :
    ?image: string ->
    ?ports: string list ->
    ?environment: (string * string) list ->
    ?command: string list ->
    ?privileged: bool ->
    ?volumes: string list ->
    ?raw_json:(string * Yojson.Safe.json) list ->
    string ->
    service
  val make : service list -> t
  val render : t -> string
end = struct
  type t = Yojson.Safe.json

  let string s =
    (* Docker-compose has a very annoying variable subtitution thing going on
       we need to escape the `$` characters: *)
    let b = Buffer.create 42 in
    String.iter s (function
      | '$' ->  Buffer.add_string b "$$"
      | other -> Buffer.add_char b other);
    `String (Buffer.contents b)

  type service = string * t
  let service
      ?image ?ports ?environment ?command ?privileged ?volumes
      ?raw_json name : service =
    let optl o f = Option.value_map o ~default:[] ~f in
    let strlist l = `List (List.map l ~f:(fun s -> string s)) in
    let srv =
      optl image (fun s -> ["image", string s])
      @ optl ports (fun l -> ["ports", strlist l])
      @ optl command (fun l -> ["command", strlist l])
      @ optl environment (fun l ->
          ["environment",
           strlist (List.map l ~f:(fun (n, v) -> sprintf "%s=%s" n v))])
      @ optl raw_json (fun l -> l)
      @ optl volumes (fun l -> ["volumes", strlist l])
      @ optl privileged (fun b -> ["privileged", `Bool b])
      @ []
    in
    (name, `Assoc srv)

  let make services : t =
    `Assoc [
      "version", `String "2";
      "services", `Assoc services;
    ]

  let render t = Yojson.Safe.pretty_to_string ~std:true t
end


let ensure_software =
  (* Cf. https://docs.docker.com/compose/install/ *)
  let open Genspio_edsl in
  ensure ~name:("Docker and Docker-compose")
    ((exec ["which"; "docker"] |> succeeds_silently)
     &&&
     (exec ["which"; "docker-compose"] |> succeeds_silently))
    [
      Apt.install ["docker.io"];
      exec ["sudo"; "bash"; "-c";
            "curl -L \
             \"https://github.com/docker/compose/releases/download/1.9.0/\
             docker-compose-$(uname -s)-$(uname -m)\" \
             -o /usr/local/bin/docker-compose"];
      exec ["sudo"; "chmod"; "755"; "/usr/local/bin/docker-compose"];
    ]

let make_command ?(with_software = true)
    ?save_output ~use_sudo ~compose_config ~tmp_dir cli =
  let docker_compose = tmp_dir // "docker-compose.json" in
  let open Genspio.EDSL in
  let with_saving c =
    Option.value_map save_output ~f:(fun path -> write_stdout ~path c)
      ~default:c in
  seq [
    (if with_software then ensure_software else nop);
    exec ["rm"; "-fr"; tmp_dir];
    exec ["mkdir"; "-p"; tmp_dir];
    string (Configuration.render compose_config)
    >> exec ["cat"] |> write_stdout ~path:(string docker_compose);
    exec ["cd"; tmp_dir]; (* maybe useless since we use `-f` below: *)
    exec (
      (if use_sudo then ["sudo"] else [])
      @ [ "docker-compose"; "-f"; docker_compose] @ cli
    ) |> with_saving;
  ]
