open Common

module Configuration : sig
  type t
  type service
  val service :
    ?image: string ->
    ?ports: string list ->
    ?environment: (string * string) list ->
    ?start_up_script: string ->
    ?privileged: bool ->
    ?volumes: string list ->
    ?raw_json:(string * Yojson.Safe.json) list ->
    string ->
    service
  val make : service list -> t
  val render : t ->
    [ `Configuration of string | `File_relative of string * string ] list
end = struct

  let string s =
    (* Docker-compose has a very annoying variable subtitution thing going on
       we need to escape the `$` characters: *)
    let b = Buffer.create 42 in
    String.iter s (function
      | '$' ->  Buffer.add_string b "$$"
      | other -> Buffer.add_char b other);
    `String (Buffer.contents b)

  type local_file = {
    path: string;
    content: string;
  } [@@deriving make]
  type service = {
    configuration: Yojson.Safe.json;
    script: local_file option;
    name: string [@main];
  } [@@deriving make]
  let service
      ?image ?ports ?environment ?start_up_script ?privileged ?volumes
      ?raw_json name : service =
    let optl o f = Option.value_map o ~default:[] ~f in
    let strlist l = `List (List.map l ~f:(fun s -> string s)) in
    let script, all_volumes =
      match start_up_script with
      | Some s ->
        let script_filename =
          (* Hashing the content makes docker-compose notice when it changed
             and hence it recreates the service. *)
          sprintf "secotrec-service-%s-%s.sh" name Digest.(string s |> to_hex)
        in
        let script_local_path = "." // script_filename in
        let script_mount_path = "/" // script_filename in
        let script = make_local_file ~path:script_local_path ~content:s in
        Some script,
        Some (sprintf "%s:%s" script_local_path script_mount_path ::
              Option.value ~default:[] volumes)
      | None -> None, volumes
    in
    let srv =
      optl image (fun s -> ["image", string s])
      @ optl ports (fun l -> ["ports", strlist l])
      @ Option.value_map ~default:[] script
        ~f:(fun s -> ["command", strlist ["sh"; "/" // s.path]])
      @ optl environment (fun l ->
          ["environment",
           strlist (List.map l ~f:(fun (n, v) -> sprintf "%s=%s" n v))])
      @ optl raw_json (fun l -> l)
      @ optl all_volumes (fun l -> ["volumes", strlist l])
      @ optl privileged (fun b -> ["privileged", `Bool b])
      @ []
    in
    make_service name ~configuration:(`Assoc srv)
      ?script

  type t = {
    services: service list [@main];
  } [@@deriving make]

  let render t =
    let json =
      `Assoc [
        "version", `String "2";
        "services", `Assoc (List.map t.services ~f:(fun s ->
            s.name, s.configuration));
      ]
    in
    `Configuration (Yojson.Safe.pretty_to_string ~std:true json)
    ::
    List.filter_map t.services (fun s ->
        Option.map s.script (fun s ->
            `File_relative (s.path, s.content)))

end

let yum_docker_ec2 = Genspio_edsl.[
  exec ["sudo"; "yum"; "update"; "-y"; "";];
  exec ["sudo"; "yum"; "install"; "-y"; "docker";];
  exec ["sudo"; "service"; "docker"; "start";];
  exec ["sudo"; "usermod"; "-a"; "-G"; "docker"; "ec2-user";];
  (* exec ["docker"; "info";]; *)
]

let ensure_software =
  (* Cf. https://docs.docker.com/compose/install/ *)
  let open Genspio_edsl in
  let which exe = exec ["which"; exe] |> succeeds_silently in
  ensure ~name:("Docker and Docker-compose")
    (which "docker" &&& which "docker-compose")
    [
      switch [
        case (which "apt-get") [
          Apt.install ["docker.io"];
        ];
        case (which "yum") [
          seq_succeeds_or ~clean_up:[fail]
            ~name:"yum-docker" ~silent:false yum_docker_ec2;
        ];
        default [
          sayf "No APT, no Yum, don't know how to get Docker, \
                hoping for the best.";
        ];
      ];
      exec ["sudo"; "bash"; "-c";
            "curl -L \
             \"https://github.com/docker/compose/releases/download/1.9.0/\
             docker-compose-$(uname -s)-$(uname -m)\" \
             -o /usr/local/bin/docker-compose"];
      exec ["sudo"; "chmod"; "755"; "/usr/local/bin/docker-compose"];
    ]

let make_command ?(with_software = true)
    ?(docker_compose_exec = "/usr/local/bin/docker-compose")
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
    seq begin List.map (Configuration.render compose_config) ~f:(function
      | `Configuration cc ->
        string cc >> exec ["cat"] |> write_stdout ~path:(string docker_compose);
      | `File_relative (relpath, content) ->
        let path = tmp_dir // relpath in
        string content >> exec ["cat"] |> write_stdout ~path:(string path);
      )
    end;
    exec ["cd"; tmp_dir]; (* maybe useless since we use `-f` below: *)
    exec (
      (if use_sudo then ["sudo"] else [])
      @ [ docker_compose_exec; "-f"; docker_compose] @ cli
    ) |> with_saving;
  ]
