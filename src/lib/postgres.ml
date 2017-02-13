open Common

type t = Uri.t

let of_uri u : t = u
let to_uri u = u

let container_name t = "pg"

let to_service u =
  Docker_compose.Configuration.service "pg"
    ~image:"postgres" ~ports:["5432:5432"]

let wait_for u =
  let open Genspio_edsl in
  (* Cf. https://docs.docker.com/compose/startup-order/ *)
  seq [
    exec ["sudo"; "apt"; "install"; "--yes"; "postgresql-client"];
    seq_succeeds_or
      ~name:"Waiting-for-postgres"
      ~clean_up:[fail] [
      loop_until_ok
        (exec ["psql"; "-h"; "pg"; "-U"; "postgres"; "-c"; "\\l"]
         |> succeeds)
        ~attempts:40
        ~sleep:2;
    ]
  ]
