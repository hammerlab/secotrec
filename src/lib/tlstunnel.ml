open Common

type t = {
  image: string [@default "hammerlab/keredofi:coclobas-gke-biokepi-dev"];
  backend_address: string option;
  backend_port: int;
  frontend_address: string option;
  frontend_port: int;
  certificate: [ `Fake | `Mount of (string * string * string)] [@default `Fake];
  name: string [@main ];
} [@@deriving make]

let to_service t =
  let shell_cmd =
    let open Genspio_edsl in
    let cert, key =
      match t.certificate with
      | `Fake -> "_fake_tls/certificate.pem", "_fake_tls/privkey-nopass.pem"
      | `Mount (_, cert, key) -> cert, key
    in
    seq [
      begin if t.certificate = `Fake then
          exec [
            "ketrew"; "init"; "--config"; "_fake_tls";
            "--self-signed";
            "--use-database"; "/nope"]
        else nop
      end;
      exec [
        "tlstunnel";
        "--cert"; cert;
        "--key"; key;
        "--backend";
        sprintf "%s:%d"
          (Option.value ~default:"" t.backend_address) t.backend_port;
        "--frontend";
        sprintf "%s:%d"
          (Option.value ~default:"" t.frontend_address) t.frontend_port;
      ]
    ]
  in
   Docker_compose.Configuration.service t.name
    ~image:t.image
    ~ports:[sprintf "443:%d" t.frontend_port]
    ~command:["sh"; "-c"; genspio_to_one_liner ~name:"tlstun-compose" shell_cmd]
    ~volumes:(
      match t.certificate with
      | `Fake -> []
      | `Mount (dir, _, _) -> [sprintf "%s:%s" dir dir]
    )
