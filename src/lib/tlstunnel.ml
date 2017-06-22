open Common

type t = {
  image: string [@default "hammerlab/keredofi:tlstunnel"];
  backend_address: string option;
  backend_port: int;
  frontend_address: string option;
  frontend_port: int [@default 8443];
  exposed_port: int [@default 443];
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
    seq begin
      (if t.certificate = `Fake then [
          exec ["mkdir"; "-p"; "_fake_tls"];
          exec [
            "openssl"; "req"; "-x509"; "-newkey"; "rsa:2048";
            "-keyout"; key;
            "-out"; cert;
            "-days"; "100";
            "-nodes"; "-subj"; "/CN=secotrec";
          ]
        ]
      (* "ketrew"; "init"; "--config"; "_fake_tls"; *)
      (* "--self-signed"; *)
      (* "--use-database"; "/nope"] *)
       else [])
      @ [
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
        ];
      ]
    end
  in
  Docker_compose.Configuration.service t.name
    ~image:t.image
    ~ports:[sprintf "%d:%d" t.exposed_port t.frontend_port]
    ~start_up_script:(Genspio.Language.to_many_lines shell_cmd)
    ~volumes:(
      match t.certificate with
      | `Fake -> []
      | `Mount (dir, _, _) -> [sprintf "%s:%s" dir dir]
    )
