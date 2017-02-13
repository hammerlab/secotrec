open Common

type t = <
  certificate : string;
  ensure :
    before_generation:unit Genspio_edsl.t ->
    unit Genspio_edsl.t;
  path : string;
  private_key : string;
>

let make ~dns_name ~email : t =
  let open Genspio_edsl in
  let cert =
    sprintf "/etc/letsencrypt/live/%s/cert.pem" dns_name in
  let key =
    sprintf "/etc/letsencrypt/live/%s/privkey.pem" dns_name in
  let should_succeed name = seq_succeeds_or ~name ~clean_up:[fail] in
  let ensure ~before_generation =
    seq [
      ensure ~name:"Letsencrypt-ceriticates"
        (file_exists (string cert) &&& file_exists (string key)) [
        should_succeed "Installing-letsencrypt" [
          exec ["sudo"; "apt-get"; "install"; "--yes"; "letsencrypt"];
        ];
        before_generation;
        should_succeed "Running-letsencrypt" [
          exec ["sudo"; "letsencrypt"; "certonly"; "--standalone";
                "-d"; dns_name;
                "--cert-path"; cert;
                "--key-path"; key;
                "--keep-until-expiring"; "--agree-tos";
                "--email"; email
               ];
        ];
        exec ["sudo"; "chmod"; "-R"; "a+rx"; "/etc/letsencrypt"];
        exec ["ls"; "-la"; cert; key];
      ]
    ]
  in
  object
    method certificate = cert
    method private_key = key
    method path = "/etc/letsencrypt"
    method ensure ~before_generation = ensure ~before_generation
  end
