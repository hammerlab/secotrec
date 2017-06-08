open Common
    
type t = {
  access_key: string [@env "AWS_KEY_ID"];
  (** AWS Access Key ID (looks like "AKIDEDJEIIDDENJNJDE435F"). *)

  secret_key: string [@env "AWS_SECRET_KEY"];
  (** AWS Secret Access Key (looks like "8fJIDe933900n45GTe9deiDJEIjj/deyRdiO90C"). *)

  default_region: string [@default "us-east-1"];
  (** AWS Configured default region. *)

} [@@deriving make, cmdliner]

let guess () =
  let access_key = env_exn "AWS_KEY_ID" in
  let secret_key = env_exn "AWS_SECRET_KEY" in
  make ~access_key ~secret_key ()


let configure t =
  let open Genspio_edsl in
  let set k v =
    exec ["aws"; "configure"; "set"; k; v] in
  seq_succeeds_or
    ~silent:true
    ~name:"Aws-cli-configuration"
    ~clean_up:[fail] [
      set "aws_access_key_id" t.access_key;
      set "aws_secret_access_key" t.secret_key;
      set "default.region" t.default_region;
    ]
