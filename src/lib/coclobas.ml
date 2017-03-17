open Common

type t = {
  name:string [@default "coclo"];
  opam_pin: Opam.Pin.t list;
  root: string [@default "/tmp/cocloroot"];
  port: int [@default 8082];
  tmp_dir: string [@default "/tmp/coclosecolocal"];
  db:Uri.t;
  image: string [@default "hammerlab/keredofi:coclobas-gke-biokepi-default"];
  cluster: 
    [ `GKE of Gke_cluster.t | `Local of int ] [@main ];
} [@@deriving make]

let cluster t = t.cluster
let name t = t.name
let tmp_dir t = t.tmp_dir
let base_url t =
  sprintf "http://%s:%d" t.name t.port
let url_for_local_client t =
  sprintf "http://127.0.0.1:%d" t.port

let root t = t.root
let logs_path t = t.root // "logs"

let to_service t =
  let cluster_options =
    match t.cluster with
    | `GKE kube -> [
        "--cluster-kind"; "gke";
        "--gke-cluster-name"; Gke_cluster.name kube;
        "--gcloud-zone"; Gke_cluster.zone kube;
        "--max-nodes";
        Gke_cluster.max_nodes kube |> Int.to_string;
      ]
    | `Local max_nodes -> [
        "--cluster-kind"; "local-docker";
        "--max-nodes"; max_nodes |> Int.to_string
      ] in
  let additional_setup =
    let open Genspio_edsl in
    match t.cluster with
    | `GKE kube -> [
        setenv (string "PATH")
          (string_concat [
              getenv (string "PATH");
              string ":";
              string "/home/opam/google-cloud-sdk/bin/";
            ]);
        exec ["sh"; "-c"; "echo \"PATH: $PATH\""];
        exec ["gcloud"; "--version"];
        exec ["kubectl"; "version"];
      ]
    | `Local max_nodes -> [
        Apt.install ["docker.io"];
        (* See http://askubuntu.com/questions/477551/how-can-i-use-docker-without-sudo *)
        exec ["sudo"; "usermod"; "-aG"; "docker"; "opam"];
        exec ["sudo"; "chmod"; "666"; "/var/run/docker.sock"];
      ] in
  let shell_cmd =
    Genspio.EDSL.(
      seq [
        Opam.Pin.list_to_command t.opam_pin;
        Postgres.wait_for t.db;
        seq additional_setup;
        if_seq
          (exec ([
               "coclobas"; "config";
               "--root"; t.root;
               "--database-uri"; Postgres.to_uri t.db |> Uri.to_string;]
               @ cluster_options)
           |> succeeds)
          ~t:[
            exec [
              "coclobas"; "start-server";
              "--root"; t.root;
              "--port"; Int.to_string t.port;
            ];
          ]
          ~e:[
            exec ["echo"; "coclobas config failed"]
          ]
      ]
    )
    |> Genspio.Language.to_one_liner
  in
  let volumes =
    let open Genspio_edsl in
    match t.cluster with
    | `GKE kube -> []
    | `Local max_nodes -> [
        (* Cf.
           http://jpetazzo.github.io/2015/09/03/do-not-use-docker-in-docker-for-ci/#the-solution
        *)
        (let sock = "/var/run/docker.sock" in sprintf "%s:%s" sock sock);
        sprintf "%s:%s" t.tmp_dir t.tmp_dir;
      ] in
  Docker_compose.Configuration.service t.name
    ~image:t.image
    ~ports:[sprintf "%d:%d" t.port t.port]
    ~volumes
    ~command:["/bin/bash"; "-c"; shell_cmd]

