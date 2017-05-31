open Common

module Aws_batch_cluster: sig
  type t = {
    queue: string;
    bucket: string;
  } [@@deriving make]
end
type cluster = [
  | `GKE of Gke_cluster.t
  | `Local of int
  | `Aws_batch of Aws_batch_cluster.t
]
type t
val make :
  ?name:string ->
  ?opam_pin: Opam.Pin.t list ->
  ?root:string ->
  ?port:int ->
  ?tmp_dir: string ->
  db:Postgres.t ->
  ?image: string ->
  cluster ->
  t

val cluster : t -> cluster
val name: t -> string
val base_url: t -> string
val url_for_local_client: t -> string
val tmp_dir: t -> string
val root: t -> string
val logs_path: t -> string

val to_service : t -> Docker_compose.Configuration.service
