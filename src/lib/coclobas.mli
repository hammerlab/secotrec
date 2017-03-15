open Common

type t
val make :
  ?name:string ->
  ?opam_pin: Opam.Pin.t list ->
  ?root:string ->
  ?port:int ->
  ?tmp_dir: string ->
  db:Postgres.t ->
  ?image: string ->
  [ `GKE of Gke_cluster.t | `Local of int ] ->
  t

val cluster : t -> [ `GKE of Gke_cluster.t | `Local of int ]
val name: t -> string
val base_url: t -> string
val url_for_local_client: t -> string
val tmp_dir: t -> string
val root: t -> string
val logs_path: t -> string

val to_service : t -> Docker_compose.Configuration.service
