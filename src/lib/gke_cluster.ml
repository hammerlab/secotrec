open Common

type t = {
  zone : string;
  machine_type: string [@default "n1-highmem-8"];
  max_nodes: int [@default 15];
  name: string [@main];
} [@@deriving make]

let zone {zone; _} = zone
let name {name; _} = name
let max_nodes {max_nodes; _} = max_nodes
let machine_type t = t.machine_type

let destroy t =
  let open Genspio_edsl in
  exec [
    "gcloud"; "container"; "clusters"; "delete"; "--quiet"; "--wait";
    t.name; "--zone"; t.zone;
  ]
