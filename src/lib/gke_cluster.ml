open Common

type t = {zone : string; max_nodes: int; name: string}
let make ?(max_nodes = 15) ?(zone = "us-east1-c") name =
  {zone; max_nodes; name}
let zone {zone; _} = zone
let name {name; _} = name
let max_nodes {max_nodes; _} = max_nodes

let destroy t =
  let open Genspio_edsl in
  exec [
    "gcloud"; "container"; "clusters"; "delete"; "--quiet"; "--wait";
    t.name; "--zone"; t.zone;
  ]
