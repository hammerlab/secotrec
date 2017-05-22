open Common


let script_header =
  {ocaml|
#use "topfind";;
#thread
#require "coclobas.ketrew_backend,biokepi";;
|ocaml}

let pervasives_header =
  {ocaml|
open Nonstd
module String = Sosa.Native_string
let (//) = Filename.concat
let env_exn s =
  try Sys.getenv s with _ ->
    ksprintf failwith "Missing environment variable %S" s
|ocaml}

let get_work_dir ~default_work_dir = sprintf {ocaml|
let work_dir =
  try
    env_exn "BIOKEPI_WORK_DIR"
  with _ -> %S
|ocaml} default_work_dir
    
let optional_setup = {ocaml|
let install_tools_path =
  try env_exn "INSTALL_TOOLS_PATH"
  with _ -> work_dir // "toolkit"
let pyensembl_cache_dir =
  try env_exn "PYENSEMBLE_CACHE_DIR"
  with _ -> work_dir // "pyensembl-cache"
let reference_genomes_path =
  try env_exn "REFERENCE_GENOME_PATH"
  with _ -> work_dir // "reference-genome"
let allow_daemonize =
  try (env_exn "ALLOW_DAEMONIZE" = "true")
  with _ -> false
let image =
  try env_exn "DOCKER_IMAGE"
  with _ -> "hammerlab/keredofi:biokepi-run-gcloud"
let env_exn_tool_loc s tool =
  try (`Wget (Sys.getenv s)) with _ ->
    `Fail (sprintf "No location provided for %s" tool)
let netmhc_tmpdir =
  try env_exn "NETMHC_TMPDIR"
  with _ -> "/tmp" (* local to each worker pod *)
let netmhc_config () = Biokepi.Setup.Netmhc.(
  create_netmhc_config
    ~netmhc_tmpdir
    ~netmhc_loc:(env_exn_tool_loc "NETMHC_TARBALL_URL" "NetMHC")
    ~netmhcpan_loc:(env_exn_tool_loc "NETMHCPAN_TARBALL_URL" "NetMHCpan")
    ~pickpocket_loc:(env_exn_tool_loc "PICKPOCKET_TARBALL_URL" "PickPocket")
    ~netmhccons_loc:(env_exn_tool_loc "NETMHCCONS_TARBALL_URL" "NetMHCcons")
    ()
)
let gatk_jar_location () = env_exn_tool_loc "GATK_JAR_URL" "GATK"
let mutect_jar_location () = env_exn_tool_loc "MUTECT_JAR_URL" "MuTect"
|ocaml}

let set_name name = {ocaml|
let name = "Coclomachine"
|ocaml}

let make_volume_mounts mounts =
  let buf = Buffer.create 42 in
  let addf fmt = ksprintf (Buffer.add_string buf) fmt in
  addf "let volume_mounts = [\n";
  List.iter mounts ~f:begin function
  | `Nfs_kube nfs_mount ->
    let host, path, point =
      Nfs.Mount.(server nfs_mount, remote_path nfs_mount, mount_point nfs_mount)
    in
    addf "  `Nfs (\
          Coclobas.Kube_job.Specification.Nfs_mount.make \
          ~host:%S ~path:%S ~point:%S ());\n" host path point
  | `Local (host, mount) ->
    addf "  `Local (%S, %S);\n" host mount
  end;
  addf "]\n";
  Buffer.contents buf


let biokepi_machine_value actual_run_program_blob =
{ocaml|
let biokepi_machine =
  let host = Ketrew.EDSL.Host.parse "/tmp/KT-coclomachine/" in
  let max_processors = 7 in
  let run_program ?name ?(requirements = []) p =
    let open Ketrew.EDSL in
    let how =
      if
        (List.mem ~set:requirements `Quick_run
         || List.mem ~set:requirements `Internet_access)
        && allow_daemonize
      then `On_server_node
      else `Submit_to_coclobas
    in
    let with_more_info prog =
      let open Program in
      let cmd c =
        shf "echo \"## Biokepi machine: %s\"" (Filename.quote c)
        && shf "%s || echo 'Command failed'" c
      in
      cmd "umask 000"
      && cmd "whoami"
      && cmd "groups"
      && cmd "hostname"
      && cmd "uname -a"
      && cmd "export PATH=/opt/google-cloud-sdk/bin:$PATH"
      && cmd "ls /opt/google-cloud-sdk/bin"
      && prog
    in
  |ocaml}
^ actual_run_program_blob
^ {ocaml|
  in
  let open Biokepi.Setup.Download_reference_genomes in
  let toolkit =
    Biokepi.Setup.Tool_providers.default_toolkit ()
      ~host
      ~install_tools_path
      ~run_program
      ~gatk_jar_location
      ~mutect_jar_location
      ~netmhc_config in
  Biokepi.Machine.create name
    ~pyensembl_cache_dir
    ~max_processors
    ~get_reference_genome:(fun name ->
        Biokepi.Setup.Download_reference_genomes.get_reference_genome name
          ~toolkit
          ~host ~run_program
          ~destination_path:reference_genomes_path)
    ~host
    ~toolkit
    ~run_program
    ~work_dir:(work_dir // "work")
|ocaml}

let run_program_blob_for_local_docker ~coclobas_service =
  let burl = Coclobas.base_url coclobas_service in
  let tmp = Coclobas.tmp_dir coclobas_service in
  sprintf {ocaml|
    match how with
    | `On_server_node
    | `Submit_to_coclobas ->
      Coclobas_ketrew_backend.Plugin.local_docker_program
        ~tmp_dir:%S
        ~base_url:%S
        ~image
        ~volume_mounts
        (with_more_info p)
|ocaml} tmp burl

let run_program_blob_for_gke = {ocaml|
    match how with
    | `On_server_node ->
      daemonize ~host ~using:`Python_daemon (with_more_info p)
    | `Submit_to_coclobas ->
      Coclobas_ketrew_backend.Plugin.kubernetes_program
        ~base_url:"http://coclo:8082"
        ~image
        ~volume_mounts
        (with_more_info p)
  |ocaml}
  
type t = {
  name: string [@main ];
  default_work_dir: string;
  mounts: [ `Nfs_kube of Nfs.Mount.t | `Local of string * string] list;
  coclobas: Coclobas.t;
} [@@deriving make]

let show t =
  sprintf "Biokepi-machine %S: work on %s" t.name t.default_work_dir

let to_ocaml ?(with_script_header = true) t =
  let only_if b s = if b then s else [] in
  let run_program_blob =
    match Coclobas.cluster t.coclobas with
    | `GKE _ -> run_program_blob_for_gke
    | `Local _ -> run_program_blob_for_local_docker ~coclobas_service:t.coclobas
  in
  let pieces =
    only_if with_script_header [script_header]
    @ [
      pervasives_header;
      get_work_dir ~default_work_dir:t.default_work_dir;
      optional_setup;
      set_name t.name;
      make_volume_mounts t.mounts;
      biokepi_machine_value run_program_blob;
    ]
  in
  String.concat ~sep:"\n" pieces

