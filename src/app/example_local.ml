open Secotrec
open Common

let configuration =
  Configuration_dot_env.[
    section "Optional GCloud Instance Setup" [
      env "gcloud_name" ~required:false ~example:"user-seco-one-box"
        ~help:"The name of the GCloud instance to create.";
      env "gcloud_zone" ~required:false ~example:"us-east1-x"
        ~help:"The Google Cloud zone to operate in.";
      env "gcloud_disk_size" ~required:false ~example:"2000"
        ~help:"The size of the disk of the instance in GiB (default: 2000 GiB).";
    ];
    section "Biokepi" [
      env "biokepi_work" ~required:false
        ~help:"The directory (on the running host) used for BIOKEPI_WORK \
               (default: ~/biokepi-work-dir).";
    ];
    section "Ketrew/Coclobas Configuration"
      begin
        [
          env "ketrew_debug_functions" ~required:false
            ~help:"Run Ketrew with debug `on` for a given comma-separated list of \
                   functions.";
          env "ketrew_auth_token" ~required:false ~default:"nekot"
            ~help:"The auth-token used by Ketrew, if you are running on a \n\
                   GCloud instance it is highly recommended to change this!";
          env "coclobas_max_jobs" ~required:false ~default:"2"
            ~help:"The limit on Coclobas' local-docker scheduler.";
        ]
        @ Util.common_opam_pins#configuration
      end;
  ]


let example () =
  let conf n = Configuration_dot_env.get_value_exn configuration n in
  let conf_opt n = Configuration_dot_env.get_exn configuration n in
  let biokepi_work =
    object
      method host =
        try conf "biokepi_work"
        with _ -> (Sys.getenv "HOME") // "biokepi-work-dir"
      method mount = "/nfsaa"
    end in
  let coclo_tmp_dir =
    "/tmp/secotrec-local-shared-temp" in
  let node =
    let properties = [ Deployment.Node.has_directory coclo_tmp_dir] in
    match conf_opt "gcloud_name" with
    | Some name ->
      Deployment.Node.gcloud ~properties (
        let boot_disk_size =
          match conf "gcloud_disk_size" |> int_of_string with
          | n -> `GB n
          | exception _ -> `GB 2_000 in
        Gcloud_instance.make ~zone:(conf "gcloud_zone") name
          ~boot_disk_size
          ~machine_type:(`Google_cloud `Highmem_16)
      )
    | None ->
      Deployment.Node.localhost () ~properties
  in
  let db =
    Postgres.of_uri
      (Uri.of_string "postgresql://pg/?user=postgres&password=kpass") in
  let opam_pin = Util.common_opam_pins#opam_pins configuration in
  let coclo =
    Coclobas.make (`Local (conf "coclobas_max_jobs" |> int_of_string))
      ~db ~opam_pin ~tmp_dir:coclo_tmp_dir in
  let auth_token = conf "ketrew_auth_token" in
  let ketrew =
    let ketrew_debug_functions =
      try
        conf "ketrew_debug_functions"|> String.split ~on:(`Character ',')
      with _ -> [] in
    Ketrew_server.make ~port:8123 "kserver" ~auth_token ~db
      ~ketrew_debug_functions
      ~local_volumes:[
        coclo_tmp_dir, coclo_tmp_dir;
        biokepi_work#host, biokepi_work#mount;
      ]
      ~opam_pin
  in
  let biokepi_machine =
    Biokepi_machine_generation.make
      ~default_work_dir:(biokepi_work#mount // "workdir")
      ~coclobas:coclo
      ~mounts:[ `Local (biokepi_work#host, biokepi_work#mount) ]
      "The-Local-Machine" in
  let tlstunnel =
    Option.map (conf_opt "gcloud_name") ~f:(fun _ ->
        let backend_address, backend_port = "kserver", 8080 in
        Tlstunnel.make "tlstun"
          ~certificate:`Fake
          ~backend_address ~backend_port
          ~frontend_port:8443)
  in
  let preparation =
    let open Data_preparation in
    make [
      download
        "https://storage.googleapis.com/hammerlab-biokepi-data/precomputed/b37decoy_20160927.tgz"
        ~in_directory:(biokepi_work#mount // "workdir/reference-genome");
      download
        "https://storage.googleapis.com/hammerlab-biokepi-data/precomputed/b37_20161007.tgz"
        ~in_directory:(leading_extra_nfs // "workdir/reference-genome");
    ] in
  Deployment.make "Light-local" ~node
    ?tlstunnel
    ~preparation
    ~biokepi_machine
    ~db
    ~ketrew
    ~coclobas:coclo

let () = Command_line.run example ~configuration

