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
    section "Additional Services" [
      env "tls_port" ~required:false ~example:"22443"
        ~help:"Force the use of a TLS tunnel exposed at a given port (highly \
               recommended when using a publicly visible instance)."
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
          Util.nfs_mounts_configuration ();
          env "aws_batch" ~required:false
            ~help:"Use the experimental \
                   AWS-Batch backend of Coclobas instead of “Local-Docker.”\n\
                   You must provide the AWs-Batch queue name and an `s3://` \
                   bucket URI separated by a comma.\n\
                   This setup also requires to pass the AWS credentials \
                   through environment variables: \
                   `AWS_KEY_ID` and `AWS_SECRET_KEY`."
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
    let cluster, image =
      match conf_opt "aws_batch" with
      | None -> `Local (conf "coclobas_max_jobs" |> int_of_string), None
      | Some s ->
        begin match String.split ~on:(`Character ',') s with
        | [queue; bucket] ->
          `Aws_batch (Coclobas.Aws_batch_cluster.make ~queue ~bucket),
          Some "hammerlab/keredofi:coclobas-aws-biokepi-dev"
        | _ -> failwith (sprintf "wrong format for `aws_batch`: %S" s)
        end in
    Coclobas.make cluster ~db ~opam_pin ~tmp_dir:coclo_tmp_dir ?image in
  let auth_token = conf "ketrew_auth_token" in
  let ketrew =
    let nfs_mounts =
      Option.value_map ~default:[] (conf_opt "nfs_mounts")
      ~f:Nfs.Mount.of_colon_separated_csv in
    let ketrew_debug_functions =
      try
        conf "ketrew_debug_functions"|> String.split ~on:(`Character ',')
      with _ -> [] in
    Ketrew_server.make ~port:8123 "kserver" ~auth_token ~db
      ~ketrew_debug_functions
      ~nfs_mounts
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
    Option.map (conf_opt "tls_port") ~f:(fun port_string ->
        let backend_address, backend_port = "kserver", 8080 in
        let exposed_port =
          Int.of_string port_string
          |> Option.value_exn ~msg:"`tls_port` should be an integer!" in
        Tlstunnel.make "tlstun"
          ~certificate:`Fake
          ~backend_address ~backend_port
          ~exposed_port)
  in
  let preparation =
    let open Data_preparation in
    make [
      download
        "https://storage.googleapis.com/hammerlab-biokepi-data/precomputed/b37decoy_20160927.tgz"
        ~in_directory:(biokepi_work#mount // "workdir/reference-genome");
      download
        "https://storage.googleapis.com/hammerlab-biokepi-data/precomputed/b37_20161007.tgz"
        ~in_directory:(biokepi_work#mount // "workdir/reference-genome");
    ] in
  Deployment.make "Light-local" ~node
    ?tlstunnel
    ~preparation
    ~biokepi_machine
    ~db
    ~ketrew
    ~coclobas:coclo

let () = Command_line.run example ~configuration

