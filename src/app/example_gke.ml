open Secotrec
open Common


let configuration =
  Configuration_dot_env.[
    section "Basic GCloud setup" [
      env "prefix" ~required:true ~example:"secosetup42"
        ~help:"A prefix string used to create names (machines, storage, etc.).";
      env "gcloud_zone" ~required:true ~example:"us-east1-x"
        ~help:"The Google Cloud zone to operate in.";
    ];
    section "NGinx Authentication Proxy" [
      env "htpasswd" ~required:false
        ~example:
          "alice:$2y$11$4ZDtPd5EOaVAe.y05G9XMui72ZnbXo6QrnbSvbRXShEDpuU1YF/.S\n\
           bob:$2y$11$X1iDjOgpBZPhxtUl2CpXCeNG5vyZzksT1Whk09uCbwSdZWQmECXBK\n"
        ~help:
          "Optional users and passwords, if you want the authentication proxy.\n\
           Generate this with the `htpasswd` utiliy or with \n\
           <http://aspirine.org/htpasswd_en.html>\n\
           Please use the `bcrypt` hash (others may not work with nginx by default)";
    ];
    section "NFS Servers" [
      env "extra_nfs_servers" ~default:"((extranfs ((size 5000))))"
        ~example:Deployment.Extra_nfs_server.sexp_syntax_example
        ~help:(sprintf "Describe additional NFS servers to setup.\n%s"
                 Deployment.Extra_nfs_server.sexp_syntax_help);
      env "nfs_mounts"
        ~example:"my-nfs-server-01-vm,/nfs-pool,.tmp/witness,/nfs01:\n\
                  nfs-42-vm,/nfs-pool,local/path/some/file,/nfs42"
        ~help:"List of additional NFS servers to mount on the Ketrew server \n\
               and on Coclobas' Kubernernetes jobs.\n\
               It is a colon-separated list of NFS mounts, and each mount is a coma-separated tuple:\n\
               <server-hostname>,<server-side-storage-path>,<witness-file>,<mount-point>
              ";
    ];
    section "DNS / TLS" [
      env "dns_suffix" ~required:true ~example:"mygcloudzone.example.com"
        ~help:"The deployment will be registered at <prefix>.<dns_suffix>.";
      env "gcloud_dns_zone" ~example:"some-zone-name"
        ~help:"The GCloud-DNS “zone” to register the name with. See:\n\
              \   https://console.cloud.google.com/networking/dns/zones\n\
               to create a fresh one.";
      env "certificate_email" ~required:true ~example:"sherlock@example.com"
        ~help:"Let's Encrypt wants an email address to associate with\
               the TLS certificate";
    ];
    section "Ketrew/Coclobas" [
      env "auth_token"
        ~required:true
        ~example:"ddefefeijdenjcndijdlei9180128012"
        ~help:"Authentication token for the Ketrew UIs & API, just a random \
               string.";
      env "cluster_max_nodes" ~default:"15"
        ~help:"The maximal size of the Kubernetes cluster.";
    ];
    section "Additional Biokepi Configuration"
      ~intro:"The Biokepi Machine generated uses these environment variables.\n\
              The paths can be used to override defaults (where everything is \n\
              under a given path inside the Extra-NFS mount).\n\
              All the URLs correspond to software that is not really free/open-source\n\
              so we cannot distribute public URLs for Biokepi to download them \n\
              automatically."
      begin
        let annoying_url envvar example_file name =
          let example = sprintf "http://example.com/path/to/%s" example_file in
          let help = sprintf "An URL to get '%s' from." name in
          env envvar ~example ~help in
        let path envvar desc default =
          let help =
            sprintf "%s (default: <workd-dir>/%s)" desc default in
          env envvar ~help ~example:(sprintf "/path/to/%s" default) in
        [
          path "INSTALL_TOOLS_PATH" "Where to download/install software" "toolkit";
          path "PYENSEMBLE_CACHE_DIR" "Where to instruct `pyensemble` to store its cache" "pyensembl-cache";
          path "REFERENCE_GENOME_PATH"
            "Where to store reference data (independent from sample data)"
            "reference-genome";
          env "ALLOW_DAEMONIZE"
            ~help:"Whether to use the `daemonize` backend for some jobs (might be \n\
                   necessary in some cases, e.g. when one needs to use `gsutil`, \n\
                   that's why the default is `true`)"
            ~example:"false";
          env "DOCKER_IMAGE"
            ~example:"hammerlab/biokepi-run:some-tag"
            ~help:"The docker image used for the Biokepi jobs \
                   (default `hammerlab/biokepi-run`).";
          annoying_url "GATK_JAR_URL" "GenomeAnalysisTK_35.jar" "the Broad's GATK";
          annoying_url "MUTECT_JAR_URL" "muTect-1.1.6-10b1ba92.jar" "MuTect 1.x";
          annoying_url "NETMHC_TARBALL_URL" "netMHC-3.4a.Linux.tar.gz" "NetMHC";
          annoying_url "NETMHCPAN_TARBALL_URL" "netMHCpan-2.8a.Linux.tar.gz" "NetMHCPan";
          annoying_url "PICKPOCKET_TARBALL_URL" "pickpocket-1.1a.Linux.tar.gz" "PickPocket";
          annoying_url "NETMHCCONS_TARBALL_URL" "netMHCcons-1.1a.Linux.tar.gz" "NetMHCCons";
        ]
      end;
  ]



let example_1 () =
  let conf n = Configuration_dot_env.get_value_exn configuration n in
  let conf_opt n = Configuration_dot_env.get_exn configuration n in
  let zone = conf "gcloud_zone" in
  let prefix = conf "prefix" in
  let gchost = prefix ^ "-secobox" in
  let node =
    Gcloud_instance.make ~zone gchost
      ~machine_type:(`Google_cloud `Highmem_16) in
  let db =
    Postgres.of_uri
      (Uri.of_string "postgresql://pg/?user=postgres&password=kpass") in
  let cluster =
    Gke_cluster.make
      ~max_nodes:(conf "cluster_max_nodes"|> Int.of_string
                  |> Option.value_exn
                    ~msg:"cluster_max_nodes should be an integer")
      (prefix ^ "-kube-cluster") ~zone in
  let coclo_pin =
    Opam.Pin.make "coclobas"
      ~pin:"https://github.com/hammerlab/coclobas.git#master" in
  let ketrew_pin =
    Opam.Pin.make "ketrew"
      ~pin:"https://github.com/hammerlab/ketrew.git#master" in
  let coclo =
    Coclobas.make (`GKE cluster) ~db ~opam_pin:[coclo_pin; ketrew_pin] in
  let auth_token = conf  "auth_token" in
  let extra_nfs_servers =
    conf "extra_nfs_servers"
    |> Deployment.Extra_nfs_server.parse_sexp ~zone ~prefix in
  let nfs_mounts =
    List.map extra_nfs_servers ~f:Deployment.Extra_nfs_server.to_mount
    @ Option.value_map ~default:[] (conf_opt "nfs_mounts")
      ~f:Nfs.Mount.of_colon_separated_csv
  in
  let ketrew =
    Ketrew_server.make
      ~opam_pin:[coclo_pin; ketrew_pin]
      "kserver" ~auth_token ~db ~nfs_mounts in
  let proxy_port = 8842 in
  let proxy_nginx =
    Option.map (conf_opt "htpasswd") ~f:(fun htpasswd ->
        Nginx.Proxy.make "authproxy"
          ~port:proxy_port
          ~proxy_uri:"http://kserver:8080"
          ~htpasswd)
  in
  let dns =
    Gcloud_dns.make
      ~zone:(conf "gcloud_dns_zone")
      ~name:(sprintf "%s.%s" gchost (conf "dns_suffix")) in
  let letsencrypt =
    Letsencrypt.make
      ~dns_name:dns.Gcloud_dns.name ~email:(conf "certificate_email") in
  let tlstun =
    let backend_address, backend_port =
      match proxy_nginx with
      | None -> "kserver", 8080
      | Some _ -> "authproxy", proxy_port in
    Tlstunnel.make "tlstun"
      ~certificate:(`Mount (letsencrypt#path,
                            letsencrypt#certificate,
                            letsencrypt#private_key))
      ~backend_address
      ~backend_port
      ~frontend_port:8443
  in
  let leading_extra_nfs =
    List.hd extra_nfs_servers
    |> Option.value_exn ~msg:"I need at least one NFS server"
    |> Deployment.Extra_nfs_server.mount_point in
  let biokepi_machine =
    Biokepi_machine_generation.make
      ~default_work_dir:(leading_extra_nfs // "workdir")
      ~coclobas:coclo
      ~mounts:(List.map nfs_mounts ~f:(fun nfsm -> `Nfs_kube nfsm))
      "The-CocloKetrew-Machine" in
  let preparation =
    let open Data_preparation in
    make [
      download
        "https://storage.googleapis.com/hammerlab-biokepi-data/precomputed/b37decoy_20160927.tgz"
        ~in_directory:(leading_extra_nfs // "workdir/reference-genome");
    ] in
  Deployment.make "Ex-almost-full"
    ~node:(Deployment.Node.gcloud node)
    ~tlstunnel:tlstun
    ?proxy_nginx
    ~biokepi_machine
    ~gke_cluster:cluster
    ~db
    ~dns
    ~extra_nfs_servers
    ~ketrew
    ~coclobas:coclo
    ~letsencrypt
    ~preparation

let () = Command_line.run example_1 ~configuration

