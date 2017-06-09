open Secotrec
open Common

let configuration =
  Configuration_dot_env.[
    section "Basic AWS setup" [
      env "aws_node" ~required:true ~example:"ec2-42-142-51-69.compute-1.amazonaws.com"
        ~help:"An AWS server, created with the WebUi or the `aws-node` tool.";
      env "aws_private_key" ~required:true ~example:"/home/who/.ssh/aws-node01.pem"
        ~help:"The private key to log on the `$aws_node` server.";
      env "efs_name" ~required:true ~example:"who-efs02"
        ~help:"The name of the EFS service to create, please choose a \
               filename-friendly name, as they are mounted, by default, at \
               `/mnt-$efs_name`).";
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
    section "DNS / TLS" [
      env "certificate_email" ~required:true ~example:"sherlock@example.com"
        ~help:"Let's Encrypt wants an email address to associate with\
               the TLS certificate";
    ];
    section "Ketrew/Coclobas"
      begin [
        env "auth_token"
          ~required:true
          ~example:"ddefefeijdenjcndijdlei9180128012"
          ~help:"Authentication token for the Ketrew UIs & API, just a random \
                 string.";
        env "aws_batch_queue" ~required:true ~example:"who-queue-01"
          ~help:"The AWS batch queue to configure Coclobas with.";
        env "aws_scripts_bucket" ~required:true ~example:"s3://some-bucket/some/path/"
          ~help:"The AWS S3 bucket to configure Coclobas with.";
      ] @ Util.common_opam_pins#configuration
      end;
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
                   necessary in some cases, although the default is `false`)"
            ~example:"true";
          env "DOCKER_IMAGE"
            ~example:"organization/some-repo:some-tag"
            ~help:"The docker image used for the Biokepi jobs \
                   (default `hammerlab/keredofi:biokepi-run`).";
          annoying_url "GATK_JAR_URL" "GenomeAnalysisTK_35.jar" "the Broad's GATK";
          annoying_url "MUTECT_JAR_URL" "muTect-1.1.6-10b1ba92.jar" "MuTect 1.x";
          annoying_url "NETMHC_TARBALL_URL" "netMHC-3.4a.Linux.tar.gz" "NetMHC";
          annoying_url "NETMHCPAN_TARBALL_URL" "netMHCpan-2.8a.Linux.tar.gz" "NetMHCPan";
          annoying_url "PICKPOCKET_TARBALL_URL" "pickpocket-1.1a.Linux.tar.gz" "PickPocket";
          annoying_url "NETMHCCONS_TARBALL_URL" "netMHCcons-1.1a.Linux.tar.gz" "NetMHCCons";
          env "NETMHC_TMPDIR"
            ~help:"Where the NetMHC-family of tools should store their \n\
                   temporary files on the pod. Note that these tools can \n\
                   misbehave and arbitrarily drop some predictions if you \n\
                   use a path to a folder on a mounted device."
            ~default:"/tmp/"
            ~example:"/path/to/tmp/";
        ]
      end;
  ]

let conf n = Configuration_dot_env.get_value_exn configuration n
let conf_opt n = Configuration_dot_env.get_exn configuration n

let awsnode =
  let url = conf "aws_node"  in
  Aws_instance.Ssh.make url
    ~private_key:(`File (conf "aws_private_key"))

let () =
  let name = "Test-node" in
  Aws_instance.Ssh.run_on ~name awsnode Common.Genspio_edsl.(
      seq [
        sayl "Hello from %s" [exec ["hostname"] |> output_as_string];
        exec ["uname"; "-a"];
      ]
    )
  |> Common.run_genspio ~name
    ~output_errors:true ~returns:0

let deployment () =
  let db =
    Postgres.of_uri
      (Uri.of_string "postgresql://pg/?user=postgres&password=kpass") in
  let opam_pin = Util.common_opam_pins#opam_pins configuration in
  let coclobas =
    let cluster =
      let queue = conf "aws_batch_queue" in
      let bucket = conf "aws_scripts_bucket" in
      `Aws_batch (Coclobas.Aws_batch_cluster.make ~queue ~bucket) in
    let image = "hammerlab/keredofi:coclobas-aws-biokepi-dev" in
    Coclobas.make cluster ~image ~db ~opam_pin in
  let efs = Aws_efs.make (conf "efs_name") in
  let ketrew =
    let image = "hammerlab/keredofi:coclobas-aws-biokepi-dev" in
    Ketrew_server.make ~port:8123 "kserver" ~auth_token:"dielajdilejadelij"
      ~db ~opam_pin ~image
      ~after_mounts:(Aws_efs.To_genspio.full_mount_script efs)
  in
  let tlstunnel =
    let backend_address, backend_port = "kserver", 8080 in
    let exposed_port = 443 in
    Tlstunnel.make "tlstun"
      ~certificate:`Fake
      ~backend_address ~backend_port
      ~exposed_port
  in
  let biokepi_machine =
    Biokepi_machine_generation.make
      ~default_work_dir:(Aws_efs.default_mount_point efs // "workdir")
      ~coclobas
      ~mounts:[]
      "The-CocloKetrew-Machine" in
  let preparation =
    let open Data_preparation in
    make [
      download
        "https://storage.googleapis.com/hammerlab-biokepi-data/precomputed/b37decoy_20160927.tgz"
        ~in_directory:(Aws_efs.default_mount_point efs  // "workdir/reference-genome");
      download
        "https://storage.googleapis.com/hammerlab-biokepi-data/precomputed/b37_20161007.tgz"
        ~in_directory:(Aws_efs.default_mount_point efs  // "workdir/reference-genome");
    ] in
  Deployment.make "AWS-Test" ~node:(Deployment.Node.aws_ssh awsnode)
    ~biokepi_machine ~preparation
    ~tlstunnel
    ~coclobas
    ~ketrew
    ~db
    ~efs


let () = Command_line.run deployment