open Nonstd
open Solvuu_build.Std

let project_name = "secotrec"
let version = "0.0.0-dev"

let build_aws_cli_app =
  try Sys.getenv "WITH_AWS_NODE" = "true" with _ -> false

let findlib_deps = [
  "genspio";
  "coclobas.ketrew_backend";
  "ppx_deriving_cmdliner";
]

let lib : Project.item =
  Project.lib project_name
    ~bin_annot:()
    ~thread:()
    ~findlib_deps
    ~dir:"src/lib"
    ~style:(`Pack project_name)
    ~pkg:project_name

let make_dockerfiles =
  Project.app ("secotrec-make-dockerfiles")
    ~bin_annot:()
    ~thread:()
    ~file:"src/app/make_dockerfiles.ml"
    ~findlib_deps:["dockerfile.opam"]
    ~internal_deps:[lib]
let example_gke =
  Project.app ("secotrec-gke")
    ~bin_annot:()
    ~thread:()
    ~file:"src/app/example_gke.ml"
    ~internal_deps:[lib]
let example_local : Project.item =
  Project.app ("secotrec-local")
    ~bin_annot:()
    ~thread:()
    ~file:"src/app/example_local.ml"
    ~internal_deps:[lib]

let simple_efs : Project.item =
  Project.app ("secotrec-aws-efs")
    ~bin_annot:()
    ~thread:()
    ~file:"src/app/aws_efs.ml"
    ~internal_deps:[lib]

let aws_node =
  if build_aws_cli_app
  then
    Some begin
      Project.app ("secotrec-aws-node")
        ~bin_annot:()
        ~thread:()
        ~file:"src/app/aws_node.ml"
        ~findlib_deps:[
          "aws";
          "aws-ec2";
          "aws.lwt";
          "nonstd";
          "sosa";
          "pvem_lwt_unix";
          "ppx_deriving.std";
          "ppx_deriving_yojson";
          "ppx_deriving_cmdliner";
        ]
    end
  else
    None



let ocamlinit_postfix = [
  sprintf "open %s" (String.capitalize_ascii project_name);
]

let () =
  Project.basic1 ~project_name ~version ~ocamlinit_postfix
    (List.filter_opt [
        Some lib;
        Some example_gke;
        Some example_local;
        Some make_dockerfiles;
        Some simple_efs;
        aws_node;
      ])
