open Nonstd
open Solvuu_build.Std

let project_name = "secotrec"
let version = "0.0.0-dev"

let build_tests =
  try Sys.getenv "WITH_TESTS" = "true" with _ -> false

let findlib_deps = [
  "genspio";
  "coclobas.ketrew_backend";
]

let lib : Project.item =
  Project.lib project_name
    ~bin_annot:()
    ~thread:()
    ~findlib_deps
    ~dir:"src/lib"
    ~style:(`Pack project_name)
    ~pkg:project_name

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

let ocamlinit_postfix = [
  sprintf "open %s" (String.capitalize_ascii project_name);
]

let () =
  Project.basic1 ~project_name ~version ~ocamlinit_postfix
    (List.filter_opt [
        Some lib;
        Some example_gke;
        Some example_local;
      ])
