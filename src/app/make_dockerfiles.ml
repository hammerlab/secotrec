
open Secotrec
open Common

let in_dir dir f =
  let original_dir = Sys.getcwd () in
  Sys.chdir dir;
  begin try
    f ();
    Sys.chdir original_dir;
  with e ->
    Sys.chdir original_dir;
    raise e
  end

module Dockerfiles = struct

  let seb_maintains =
    Dockerfile.maintainer "Sebastien Mondet <seb@mondet.org>"

  let bash_c ?(sudo = true) fmt =
    ksprintf (fun s ->
        Dockerfile.run "%sbash -c %s" (if sudo then "sudo " else "") (Filename.quote s)
      ) fmt

  let or_empty c v = if c then v else Dockerfile.empty

  let upgrade_ubuntu ?(sudo = true) () =
    Dockerfile.(
      comment "Update Ubuntu (One upgrade can trigger a minor version change \
               that requires a second `update`)" @@@ [
        bash_c ~sudo "apt-get update -y";
        bash_c ~sudo "DEBIAN_FRONTEND=noninteractive apt-get -y upgrade";
        bash_c ~sudo "apt-get update -y";
      ]
    )

  let apt_get_install ?(sudo = true) packages =
    Dockerfile.(
      bash_c ~sudo
        "DEBIAN_FRONTEND=noninteractive apt-get -y install %s"
        (String.concat ~sep:" " packages)
    )

  let opam_default_remote =
    let open Dockerfile in
    comment "We put back the mothership reprository" @@@ [
      run "opam remote add mothership https://opam.ocaml.org";
      run "opam remote remove default" ;
    ]

  let opam_pins ?(more_installs = []) pins =
    let open Dockerfile in
    comment "A few opam-pins:" @@@ 
    List.concat [
      List.map pins ~f:(fun (p, uri) ->
          run "opam pin --yes -n add '%s' '%s'" p uri);
      [
        run "opam upgrade --yes";
        run "opam install --yes %s"
          (List.map pins ~f:fst @ more_installs |> String.concat ~sep:" ");
      ]
    ]

  let opam_base ?(use_opam_default_remote = true) () =
    let open Dockerfile in
    from "ocaml/opam" ~tag:"ubuntu-16.04_ocaml-4.03.0" @@@ [
      (* Dockerfile_linux.Apt.update; -> does it! *)
      seb_maintains;
      upgrade_ubuntu ~sudo:true ();
      or_empty use_opam_default_remote (comment "" @@@ [
          opam_default_remote;
          run "opam update";
          run "opam upgrade --yes";
        ]);
      opam_pins ["ocamlbuild", "0.9.3"];
    ]


  let github_pin ?(org = "hammerlab") p checkout =
    (p, sprintf "https://github.com/%s/%s.git#%s" org p checkout)

  let ketrew_dependencies how =
    let sqlite =
      match how with
      | `K300 -> ["libsqlite3-dev"]
      | `Branch b -> [] in
    sqlite @ ["libpq-dev"; "libev-dev"; "libgmp-dev"]

  let ketrew_server how =
    Dockerfile.(
      opam_base () @@@ [
        apt_get_install (ketrew_dependencies how);
        run "opam install -y tls conf-libev";
        begin match how with
        | `K300 -> run "opam install ketrew.3.0.0"
        | `Branch b -> opam_pins [github_pin "ketrew" b]
        (* run *)
        (*   "opam pin --yes add ketrew https://github.com/hammerlab/ketrew.git#%s" b; *)
        end;
      ]
    )

  let biokepi_user =
    let open Dockerfile in
    object (self)
      method create =
        comment "A user: biokepi with a consistent UID: 20042" @@@ [
          bash_c "adduser --uid 20042 --disabled-password --gecos '' biokepi && \
                  passwd -l biokepi && \
                  chown -R biokepi:biokepi /home/biokepi";
        ]
      method switch_to =
        empty @@@ [
          user "biokepi";
          env ["HOME", "/home/biokepi"];
          workdir "/home/biokepi";
          (* RUN opam init -a --yes --comp 4.02.3 *)
        ]
      method create_and_switch_to = self#create @@ self#switch_to
    end

  let biokepi_run () =
    let open Dockerfile in
    let biokepi_dependencies = [
      "cmake"; "r-base"; "tcsh"; "libx11-dev";
      "libfreetype6-dev"; "pkg-config"; "wget";
      "gawk"; "graphviz"; "xvfb"; "git";
    ] in
    let install_wkhtmltopdf =
      comment
        "Install wkhtmltopdf from source, this version \
         comes with patched QT necessary for PDF gen" @@@ [
        run "cd /tmp ; \
             wget http://download.gna.org/wkhtmltopdf/0.12/0.12.3/wkhtmltox-0.12.3_linux-generic-amd64.tar.xz";
        run "cd /tmp && tar xvfJ wkhtmltox-0.12.3_linux-generic-amd64.tar.xz";
        run "cd /tmp/wkhtmltox/bin && sudo chown root:root wkhtmltopdf";
        run "sudo cp /tmp/wkhtmltox/bin/wkhtmltopdf /usr/local/bin/wkhtmltopdf";
        run "sudo wget \
             https://raw.githubusercontent.com/hammerlab/biokepi/9ee26064498d87a04318ced6613ca50629446465/tools/docker/run/fonts.conf \
             -O /etc/fonts/local.conf"
      ]
    in
    let oracle_java_7 =
      comment "The hard-one: Installing Oracle's Java 7" @@@ [
        run "sudo add-apt-repository --yes ppa:webupd8team/java";
        run "sudo apt-get update";
        comment "On top of that we have to fight with interactive licensing questions";
        comment "Cf. http://askubuntu.com/questions/190582/installing-java-automatically-with-silent-option";
        bash_c "echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections";
        bash_c "echo debconf shared/accepted-oracle-license-v1-1 seen true |  debconf-set-selections";
        bash_c "DEBIAN_FRONTEND=noninteractive apt-get install --yes \
                --allow-unauthenticated oracle-java7-installer";
      ] in
    let intro =
      opam_base () @@@ [
        apt_get_install biokepi_dependencies;
      ]
    in
    intro
    @@ install_wkhtmltopdf
    @@ oracle_java_7
    @@ biokepi_user#create_and_switch_to

  let install_gcloud =
    let open Dockerfile in
    comment "Installing GCloud command-line tool with kubectl" @@@ [
      apt_get_install ["python"; "build-essential"];
      env ["CLOUDSDK_CORE_DISABLE_PROMPTS", "true"];
      bash_c ~sudo:false "curl https://sdk.cloud.google.com | bash";
      env ["PATH", "/home/opam/google-cloud-sdk/bin/:${PATH}"];
      run "gcloud components install kubectl";
    ]

  let coclobas
      ?(with_gcloud = false)
      ?(with_gcloudnfs = false)
      ?(with_biokepi_user = false)
      ?(with_secotrec_gke = false)
      ~ketrew ~coclobas () =
    let open Dockerfile in
    let install_gcloudnfs =
      comment "Installing GCloudNFS: cioc/gcloudnfs" @@@ [
        apt_get_install ["python-pip"; "python-dev"; "build-essential"; "wget"];
        bash_c "pip install --upgrade google-api-python-client";
        bash_c "wget \
                https://raw.githubusercontent.com/cioc/gcloudnfs/master/gcloudnfs \
                -O/usr/bin/gcloudnfs";
        bash_c "chmod a+rx /usr/bin/gcloudnfs";
      ] in
    let secotrec_gke_stuff =
      let more_packages =
        ["zlib1g-dev"; "screen"; "nfs-common"; "graphviz"];
      in
      comment "Getting more things usefull for Secotrec-GKE deployments" @@@ [
        apt_get_install more_packages;
        run "opam install --yes tlstunnel";
      ] in
    ketrew_server ketrew @@@ [
      (* biokepi user *)
      or_empty with_biokepi_user biokepi_user#create;
      or_empty with_gcloud install_gcloud;
      or_empty with_gcloudnfs install_gcloudnfs;
      or_empty with_secotrec_gke secotrec_gke_stuff;
      begin match coclobas with
      | `Branch b -> opam_pins [github_pin "coclobas" b]
      end;
    ]

  let secotrec () =
    let open Dockerfile in
    opam_base () @@@ [
      install_gcloud;
      apt_get_install (["dnsutils"] @ ketrew_dependencies (`Branch "master"));
      opam_pins
        ~more_installs:["tls"]
        [
          github_pin "coclobas" "master";
          github_pin "ketrew" "master";
          github_pin "biokepi" "master";
          github_pin "genspio" "master";
          github_pin "secotrec" "master";
        ];
    ]


  let all = [
    ketrew_server `K300, "ketrew-server-300";
    ketrew_server (`Branch "master"), "ketrew-server";
    biokepi_run (), "biokepi-run";
    coclobas ~with_gcloud:true ~ketrew:(`Branch "master")
      ~coclobas:(`Branch "master") (), "coclobas-gke-dev";
    coclobas ~with_gcloud:true
      ~with_gcloudnfs:true
      ~with_biokepi_user:true
      ~with_secotrec_gke:true
      ~ketrew:(`Branch "master")
      ~coclobas:(`Branch "master") (), "coclobas-gke-biokepi-dev";
    secotrec (), "secotrec-default";
  ]
end

module Github_repo = struct

  let in_branch ~repo_dir ~branch =
    let open Genspio_edsl in
    let name = sprintf "goto-branch-%s" branch in
    seq_succeeds_or ~silent:false ~clean_up:[fail] ~name [
      sayf "Going to branch %S in %s" branch repo_dir;
      if_seq
        (exec ["git"; "checkout";  branch] |> succeeds_silently)
        ~t:[sayf "Branch %S exists" branch]
        ~e:[
          seq_succeeds_or ~silent:false ~clean_up:[fail] ~name [
            sayf "Creating branch %S" branch;
            exec ["git"; "checkout"; "-b"; branch]
          ]
        ];
    ]

  let commit_maybe ~branch files =
    let open Genspio_edsl in
    let name = sprintf "commit-%s" branch in
    seq_succeeds_or ~silent:false ~clean_up:[fail] ~name [
      if_seq (exec ["git"; "diff"; "--quiet"; "--exit-code"]
              |> returns ~value:0) 
        ~t:[
          sayf "Nothing to commit for %s" branch;
        ]
        ~e:[
          sayf "Committing %s for %s" (String.concat ~sep:", " files) branch;
          seq_succeeds_or  ~clean_up:[fail] ~name:"Commititng" [
            exec (["git"; "add"] @ files);
            exec ["git";"commit"; "-m";
                  sprintf "Update %s for %s"
                    (String.concat ~sep:", " files)
                    branch];
          ]
        ]
    ]

  let write repo_dir dockerfile branch =
    in_dir repo_dir begin fun () ->
      let name = sprintf "write-and-commit-%s" branch in
      run_genspio ~output_errors:true ~name ~returns:0
        Genspio_edsl.(
          seq [
            in_branch ~repo_dir ~branch;
            seq_succeeds_or ~silent:false ~clean_up:[fail] ~name [
              (Dockerfile.string_of_t dockerfile ^ "\n" |> string
               >> exec ["cat"] |> write_stdout ~path:(string "./Dockerfile"));
            ];
            commit_maybe ~branch ["./Dockerfile"];
            sayf "Going to back to master branch";
            exec ["git"; "checkout"; "master"] |> silently;
          ]
        )
    end

  let write_readme repo_dir dockerfiles =
    let header =
      "Keredofi\n\
       ========\n\
       \n\
       Ketrew-Related `DockerFile`s \
       (contents of this repo are software-generated, cf. \n\
       [`hammerlab/secotrec`](https://github.com/hammerlab/secotrec)).\n\n"
    in
    let branch_list_section =
      "Available Tags\n\
       --------------\n\n\
      See also the Docker-hub \
       [tags](https://hub.docker.com/r/hammerlab/keredofi/tags/).\n\n\
      "
      ^ String.concat ~sep:"" 
        (List.map dockerfiles ~f:(fun (_, b) ->
             let dockerfile_github =
               sprintf
                 "https://github.com/hammerlab/keredofi/blob/%s/Dockerfile" b in
             sprintf "* `hammerlab/keredofi:%s` (see [`Dockerfile`](%s)).\n"
               b dockerfile_github))
    in
    let readme_content =
      header ^ branch_list_section in
    in_dir repo_dir begin fun () ->
      let name = sprintf "write-and-commit-README" in
      run_genspio ~output_errors:true ~name ~returns:0
        Genspio_edsl.(
          seq [
            in_branch ~repo_dir ~branch:"master";
            seq_succeeds_or ~silent:false ~clean_up:[fail] ~name [
              (readme_content |> string
               >> exec ["cat"] |> write_stdout ~path:(string "./README.md"));
            ];
            commit_maybe ~branch:"master" ["./README.md"];
          ]
        )
    end
end

module Test_build = struct

  let build_all_workflow l =
    let open Ketrew.EDSL in
    let build_one (dockerfile, branch) =
      let tmp_dir = "/tmp/secotrec-local-shared-temp" in
      let witness_file =
        sprintf "docker-build-%s-%s"
          branch
          (Dockerfile.string_of_t dockerfile |> Digest.string |> Digest.to_hex)
      in
      workflow_node (tmp_dir // witness_file |> single_file)
        ~name:(sprintf "Make %s" branch)
        ~make:(
          let dirname = sprintf "/tmp/build-%s" branch in
          Coclobas_ketrew_backend.Plugin.local_docker_program
            ~base_url:"http://coclo:8082"
            ~image:"ubuntu"
            ~tmp_dir
            ~volume_mounts:[
              `Local ("/var/run/docker.sock", "/var/run/docker.sock");
            ]
            Program.(
              chain [
                sh "apt-get update -y";
                sh "apt-get install -y docker.io";
                shf "mkdir -p %s" dirname;
                shf "cd %s" dirname;
                shf "echo %s > Dockerfile"
                  (Dockerfile.string_of_t dockerfile |> Filename.quote);
                shf "docker build -t hammerlab/keredofi-test:%s ." branch;
                shf "mkdir -p /shared";
                shf "printf \"Done: $(date -R)\\n\" > /coclobas-ketrew-plugin-playground/%s" witness_file;
                shf "chmod 777 /coclobas-ketrew-plugin-playground/%s" witness_file;
              ]
            )
        )
    in
    workflow_node without_product
      ~name:"Test-Build All Images"
      ~edges:(List.map l ~f:(fun v -> build_one v |> depends_on))


end

let env_exn s =
  try Sys.getenv s with _ -> ksprintf failwith "Missing env-var: %S" s
let env_opt s =
  try Some (Sys.getenv s) with _ -> None

let () =
  let what = env_opt "do" in
  begin match what with
  | Some "write" ->
    let dir = env_exn "dir" in
    List.iter Dockerfiles.all ~f:(fun (d, b) ->
        printf "====== Making %s ======\n%!" b;
        Github_repo.write dir d b); 
    Github_repo.write_readme dir Dockerfiles.all;
    cmdf
      "cd %s && \
       git status -s && \
       git --no-pager log --graph --decorate --color --oneline --all -n 20"
      dir;
    ()
  | Some "test" ->
    Ketrew.Client.submit_workflow
      ~add_tags:["secotrec"; "make-dockerfiles"]
      (Test_build.build_all_workflow Dockerfiles.all)
  | None | Some "view" ->
    List.iter Dockerfiles.all ~f:(fun (d, b) ->
        printf "Branch `%s`:\n\n```\n%s\n```\n\n" b
          (Dockerfile.string_of_t d));
  | Some other ->
    ksprintf failwith "Don't know what %S ($what) means" other
  end;
  printf "Done.\n%!";
  ()
