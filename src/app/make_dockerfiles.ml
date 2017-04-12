
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
          bash_c "echo 'biokepi ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/biokepi && \
                  chmod 440 /etc/sudoers.d/biokepi && \
                  chown root:root /etc/sudoers.d/biokepi";
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

  let install_gcloud =
    let open Dockerfile in
    let profile = "/etc/profile.d/gcloud_installation.sh" in
    let profile_content = [
      "# GCloud installation:";
      "export CLOUDSDK_INSTALL_DIR=/opt";
      "export PATH=${CLOUDSDK_INSTALL_DIR}/google-cloud-sdk/bin/:${PATH}";
    ] in
    comment "Installing GCloud command-line tool with kubectl" @@@ [
      apt_get_install ["python"; "build-essential"];
      env ["CLOUDSDK_CORE_DISABLE_PROMPTS", "true"];
      env ["CLOUDSDK_INSTALL_DIR", "/opt"];
      bash_c ~sudo:false "curl https://sdk.cloud.google.com | bash";
      env ["PATH", "${CLOUDSDK_INSTALL_DIR}/google-cloud-sdk/bin/:${PATH}"];
      run "gcloud components install kubectl";
    ]
      @ List.map profile_content ~f:(fun line ->
          bash_c ~sudo:true "echo %s >> %s" (Filename.quote line) profile;
        )
      @ [
        bash_c ~sudo:true "chmod 666 %s" profile;
        bash_c ~sudo:false "echo Created file %s" profile;
        bash_c ~sudo:false "cat %s " profile;
      ]


  let biokepi_run
      ?(with_gcloud = false)
      () =
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
    @@ or_empty with_gcloud install_gcloud
    @@ entrypoint "bash"

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
      | `Version v -> opam_pins ["coclobas", v]
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
          github_pin "ketrew" "master";
          github_pin "biokepi" "master";
          github_pin "secotrec" "master";
        ];
    ]

  let ubuntu_docker () =
    let open Dockerfile in
    comment "Ubuntu, updated, and with Docker installed" @@@ [
      from "ubuntu";
      seb_maintains;
      upgrade_ubuntu ~sudo:false ();
      apt_get_install ~sudo:false ["docker.io"];
    ]

  let epidisco_dev () =
    let open Dockerfile in
    secotrec () @@@ [
      apt_get_install ["vim"; "emacs"; "tmux"; "git-hub";
                       "samtools"; "vcftools"; "bwa"];
      opam_pins
        [
          github_pin "epidisco" "master";
          github_pin "ogene" "master";
        ];
    ]

end

module Test = struct
  type t = {
    procedure: [ `Genspio of unit Genspio.EDSL.t ];
    name: string [@main];
  } [@@deriving make]

  let genspio name ~procedure = make name ~procedure:(`Genspio procedure)

  let to_shell t =
    match t.procedure with
    | `Genspio g -> Genspio.Language.to_many_lines g

  let gassert ?(on_failure = []) name cond =
    let open Genspio.EDSL in
    if_seq cond ~t:[] ~e:[
      eprintf (string "\n\nTest %s FAILED!\n\n") [string name];
      seq on_failure;
      fail;
    ]

  let test_dashdashversion cmdname ~expect =
    let name = sprintf "%s-version" cmdname in
    let procedure =
      let open Genspio.EDSL in
      let cv = exec [cmdname; "--version"] |> output_as_string in
      gassert name
        ~on_failure:[
          eprintf (string "→ %s --version: ```\\n%s```\\n") [string cmdname; cv];
        ]
        (cv =$= (ksprintf string "%s\n" expect))
    in
    genspio name ~procedure

  let succeeds cmd =
    let name = sprintf "Command `%s` succeeds" cmd in
    genspio name
      Genspio.EDSL.(gassert name (exec ["sh"; "-c"; cmd] |> succeeds))
      

    


end

module Image = struct

  type t = {
    dockerfile: Dockerfile.t;
    tests: Test.t list;
    description: string option;
    tag: string [@main];
  } [@@deriving make]

  let show t = sprintf "Image `%s`" t.tag

  let tag t = t.tag
  let dockerfile t = t.dockerfile
  let description t = t.description
  let branch = tag
  let tests t = t.tests

  let all =
    let open Dockerfiles in
    [
      make "ketrew-server-300"
        ~dockerfile:(ketrew_server `K300)
        ~description:"OCaml/Opam environment with \
                      [Ketrew](https://github.com/hammerlab/ketrew) 3.0.0 \
                      installed."
        ~tests:[
          Test.test_dashdashversion "ketrew" "3.0.0";
        ];
      make "ketrew-server" ~dockerfile:(ketrew_server (`Branch "master"))
        ~description:"OCaml/Opam environment with the `master` version of \
                      [Ketrew](https://github.com/hammerlab/ketrew) installed."
        ~tests:[
          Test.test_dashdashversion "ketrew" "3.1.0+dev";
        ];
      make "biokepi-run"
        ~description:"Ubuntu image with the “system” dependencies that \
                      [Biokepi](https://github.com/hammerlab/biokepi) \
                      workflows require, and a special `biokepi` user with a \
                      fixed UID (useful for shared file-systems)."
        ~dockerfile:(biokepi_run ());
      make "biokepi-run-gcloud"
        ~description:"Image similar to `biokepi-run` but with the `gcloud` and \
                      `gsutil` tools installed (for the `biokepi` user)"
        ~dockerfile:(biokepi_run ~with_gcloud:true ())
        ~tests:[
          Test.succeeds "gcloud version";
          Test.succeeds "gsutil version";
          Test.succeeds "kubectl version --client";
          Test.succeeds "whoami | grep biokepi";
        ];
      make "coclobas-gke-dev"
        ~dockerfile:(coclobas ~with_gcloud:true ~ketrew:(`Branch "master")
                       ~coclobas:(`Branch "master") ());
      make "coclobas-gke-biokepi-default"
        ~description:"The default image used by Secotrec for GKE/Local \
                      deployments, it has `gcloud`, `gcloudnfs`, the Biokepi \
                      NFS-friendly user, TLS-tunnel, Coclobas 0.0.1 and \
                      Ketrew `master` branch (until next version)."
        ~dockerfile:(coclobas ~with_gcloud:true ~with_gcloudnfs:true
                       ~with_biokepi_user:true ~with_secotrec_gke:true
                       ~ketrew:(`Branch "master")
                       ~coclobas:(`Version "0.0.1") ())
        ~tests:begin
          let cmds_opam_and_biokepi = [
            "echo $PATH";
            "ls -la /etc/profile.d/";
            "cat /etc/profile.d/*";
            "gcloud --version | grep 'Google Cloud'";
            "gsutil --version | grep 'gsutil version'";
            "kubectl version --client | grep 'Client'";
          ] in
          [
            Test.test_dashdashversion "ketrew" "3.1.0+dev";
            Test.test_dashdashversion "coclobas" "0.0.0";
            Test.succeeds "ocamlfind list | grep coclobas | grep 0.0.1";
          ]
          @ List.map cmds_opam_and_biokepi ~f:Test.succeeds
          @ List.map cmds_opam_and_biokepi ~f:(fun s ->
              (* It seems we need the `-l` in order to see `/etc/profile.d/` *)
              ksprintf Test.succeeds
                "sudo su biokepi sh -lc %s" (Filename.quote s))
        end;
      make "coclobas-gke-biokepi-dev"
        ~description:"Image similar to `coclobas-gke-biokepi-default` but \
                      with Coclobas pinned to its `master` branch."
        ~dockerfile:(coclobas ~with_gcloud:true ~with_gcloudnfs:true
                       ~with_biokepi_user:true ~with_secotrec_gke:true
                       ~ketrew:(`Branch "master")
                       ~coclobas:(`Branch "master") ())
        ~tests:[
          Test.test_dashdashversion "ketrew" "3.1.0+dev";
          (* coclobas --version gives: "0.0.2-dev+<commit-hash-prefix>": *)
          Test.succeeds "coclobas --version | grep 0.0.2-dev";
          Test.succeeds "ocamlfind list | grep coclobas | grep 0.0.2-dev";
        ];
      make "secotrec-default" ~dockerfile:(secotrec ())
        ~description:"OCaml/Opam environment with the `master` version of \
                      [Secotrec](https://github.com/hammerlab/secotrec) \
                      (and some tools it may use) installed."
        ~tests:[
          Test.test_dashdashversion "ketrew" "3.1.0+dev";
          Test.test_dashdashversion "coclobas" "0.0.0";
          Test.test_dashdashversion "secotrec-gke" "0.0.0";
          Test.test_dashdashversion "secotrec-local" "0.0.0";
          Test.succeeds "nslookup hammerlab.org";
          Test.succeeds "gcloud version";
          Test.succeeds "kubectl version --client";
        ];
      make "ubuntu-docker"
        ~description:"Just an Ubuntu image with `docker.io` installed \
                      (useful for testing these images, cf. \
                      Secotrec \
                      [docs](https://github.com/hammerlab/secotrec#secotrec-make-dockerfiles))."
        ~dockerfile:(ubuntu_docker ());
      make "epidisco-dev"
        ~description:"Developer-friendly environment for Biokepi/Secotrec \
                      projects with various tools (text editors, git-hub, \
                      Epidisco)."
        ~dockerfile:(epidisco_dev ());
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

  let write repo_dir image =
    in_dir repo_dir begin fun () ->
      let branch = Image.tag image in
      let name = sprintf "write-and-commit-%s" branch in
      run_genspio ~output_errors:true ~name ~returns:0
        Genspio_edsl.(
          seq [
            in_branch ~repo_dir ~branch;
            seq_succeeds_or ~silent:false ~clean_up:[fail] ~name [
              (Dockerfile.string_of_t (Image.dockerfile image) ^ "\n" |> string
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
        (List.map dockerfiles ~f:(fun im ->
             let dockerfile_github =
               sprintf
                 "https://github.com/hammerlab/keredofi/blob/%s/Dockerfile"
                 (Image.branch im) in
             sprintf "* `hammerlab/keredofi:%s`:\n%s\
                     \    * See [`Dockerfile`](%s).\n"
               (Image.tag im)
               (Image.description im
                |> Option.value_map ~f:(sprintf "    * %s\n") ~default:"")
               dockerfile_github))
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

  let build_all_workflow ~coclobas_tmp_dir ~coclobas_base_url l =
    let open Ketrew.EDSL in
    let tmp_dir =
      (* 
         This directory is mounted by the Ketrew and Coclobas containers as
         well as all the local-docker jobs (as
         `Coclobas_ketrew_backend.Plugin.extra_mount_container_side`).
      *)
      coclobas_tmp_dir
    in
    let run_program p =
      Coclobas_ketrew_backend.Plugin.local_docker_program
        ~base_url:coclobas_base_url
        ~image:"hammerlab/keredofi:ubuntu-docker"
        ~tmp_dir
        ~volume_mounts:[
          `Local ("/var/run/docker.sock", "/var/run/docker.sock");
        ]
        p in
    let build_one image =
      let branch = Image.branch image in
      let dockerfile = Image.dockerfile image in
      let witness_file =
        sprintf "docker-build-%s-%s"
          branch
          (Dockerfile.string_of_t dockerfile |> Digest.string |> Digest.to_hex)
      in
      workflow_node (tmp_dir // witness_file |> single_file)
        ~name:(sprintf "Make %s" branch)
        ~make:(
          let dirname = sprintf "/tmp/build-%s" branch in
          run_program
            Program.(
              chain [
                shf "mkdir -p %s" dirname;
                shf "cd %s" dirname;
                shf "echo %s > Dockerfile"
                  (Dockerfile.string_of_t dockerfile |> Filename.quote);
                shf "docker build -t hammerlab/keredofi-test:%s ." branch;
                shf "printf \"Done: $(date -R)\\n\" > %s/%s"
                  Coclobas_ketrew_backend.Plugin.extra_mount_container_side
                  witness_file;
                shf "chmod 777 %s/%s"
                  Coclobas_ketrew_backend.Plugin.extra_mount_container_side
                  witness_file;
              ]
            )
        )
    in
    let with_tests image =
      match Image.tests image with
      | [] -> build_one image |> depends_on
      | more ->
        let tests =
          List.map more ~f:(fun t ->
              Program.(
                chain [
                  exec ["echo"; sprintf "Running test %s" t.Test.name];
                  exec [
                    "docker";"run";
                    sprintf "hammerlab/keredofi-test:%s" (Image.tag image);
                    "bash"; "-c"; Test.to_shell t;
                  ]
                ]
              )) in
        workflow_node without_product ~name:(sprintf "Test %s" (Image.show image))
          ~edges:[depends_on (build_one image)]
          ~make:(run_program Program.(chain tests))
        |> depends_on
    in
    workflow_node without_product
      ~name:"Test-Build All Images"
      ~edges:(List.map l ~f:(fun v -> with_tests v))


end

let env_exn s =
  try Sys.getenv s with _ -> ksprintf failwith "Missing env-var: %S" s
let env_opt s =
  try Some (Sys.getenv s) with _ -> None

module Repository = struct
  type t = {
    path : string;
  } [@@deriving cmdliner]
  let term () =
    let open Cmdliner.Term in
    pure (fun repo ->
        List.iter Image.all ~f:(fun i ->
            printf "====== Making %s ======\n%!" (Image.show i);
            Github_repo.write repo.path i); 
        Github_repo.write_readme repo.path Image.all;
        cmdf
          "cd %s && \
           git status -s && \
           git --no-pager log --graph --decorate --color --oneline --all -n 20"
          repo.path;
        ()
      )
    $ cmdliner_term ()
end

module Test_workflow = struct
  type t = {
    tags : string list;
  } [@@deriving cmdliner]
  let term () =
    let open Cmdliner.Term in
    pure (fun { tags } ->
        let coclobas_base_url = env_exn "COCLOBAS_BASE_URL" in
        let coclobas_tmp_dir = env_exn "COCLOBAS_TMP_DIR" in
        let images =
          match tags with
          | [] -> Image.all
          | _::_ ->
            List.filter Image.all (fun i -> List.mem ~set:tags (Image.tag i))
        in
        Ketrew.Client.submit_workflow
          ~add_tags:["secotrec"; "make-dockerfiles"]
          (Test_build.build_all_workflow
             ~coclobas_tmp_dir ~coclobas_base_url images)
      )
    $ cmdliner_term ()
end

let () =
  let open Cmdliner.Term in
  let subcmd name ?doc term = term, info name ?doc in
  eval_choice (ret (pure (`Help (`Plain, None))), info "make-dockerfiles") [
    subcmd "write-repository" (Repository.term ());
    subcmd "test-workflow" (Test_workflow.term ());
    subcmd "view" begin
      pure (fun () ->
          List.iter Image.all ~f:(fun im ->
              printf "Branch `%s`:\n\n```\n%s\n```\n\n" (Image.branch im)
                (Dockerfile.string_of_t (Image.dockerfile im)));
        )
      $ pure ()
    end
  ]
  |> function
  | `Error _ -> Pervasives.exit 1
  | `Ok () -> Pervasives.exit 0
  | `Version -> Pervasives.exit 0
  | `Help -> Pervasives.exit 0
