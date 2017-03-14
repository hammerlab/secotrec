open Common

type item =
  | Download of {url: string; in_directory: string}

type t = item list

let make l : t = l

let download url ~in_directory = Download {url; in_directory}

let make_downloader ~tmpdir =
  let open Genspio_edsl in
  let url =
    "https://www.dropbox.com/s/2jbydjb9m7cer14/downloader.sh?raw=1" in
  let path = tmpdir // "downloader.sh" in
  object (self)
    method ensure =
      ensure ~name:"Getting downloader"
        (file_exists (string path)) [
        exec ["mkdir"; "-p"; tmpdir];
        exec ["curl"; "-k"; "-L"; url; "-o"; path]
      ]
    method call url =
      seq [
        self#ensure;
        call [string "sh"; string path;
              string "-u"; url;
              string "-t"; string tmpdir;]
      ]
  end


let item_to_workflow item =
  let open Ketrew.EDSL in
  match item with
  | Download { url; in_directory } ->
    let witness =
      sprintf "download-witness-%s" Digest.(string url |> to_hex) in
    let product = single_file (in_directory // witness) in
    let name = sprintf "Download %s to %s" url in_directory in
    let digest_url = Digest.(string url |> to_hex) in
    let program =
      let open Genspio_edsl in
      let cmd c =
        seq [
          exec ["printf"; "Command: %s\\n"; c];
          exec ["bash"; "-c"; c];
        ] in
      let downloader =
        make_downloader
          ~tmpdir:(in_directory // sprintf "tmp-%s" digest_url) in
      let p =
        seq_succeeds_or ~name:"preparation" ~clean_up:[fail] [
          cmd "whoami";
          cmd "ls -la";
          cmd "df -h";
          downloader#call (string url);
          write_output
            ~stdout:(in_directory // witness |> string)
            (exec ["date"]);
        ] in
      Program.(
        chain [
          exec ["mkdir"; "-p"; in_directory];
          exec ["cd"; in_directory];
          exec ["bash"; "-c"; genspio_to_one_liner ~name p]
        ]) in
    let make =
      daemonize ~using:`Python_daemon program in
    workflow_node product ~make ~name

let to_workflow t =
  let open Ketrew.EDSL in
  workflow_node ~name:"Data-Preparation" without_product
    ~edges:(List.map t ~f:(fun i -> item_to_workflow i |> depends_on))
