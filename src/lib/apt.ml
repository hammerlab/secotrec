open Common

let install ?(sudo = true) ?(update = true) ?(upgrade = true) packages =
  let open Genspio_edsl in
  let call_apt args =
    let l = "apt-get" :: args in
    exec (if sudo then "sudo" :: l else l) in
  let if_opt cond v =
    if cond then v else [] in
  List.concat [
    if_opt update [call_apt [ "update" ]];
    if_opt upgrade [call_apt [ "upgrade"; "--yes" ]];
    [
      begin match packages with
      | [] -> sayf "No packages to install"
      | _::_ ->
        call_apt ("install" :: "--yes" :: packages)
      end
    ];
  ]
  |> seq_succeeds_or
    ~name:(sprintf "apt-install-%s" (String.concat ~sep:"-" packages))
    ~clean_up:[fail]
