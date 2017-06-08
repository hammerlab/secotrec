open Common

module Ssh = struct
  type t = {
    user : string [@default "ec2-user"];
    private_key : [ `File of string ] option;
    hostname: string [@main];
  } [@@deriving make]

  let hostname t = t.hostname

  let minus_i_option t =
    match t.private_key with
    | None -> [], []
    | Some (`File path) ->
      [Genspio_edsl.(exec ["chmod"; "600"; path])],
      ["-i"; path]

  let ssh_options t =
    ["-oBatchMode=yes"; "-oStrictHostKeyChecking=no"]


  let ssh_address t =
    sprintf "%s@%s" t.user t.hostname

  let copy_file t file_from file_to =
    let open Genspio_edsl in
    let arg file_spec =
      match file_spec with
      | `Local p -> p
      | `On_node p -> sprintf "%s:%s" (ssh_address t) p
    in
    let pre_cmds, minus_i = minus_i_option t in
    pre_cmds
    @ [
      exec (["scp"] @ ssh_options t @ minus_i @ [arg file_from; arg file_to])
    ]

  let exec_cmd t cmd =
    let open Genspio_edsl in
    let pre_cmds, minus_i = minus_i_option t in
    pre_cmds
    @ [
      exec (["ssh"] @ ssh_options t @ minus_i @ [ssh_address t; cmd])
    ]


  let run_on ~name t genspio =
    let tmp =
      Filename.temp_file
        ~temp_dir:"/tmp" "secotrec" (sprintf "aws-%s-script.sh" name) in
    let content = Genspio.Language.to_many_lines genspio in
    write_file tmp ~content;
    let open Genspio_edsl in
    seq_succeeds_or ~name:(sprintf "Executing %S (%s)" name tmp)
      ~silent:false
      ~clean_up:[fail]
      (
        copy_file t (`Local tmp) (`On_node tmp)
        @ [
          sayf "DBG-AWS: Executing %s" tmp;
        ]
        @ exec_cmd t (sprintf "bash %s" tmp);
      )

end