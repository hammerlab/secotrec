
open Common

type env_value = {
  default: string option;
  example: string option;
  required: bool [@default false];
  help: string;
  name: string [@main ];
} [@@deriving make]
let env = make_env_value
type section = {
  values: env_value list;
  intro: string option;
  title: string [@main ];
} [@@deriving make]
let section ?intro t v = make_section ?intro t ~values:v
type t =
  section list

let generate t ~path =
  let o = open_out path in
  let line fmt =
    fprintf o (fmt ^^ "\n") in
  let comment_line fmt =
    line ("# " ^^ fmt) in
  let comment ?(indent = 0) fmt =
    ksprintf (fun s ->
        let indent_str = String.make indent ' ' in
        List.iter (String.split ~on:(`Character '\n') s)
          ~f:(comment_line "%s%s" indent_str);
      ) fmt in
  List.iter t ~f:begin fun {title; intro; values} ->
    comment_line "%s" title;
    comment_line "%s" String.(make (length title) '=');
    comment ~indent:0 "%s"
      (Option.value_map intro ~default:"" ~f:(sprintf "\n%s\n"));
    List.iter values ~f:begin fun { name; default; example; required; help } ->
      comment_line "### Variable `%s` (%s):" name
        (if required then "required"
         else sprintf "optional, default: %s"
             (Option.value ~default:"None" default));
      comment_line "";
      comment ~indent:4 "export %s=%s" name
        (let value e =
           match String.find e ((=) '\n') with
           | None -> e
           | Some v -> sprintf "'%s'" e in
         match example, default with
         | Some e, _ -> value e
         | _, Some d -> value d
         | None, None -> "''");
      comment_line "";
      comment "%s" help;
      (* List.iter (String.split ~on:(`Character '\n') help) *)
      (*   ~f:(comment_line "%s"); *)
      comment_line "";
    end;
  end;
  close_out o;
  ()

let get_exn t ~name =
  let lname = name in
  let opt =
    List.find_map t ~f:(fun {title; values} ->
        List.find values
          ~f:(fun {name; _} -> name = lname))
  in
  begin match opt with
  | Some { name; default; example; required; help } ->
    begin match Sys.getenv name with
    | v when None <> default && required ->
      ksprintf failwith "Required config value with default does not make \
                         much sense here: %S" name
    | v when (Some v = example) && required ->
      ksprintf failwith "Required config value is set to the example: %S" name
    | v -> Some v
    | exception _ when required ->
      ksprintf failwith "Missing required config value: %S" name
    | exception _ -> default
    end
  | None -> ksprintf failwith "Unknown config value: %S" name
  end

let get_value_exn t ~name =
  get_exn t ~name |> Option.value_exn ~msg:"This option was not required?"

let display t =
  let line fmt = printf (fmt ^^ "\n") in
  let bloc ~indent fmt =
    ksprintf (fun s ->
        let indent_str = String.make indent ' ' in
        List.iter (String.split s ~on:(`Character '\n'))
          ~f:(line "%s%s" indent_str)
      ) fmt in
  List.iter t ~f:begin fun {title; values} ->
    line "# %s" title;
    (* line ""; *)
    List.iter values ~f:begin fun { name; default; example; required; help } ->
      line "## Variable %S (%s):" name
        (if required then "required"
         else sprintf "optional, default: %S"
             (Option.value ~default:"NONE" default));
      bloc ~indent:4 "%s"
        (match get_exn t name with
        | Some s -> s
        | None -> "None"
        | exception (Failure f) -> sprintf "ERROR: %s" f);
      (* line ""; *)
    end;
  end;
  ()
