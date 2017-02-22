

include Nonstd
module String = Sosa.Native_string
let (//) = Filename.concat

let debug_mode = try Sys.getenv "secotrec_debug" = "true" with _ -> false

let read_file file =
  let i = open_in file in
  let b = Buffer.create 42 in
  (try Buffer.add_channel b i 3_000_000 with _ -> ());
  close_in i;
  Buffer.contents b

let write_file file ~content =
  let o = open_out file in
  fprintf o "%s" content;
  close_out o

let cmdf ?returns fmt =
  ksprintf (fun cmd ->
      match Sys.command cmd with
      | n when returns = Some n || returns = None -> ()
      | other ->
        ksprintf failwith "CMD: %S returned %d" cmd other)
    fmt


let check_size_of_script ?(name = "") str =
  (*
     Got the magic number on Linux/Ubuntu 16.04.
     See `xargs --show-limits`.
  *)
  let len = String.length str in
  if len > 131071
  then (
    eprintf "WARNING: Command %S is too big for `sh -c <>`: %d B\n%!" name len;
    failwith "Script too long"
  ) else
    (* eprintf "DBG: Command %S is not too big for `sh -c <>`: %d B\n%!" name len; *)
    str

let genspio_to_one_liner ?(name = "") genspio =
  let save = Filename.temp_file "run-genspio" "-cmd.sh" in
  let cmd_str =
    Genspio.Language.to_one_liner genspio in
  begin match debug_mode with
  | true ->
    let cmd_lines = Genspio.Language.to_many_lines genspio in
    let save_lines = Filename.temp_file "run-genspio" "-cmd.sh" in
    write_file save cmd_str;
    write_file save_lines cmd_lines;
    eprintf "DBG: Command %S -> %s (and lines: %s)\n%!" name save save_lines;
  | false -> ()
  end;
  cmd_str |> check_size_of_script ~name

let run_genspio ?(name = "run_genspio")
    ?(output_errors = true) ?returns genspio =
  let save = Filename.temp_file "run-genspio" "-cmd.sh" in
  (* let cmd_str = genspio_to_one_liner ~name genspio in *)
  let cmd_str = Genspio.Language.to_many_lines genspio in
  write_file save cmd_str;
  try
    cmdf ?returns "bash %s" save
  with
  | Failure lots_of_stuff when output_errors ->
    let f = Filename.temp_file "secotrec" "error.txt" in
    let o = open_out f in
    fprintf o "Error:\n%s\n" lots_of_stuff;
    close_out o;
    eprintf "Run-genspio: fatal error (script: %s, errors: %s)\n%!" save f;
    failwith "Run-genspio: fatal error"
  | other ->
    raise other

module Genspio_edsl = struct

  include Genspio.EDSL

  let sayf fmt =
    ksprintf (fun s -> exec ["printf"; sprintf "SECOTREC: %s\\n" s]) fmt

  let sayl fmt l =
    call (string "printf" :: (sprintf "SECOTREC: %s\\n" fmt |> string) :: l)

  let cat_markdown file tag =
    seq [
      exec ["printf"; sprintf "``````````%s\\n" tag];
      call [string "cat"; file];
      exec ["printf"; sprintf "\\n``````````\\n"];
    ]

  let sanitize_name n =
    String.map n ~f:(function
      | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-' as c -> c
      | other -> '_')

  let seq_and l =
    List.fold l ~init:(bool true) ~f:(fun u v -> u &&& succeeds v)

  let seq_succeeds_or ?(silent = true) ~name ?(clean_up = []) cmds  =
    let stdout i =
      ksprintf string "/tmp/cmd-%s-stdout-%d" (sanitize_name name) i in
    let stderr i =
      ksprintf string "/tmp/cmd-%s-stderr-%d" (sanitize_name name) i in
    let log i u =
      if silent
      then write_output ~stdout:(stdout i) ~stderr:(stderr i) u
      else u in
    if_seq (seq_and (List.mapi ~f:log cmds) |> succeeds)
      ~t:[nop]
      ~e:(sayf "%s; FAILED:\\n" name
          ::
          begin if silent then
              List.mapi cmds ~f:(fun i u ->
                  seq [
                    (* sayf "Command: `%s`" (genspio_to_one_liner u); *)
                    cat_markdown (stdout i) "stdout";
                    cat_markdown (stderr i) "stderr";
                  ])
            else
              []
          end
          @ clean_up)


  let ensure ~name condition how =
    seq [
      sayf "%s: Checking..." name;
      if_seq condition
        ~t:[sayf "%s: Already Done." name]
        ~e:(sayf "%s: Build In Progress" name :: how)
    ]

  let silently u =
    let dev_null = string "/dev/null" in
    write_output ~stdout:dev_null ~stderr:dev_null u

  let succeeds_silently u =
     silently u |> succeeds

  let loop_until_ok ?(attempts = 20) ?(sleep = 2) cmd =
    let intvar =
      let varname = string "C_ATTEMPTS" in
      object
        method set v = setenv ~var:varname (Integer.to_string v)
        method get = getenv varname |> Integer.of_string
      end in
    seq [
      intvar#set (int 0);
      loop_while (
        Integer.(intvar#get <= int attempts)
        &&&
        not cmd
      )
        ~body:(seq [
            call [string "printf"; string "%d."; Integer.to_string intvar#get];
            exec ["sleep"; sprintf "%d" sleep];
            intvar#set Integer.(intvar#get + int 1);
          ]);
      exec ["printf"; "\\n"];
      if_then Integer.(intvar#get > int attempts)
        (seq [
            sayf "Command failed %d times!" attempts;
            exec ["sh"; "-c"; "exit 2"]
          ])
    ]

end





