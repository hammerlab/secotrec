open Common

module Pin = struct
  type t = {
    pin: string;
    kind: [ `Git ] [@default `Git];
    package: string [@main ];
  } [@@deriving make]

  let list_to_command t_list =
    let open Genspio_edsl in
    match t_list with
    | [] -> nop
    | _::_ ->
      seq [
        exec ["opam"; "remote"; "add"; "mothership"; "https://opam.ocaml.org"];
        exec ["opam"; "remote"; "remove"; "default" ];
        seq_succeeds_or ~name:(sprintf "opam-pins-preparation")
          ~silent:false
          ~clean_up:[fail] [
          exec ["opam"; "update"];
          exec ["opam"; "--version"];
        ];
        seq_succeeds_or ~name:("opam-pins-no-action")
          ~silent:false
          ~clean_up:[fail]
          (List.map t_list ~f:(fun t ->
               exec ["opam"; "pin"; "--yes"; "add"; "-n"; "-k"; "git";
                     t.package; t.pin]
             ));
        seq_succeeds_or ~name:("opam-pins-upgrade")
          ~silent:false
          ~clean_up:[fail] [
          exec (
            ["opam"; "reinstall"; "--yes"]
            @ (List.map t_list ~f:(fun t -> t.package))
          );
        ];
      ]

end
