open Common


let common_opam_pins =
  let open Configuration_dot_env in
  object
    method configuration = [
      env "pin_ketrew"
        ~help:"Pin Ketrew to a given branch/tag.";
      env "pin_coclobas"
        ~help:"Pin Coclobas to a given branch/tag.";
      env "pin_trakeva"
        ~help:"Pin Trakeva to a given branch/tag.";
    ]
    method opam_pins configuration =
      let conf_opt n = Configuration_dot_env.get_exn configuration n in
      let pin_opt repo o =
        Option.map o ~f:(fun branch ->
            Opam.Pin.make (Filename.basename repo)
              ~pin:(sprintf "https://github.com/%s.git#%s" repo branch)) in
      List.filter_opt [
        pin_opt "hammerlab/ketrew" @@ conf_opt "pin_ketrew";
        pin_opt "hammerlab/coclobas" @@ conf_opt "pin_coclobas";
        pin_opt "smondet/trakeva" @@ conf_opt "pin_trakeva";
      ]
  end
