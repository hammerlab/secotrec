open Common

type machine_type = [
  | `Google_cloud of [
      | `Highmem_8 (** "n1-highmem-8" (8 vCPUs, 52 GB memory) *)
      | `Highmem_16 (** "n1-highmem-16" (16 vCPUs, 104 GB memory) *)
      | `Small (** "g1-small" (1 vCPU, 1.7 GB memory) *)
      | `Custom_named of string * int (* name * max-processors *)
    ]
] [@@deriving yojson, show, eq]

type t = {
  name: string [@main];
  scopes: string list [@default ["cloud-platform"]];
  zone: string;
  os: [ `Ubuntu_1604 ] [@default `Ubuntu_1604];
  boot_disk_size: [`GB of int] [@default `GB 50];
  machine_type: machine_type [@default `Google_cloud `Small];
        (* additional_packages: string list [@default []]; *)
} [@@deriving yojson, show, make, eq]


let destroy t =
  let open Genspio.EDSL in
  seq [
    exec ["gcloud"; "compute"; "instances"; "delete"; t.name;
          "--zone"; t.zone; "--quiet"];
    exec ["gcloud"; "compute"; "firewall-rules"; "delete";
          "https-on-" ^ t.name; "--quiet"];
  ]

let machine_type_name t =
  let gmt =
    match t.machine_type with
    | `Google_cloud g ->
      (match g with
      | `Small -> "g1-small"
      | `Highmem_8 -> "n1-highmem-8"
      | `Highmem_16 -> "n1-highmem-16"
      | `Custom_named (n, _) -> n)
  in
  gmt

let instance_is_up ?(gcloud = "gcloud") t =
  let open Genspio_edsl in
  succeeds_silently
    (output_as_string
       (exec [gcloud; "compute"; "instances"; "describe"; t.name;
              "--zone"; t.zone])
     >> exec ["grep"; "-q"; "-e"; "status: RUNNING"])

let ensure t =
  let open Genspio_edsl in
  let image_options t =
    match t.os with
    | `Ubuntu_1604 ->
      ["--image-family"; "ubuntu-1604-lts";
       "--image-project"; "ubuntu-os-cloud"]
  in
  let boot_disk_size_option t =
    match t.boot_disk_size with
    | `GB gb -> ["--boot-disk-size"; sprintf "%dGB" gb]
  in
  let fwrule_is_up t =
    succeeds_silently
      (exec ["gcloud"; "compute"; "firewall-rules";
             "describe"; "https-on-" ^ t.name;])
  in
  seq [
    ensure ~name:("instance--" ^ t.name) (instance_is_up t) [
      seq_succeeds_or ~name:"gcloud-create"
        [
          exec (
            ["gcloud"; "compute"; "instances"; "create"; t.name;
             "--zone"; t.zone; "--scopes"; String.concat ~sep:"," t.scopes;
            ]
            @ image_options t
            @ boot_disk_size_option t
            @ ["--machine-type"; machine_type_name t]
          )
        ]
        ~clean_up:[destroy t; fail];
    ];
    ensure ~name:("firewall-rule--" ^ t.name) (fwrule_is_up t) [
      seq_succeeds_or ~name:"gcloud-fw-create"
        [
          exec (
            ["gcloud"; "compute"; "firewall-rules"; "create";
             "https-on-" ^ t.name;
             "--allow"; "tcp:443";
             "--source-tags"; t.name;
             "--source-ranges"; "0.0.0.0/0"
            ]
          )
        ]
        ~clean_up:[destroy t; fail];
    ];
  ]

let external_ip t =
  let open Genspio_edsl in
  let all =
    exec ["gcloud"; "compute"; "instances"; "describe"; t.name;
          "--zone"; t.zone]
    |> output_as_string
  in
  all >> exec ["grep"; "natIP"] |> output_as_string
  >> exec ["sed"; "s/.*natIP: *\\(.*\\)/\\1/"] |> output_as_string
  >> exec ["tr"; "-d"; "\\n"] |> output_as_string

let run_command ?(use_script = false) t cmd =
  let open Genspio_edsl in
  begin match use_script with
  | true ->
    let tmp = Filename.temp_file ~temp_dir:"/tmp" "secotrec" "script.sh" in
    write_file tmp ~content:cmd;
    (* printf "Running command on %s, %s\n%!" t.name tmp; *)
    seq [
      seq_succeeds_or ~name:(sprintf "Copying %s" tmp)
        ~clean_up:[fail] [
        exec ["gcloud"; "compute"; "copy-files"; "--zone"; t.zone;
              tmp; sprintf "%s:%s" t.name tmp];
      ];
      exec ["gcloud"; "compute"; "ssh"; t.name;
            "--zone"; t.zone; "--command"; sprintf "sudo chmod 777 %s" tmp ];
      exec ["gcloud"; "compute"; "ssh"; t.name;
            "--zone"; t.zone; "--command"; sprintf "sh %s" tmp ];
    ]
  | false ->
    exec ["gcloud"; "compute"; "ssh"; t.name;
          "--zone"; t.zone; "--command"; cmd ];
  end

let copy_file t file_from file_to =
  let open Genspio_edsl in
  let arg file_spec =
    match file_spec with
    | `Local p -> p
    | `On_node p -> sprintf "%s:%s" t.name p
  in
  exec ["gcloud"; "compute"; "copy-files"; "--zone"; t.zone;
        arg file_from; arg file_to]


let run_on ?(name = "cmd") node cmd =
  (* let name = sprintf "%s-on-%s" name node.name in *)
  run_command ~use_script:true node 
    (Genspio.Language.to_many_lines cmd)
