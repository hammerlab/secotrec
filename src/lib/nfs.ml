open Common

module Mount = struct
  (** Mount existing NFS servers on nodes. *)

  type t = {
    server: string;
    remote_path: string;
    witness: string; (* Relative path to a file in the NFS path *)
    mount_point: string;
  } [@@deriving make,show]

  let mount_command {server; remote_path; mount_point; _} =
    let open Genspio_edsl in
    exec [
      "sudo"; "mount"; "-t"; "nfs";
      sprintf "%s:%s" server remote_path;
      mount_point
    ]
  let ensure_mounted t =
    let open Genspio_edsl in
    ensure
      ~name:(sprintf "Mounted-%s" t.server)
      (file_exists (string (t.mount_point // t.witness))) [
      seq_succeeds_or
        ~name:(sprintf "Mounting-%s" t.server)
        ~clean_up:[fail]
        [
          exec ["sudo"; "apt-get"; "install"; "--yes"; "nfs-client"];
          exec ["sudo"; "mkdir"; "-p"; t.mount_point];
          mount_command t;
        ];
    ]

  let server t = t.server
  let remote_path t = t.remote_path
  let witness t = t.witness
  let mount_point t = t.mount_point

  let of_colon_separated_csv cscsv =
    cscsv
    |> String.split ~on:(`Character ':')
    |> List.map ~f:(fun csv ->
        String.split ~on:(`Character ',') csv
        |> begin function
        | server :: remote_path :: witness :: mount_point :: [] ->
          make ~server ~remote_path ~witness ~mount_point
        | other ->
          ksprintf failwith "Wrong format in Colon-separated-CSV: %S" csv
        end)
end

module Fresh = struct
  (** Setup new NFS servers with potential data-transfers to fill them up.
      {{:https://github.com/cioc/gcloudnfs}["gcloudnfs"] repository}
  *)

  type t = {
    name: string [@main];
    instance: Gcloud_instance.t;
    witness: [ `Create of string | `Existing of string ]
      [@default `Create ".strato-witness.txt"];
    size: [ `GB of int ];
    reuse_data_disk: string option;
  } [@@deriving make]

  let vm_name t = t.instance.Gcloud_instance.name
  let disk_name t =
    Option.value t.reuse_data_disk ~default:(t.name ^ "-pdisk")
  let size_gb t =
    match t.size with
    | `GB i -> Int.to_string i
  let storage_path t =
    "/nfs-pool"
  let witness_path t =
    storage_path t // (match t.witness with `Existing p -> p | `Create p -> p)

  let project =
    let open Genspio_edsl in
    exec [
      "curl";
      "http://metadata.google.internal/computeMetadata/v1/project/project-id";
      "-H"; "Metadata-Flavor: Google"
    ] |> output_as_string

  let ensure t =
    let open Genspio_edsl in
    seq [
      ensure
        ~name:(sprintf "NFS-%s-is-up" t.name)
        (Gcloud_instance.instance_is_up
           ~gcloud:"/home/opam/google-cloud-sdk/bin/gcloud"
           t.instance)
        [
          seq_succeeds_or ~name:(sprintf "Creating-NFS-%s" t.name)
            ~clean_up:[fail] [
            call [
              string "gcloudnfs";
              string "create";
              string "--zone"; string t.instance.Gcloud_instance.zone;
              string "--project"; project;
              string "--network"; string "default";
              string "--machine-type";
              Gcloud_instance.machine_type_name t.instance |> string;
              string "--server-name"; string (vm_name t);
              string "--data-disk-name"; string (disk_name t);
              string "--data-disk-type"; string "pd-standard";
              string "--data-disk-size"; string (size_gb t);
            ];
          ]
        ];
      sayf "Waiting for NFS-%s to be really avaialable" t.name;
      seq_succeeds_or
        ~name:(sprintf "Waiting-for-NFS-%s-to-be-up" t.name)
        ~clean_up:[fail] [
        loop_until_ok
          (exec ["/home/opam/google-cloud-sdk/bin/gcloud"; "compute"; "ssh"; vm_name t;
                 "--zone"; t.instance.Gcloud_instance.zone;
                 "--command"; sprintf "ls %s/" (storage_path t)]
           |> succeeds_silently)
          ~attempts:40
          ~sleep:5;
      ];
      ensure ~name:(sprintf "NFS-%s-witness" t.name)
        (exec ["/home/opam/google-cloud-sdk/bin/gcloud"; "compute"; "ssh"; vm_name t;
               "--zone"; t.instance.Gcloud_instance.zone;
               "--command"; sprintf "ls %s" (witness_path t)]
         |> succeeds_silently) [
        seq_succeeds_or ~name:(sprintf "Creating-NFS-%s-witness" t.name)
          ~clean_up:[fail] [
          exec ["/home/opam/google-cloud-sdk/bin/gcloud"; "compute"; "ssh"; vm_name t;
                "--zone"; t.instance.Gcloud_instance.zone;
                "--command"; sprintf "echo hello > %s" (witness_path t)];
        ]
      ];
    ]

  let instance t = t.instance

  let destroy t =
    let open Genspio_edsl in
    call [
      string "gcloudnfs";
      string "destroy";
      string "--zone"; string t.instance.Gcloud_instance.zone;
      string "--project"; project;
      string "--network"; string "default";
      string "--server-name"; string (vm_name t);
      string "--data-disk-name"; string (disk_name t);
    ]


  let mount t ~mount_point =
    Mount.make
      ~server:(vm_name t)
      ~witness:(witness_path t |> Filename.basename)
      ~remote_path:(storage_path t)
      ~mount_point

end
