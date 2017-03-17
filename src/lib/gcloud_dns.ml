
open Common

let do_transaction what ~dns_zone ~node dnsname =
  let open Genspio_edsl in
  let local_ip_file = Filename.temp_file "secotrec" "node-ip" in
  let local_dns_file = Filename.temp_file "secotrec" "dnszone.yaml" in
  let transaction_file = Filename.temp_file "secotrec" "transaction.yaml" in
  let transaction_file_copy =
    Filename.temp_file "secotrec" "transaction-copy.yaml" in
  Printf.printf "SECOTREC: GCloud-DNS transaction: %s\n\
          * IP-file: %s\n\
          * DNS-zone-file: %s\n\
          * Transaction-file: %s\n\
          %!"
    (match what with `Add_or_replace -> "add/replace" | `Delete -> "delete")
    local_ip_file local_dns_file transaction_file;
  run_genspio ~name:"setup-dns-transaction" ~returns:0 (seq [
      (* sayf "Filling %s" local_ip_file; *)
      seq_succeeds_or ~name:(sprintf "Filling-%s" local_ip_file)
        ~clean_up:[fail]
        [(Gcloud_instance.external_ip node >> exec ["cat"])
         |> write_output ~stdout:(string local_ip_file)];
      (* exec ["cat"; local_ip_file]; exec ["echo"; ""]; *)
      (* sayf "Filling %s" local_dns_file; *)
      seq_succeeds_or ~name:(sprintf "Filling-%s" local_dns_file)
        ~clean_up:[fail] [
        exec ["gcloud"; "dns"; "record-sets"; "export";
              local_dns_file; "--zone"; dns_zone];
      ];
      (* exec ["cat"; local_dns_file]; exec ["echo"; ""]; *)
    ]);
  let dns_content = read_file local_dns_file in
  let sections =
    String.split ~on:(`String "---\n") dns_content
    |> List.map ~f:(String.split ~on:(`Character '\n')) in
  let section_to_delete =
    List.find sections ~f:(function
      | kind :: name :: _
        when name = sprintf "name: %s." dnsname ->
        true
      | _ -> false) in
  (* let to_print section = *)
  (*   Option.value_map section ~default:"NONE" ~f:(String.concat ~sep:", ") in *)
  (* printf "Found: %s\n" (to_print section_to_delete); *)
  (* printf "First: %s\n" (to_print (List.nth sections 1)); *)
  let to_transaction section =
    "- " ^ String.concat ~sep:"\n  " section in
  let to_list_of_actions out opt =
    match opt with
    | None -> fprintf out " []\n"
    | Some sec -> fprintf out "\n%s\n" (to_transaction sec);
  in
  let addition =
    match what with
    | `Add_or_replace ->
      Some [
        "kind: dns#resourceRecordSet";
        sprintf "name: %s." dnsname;
        "rrdatas:";
        sprintf "- %s" (read_file local_ip_file);
        "ttl: 300";
        "type: A";
      ]
    | `Delete -> None
  in
  let out = open_out transaction_file in
  fprintf out "---\nadditions:";
  to_list_of_actions out addition;
  fprintf out "deletions:";
  to_list_of_actions out section_to_delete;
  close_out out;
  run_genspio ~name:"exec-dns-transaction" ~returns:0 (
    let clean_up =
      match what with
      | `Add_or_replace -> [ fail ]
      | `Delete -> [ sayf "WARNING: DNS deletion failed" ] in
    seq [
      (* sayf "Transaction file: %s" transaction_file; *)
      (* exec ["cat"; transaction_file]; *)
      seq_succeeds_or ~name:(sprintf "Executing-%s" local_dns_file)
        ~clean_up [
        (* We make a copy because `gcloud` deletes the file after executing: *)
        exec ["cp"; transaction_file; transaction_file_copy];
        exec ["gcloud"; "dns"; "record-sets"; "transaction"; "execute";
              "--transaction-file"; transaction_file_copy;
              "--zone"; dns_zone];
      ];
    ]);
  ()

let check ~dnsname node =
  let open Genspio_edsl in
  let nslook = call [string "nslookup"; string dnsname] in
  succeeds_silently nslook
  &&&
  succeeds_silently
    (output_as_string nslook >>
     call [string "grep"; Gcloud_instance.external_ip node])

type t = {
  zone: string;
  name: string;
} [@@deriving make]

let setup t ~node =
  let open Genspio_edsl in
  let dnsname = t.name in
  let dns_zone = t.zone in
  begin try
    run_genspio ~name:"check-dns" ~output_errors:false ~returns:0 (
      if_seq (check ~dnsname node)
        ~t:[sayf "DNS %s already setup" dnsname; exec ["exit"; "0"]]
        ~e:[sayf "DNS %s needs to be setup" dnsname; exec ["exit"; "1"]]
    )
  with _ ->
    do_transaction `Add_or_replace ~dns_zone ~node dnsname;
    run_genspio ~returns:0 ~name:"wait-for-dns" (
      let attempts = 60 in
      let sleep = 10 in
      seq [
        sayf "Waiting for DNS %s to be *really* up, %d attempts with %d \
              seconds in between (%d sec max)"
          dnsname attempts sleep (attempts * sleep);
        loop_until_ok ~attempts ~sleep
          (check ~dnsname node);
      ]
    )
  end;
  ()

let clean_up t ~node =
  do_transaction `Delete ~dns_zone:t.zone ~node t.name
