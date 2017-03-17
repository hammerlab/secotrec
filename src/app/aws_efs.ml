

type efs_test = {
  name: string;
  (** Name of the AWS-EFS file-system (a.k.a. "creation-token"). *)

  ssh_aws_node: string;
  (** How to connect to the AWS node over SSH (e.g. "ec2-user@instance.example.com"). *)

} [@@deriving cmdliner]


let run_genspio_over_ssh efs_test g =
  let tmp = Filename.temp_file "genspio-efs-test" ".sh" in
  Secotrec.Common.write_file tmp ~content:(Genspio.Language.to_many_lines g);
  Secotrec.Common.cmdf ~returns:0 "scp %s %s:%s"
    tmp efs_test.ssh_aws_node (Filename.basename tmp);
  Secotrec.Common.cmdf ~returns:0 "ssh %s bash %s"
    efs_test.ssh_aws_node (Filename.basename tmp);
  ()

let efs_up aws_cli efs_test =
  let efs =
    Secotrec.Aws_efs.make efs_test.name in
  let g = Secotrec.Aws_efs.To_genspio.ensure aws_cli efs in
  run_genspio_over_ssh efs_test g;
  ()

let efs_describe aws_cli efs_test =
  let efs =
    Secotrec.Aws_efs.make efs_test.name in
  let g = Secotrec.Aws_efs.To_genspio.describe aws_cli efs in
  run_genspio_over_ssh efs_test g;
  ()

let efs_down aws_cli efs_test =
  let efs =
    Secotrec.Aws_efs.make efs_test.name in
  let g = Secotrec.Aws_efs.To_genspio.destroy aws_cli efs in
  run_genspio_over_ssh efs_test g;
  ()

let () =
  let default =
    Cmdliner.Term.(pure (fun () -> ()) $ pure (), info "aws_test") in
  let clires =
    Cmdliner.Term.(
      eval_choice default [
        pure efs_up
        $ Secotrec.Aws_efs.Aws_cli.cmdliner_term ()
        $ efs_test_cmdliner_term (), info "up";
        pure efs_describe
        $ Secotrec.Aws_efs.Aws_cli.cmdliner_term ()
        $ efs_test_cmdliner_term (), info "describe";
        pure efs_down
        $ Secotrec.Aws_efs.Aws_cli.cmdliner_term ()
        $ efs_test_cmdliner_term (), info "down";
      ]) in
  begin match clires with
  | `Error _ -> exit 1
  | `Ok () -> exit 0
  | `Version -> exit 2
  | `Help -> exit 3
  end


