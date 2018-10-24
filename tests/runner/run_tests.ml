let () =
  Options.parse_cmd_line ();
  Engine.run_tests ()
