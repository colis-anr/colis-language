open SymbolicUtility

let null_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

let with_formatter_to_string f =
  let buf = Buffer.create 8 in
  let fmt = Format.formatter_of_buffer buf in
  let v =f fmt in
  Format.pp_print_flush fmt ();
  (Buffer.contents buf, v)

let eval ~utility ctx fun_and_args =
  Format.(
    eprintf "ctx.args = [%a]@."
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") (fun fmt -> fprintf fmt "%S")) ctx.args
  );
  let pos_args = Cmdliner.Arg.(non_empty & pos_all string [] & info []) in (* any list of strings *)
  let (err, result) =
    with_formatter_to_string @@ fun err ->
    Cmdliner.Term.(
      eval
        (
          fun_and_args $ pos_args,
          info utility ~exits:default_exits
        )
        ~help:null_formatter
        ~err
        ~env:(fun _ -> None) (*(fun var -> Env.IdMap.find_opt var ctx.env)*) (* FIXME *)
        ~catch:false
        ~argv:((utility :: ctx.args) |> Array.of_list)
    )
  in
  match result with
  | `Ok a -> a
  | `Version -> Errors.unsupported ~utility "version"
  | `Help -> Errors.unsupported ~utility "help"
  | `Error (`Parse | `Term) -> unsupported ~utility ("parse error: " ^ err)
  | `Error `Exn -> assert false (* because ~catch:false *)

let flag l =
  Format.(
    eprintf "flag: [%a]@."
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") (fun fmt -> fprintf fmt "%S")) l
  );
  Cmdliner.Arg.(value & flag & info l)

let fun_ f ctx = Cmdliner.Term.const (f ctx)
let ($) = Cmdliner.Term.($)

type 'a t = 'a Cmdliner.Term.t
