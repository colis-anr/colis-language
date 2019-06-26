open SymbolicUtility

let null_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

let with_formatter_to_string f =
  let buf = Buffer.create 8 in
  let fmt = Format.formatter_of_buffer buf in
  let v =f fmt in
  Format.pp_print_flush fmt ();
  (Buffer.contents buf, v)

let eval ~utility ctx ?(ignore=[]) ?(refuse=[]) fun_and_args =
  let pos_args = Cmdliner.Arg.(non_empty & pos_all string [] & info []) in (* any list of strings *)
  let argv =
    ctx.args
    |> List.filter (fun arg -> not (List.mem arg ignore))
    |> (fun args -> utility :: args)
  in
  List.iter
    (fun arg ->
       if List.mem arg refuse then
         Errors.unsupported ~utility ("forbidden argument: " ^ arg))
    argv;
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
        ~env:(fun var -> Env.IdMap.find_opt var ctx.env)
        ~catch:false
        ~argv:(argv |> Array.of_list)
    )
  in
  match result with
  | `Ok a -> a
  | `Version -> Errors.unsupported ~utility "version"
  | `Help -> Errors.unsupported ~utility "help"
  | `Error (`Parse | `Term) -> unsupported ~utility ("parse error: " ^ err)
  | `Error `Exn -> assert false (* because ~catch:false *)

let flag l = Cmdliner.Arg.(value & flag & info l)

let fun_ f ctx = Cmdliner.Term.const (f ctx)
let ($) = Cmdliner.Term.($)

type 'a t = 'a Cmdliner.Term.t
