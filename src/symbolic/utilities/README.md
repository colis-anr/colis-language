How to Add a Utility
====================

A utility is defined as a module of signature:

```ocaml
val name : string
val interprete : context -> utility
```

where:

- `context` provides information about the arguments, the current
  working directory and the environment in which the utility is
  executed.

- `type utility = state -> (state * bool) list`

One common problem in writing utilities is to handle arguments. For
utilities following the XBD Utility Syntax Guidelines (that is, most
of the POSIX or GNU utilities), cmdliner proves to be a good arguments
parser.

Let us give a simple example definition of `mkdir`. We assume here
that we have functions:

```ocaml
val mkdir : cwd:Path.normal -> string -> utility
val mkdir_parents : cwd:Path.normal -> string -> utility
```

that take the current working directory and a path and returns the
corresponding utility definition. We will first wrap them in only one
interpretation function

```ocaml
let interprete parents ctx args =
  multiple_times
    (if parents then mkdir_parents else mkdir)
    ctx.cwd args

val interprete : bool -> context -> string list -> utility
```

that assumes arguments parsing and takes first all the flags (as
booleans) then the context and then the positional arguments as a list
of strings. We then wrap that one in a function doing the actual
parsing with cmdliner

```ocaml
let interprete ctx =
  let parents = Cmdliner.Arg.(value & flag & info ["p"; "parents"]) in
  cmdliner_eval_utility
    ~utility:"mkdir"
	Cmdliner.Term.(cons interprete & parents)
	ctx

val interprete : context -> utility
```

This function takes only the context. It first defines a cmdliner flag
`parents` that can be either `-p` or `--parents` (one letter arguments
take one dash and can be combined, longer arguments take two
dashes). It then calls `cmdliner_eval_utility`, a wrapper around
`Cmdliner.Term.eval` for utilities. This function requires the
utility's name (mainly for its error messages), a cmdliner definition
of the function and its flags and the context.
