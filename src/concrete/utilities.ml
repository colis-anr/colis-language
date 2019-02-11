(** Concrete interpretation of selected shell builtins.

    See subsections of http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap04.html#tag_20
  *)

open Format
open Semantics__State
open Semantics__Buffers

(** All known utilities, from https://pubs.opengroup.org/onlinepubs/9699919799/ *)
let known_utilities = [
  "admin"; "alias"; "ar"; "asa"; "at"; "awk"; "basename"; "batch"; "bc"; "bg";
  "c99"; "cal"; "cat"; "cd"; "cflow"; "chgrp"; "chmod"; "chown"; "cksum"; "cmp";
  "comm"; "command"; "compress"; "cp"; "crontab"; "csplit"; "ctags"; "cut";
  "cxref"; "date"; "dd"; "delta"; "df"; "diff"; "dirname"; "du"; "echo"; "ed";
  "env"; "ex"; "expand"; "expr"; "false"; "fc"; "fg"; "file"; "find"; "fold";
  "fort77"; "fuser"; "gencat"; "get"; "getconf"; "getopts"; "grep"; "hash";
  "head"; "iconv"; "id"; "ipcrm"; "ipcs"; "jobs"; "join"; "kill"; "lex"; "link";
  "ln"; "locale"; "localedef"; "logger"; "logname"; "lp"; "ls"; "m4"; "mailx";
  "make"; "man"; "mesg"; "mkdir"; "mkfifo"; "more"; "mv"; "newgrp"; "nice"; "nl";
  "nm"; "nohup"; "od"; "paste"; "patch"; "pathchk"; "pax"; "pr"; "printf"; "prs";
  "ps"; "pwd"; "qalter"; "qdel"; "qhold"; "qmove"; "qmsg"; "qrerun"; "qrls";
  "qselect"; "qsig"; "qstat"; "qsub"; "read"; "renice"; "rm"; "rmdel"; "rmdir";
  "sact"; "sccs"; "sed"; "sh"; "sleep"; "sort"; "split"; "strings"; "strip";
  "stty"; "tabs"; "tail"; "talk"; "tee"; "test"; "time"; "touch"; "tput"; "tr";
  "true"; "tsort"; "tty"; "type"; "ulimit"; "umask"; "unalias"; "uname";
  "uncompress"; "unexpand"; "unget"; "uniq"; "unlink"; "uucp"; "uudecode";
  "uuencode"; "uustat"; "uux"; "val"; "vi"; "wait"; "wc"; "what"; "who"; "write";
  "xargs"; "yacc"; "zcat"
]

let unknown_utility name sta =
  let str = sprintf "command not found: %s" name in
  let stdout = Stdout.(sta.stdout |> output str |> newline) in
  {sta with stdout}, Result false

let unknown_argument name arg sta =
  let str = sprintf "invalid argument for command %s: %s" name arg in
  let stdout = Stdout.(sta.stdout |> output str |> newline) in
  {sta with stdout}, Result false

let unimplemented_utility ?(msg="command not found") ~name sta =
  let str = sprintf "command not implemented: %s" name in
  let stdout = Stdout.(sta.stdout |> output str |> newline) in
  {sta with stdout}, Incomplete

let unimplemented_argument ?(msg="Unknown argument") ~name ~arg sta =
  let str = sprintf "Argument for command %s not implemented: %s" name arg in
  let stdout = Stdout.(sta.stdout |> output str |> newline) in
  {sta with stdout}, Incomplete

let test (sta : state) : string list -> (bool state_result) = function
  | [sa; "="; sb] ->
     (sta, Result (sa = sb))
  | [sa; "!="; sb] ->
     (sta, Result (sa <> sb))
  | _ ->
     unknown_argument "test" "" sta

let interp_utility : state -> string -> string list -> bool state_result =
  fun sta name args ->
  match name with
  | "echo" ->
     let stdout =
       match args with
       | "-n" :: args ->
          let str = String.concat " " args in
          Stdout.(sta.stdout |> output str)
       | _ ->
          let str = String.concat " " args in
          Stdout.(sta.stdout |> output str |> newline)
     in
     {sta with stdout}, Result true
  | "true" ->
     sta, Result true
  | "false" ->
     sta, Result false
  | "test" -> test sta args
  | "grep" -> (* Just for testing stdin/stdout handling *)
     begin match args with
     | [word] ->
        let stdout, result =
          let re = Str.regexp_string word in
          let f (stdout, res) line =
            try
              ignore (Str.search_forward re line 0);
              Stdout.(stdout |> output line |> newline), true
            with Not_found ->
              stdout, res
          in
          List.fold_left f (sta.stdout, false) sta.stdin
        in
        let sta' = {sta with stdout; stdin=Stdin.empty} in
        sta', Result result
     | _ ->
        let str = "grep: not exactly one argument" in
        let stdout = Stdout.(sta.stdout |> output str |> newline) in
        {sta with stdout}, Result false
     end
  | _ ->
    if List.mem name known_utilities then
      unimplemented_utility ~name sta
    else
      unknown_utility name sta

