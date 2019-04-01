exception Contents of string

type t = (string,string) Hashtbl.t

let is_space c = (c=' ') || (c='\t')

let newtable () = (Hashtbl.create 1000:t)

let scan filename table =
  let handle_line s linenumber =
    (* scan a line from right to left *)
    let rec read_packages i l packages_acc =
      (* read a package. We are currently at position [i], and we have
         already discovered a package of name of length [l] which starts at
         position [i+1]. [packages_acc] is the stack of packages we
         have already discovered on line [s]. *)
      if s.[i] = '/'
      then
        if l=0
        then raise (Contents ("empty package name in line "^
                                (string_of_int linenumber)))
        else
          skip_section (i-1) ((String.sub s (i+1) l)::packages_acc)
      else read_packages (i-1) (l+1) packages_acc
    and skip_section i packages_acc =
      (* skip from position [i] on, until we either find the symbol ',' or
       white space. *)
      let c = s.[i] in
      if c = ','
      then read_packages (i-1) 0 packages_acc
      else if is_space c then skip_spaces (i-1) packages_acc
      else skip_section (i-1) packages_acc
    and skip_spaces i packages =
      (* skip white space from here on. Once we arrive at a character
         that is not white space we have found the end of the file name. *)
      if i<0
      then
        raise (Contents ("empty file name in line "^
                           (string_of_int linenumber)))
      else
        if is_space (s.[i])
        then skip_spaces (i-1) packages
        else register (String.sub s 0 (i+1)) packages
    and register filename packages =
      List.iter
        (function package -> Hashtbl.add table package filename)
        packages;
    in let linelength = String.length s
       in if linelength > 0
          then read_packages (linelength-1) 0 []
  in
  let inc = open_in filename in
  let rec handle_lines linenumber =
    handle_line (input_line inc) linenumber;
    handle_lines (linenumber+1)
  in
  try handle_lines 1
  with End_of_file -> close_in inc;;

let print t =
  let lastpackage=ref ""
  in
  Hashtbl.iter
    (fun package filename->
      if package <> !lastpackage
      then begin
          Printf.printf "\nPackage: %s\n" package;
          lastpackage := package
        end;
      Printf.printf "%s\n" filename
    )
    t

let save filename t =
  let cout = open_out filename in
  begin
    output_value cout t;
    close_out cout
  end

let load filename =
  let cin = open_in filename in
  let t = input_value cin in
  begin
    close_in cin;
    t
  end
