open Format
open Constraints
open Clause
open SymbolicUtility

module Install = struct
  let name = "emacs-package-install"

  let interprete preInst _postInst ctx args =
    let pkgName = match args with
      | [pkg] -> pkg
      | _ -> assert false
    in
    let pkgFile = String.concat "/" ["/var/lib/emacsen-common/state/package";
                                     pkgName]
    in
    if preInst then
      (if_then_else
         (call "test" ctx ["-e"; pkgFile])
         (call "rm" ctx [pkgFile])
         (under_specifications @@ fun ~root ~root' ->
          [
            success_case
              ~descr:(asprintf "emacs-package-install --preinst '%s': not found"
                        pkgFile)
              begin
                eq root root'
              end
          ;
          ]
         )
      )
    else (* --postInst is optional *)
      (call "touch" ctx [pkgFile])


  let interprete ctx : utility =
    let preInst = Cmdliner.Arg.(value & flag & info ["--preinst"]) in
    let postInst = Cmdliner.Arg.(value & flag & info ["--postinst"]) in
    cmdliner_eval_utility
      ~utility:name
      Cmdliner.Term.(const interprete $ preInst $ postInst)
      ctx

end

module Remove = struct
  let name = "emacs-package-remove"

  let interprete preRm ctx args =
    let pkgName = match args with
      | [pkg] -> pkg
      | _ -> assert false
    in
    let pkgFile = String.concat "/" ["/usr/lib/emacsen-common/packages/install";
                                     pkgName]
    in
    (if_then_else
       (call "test" ctx ["-e"; pkgFile])
       (call "rm" ctx [pkgFile])
       (under_specifications @@ fun ~root ~root' ->
        [
          success_case
            ~descr:(asprintf "emacs-package-install --preinst '%s': not found"
                      pkgFile)
            begin
              eq root root'
            end
        ;
        ]
       )
    )

  let interprete ctx : utility =
    let preRm = Cmdliner.Arg.(value & flag & info ["--prerm"]) in
    cmdliner_eval_utility
      ~utility:name
      Cmdliner.Term.(const interprete $ preRm)
      ctx

end

