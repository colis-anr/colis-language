open SymbolicUtility

module True = struct
  let name = "true"

  let interprete : context -> utility =
    fun _ ->
    return true
end

module False = struct
  let name = "false"

  let interprete : context -> utility =
    fun _ ->
    return false
end

module Echo = struct
  let name = "echo"

  let interprete : context -> utility =
    fun ctx sta ->
    let str = String.concat " " ctx.args in
    let sta = print_stdout ~newline:true str sta in
    [sta, true]
end
