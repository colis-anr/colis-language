module Errors = Errors
module Options = Options

let check_cpu_time_limit () =
  if Sys.time () >= !Options.cpu_time_limit then
    raise Errors.CPU_time_limit_exceeded
