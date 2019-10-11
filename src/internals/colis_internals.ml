module Errors = Errors
module Options = Options

let check_cpu_time_limit () =
  if Sys.time () >= !Options.cpu_time_limit then
    raise Errors.CpuTimeLimitExceeded

let check_memory_limit () =
  if Gc.allocated_bytes () >= !Options.memory_limit then
    raise Errors.MemoryLimitExceeded
