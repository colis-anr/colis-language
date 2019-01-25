let log level f =
  Format.eprintf "[%s]" level;
  f Format.eprintf;
  Format.eprintf "@."

let debug f = log "DEBUG" f
let info f = log "INFO" f
let warn f = log "WARN" f
