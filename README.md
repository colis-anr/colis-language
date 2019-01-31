# The CoLiS language

Syntax, parsers, and interpreters for the CoLiS language.

## Notes

The file [NOTES.org](NOTES.org) documents discussion, statistics, decisions, and
hypotheses about the design of the CoLiS language. Headings from the notes can be
referenced in the source code to clarify the implentation using the format
`NOTES[Heading name]`.


## Tests

The tests are run using `make test`. This executes every CoLiS file
(with extension `.cls`) and every Shell file (extension `.sh`) in the
directory `tests/`. The behaviour of the execution, composed by the
return code and the stdout, is compared with the behaviour given in an
accompanying oracle file (`NAME.meta` for `NAME.cls` or `NAME.sh`).

The oracle file is a Yaml-serialised file of the following format:

```yaml
input:
  stdin: <a string>
  arguments: <a list of arguments>
  
output:
  stdout: <a string>
  stderr: <a string>
  return_code: <an integer>
```

## Return Codes

The tool uses different return codes for different kind of errors.

| Code | Meaning
|------|---------
|   0  | Success
|   1  | Success of the concrete execution ending up on a failure
|   2  | Unhandled/unexpected OCaml exception
|   3  | Error in command-line parsing
|   4  | Error while reading input file
|   5  | Error in parsing (Shell or CoLiS)
|   6  | Error in conversion
|   7  | Unsupported utility
|   8  | Unsupported option for a supported utility
|   9  | Error in pretty-printing (Shell or CoLiS)
|  10  | Failure in symbolic execution (while loop boundary reached)
