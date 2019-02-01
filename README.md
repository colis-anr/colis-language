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
|   0  | In concrete execution: Execution resulted in success
|   0  | In symbolic execution: Only success states found
|   1  | In concrete execution: Execution resulted in error
|   1  | In symbolic execution: At least one error state found
|   2  | Unhandled/unexpected OCaml exception
|   3  | Error in command-line parsing
|   4  | Error while reading input file
|   5  | Error in parsing (Shell or CoLiS)
|   6  | Error in conversion
|   7  | Unsupported utility (only with option --fail-on-unknown-utility)
|   8  | Unsupported option for a supported utility (only with option --fail-on-unknown-utility)
|   9  | Error in pretty-printing (Shell or CoLiS)
|  10  | In symbolic execution: No error states found, but maximum number of loop iterations reached for some state
