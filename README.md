# The CoLiS language

Syntax, parsers, and interpreters for the CoLiS language.

## Notes

The file [NOTES.org](NOTES.org) documents discussion, statistics, decisions, and
hypotheses about the design of the CoLiS language. Headings from the notes can be
referenced in the source code to clarify the implentation using the format
`NOTES[Heading name]`.


## Tests

The tests are run using `make tests`. This executes every CoLiS file (with extension
`.cls`) and every Shell file (extension `.sh`) in the directory `tests/`. The behaviour of
the execution, composed by the return code and the stdout, is compared with the behaviour
given in an accompanying oracle file (`NAME.oracle` for `NAME.cls` or `NAME.sh`).

The oracle file hase the following format (the stdout is prefixed by `> ` to clearly
indicate empty lines):

```
ORACLE FILE FORMAT

RESULT: true|false
STDOUT:
> ...
```
