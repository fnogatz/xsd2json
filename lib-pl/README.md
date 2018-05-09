# xsd2json

Prolog/CHR module to translate an XML Schema into equivalent JSON Schema.

## Installation

All you need is [SWI-Prolog](http://www.swi-prolog.org/). See there for installation instructions.

### Pre-Compilation

It is possible to create a pre-compiled file which increases the tool's performance significantly. The command line interface is compiled using swipl's [`-c` option](http://www.swi-prolog.org/pldoc/doc_for?object=section%282,%272.10%27,swi%28%27/doc/Manual/compilation.html%27%29%29):

```bash
swipl -g main -o cli.exe -c cli.pl
```

*The `.exe` suffix is chosen for compatibility with Windows systems. See [issue #51](https://github.com/fnogatz/xsd2json/issues/51) for more details.*

## Usage as CLI

`xsd2json` provides a command line interface. You can directly execute it via

```bash
swipl -g main cli.pl -- /path/to/your.xsd
```

Call with `--help` instead of the XSD path to get more options.

After the pre-compilation step mentioned before, the created executable can be called via:

```bash
./cli.exe /path/to/your.xsd
```

### Synopsis

```
USAGE: xsd2json [options] <path>

convert a XSD file into equivalent JSON schema

Options:
--whitespace   -s  atom=remove    whitespace handling, one of
                                      remove   :  clean whitespace  [default]
                                      preserve :  keep whitespace
--indentation  -i  integer=2      set indentation level
                                    use 0 for single-line output
--trace        -t  boolean=false  run with activated tracing
--version      -v  boolean=false  show xsd2json version information
--debug-info       boolean=false  show information usually needed for debugging
--help         -h  boolean=false  display this help
```

## Usage with SWI-Prolog

The `xsd2json.pl` module provides a predicate `xsd2json/2` which can be used to convert a given XSD file into the equivalent JSON Schema. Call it via `swipl -s xsd2json.pl` followed by

```prolog
xsd2json('/path/to/your.xsd',JSON)
```

This binds the `JSON` variable to the created JSON Schema. If you want a pretty output, simply call instead:

```prolog
use_module(library(http/json)).

xsd2json('/path/to/your.xsd',JSON),
  json_write(user_output,JSON).
```

This will print the produced JSON Schema in front of the content of the CHR constraint store, so make sure to scroll up if needed.
