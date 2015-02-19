# xsd2json

Prolog/CHR module to translate an XML Schema into equivalent JSON Schema.

## Installation

All you need is [SWI-Prolog](http://www.swi-prolog.org/). See there for installation instructions.

The command line interface is compilated using swipl's [`-c` option](http://www.swi-prolog.org/pldoc/doc_for?object=section%282,%272.10%27,swi%28%27/doc/Manual/compilation.html%27%29%29):

	swipl --goal=main -o cli -c cli.pl

## Usage

`xsd2json` provides a command line interface. You can directly execute it via

	./cli /path/to/your.xsd

The `xsd2json.pl` module provides a predicate `xsd2json/2` which can be used to convert a given XSD file into the equivalent JSON Schema. Call it via `swipl -s xsd2json.pl` followed by

	xsd2json('/path/to/your.xsd',JSON)

which binds the `JSON` variable to the created JSON Schema. If you want a pretty output, simply call

	use_module(library(http/json)).

	xsd2json('/path/to/your.xsd',JSON),
	  json_write(user_output,JSON).

instead. This will print the produced JSON Schema in front of the content of the CHR constraint store, so make sure to scroll up if needed.
