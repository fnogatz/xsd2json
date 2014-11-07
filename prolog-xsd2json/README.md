# xsd2json

CHR module to translate an XML Schema into equivalent JSON Schema.

## Installation

All you need is [SWI-Prolog](http://www.swi-prolog.org/). See there for installation instructions.

## Usage

`xsd2json` provides a command line interface. You can use it via

	swipl -q -f cli.pl -- < /path/to/your.xsd

Unfortunately the command line version is way slower than using `xsd2json` programmatically. The `xsd2json.pl` module provides a predicate `xsd2json/2` which can be used to convert a given XSD file into the equivalent JSON Schema. Call it via `swipl -s xsd2json.pl` followed by

	xsd2json('/path/to/your.xsd',JSON)

which binds the `JSON` variable to the created JSON Schema. If you want a pretty output, simply call

	use_module(library(http/json)).

	xsd2json('/path/to/your.xsd',JSON),
	  json_write(user_output,JSON).

instead. This will print the produced JSON Schema in front of the content of the CHR constraint store, so make sure to scroll up if needed.
