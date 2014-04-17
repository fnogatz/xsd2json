# TAP test suite for xsd2json

Tools to test the `xsd2json` library.

## Installation

The tests are run by [node.js](http://nodejs.org/). Make sure you have installed version 0.10 or later. Before using the testing suite you have to install its dependencies via

	npm install

## Provided tests

### Run interpreted tests

Each file in the `xsd` directory will be converted to a JSON Schema instance. Its output gets compared with the related JSON file in the `json` directory. You can run the interpreted test for all files via

	node test.js interpreted

By specifying files via the `--ignore` parameter it's possible to exclude some, for example:

	node test.js interpreted --ignore schema2 --ignore schema3

If you want to restrict your test to several files use the `--file` parameter, e.g.

	node test.js interpreted --file element_boolean --file /sequence/

The `--file` parameter expects either a file name or a regular expression to filter the available test files.

Use `node test.js interpreted --files` to get a list of possible test files and `node test.js interpreted --help` to display all options.

### Validate tested JSON output

As the expected JSON output files in the `json` subfolder are created manually it might be useful to check if they really satisfy the [JSON Schema Core Meta-Schema](http://json-schema.org/schema), i.e. are valid JSON Schema instances. This test can be run via

	node test.js validate-json

## Prettify TAP output

You can prettify the console output of all tests by using the bundled [tap-prettify](https://npmjs.org/package/tap-prettify) command line tool. Simply pipe the normal TAP output to `node node_modules/tap-prettify/bin/tap-prettify.js -`, e.g.

	node test.js validate-json | node node_modules/tap-prettify/bin/tap-prettify.js -

## Transform a single XSD file

For testing purposes you can convert a single XSD file via `node test.js transform`, e.g.

	node test.js transform < xsd/element_boolean.xsd

This just pipes input and output to the `xsd2json` command line interface.