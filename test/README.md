# TAP test suite for xsd2json

Tools to test the `xsd2json` Prolog library.

## Provided tests

### Run interpreted tests

Each file in the `xsd` directory will be converted to a JSON Schema instance. Its output gets compared with the related JSON file in the `json` directory. You can run the interpreted test for all files via

	npm test

By specifying files via the `--ignore` parameter it's possible to exclude some, for example:

	node test/index.js interpreted --ignore schema2 --ignore schema3

If you want to restrict your test to several files use the `--file` parameter, e.g.

	node test/index.js interpreted --file element_boolean --file /sequence/

The `--file` parameter expects either a file name or a regular expression to filter the available test files.

Use `node test/index.js interpreted --files` to get a list of possible test files and `node test/index.js interpreted --help` to display all options.

### Validate tested JSON output

As the expected JSON output files in the `json` subfolder are created manually it might be useful to check if they really satisfy the [JSON Schema Core Meta-Schema](http://json-schema.org/schema), i.e. are valid JSON Schema instances. This test can be run via

	npm run-script test-examples

## Prettify TAP output

You can prettify the console output of all tests by using the [tap-prettify](https://npmjs.org/package/tap-prettify) command line tool. Simply pipe the normal TAP output to `tap-prettify -`, e.g.

	npm run-script test-examples | tap-prettify -
