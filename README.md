# xsd2json

Translate an XML Schema into equivalent JSON Schema based on SWI-Prolog and Constraint Handling Rules (CHR). For usage directly in Prolog or node.js.

## Installation

First, make sure [SWI-Prolog](http://www.swi-prolog.org/) is installed. This module expects the `swipl` binary to be in PATH somewhere. In other words, type this:

```bash
swipl
```

If that works, so will xsd2json.

Using [npm](http://npmjs.org/), you can install xsd2json by calling this:

```bash
npm install -g xsd2json
```

Or simply clone this repository and manually run the `preinstall` scripts to [create pre-compiled Prolog files](https://github.com/fnogatz/xsd2json/tree/master/lib-pl#pre-compilation):

```bash
git clone https://github.com/fnogatz/xsd2json.git
npm run preinstall
```

## Usage as CLI

If you install xsd2json via npm it will create a new command `xsd2json`:

```bash
xsd2json /path/to/your.xsd > /created/schema.json
```

## Usage with node.js

The xsd2json module can be used programmatically as function or stream:

```js
var xsd2json = require('xsd2json');
var filename = 'test.xsd';

// use as stream: Read from stdin
xsd2json(filename)
  .pipe(process.stdout);

// use as function
xsd2json(filename, function(err, schemaObject) {
  console.log(JSON.stringify(schemaObject, null, 2));
});
```

In addition to the [command line options provided by the Prolog module](https://github.com/fnogatz/xsd2json/tree/master/lib-pl#synopsis), there are the following options available in the node.js module:
- `noExe: true | false (default)`: Use the native Prolog interface instead of the pre-compiled `cli.exe`. This might be useful for MacOS users (see [issue #87](https://github.com/fnogatz/xsd2json/issues/87) for more details).
- `swi: 'swipl' (default)`: Executable to call SWI-Prolog.

## Usage with Prolog

xsd2json provides a predicate `xsd2json(+XSD,-JSON)`, which holds for a given XML Schema (either file path, URL or `stream`). For instructions on how to use xsd2json programmatically in Prolog, have a look at the Prolog module's [Readme](https://github.com/fnogatz/xsd2json/tree/master/lib-pl).

## Background

This tool has been developed as part of my Bachelor's Thesis at University of Ulm, Germany. The thesis ([PDF](http://www.informatik.uni-ulm.de/pm/fileadmin/pm/home/fruehwirth/drafts/Bsc-Nogatz.pdf)) explains the general idea of the translation process via Prolog and [Constraint Handling Rules](http://dtai.cs.kuleuven.be/CHR/about.shtml) (CHR). It also contains tabular and graphical representations of the implemented translations of XML Schema fragments. A shorter explanation of the translation process can be found in the CHR Workshop paper ["From XML Schema to JSON Schema:
Translation with CHR"](http://arxiv.org/pdf/1406.2125v1.pdf).

## Project Structure

xsd2json is developed in a test-driven way. This reflects in the project's folder structure, too: The `/lib-pl` directory contains the Prolog and CHR stuff while you will find the [TAP](http://testanything.org/) testing framework implemented in [node.js](http://nodejs.org/) under `/test`. Both directories contain their own Readme-File that explain their usage.

## Known Bugs

xsd2json's tests are always executed using the latest stable release of SWI-Prolog. Make sure to have the latest version installed before you file a bug.

Apparently SWI-Prolog of version 7.2.x ships with a bug in the CHR implementation. Use version 7.4 or higher instead.

## Alternatives

* [node-xsd2json](https://github.com/MGDIS/xsd2json) (Fork of this module.)
* [Jsonix Schema Compiler](https://github.com/highsource/jsonix-schema-compiler)
