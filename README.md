node-xsd2json
=============

[![Build Status](https://travis-ci.org/MGDIS/xsd2json.svg?branch=master)](https://travis-ci.org/MGDIS/xsd2json)
[![npm version](https://badge.fury.io/js/xsd2json2.svg)](http://badge.fury.io/js/xsd2json2)

*Transform complex XSD schemas into JSON schemas*

**Disclaimer: this is a fork of the original [xsd2json](https://github.com/fnogatz/xsd2json) program.
It fixes a few issues that we encountered but it is also a few commits behind on the prolog code.
The first project you should have a look at is the original one.**

This small program is applied to a XSD file, and performs a few transformations. It is mostly a wrapper to [xsd2json](https://github.com/fnogatz/xsd2json).

  - recursively fetch the included sub-schemas and append their type definitions to the root schema
  - merge extended types so that a type definition contains all that it needs
  - apply the original prolog based [xsd2json](https://github.com/fnogatz/xsd2json) program
  - post-process the json-schema to fix a few bugs

Install
-------

To install the command line tool:

    npm install -g xsd2json2

To install as a dependancy in a project:

    npm install xsd2json2

In both cases you will also need [SWI Prolog](http://swi-prolog.org/) installed and the swipl executable available in your path.

Usage
-----

As a command line tool:

    xsd2json2 --help
    xsd2json2 -v ./test/resources/chapter04ord1.xsd

As a library:

    var xsd2json = require('xsd2json2').xsd2json;

    xsd2json('./users.xsd', function(err, jsonSchema) {
        fs.writeFile('./users.json', jsonSchema, callback);
    });
