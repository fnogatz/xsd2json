node-xsd2json
=============

*Transform complex XSD schemas into JSON schemas*

This small program is applied to a XSD file, and performs a few transformations. It is mostly a wrapper to [xsd2json](https://github.com/fnogatz/xsd2json).

  - recursively fetch the included sub-schemas and append their type definitions to the root schema
  - merge extended types so that a type definition contains all that it needs
  - apply the original prolog based [xsd2json](https://github.com/fnogatz/xsd2json) program
  - post-process the json-schema to fix a few bugs

Install
-------

*Not published to NPM yet. You should just clone this repository.*

    git clone https://github.com/MGDIS/xsd2json
    cd xsd2json
    npm install

You will also need [SWI Prolog](http://swi-prolog.org/).

Usage
-----

    ./bin/xsd2json --help
    ./bin/xsd2json -v ./test/resources/chapter04ord1.xsd