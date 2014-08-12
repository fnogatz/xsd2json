# xsd2json

Prolog/CHR module to translate an XML Schema into equivalent JSON Schema.


## Background

This tool has been developed as part of my Bachelor's Thesis at University of Ulm, Germany. The thesis ([PDF](http://www.informatik.uni-ulm.de/pm/fileadmin/pm/home/fruehwirth/drafts/Bsc-Nogatz.pdf)) explains the general idea of the translation process via Prolog and [Constraint Handling Rules](http://dtai.cs.kuleuven.be/CHR/about.shtml) (CHR). It also contains tabular and graphical representations of the implemented translations of XML Schema fragments. A shorter explanation of the translation process can be found in the CHR Workshop paper ["From XML Schema to JSON Schema:
Translation with CHR"](http://arxiv.org/pdf/1406.2125v1.pdf).


## Installation & Usage

All you need is [SWI-Prolog](http://www.swi-prolog.org/). `xsd2json` provides a command line interface. You can use it via

	swipl --quiet --nodebug --g 'main,halt' -s cli.pl -- < /path/to/your.xsd

For further instructions, e.g. how to use `xsd2json` programmatically, have a look at the tool's [Readme](https://github.com/fnogatz/xsd2json/tree/master/lib).


## Project Structure

`xsd2json` is developed in a test-driven way. This reflects in the project's folder structure, too: The `/lib` directory contains the Prolog and CHR stuff while you will find the [TAP](http://testanything.org/) testing framework implemented in [node.js](http://nodejs.org/) under `/test`. Both directories contain their own Readme-File that explain the usage of the given tools.
