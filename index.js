module.exports = xsd2json;

var childProcess = require('duplex-child-process');
var concat = require('concat-stream');
var path = require('path');
var fs = require('fs');


var PROLOG = process.env.SWIPL || 'swipl';
var XSD2JSON = process.env.XSD2JSON || path.resolve(__dirname, 'lib-pl', 'cli.pl');


function xsd2json(xsdString, callback) {
  if (arguments.length === 0) {
    // use as Stream
    return converter();
  }

  if (xsdString[0] !== '<') {
    // is no XML string  --> use as file path
    fs.createReadStream(xsdString)
      .pipe(converter())
      .pipe(reader(callback));
    return;
  }

  var stream = converter();
  stream.pipe(reader(callback));
  stream.write(xsdString);
  stream.end();
}


function converter() {
  return childProcess.spawn(PROLOG, ['-q', '-f', XSD2JSON, '--']);
}


function reader(callback) {
  return concat(function(jsonBuff) {
    var jsonString = jsonBuff.toString();
    try {
      var schema = JSON.parse(jsonString);
    } catch(err) {
      callback(err);
    }

    callback(null, schema);
  });
}
