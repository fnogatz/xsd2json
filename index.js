module.exports = xsd2json;

var childProcess = require('duplex-child-process');
var concat = require('concat-stream');
var path = require('path');


var PROLOG = process.env.SWIPL || 'swipl';
var XSD2JSON = process.env.XSD2JSON || path.resolve(__dirname, 'lib-pl', 'cli');


function xsd2json(filename, callback) {
  var outputStream = childProcess.spawn(PROLOG, ['-q', '-f', XSD2JSON, filename]);

  if (arguments.length === 1) {
    // no callback given --> return stream
    return outputStream;
  }

  outputStream.pipe(reader(callback));
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
