module.exports = xsd2json;

var childProcess = require('child_process');
var concat = require('concat-stream');
var path = require('path');


// Prolog executable:
var PROLOG = process.env.SWIPL || 'swipl';

var SOURCE = {
  // Compiled (qlf) Prolog file, created by `npm run-script create-qlf`:
  COMPILED: {
    exec: process.env.XSD2JSON || path.resolve(__dirname, 'lib-pl', 'cli'),
    args: ['-x']
  },
  // Uncompiled Prolog file, for on-the-fly translations (e.g. for development purposes):
  UNCOMPILED: {
    exec: process.env.XSD2JSONPL || path.resolve(__dirname, 'lib-pl', 'cli.pl'),
    args: ['-q', '-g', 'main', '-s']
  }
};


function xsd2json(filename, options, callback) {
  if (arguments.length === 1) {
    options = {};
    callback = null;
  }
  else if (arguments.length === 2) {
    if (typeof options === 'function') {
      callback = options;
      options = {};
    }
    else {
      callback = null;
    }
  }

  var exec;
  var spawnArgs;
  if (options.uncompiled) {
    exec = SOURCE.UNCOMPILED.exec;
    spawnArgs = SOURCE.UNCOMPILED.args;
  }
  else {
    exec = SOURCE.COMPILED.exec;
    spawnArgs = SOURCE.COMPILED.args;
  }
  spawnArgs.push(exec, filename);

  if (options.trace) {
    spawnArgs.push('trace');
  }

  var outputStream = childProcess.spawn(PROLOG, spawnArgs);

  if (typeof callback !== 'function') {
    // no callback given --> return stream
    return outputStream;
  }

  outputStream.stderr.on('data', function(err) {
    if (options.trace) {
      var lines = err.toString().split(/\n/);
      lines.forEach(function(line) {
        if (/^CHR:\s+\([0-9]+\)\s+Apply:.*$/.test(line)) {
          console.log(line);
        }
      });
    }
    else {
      callback(err);
    }
  });

  outputStream.stdout.pipe(reader(callback));
}


function reader(callback) {
  return concat(function(jsonBuff) {
    var jsonString = jsonBuff.toString();
    try {
      var schema = JSON.parse(jsonString);
    } catch(err) {
      return callback(err);
    }

    callback(null, schema);
  });
}
