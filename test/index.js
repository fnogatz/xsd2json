var fs = require('fs');
var path = require('path');

var xsd2json = require('../index');

var interpreted = require('interpreted');
var tap = require('tap');
var tv4 = require('tv4');
var parser = require('nomnom');
var async = require('async');


// get possible filenames
fs.readdir(path.resolve(__dirname, 'xsd'), function(err, filenames) {
  filenames = filenames.map(function(filename) {
    return filename.split('.').slice(0,-1).join('.');
  });

  parseArguments(filenames);
});


function parseArguments(filenames) {
  parser.command('interpreted')
    .help('run all interpreted tests')
    .option('files', {
      help: 'display available files to test',
      flag: true,
      callback: function() {
        return filenames.join('\n');
      }
    })
    .option('file', {
      abbr: 'f',
      help: 'XSD file to test (all if not provided). You can specify a regular expression by encapsuling the term in single slashes.',
      list: true,
      default: filenames
    })
    .option('ignore', {
      abbr: 'i',
      help: 'XSD file to ignore',
      list: true,
      choices: filenames,
      default: []
    })
    .option('available-filenames', {
      hidden: true,
      list: true,
      default: filenames
    })
    .option('update', {
      flag: true,
      help: 'Update the JSON files by the interpreted result'
    })
    .callback(runInterpretedTests);

  parser.command('validate-json')
    .help('validate the JSON test files against the draft-04 JSON Schema')
    .callback(validateJSONfiles);

  parser.parse();
}


function setOptions(options) {
  var files = options['available-filenames'];
  if (options.file && options.file.length > 0) {
    var run = [];
    options.file.forEach(function(file) {
      if (file[0] === '/' && file[0].slice(-1)[0] === '/') {
        run = run.concat(files.filter(function(filename) {
          return (new RegExp(file.slice(1,-1))).test(filename);
        }));
      }
      else {
        run.push(file);
      }
    });
    options.file = run;
  } else {
    options.file = [];
  }

  options.ignore = options.ignore || [];
}


function runInterpretedTests(options) {
  setOptions(options);
  var run = options.file.filter(function(el) { return options.ignore.indexOf(el) === -1; });

  interpreted({
    source: path.resolve(__dirname, 'xsd'),
    expected: path.resolve(__dirname, 'json'),
    run: run,
    update: options.update,

    // This method will be used to test the files.
    test: function (name, content, callback) {
      xsd2json(content, callback);
    },

    // This method will execute before the file tests.
    start: function (callback) {
      callback(null);
    },

    // This method will execute after the file tests.
    close: function (callback) {
      callback(null);
    }
  });
}


function validateJSONfiles(options) {
  fs.readdir(path.resolve(__dirname, 'json'), function(err, files) {
    if (err) throw err;

    var schema = require(path.resolve(__dirname, 'meta-schema-04.json'));

    tap.test(files.length+' files', function(t) {
      async.eachSeries(files, function validateFile(filename, callback) {
        t.test(filename, function(t) {
          var data = require(path.resolve(__dirname, 'json', filename));
          var valid = tv4.validate(data, schema);

          if (!valid)
            throw new Error(util.inspect(tv4.error));
          
          t.ok(valid, 'is valid JSON Schema');
          t.end();

          callback(valid ? null : tv4.error)
        });
      }, function onEnd(err) {
        t.end();
      });
    });
  });
}