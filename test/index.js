#!/usr/bin/env node

var fs = require('fs')
var path = require('path')

var xsd2json = require('../index')

var interpreted = require('interpreted')
var tap = require('tap')
var JaySchema = require('jayschema')
var JSON_META_SCHEMA = require('jayschema/lib/suites/draft-04/json-schema-draft-v4.json')
var program = require('commander')
var async = require('async')

// get possible filenames
fs.readdir(path.resolve(__dirname, 'xsd'), function (err, filenames) {
  if (err) {
    throw err
  }

  filenames = filenames.map(function (filename) {
    return filename.split('.').slice(0, -1).join('.')
  })

  parseArguments(filenames)
})

function parseArguments (filenames) {
  program
    .command('interpreted')
    .description('run all interpreted tests')
    .option('--files', 'display available files to test')
    .option('--file <list>', 'XSD file to test (all if not provided). You can specify a regular expression by encapsuling the term in single slashes', list)
    .option('--ignore <list>', 'XSD files to ignore', list)
    .option('--update', 'Update the JSON files by the interpreted result')
    .action(function (cmd) {
      if (cmd.files) {
        console.log(filenames.join('\n'))
        process.exit()
      }

      if (cmd.file === undefined) {
        cmd.file = filenames
      }
      if (cmd.ignore === undefined) {
        cmd.ignore = []
      }
      if (cmd.update === undefined) {
        cmd.update = false
      }

      cmd['available-filenames'] = filenames

      runInterpretedTests(cmd)
    })

  program
    .command('validate-json')
    .description('Validate the JSON test files against the draft-04 JSON Schema')
    .action(function (cmd) {
      validateJSONfiles()
    })

  program.parse(process.argv)
}

function setOptions (options) {
  var files = options['available-filenames']
  if (options.file && options.file.length > 0) {
    var run = []
    options.file.forEach(function (file) {
      if (file[0] === '/' && file[0].slice(-1)[0] === '/') {
        run = run.concat(files.filter(function (filename) {
          return (new RegExp(file.slice(1, -1))).test(filename)
        }))
      } else {
        run.push(file)
      }
    })
    options.file = run
  } else {
    options.file = []
  }

  options.ignore = options.ignore || []
}

function runInterpretedTests (options) {
  setOptions(options)
  var run = options.file.filter(function (el) { return options.ignore.indexOf(el) === -1 })

  interpreted({
    source: path.resolve(__dirname, 'xsd'),
    expected: path.resolve(__dirname, 'json'),
    run: run,
    update: options.update,

    // This method will be used to test the files.
    test: function (name, content, callback) {
      var filename = path.resolve(__dirname, 'xsd', name + '.xsd')
      xsd2json(filename, callback)
    },

    // This method will execute before the file tests.
    start: function (callback) {
      callback(null)
    },

    // This method will execute after the file tests.
    close: function (callback) {
      callback(null)
    }
  })
}

function validateJSONfiles (options) {
  fs.readdir(path.resolve(__dirname, 'json'), function (err, files) {
    if (err) throw err

    tap.test(files.length + ' files', function (t) {
      var js = new JaySchema()

      async.eachSeries(files, function validateFile (filename, callback) {
        t.test(filename, function (t) {
          var data = require(path.resolve(__dirname, 'json', filename))
          var valid = js.validate(data, JSON_META_SCHEMA).length === 0

          t.ok(valid, 'is valid JSON Schema')
          t.end()

          callback(null)
        })
      }, function onEnd (err) {
        if (err) {
          throw err
        }

        t.end()
      })
    })
  })
}

function list (val) {
  return val.split(',')
}
