module.exports = xsd2json

var childProcess = require('child_process')
var concat = require('concat-stream')
var path = require('path')

var CLI = path.resolve(__dirname, 'lib-pl', 'cli')

function xsd2json (filename, options, callback) {
  if (arguments.length === 1) {
    options = {}
    callback = null
  } else if (arguments.length === 2) {
    if (typeof options === 'function') {
      callback = options
      options = {}
    } else {
      callback = null
    }
  }

  var spawnArgs = []
  for (var key in options) {
    spawnArgs.push('--' + key + '=' + options[key])
  }
  spawnArgs.push(filename)

  var outputStream = childProcess.spawn(CLI, spawnArgs)

  if (typeof callback !== 'function') {
    // no callback given --> return stream
    return outputStream
  }

  outputStream.stderr.on('data', function (err) {
    if (options.trace) {
      var lines = err.toString().split(/\n/)
      lines.forEach(function (line) {
        if (/^CHR:\s+\([0-9]+\)\s+Apply:.*$/.test(line)) {
          console.log(line)
        }
      })
    } else {
      callback(err)
    }
  })

  outputStream.stdout.pipe(reader(callback))
}

function reader (callback) {
  return concat(function (jsonBuff) {
    var jsonString = jsonBuff.toString()
    try {
      var schema = JSON.parse(jsonString)
    } catch (err) {
      return callback(err)
    }

    callback(null, schema)
  })
}
