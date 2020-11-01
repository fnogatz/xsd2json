module.exports = xsd2json

const childProcess = require('child_process')
const concat = require('concat-stream')
const path = require('path')

const CLI = path.resolve(__dirname, 'lib-pl', 'cli.exe')
const CLIPL = path.resolve(__dirname, 'lib-pl', 'cli.pl')
const SWI = 'swipl'

const reservedKeys = [
  'noExe',
  'swi'
]

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

  let spawnArgs = []
  for (const key in options) {
    if (reservedKeys.indexOf(key) >= 0) {
      continue
    }

    spawnArgs.push('--' + key + '=' + options[key])
  }
  spawnArgs.push(filename)

  let outputStream
  if (options.noExe) {
    spawnArgs = [
      '-g',
      'main',
      CLIPL,
      '--'
    ].concat(spawnArgs)

    outputStream = childProcess.spawn(options.swi || SWI, spawnArgs)
  } else {
    outputStream = childProcess.spawn(CLI, spawnArgs)
  }

  if (typeof callback !== 'function') {
    // no callback given --> return stream
    return outputStream
  }

  outputStream.stderr.on('data', function (err) {
    if (options.trace) {
      const lines = err.toString().split(/\n/)
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
    const jsonString = jsonBuff.toString()
    let schema
    try {
      schema = JSON.parse(jsonString)
    } catch (err) {
      return callback(err)
    }

    callback(null, schema)
  })
}
