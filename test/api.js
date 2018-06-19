var assert = require('assert')
var path = require('path')

var async = require('async')

var xsd2json = require('../index')

var schema = {
  $schema: 'http://json-schema.org/draft-04/schema#',
  type: 'string'
}

async.parallel([
  function (cb) {
    xsd2json(path.resolve(__dirname, 'xsd', 'schema.xsd'), {}, function (err, res) {
      if (err) {
        process.exit(1)
      }

      assert.deepStrictEqual(res, schema)

      cb(null, res)
    })
  },
  function (cb) {
    xsd2json(path.resolve(__dirname, 'xsd', 'schema.xsd'), { noExe: true }, function (err, res) {
      if (err) {
        console.log(err)
        process.exit(1)
      }

      assert.deepStrictEqual(res, schema)

      cb(null, res)
    })
  }
], function (err, results) {
  if (err) {
    process.exit(1)
  }

  console.log(results)
  process.exit(0)
})
