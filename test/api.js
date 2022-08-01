const assert = require('assert')
const path = require('path')

const async = require('async')

const xsd2json = require('../index')

const schema = {
  $schema: 'http://json-schema.org/draft-04/schema#',
  type: 'string'
}

async.parallel([
  function (cb) {
    xsd2json(path.resolve(__dirname, 'xsd', 'schema.xsd'), {}, function (err, res) {
      if (err) {
        return cb(err)
      }

      assert.deepStrictEqual(res, schema)

      cb(null, res)
    })
  },
  function (cb) {
    xsd2json(path.resolve(__dirname, 'xsd', 'schema.xsd'), { noExe: true }, function (err, res) {
      if (err) {
        return cb(err)
      }

      assert.deepStrictEqual(res, schema)

      cb(null, res)
    })
  }
], function (err, results) {
  if (err) {
    console.log(err)
    process.exit(1)
  }

  console.log(results)
  process.exit(0)
})
