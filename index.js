/**
 * Main for the node-xsd2json project.
 *
 * The function used by the command line interface is the xsd2json function.
 * Other functions are internals exposed for a potential API usage.
 */

var fs = require('fs');
var xml2js = require('xml2js');
var temp = require('temp').track();

var xsdInclusions = require('./lib/xsd-inclusions');
var xsdExtensions = require('./lib/xsd-extensions');
var prologWrapper = require('./lib/prolog-wrapper');
var jsonProcessing = require('./lib/json-processing');

/**
 * @param [string] filePath - the path to the XSD schema file that needs to be translated into JSON schema
 */
exports.xsd2json = function(filePath, callback) {
	xsdInclusions.mergeInclusions('./', filePath, function(err, schema) {
		if (err) return callback(err);
		var mergedSchema = xsdExtensions.mergeExtensions(schema);
		var builder = new xml2js.Builder();
		var xml = builder.buildObject(mergedSchema);

		temp.open('xsd2json2', function(err, info) {
			if (err) return callback(err);

			fs.write(info.fd, xml);
			fs.close(info.fd, function(err) {
				if (err) return callback(err);

				prologWrapper.xsd2jsonWrapper(info.path, function(err, schema) {
					if (err) return callback(err);
					callback(null, jsonProcessing.postProcessing(schema));
				});
			});
		});
	});
};

// Internals exposed for API usage
exports.mergeInclusions = xsdInclusions.mergeInclusions;
exports.mergeExtensions = xsdExtensions.mergeExtensions;
exports.xsd2jsonWrapper = prologWrapper.xsd2jsonWrapper;
exports.postProcessing = jsonProcessing.postProcessing;