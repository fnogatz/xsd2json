var fs = require('fs');
var path = require('path');
var xml2js = require('xml2js');
var async = require('async');
var request = require('request');
var _ = require('lodash');
_.mixin(require("lodash-deep"));

var log = require('winston').loggers.get('xsd2json');

// prepare options for xml2js parser
var parseStringOpts = {
	tagNameProcessors: [

		function(name) {
			// All xml schema namespace element should be prefixed by xs:
			if (/:schema|:sequence|:complexType|:simpleType|:include|:import|:complexContent|:extension|:all|:element|:attribute|:simpleContent/.test(name)) {
				return 'xs:' + name.substring(name.indexOf(':') + 1);
			} else {
				return name;
			}
		}
	]
};

/**
 * Fetch the schemas, either in the filesystem or online
 */
var alreadyFetched = {};

function fetchSchema(currentLocation, location, callback) {
	if (location.indexOf('http') === 0) {
		// 1st case: the location is a URL
		if (alreadyFetched[location]) return callback(null, null);
		alreadyFetched[location] = true;
		log.debug('Fetch distant schema %s ...', location);
		request.get(location, function(err, response, body) {
			log.debug('... ok');
			callback(err, body, location);
		});
	} else if (currentLocation.indexOf('http') === 0) {
		// 2nd case the base path is a URL
		var schemaLink = currentLocation + '/' + location; // TODO better resolution of path
		if (alreadyFetched[schemaLink]) return callback(null, null);
		alreadyFetched[schemaLink] = true;
		log.debug('Fetch distant schema %s ...', schemaLink);
		request.get(schemaLink, function(err, response, body) {
			log.debug('... ok');
			callback(err, body, schemaLink);
		});
	} else {
		// 3rd case: neither is a URL
		var schemaPath = path.resolve(path.dirname(currentLocation), location);
		if (alreadyFetched[schemaPath]) return callback(null, null);
		alreadyFetched[schemaPath] = true;
		log.debug('Fetch local schema %s ...', schemaPath);
		fs.readFile(schemaPath, 'utf8', function(err, data) {
			log.debug('... ok');
			callback(err, data, schemaPath);
		});
	}
}

/**
 * Take all element from an included schema that should be merged into its parent
 */
function appendSchema(schema, includedSchema) {
	_.defaults(schema['xs:schema'].$, includedSchema['xs:schema'].$);
	schema['xs:schema']['xs:complexType'] = _.union(schema['xs:schema']['xs:complexType'], includedSchema['xs:schema']['xs:complexType']);
	schema['xs:schema']['xs:simpleType'] = _.union(schema['xs:schema']['xs:simpleType'], includedSchema['xs:schema']['xs:simpleType']);
}


/**
 * Take the last part of the path of a namespace as its universal prefix.
 * This is not perfect of course. But using hashes of the ns generates bugs in xsd2jon.
 * It seems that the prefix should only contain letters.
 */
function getNSPrefix(ns) {
	//var splitted = ns.split('/');
	//return splitted[splitted.length - 1];
	return ns.replace(/[^a-zA-Z0-9]+/g, '').replace('http', '');
}

/**
 * Replace the prefix of an element's attribute by one that will be always be the same for a given namespace
 */
function attributePrefix(schema, element, attrKey) {
	if (!element || !element.$ || !element.$[attrKey]) return;
	if (element.$[attrKey].indexOf(':') !== -1) {
		// Case when the attribute is prefixed
		// replace this prefixed by a non ambiguous one and remember it in the document prefixes
		var prefix = element.$[attrKey].substring(0, element.$[attrKey].indexOf(':'));
		var ns = schema['xs:schema'].$['xmlns:' + prefix];

		if (ns === 'http://www.w3.org/2001/XMLSchema') {
			// special case, do not impact xs: prefixes
			element.$[attrKey] = 'xs:' + element.$[attrKey].substring(element.$[attrKey].indexOf(':') + 1);
		} else if (ns) {
			var newPrefix = getNSPrefix(ns);
			schema['xs:schema'].$['xmlns:' + newPrefix] = ns;
			element.$[attrKey] = newPrefix + ':' + element.$[attrKey].substring(element.$[attrKey].indexOf(':') + 1);
		}

	} else {
		// case when no prefix is defined, add one with the current targetNamespace
		if (schema['xs:schema'].$.targetNamespace) {
			var targetNS = schema['xs:schema'].$.targetNamespace;
			var targetNSPrefix = getNSPrefix(targetNS);
			if (element.$[attrKey].indexOf(targetNSPrefix) === -1) {
				element.$[attrKey] = targetNSPrefix + ':' + element.$[attrKey];
			}
			schema['xs:schema'].$['xmlns:' + targetNSPrefix] = targetNS;
		}
	}
}


/**
 * Recursive function that traverse a schema definition looking for 'type' and 'base' to deal with their prefixes
 */
function prefixesSchema(schema, element) {
	var types = _.union(schema['xs:schema']['xs:complexType'], schema['xs:schema']['xs:simpleType']);
	_.each(types, function(type) {
		// for each type prefix its name
		attributePrefix(schema, type, 'name');
	});
	for (var i in element) {
		if (element[i] !== null && typeof(element[i]) === 'object') {
			attributePrefix(schema, element, 'base');
			attributePrefix(schema, element, 'type');

			// recursivity baby !
			prefixesSchema(schema, element[i]);
		}
	}
}


/**
 * Recursively merge a schema and all its included sub-schemas
 *
 * @param [string] currentLocation - The location of the schema being parsed when calling this function. Used for recursivity, use '.' for first call.
 * @param [string] location - The location of the schema to be parsed when calling this function.
 * @param [function] callback - Will be called with first param containing an error and second param containing the xml2js parsed schema
 */
exports.mergeInclusions = function(currentLocation, location, callback) {
	fetchSchema(currentLocation, location, function(err, data, resolvedLocation) {
		if (err) return callback(err);
		if (!data) return callback(null, null);

		xml2js.parseString(data, parseStringOpts, function(err, schema) {
			if (err) return callback(err);

			// Use xs: as the prefix for all xml schema elements
			schema['xs:schema'].$['xmlns:xs'] = 'http://www.w3.org/2001/XMLSchema';
			//schema['xs:schema'].$.xmlns = schema['xs:schema'].$.xmlns || schema['xs:schema'].$.targetNamespace;

			// Make all prefixes unambiguous
			prefixesSchema(schema, schema);

			// Iterate over all includes and imports and merge them in the current schema
			var inclusions = _.union(schema['xs:schema']['xs:include'], schema['xs:schema']['xs:import']);
			async.each(inclusions, function(inclusion, cb) {
				// recursivity baby !
				exports.mergeInclusions(resolvedLocation, inclusion.$.schemaLocation, function(err, includedSchema) {
					if (err) return cb(err);
					if (!includedSchema) return cb();
					log.debug('Append content of schema %s into %s', inclusion.$.schemaLocation, resolvedLocation);
					appendSchema(schema, includedSchema);
					cb();
				});
			}, function(err) {
				// After all inclusions have been merged in the current schema pass it to main callback of mergedInclusions
				delete schema['xs:schema']['xs:include'];
				delete schema['xs:schema']['xs:import'];
				callback(err, schema);
			});
		});
	});
};