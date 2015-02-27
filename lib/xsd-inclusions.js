var fs = require('fs');
var path = require('path');
var url = require('url');
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
			if (/:schema|:sequence|:complexType|:simpleType|:include|:import|:complexContent|:extension|:all|:element|:attribute|:simpleContent|:any|:choice|:annotation|:group|:annotation|:union|:list/.test(name)) {
				// All xml schema namespace element should be prefixed by xs:
				return 'xs:' + name.substring(name.indexOf(':') + 1);
			}
			/* else if (/:positiveInteger|:negativeInteger|:nonNegativeInteger|:nonPositiveInteger/.test(name)) {
				// Temporary hack until bug fix, cf https://github.com/fnogatz/xsd2json/issues/6
				return 'xs:integer';
			}*/
			else {
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
			if (err) return callback(err);
			if (response.statusCode !== 200) return callback(new Error('Fetch schema failed ' + currentLocation + ' > ' + location + ' - ' + response.statusCode + ' - ' + body));
			log.debug('... ok');
			callback(err, body, location);
		});
	} else if (currentLocation.indexOf('http') === 0) {
		// 2nd case the base path is a URL
		var schemaLink = url.resolve(currentLocation, location);
		if (alreadyFetched[schemaLink]) return callback(null, null);
		alreadyFetched[schemaLink] = true;
		log.debug('Fetch distant schema %s ...', schemaLink);
		request.get(schemaLink, function(err, response, body) {
			if (err) return callback(err);
			if (response.statusCode !== 200) return callback(new Error('Fetch schema failed ' + currentLocation + ' > ' + schemaLink + ' - ' + response.statusCode + ' - ' + body));
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
	schema['xs:schema']['xs:group'] = _.map(_.union(schema['xs:schema']['xs:group'], includedSchema['xs:schema']['xs:group']), function(group) {
		return attributePrefix(includedSchema, group, 'name');
	});
	schema['xs:schema']['xs:element'] = _.map(_.union(schema['xs:schema']['xs:element'], includedSchema['xs:schema']['xs:element']), function(element) {
		return attributePrefix(includedSchema, element, 'name');
	});
}


/**
 * Take the last part of the path of a namespace as its universal prefix.
 * This is not perfect of course. But using hashes of the ns generates bugs in xsd2jon.
 * It seems that the prefix should only contain letters.
 */
function getNSPrefix(ns) {
	if (ns === 'http://www.w3.org/XML/1998/namespace') return 'xml';

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
	return element;
}


/**
 * Recursive function that traverse a schema definition looking for 'type' and 'base' to deal with their prefixes
 */
function prefixesSchema(schema, element) {
	var rootElements = _.union(
		schema['xs:schema']['xs:complexType'],
		schema['xs:schema']['xs:simpleType'],
		schema['xs:schema']['xs:element'],
		schema['xs:schema']['xs:group']
	);
	_.each(rootElements, function(rootElement) {
		attributePrefix(schema, rootElement, 'name');
	});
	for (var i in element) {
		if (element[i] !== null && typeof(element[i]) === 'object') {
			attributePrefix(schema, element, 'base');
			attributePrefix(schema, element, 'type');
			attributePrefix(schema, element, 'ref');

			// recursivity baby !
			prefixesSchema(schema, element[i]);
		}
	}
}

/**
 * Recursive function that traverse a schema definition and remove xs:annotation elements that only burden ulterior transformations
 */
function removeAnnotationsAndDefaults(schema, element) {
	for (var i in element) {
		if (element[i] !== null && typeof(element[i]) === 'object') {
			// Also clean up xs:token type, that is quite equivalent to xs:string
			if (element[i].$ && element[i].$.type === 'xs:token') {
				element[i].$.type = 'xs:string';
			}
			delete element[i]['xs:annotation'];
			delete element[i]['default'];
			// recursivity baby !
			removeAnnotationsAndDefaults(schema, element[i]);
		}
	}
}

/**
 * Recursively traverse the schema looking types definitions bundled in elements.
 * Take them out and reference them in the element. This is necessary for support by xsd2json prolog code.
 */
function extractTypesFromElements(schema, element) {
	if (element.$ && element.$.name && !element.$.type) {
		if (element['xs:simpleType'] && element['xs:simpleType'][0]) {
			log.debug('Extract simple type definition from element %s into the root schema', element.$.name);
			_.deepSet(element, 'xs:simpleType.0.$.name', element.$.name);
			element.$.type = element.$.name;
			schema['xs:schema']['xs:simpleType'] = schema['xs:schema']['xs:simpleType'] || [];
			schema['xs:schema']['xs:simpleType'].push(element['xs:simpleType'][0]);
			delete element['xs:simpleType'];
		}
		if (element['xs:complexType'] && element['xs:complexType'][0]) {
			log.debug('Extract complex type definition from element %s into the root schema', element.$.name);
			_.deepSet(element, 'xs:complexType.0.$.name', element.$.name);
			element.$.type = element.$.name;
			schema['xs:schema']['xs:complexType'] = schema['xs:schema']['xs:complexType'] || [];
			schema['xs:schema']['xs:complexType'].push(element['xs:complexType'][0]);
			delete element['xs:complexType'];
		}
	}
	for (var i in element) {
		if (element[i] !== null && typeof(element[i]) === 'object') {
			extractTypesFromElements(schema, element[i]);
		}
	}
}

/**
 * Recursively traverse the schema looking for attributes with list element as a content.
 * Replace these by an element in a sequence
 */
function fixAttributesList(element) {
	_.each(element['xs:attribute'], function(attr, i) {
		if (_.deepGet(attr, 'xs:simpleType.0.xs:list')) {
			var elementDef = _.deepGet(attr, 'xs:simpleType.0.xs:list.0');
			_.deepSet(elementDef, '$.name', _.deepGet(attr, '$.name'));
			elementDef.$.type = elementDef.$.itemType;
			delete elementDef.$.itemType;
			elementDef.$.minOccurs = 0;
			elementDef.$.maxOccurs = 'unbounded';
			element['xs:sequence'] = element['xs:sequence'] || [{
				'xs:element': []
			}];
			element['xs:sequence'][0]['xs:element'].push(elementDef);
			element['xs:attribute'].splice([i], 1);
		}
	});
	for (var i in element) {
		if (element[i] !== null && typeof(element[i]) === 'object') {
			fixAttributesList(element[i]);
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

			fixAttributesList(schema);

			//console.log(JSON.stringify(schema, null, 2));

			extractTypesFromElements(schema, schema);

			// Make all prefixes unambiguous
			prefixesSchema(schema, schema);

			// remove all annotations and default values
			removeAnnotationsAndDefaults(schema, schema);



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