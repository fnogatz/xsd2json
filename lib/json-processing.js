//var log = require('winston').loggers.get('xsd2json');
var _ = require('lodash');

/**
 * Split an element definition in a anyOf definition with multiple types
 */
function splitTypes(element, types) {
	delete element.type;
	element.anyOf = [];
	types.forEach(function(type) {
		element.anyOf.push(_.merge({}, _.omit(element, 'anyOf'), {
			type: type
		}));
	});
}

/**
 * recursive post processing of the produced json schema
 */
function recursiveProcessing(schema, element) {
	// Fix type date bug
	if (element.type && (element.type === 'xs:date' || element.type === 'xs:dateTime') || element.$ref && (element.$ref === '#/definitions/xs:date' || element.$ref === '#/definitions/xs:dateTime')) {
		delete element.type;
		element.oneOf = [
			{
				"type": "string",
				"pattern": "^[0-9]{4}\-[0-9]{2}\-[0-9]{2}$"
			},
			{
				"type": "string",
				"format": "date-time"
			}
		];
		//element.type = 'string';
		//element.format = 'date-time';
		delete element.$ref;
	}

	if ((element.type && element.type === 'xs:anyURI') || (element.$ref && element.$ref === '#/definitions/xs:anyURI')) {
		element.type = 'string';
		element.format = 'uri';
		delete element.$ref;
	}

	if ((element.type && element.type === 'xs:normalizedString') || (element.$ref && element.$ref === '#/definitions/xs:normalizedString')) {
		element.type = 'string';
		delete element.$ref;
	}

	if ((element.type && element.type === 'xs:language') || (element.$ref && element.$ref === '#/definitions/xs:language')) {
		element.type = 'string';
		delete element.$ref;
	}

	if ((element.type && element.type === 'xs:short') || (element.$ref && element.$ref === '#/definitions/xs:short')) {
		element.type = 'integer';
		delete element.$ref;
	}

	if ((element.type && element.type === 'xs:int') || (element.$ref && element.$ref === '#/definitions/xs:int')) {
		element.type = 'integer';
		delete element.$ref;
	}

	if ((element.type && element.type === 'xs:anySimpleType') || (element.$ref && element.$ref === '#/definitions/xs:anySimpleType')) {
		splitTypes(element, ['string', 'boolean', 'number', 'integer']);
		delete element.$ref;
	}

	if ((element.type && element.type === 'xs:anyType') || (element.$ref && element.$ref === '#/definitions/xs:anyType')) {
		delete element.type;
		delete element.$ref;
	}

	if ((element.type && element.type === '@xml:lang') || (element.$ref && element.$ref === '#/definitions/@xml:lang')) {
		element.type = 'string';
		delete element.$ref;
	}

	// Ensure that types with pattern or length restrictions are of type string.
	// Sometimes this information is lost.
	if (element.hasOwnProperty('minLength') || element.hasOwnProperty('maxLength') || element.pattern) {
		element.type = 'string';
	}

	// fix @ prefixed attribute names
	var arobaseAttrsKeys = _.filter(Object.keys(element), function(key) {
		return key.indexOf('@') === 0;
	});
	_.each(arobaseAttrsKeys, function(key) {
		var newKey = key.replace('@', '');
		element[newKey] = element[key];
		delete element[key];
	});

	// fix @xml attributes
	var xmlAttrsKeys = _.filter(Object.keys(element), function(key) {
		return key.indexOf('xml:') !== -1;
	});
	_.each(xmlAttrsKeys, function(key) {
		var newKey = key.replace('xml:', '');
		element[newKey] = element[key];
		delete element[key];
	});

	// type references should be $ref
	if (element.type && (schema.definitions[element.type])) {
		element.$ref = '#/definitions/' + element.type;
		delete element.type;
	}

	for (var i in element) {
		if (element[i] !== null && typeof(element[i]) === 'object') {
			// Recursivity baby !
			recursiveProcessing(schema, element[i]);
		}
	}

    // Manage XSD regex pattern conform to http://www.w3.org/TR/xmlschema-0/#regexAppendix
    if (
	element.type && element.type === 'string' && 
	element.pattern && (typeof element.pattern) === 'string' && element.pattern.indexOf('^') === -1) {
        element.pattern = '^' + element.pattern + '$';
    }
}

/**
 * Process the output of the xsd2json prolog program as it misses a few points
 *
 * @param [string|object] schema
 */
exports.postProcessing = function(schema) {
	if (typeof schema === 'string') {
		schema = JSON.parse(schema);
	}

	// Detect complex types that are actually choices at the root of another one
	for (var definitionKey in schema.definitions) {
		if (definitionKey.indexOf('CHOICE') !== -1) {
			var parentKey = definitionKey.substr(0, definitionKey.indexOf('CHOICE'));
			schema.definitions[parentKey] = schema.definitions[parentKey] || {
				anyOf: []
			};
			schema.definitions[parentKey].anyOf.push({
				$ref: '#/definitions/' + definitionKey
			});
		}
	}

	recursiveProcessing(schema, schema);

	return schema;
};
