//var log = require('winston').loggers.get('xsd2json');

/**
 * recursive post processing of the produced json schema
 */
function recursiveProcessing(schema, element) {
	// Fix type date bug
	if (element.type && (element.type === 'xs:date' || element.type === 'xs:dateTime')) {
		element.type = 'string';
		element.format = 'date-time';
	}

	if (element.type && element.type === 'xs:anyURI') {
		element.type = 'string';
		element.format = 'uri';
	}

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

	return JSON.stringify(schema, null, 2);
};