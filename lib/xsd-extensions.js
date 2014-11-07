var _ = require('lodash');
_.mixin(require("lodash-deep"));

var log = require('winston').loggers.get('xsd2json');

/**
 * Merge only the complex types that do not themselves need to be merged into
 * Meaning if 'a' extends 'b' and 'b' extends 'c', then we should not merge 'b' in 'a' right away, we should first merge 'c' in 'b'
 *
 * Return and array of 2 items arrays, 1rst item is a complexType and second item its parent
 */
function complexTypesToMerge(complexTypes) {
	return _.filter(_.map(complexTypes, function(complexType) {
		var extendedTypeKey = _.deepGet(complexType, 'xs:complexContent.0.xs:extension.0.$.base');
		var extendedType = complexTypes[extendedTypeKey];
		// This complex type doesn't extend another one, do not keep it
		if (extendedTypeKey && !extendedType) return console.error('Complex type %s should extend other type %s but it is not defined', complexType.$.name, extendedTypeKey);
		if (!extendedType) return null;

		var extendedTypeParent = _.deepGet(extendedType, 'xs:complexContent.0.xs:extension');
		// This complex extends another type that itself extend another, do not treat it in this iteration
		if (extendedTypeParent) return null;

		return [complexType, extendedType];
	}));
}

/**
 * Merge a complex type into another. Receive an array of 2, 1rst item is a complexType and second item its parent
 */
function mergeComplexTypes(complexTypeInheritance) {
	var child = complexTypeInheritance[0];
	var parent = complexTypeInheritance[1];
	log.debug('Merge complex type %s into %s', parent.$.name, child.$.name);
	_.deepSet(child, 'xs:sequence.0.xs:element', _.union(_.deepGet(parent, 'xs:sequence.0.xs:element'), _.deepGet(child, 'xs:complexContent.0.xs:extension.0.xs:sequence.0.xs:element')));
	_.deepSet(child, 'xs:all.0.xs:element', _.union(_.deepGet(parent, 'xs:all.0.xs:element'), _.deepGet(child, 'xs:complexContent.0.xs:extension.0.xs:all.0.xs:element')));
	child['xs:attribute'] = _.union(parent['xs:attribute'], child['xs:attribute']);
	delete child['xs:complexContent'];
}

/**
 * Apply a few transformations to all complexTypes of a schema that make life easier for ulterior processing
 */
function fixComplexType(complexType) {
	// when a complex type extends a simple type, simple treat the simple type as a value attribute
	var simpleContentBase = _.deepGet(complexType, 'xs:simpleContent.0.xs:extension.0.$.base');
	if (simpleContentBase) {
		complexType['xs:sequence'] = _.deepGet(complexType, 'xs:simpleContent.0.xs:extension.0.xs:sequence') || [];
		complexType['xs:all'] = _.deepGet(complexType, 'xs:simpleContent.0.xs:extension.0.xs:all') || [];
		complexType['xs:attribute'] = _.deepGet(complexType, 'xs:simpleContent.0.xs:extension.0.xs:attribute') || [];
		complexType['xs:attribute'].push({
			$: {
				name: 'value',
				type: simpleContentBase
			}
		});
		delete complexType['xs:simpleContent'];
	}
}

/**
 * Traverse a schema looking for inheritance between complex types and merge them accordingly
 *
 * @param [object] schema - An object parsed by xml2js as returned by mergeInclusions
 */
exports.mergeExtensions = function(schemaParam) {
	var schema = _.cloneDeep(schemaParam);

	log.debug('Iterate over schema to detect complex types to be merged');
	var complexTypes = {};
	_.each(schema['xs:schema']['xs:complexType'], function(complexType) {
		fixComplexType(complexType);
		complexTypes[complexType.$.name] = complexType;
	});

	var toMerge = complexTypesToMerge(complexTypes);
	while (toMerge.length > 0) {
		toMerge = complexTypesToMerge(complexTypes);
		_.each(toMerge, mergeComplexTypes);
	}

	return schema;
};