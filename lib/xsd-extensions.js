var util = require('util');
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
	var results = [];

	_.each(complexTypes, function(complexType) {
		var extendedTypeKey = _.deepGet(complexType, 'xs:complexContent.0.xs:extension.0.$.base');
		if (!extendedTypeKey) return null;

		var extendedType = complexTypes[extendedTypeKey];

		// Simple inheritance
		if (extendedType) {
			var extendedTypeParent = _.deepGet(extendedType, 'xs:complexContent.0.xs:extension');
			// This complex extends another type that itself extend another, do not treat it in this iteration
			if (extendedTypeParent) return null;
			results.push([complexType, extendedType]);
		} else {
			var complexTypesNames = _.map(complexTypes, function(complexType) {
				return _.deepGet(complexType, '$.name');
			});
			var msg = util.format('Complex type %s should extend other type %s but it is not defined. List of complexTypes is %j', complexType.$.name, extendedTypeKey, complexTypesNames);
			log.error(msg);
			throw new Error(msg);
		}
	});

	return results;
}

/**
 * Merge a complex type into another. Receive an array of 2, 1rst item is a complexType and second item its parent
 */
function mergeComplexTypes(complexTypeInheritance) {
	var child = complexTypeInheritance[0];
	var parent = complexTypeInheritance[1];
	var extension = _.deepGet(child, 'xs:complexContent.0.xs:extension.0');
	log.debug('Merge complex type %s into %s', parent.$.name, child.$.name);
	_.deepSet(child, 'xs:sequence.0.xs:element', _.union(_.deepGet(parent, 'xs:sequence.0.xs:element'), _.deepGet(extension, 'xs:sequence.0.xs:element')));
	_.deepSet(child, 'xs:sequence.0.xs:group', _.union(_.deepGet(parent, 'xs:sequence.0.xs:group'), _.deepGet(extension, 'xs:sequence.0.xs:group')));
	_.deepSet(child, 'xs:all.0.xs:element', _.union(_.deepGet(parent, 'xs:all.0.xs:element'), _.deepGet(extension, 'xs:all.0.xs:element')));
	_.deepSet(child, 'xs:all.0.xs:group', _.union(_.deepGet(parent, 'xs:all.0.xs:group'), _.deepGet(extension, 'xs:all.0.xs:group')));
	//child['xs:choice'] = _.union(parent['xs:choice'], child['xs:choice']);
	child['xs:attribute'] = _.union(parent['xs:attribute'], extension['xs:attribute']);
	child['xs:group'] = _.union(parent['xs:group'], extension['xs:group']);
	delete child['xs:complexContent'];
}

/**
 * A few bug escaping on simpleTypes definition
 */
function processSimpleTypes(schema) {
	_.each(schema['xs:schema']['xs:simpleType'], function(simpleType) {
		if (simpleType['xs:union']) {
			// DO NOT SUPPORT UNIONS YET !
			delete simpleType['xs:union'];
			simpleType['xs:restriction'] = [{
				$: {
					base: 'xs:string'
				}
			}];
		}
	});
}

/*
 * when a complex type extends a simple type, simple treat the simple type as a value attribute
 */
function fixSimpleContent(complexType) {
	var simpleContentBase = _.deepGet(complexType, 'xs:simpleContent.0.xs:extension.0.$.base');
	if (simpleContentBase) {
		log.debug('Extract the simpleContent of a complex type and put in a value attribute', complexType.$.name);
		complexType['xs:sequence'] = _.deepGet(complexType, 'xs:simpleContent.0.xs:extension.0.xs:sequence') || [];
		complexType['xs:all'] = _.deepGet(complexType, 'xs:simpleContent.0.xs:extension.0.xs:all') || [];
		//complexType['xs:choice'] = _.deepGet(complexType, 'xs:simpleContent.0.xs:extension.0.xs:choice') || [];
		complexType['xs:attribute'] = _.deepGet(complexType, 'xs:simpleContent.0.xs:extension.0.xs:attribute') || [];
		complexType['xs:attribute'].push({
			$: {
				name: 'value',
				type: simpleContentBase,
				use: 'required'
			}
		});
		delete complexType['xs:simpleContent'];
	}
}

/**
 * Apply a few transformations to all complexTypes of a schema that make life easier for ulterior processing
 */
function fixComplexType(complexType, groupsMap) {
	log.debug('Fix complex type %s', complexType.$.name);

	// when a complex extends another complex type that was not found at least put its own content
	var complexContentExtension = _.deepGet(complexType, 'xs:complexContent.0.xs:extension.0');
	if (complexContentExtension) {
		log.debug('Complex type merging for %s failed. Preserve its own content.', complexType.$.name);
		_.merge(complexType, complexContentExtension);
		delete complexType['xs:complexContent'];
		delete complexType.$.base;
	}

	// lastly merge elements from groups
	var groups = _.union(
		complexType['xs:group'],
		_.deepGet(complexType, 'xs:sequence.0.xs:group'),
		_.deepGet(complexType, 'xs:all.0.xs:group')
	);

	_.each(groups, function(groupRef) {
		log.debug('Merge group %s into %s', groupRef.$.ref, complexType.$.name);
		var group = groupsMap[groupRef.$.ref];
		if (!group) {
			var msg = util.format('Group %s not found in list of available groups %j', groupRef.$.ref, Object.keys(groupsMap));
			log.error(msg);
			throw new Error(msg);
		}
		_.deepSet(complexType, 'xs:sequence.0.xs:element', _.union(_.deepGet(complexType, 'xs:sequence.0.xs:element'), _.deepGet(group, 'xs:sequence.0.xs:element')));
		_.deepSet(complexType, 'xs:all.0.xs:element', _.union(_.deepGet(complexType, 'xs:all.0.xs:element'), _.deepGet(group, 'xs:all.0.xs:element')));
		complexType['xs:attribute'] = _.union(complexType['xs:attribute'], group['xs:attribute']);
	});

	delete complexType['xs:group'];
	if (_.deepGet(complexType, 'xs:sequence.0')) {
		//delete complexType['xs:sequence'][0]['xs:group'];
	}
	if (_.deepGet(complexType, 'xs:all.0')) {
		//delete complexType['xs:all'][0]['xs:group'];
	}
}

/**
 * Split the complex types that have a choice at their root
 */
function splitChoices(complexTypes) {
	var complexTypesSplitted = [];
	_.each(complexTypes, function(complexType) {
		// Either directly a choice
		var choices = _.deepGet(complexType, 'xs:choice.0') || _.deepGet(complexType, 'xs:sequence.0.xs:choice.0');
		// Or a choice inside an extension of another type
		var extendedChoices = _.deepGet(complexType, 'xs:complexContent.0.xs:extension.0.xs:choice.0') || _.deepGet(complexType, 'xs:complexContent.0.xs:extension.0..xs:sequence.0.xs:choice.0');
		if (choices) {
			var i = 0;
			_.each(choices['xs:sequence'], function(sequenceChoice) {
				var choiceComplexType = _.cloneDeep(complexType);
				delete choiceComplexType['xs:choice'];
				_.deepSet(choiceComplexType, 'xs:sequence.0.xs:element', _.union(_.deepGet(choiceComplexType, 'xs:sequence.0.xs:element'), sequenceChoice['xs:element']));
				_.deepSet(choiceComplexType, 'xs:sequence.0.xs:group', _.union(_.deepGet(choiceComplexType, 'xs:sequence.0.xs:group'), sequenceChoice['xs:group']));
				choiceComplexType.$.name = choiceComplexType.$.name + 'CHOICE' + i;
				i += 1;
				complexTypesSplitted.push(choiceComplexType);
				log.debug('Splitted complex type %s into %s', complexType.$.name, choiceComplexType.$.name);
			});

			_.each(choices['xs:group'], function(groupChoice) {
				var choiceComplexType = _.cloneDeep(complexType);
				delete choiceComplexType['xs:choice'];
				_.deepSet(choiceComplexType, 'xs:sequence.0.xs:group', _.union(_.deepGet(choiceComplexType, 'xs:sequence.0.xs:group'), [groupChoice]));
				choiceComplexType.$.name = choiceComplexType.$.name + 'CHOICE' + i;
				i += 1;
				complexTypesSplitted.push(choiceComplexType);
				log.debug('Splitted complex type %s into %s', complexType.$.name, choiceComplexType.$.name);
			});
		} else if (extendedChoices) {
			var j = 0;
			_.each(extendedChoices['xs:sequence'], function(sequenceChoice) {
				var choiceComplexType = _.cloneDeep(complexType);
				var extension = _.deepGet(choiceComplexType, 'xs:complexContent.0.xs:extension.0');
				delete extension['xs:choice'];
				_.deepSet(extension, 'xs:sequence.0.xs:element', _.union(_.deepGet(extension, 'xs:sequence.0.xs:element'), sequenceChoice['xs:element']));
				_.deepSet(extension, 'xs:sequence.0.xs:group', _.union(_.deepGet(extension, 'xs:sequence.0.xs:group'), sequenceChoice['xs:group']));
				choiceComplexType.$.name = choiceComplexType.$.name + 'CHOICE' + j;
				j += 1;
				complexTypesSplitted.push(choiceComplexType);
				log.debug('Splitted complex type %s into %s', complexType.$.name, choiceComplexType.$.name);
			});

			_.each(extendedChoices['xs:group'], function(groupChoice) {
				var choiceComplexType = _.cloneDeep(complexType);
				var extension = _.deepGet(choiceComplexType, 'xs:complexContent.0.xs:extension.0');
				delete extension['xs:choice'];
				_.deepSet(extension, 'xs:sequence.0.xs:group', _.union(_.deepGet(extension, 'xs:sequence.0.xs:group'), [groupChoice]));
				choiceComplexType.$.name = choiceComplexType.$.name + 'CHOICE' + j;
				j += 1;
				complexTypesSplitted.push(choiceComplexType);
				log.debug('Splitted complex type %s into %s', complexType.$.name, choiceComplexType.$.name);
			});
		} else {
			complexTypesSplitted.push(complexType);
		}
	});
	return complexTypesSplitted;
}

function splitInheritedChoices(complexTypes) {
	var results = [];

	_.each(complexTypes, function(complexType) {
		var extendedTypeKey = _.deepGet(complexType, 'xs:complexContent.0.xs:extension.0.$.base');

		var splittedExtendedTypes = _.filter(complexTypes, function(complexType) {
			return complexType.$.name.indexOf(extendedTypeKey + 'CHOICE') !== -1;
		});

		// Simple inheritance
		if (splittedExtendedTypes.length) {
			_.each(splittedExtendedTypes, function(splittedExtendedType, key) {
				var duplicateComplexType = _.cloneDeep(complexType);
				_.deepSet(duplicateComplexType, 'xs:complexContent.0.xs:extension.0.$.base', splittedExtendedType.$.name);
				duplicateComplexType.$.name += 'CHOICE' + key;
				results.push(duplicateComplexType);
			});
		} else {
			results.push(complexType);
		}
	});

	return results;
}

/**
 * Recursively traverse the schema looking for referenced element. Simply duplicate the element definition in this case.
 */
function fixElementRefs(elementRefs, element) {
	if (element.$ && element.$.ref && elementRefs[element.$.ref]) {
		log.debug('Merge referenced element %s into other element', element.$.ref);
		_.merge(element, elementRefs[element.$.ref]);
		var name = element.$.name;
		element.$.name = name.substr(name.indexOf(':') + 1);
		delete element.$.ref;
	}
	for (var i in element) {
		if (element[i] !== null && typeof(element[i]) === 'object') {
			fixElementRefs(elementRefs, element[i]);
		}
	}
}

/**
 * Traverse all complex types of a schema and identifies the complex types that are not reference by any actual element
 * This is useful for a lighter final schema cleaned of the types that were only used for inheritance
 */
function usedTypes(schema, element, types) {
	for (var i in element) {
		if (element[i] !== null && typeof(element[i]) === 'object') {
			if (element[i].type) {
				types[element[i].type] = true;
			}
			usedTypes(schema, element[i], types);
		}
	}
}

/**
 * Traverse a schema looking for inheritance and reference links and merge them accordingly
 *
 * @param [object] schema - An object parsed by xml2js as returned by mergeInclusions
 */
exports.mergeExtensions = function(schemaParam) {
	var schema = _.cloneDeep(schemaParam);

	log.debug('Iterate over schema to detect complex types to be merged');

	processSimpleTypes(schema);

	// duplicate the complex types that have a xs:choice at their root. And remember the list of complex types.
	var complexTypes = schema['xs:schema']['xs:complexType'] = splitChoices(schema['xs:schema']['xs:complexType']);
	complexTypes = schema['xs:schema']['xs:complexType'] = splitInheritedChoices(schema['xs:schema']['xs:complexType']);

	// create a map of the complex types indexed by their name
	var complexTypesMap = {};
	_.each(complexTypes, function(complexType) {
		complexTypesMap[complexType.$.name] = complexType;
	});

	// create a map of the elements indexed by their name
	var elements = schema['xs:schema']['xs:element'];
	var elementRefs = {};
	_.each(elements, function(element) {
		elementRefs[element.$.name] = element;
	});

	// create a map of the groups indexed by their name
	var groups = schema['xs:schema']['xs:group'];
	var groupsMap = {};
	_.each(groups, function(group) {
		groupsMap[group.$.name] = group;
	});

	// replace all referenced elements by their definition
	fixElementRefs(elementRefs, schema);

	// fix simpleContent extensions
	_.each(complexTypes, function(complexType) {
		fixSimpleContent(complexType);
	});

	// merge complex types that inherit of each other
	var toMerge = complexTypesToMerge(complexTypesMap);
	while (toMerge.length > 0) {
		_.each(toMerge, mergeComplexTypes);
		toMerge = complexTypesToMerge(complexTypesMap);
	}

	// filter out the complex types that not actually used
	var used = {};
	usedTypes(schema, schema, used);
	schema['xs:schema']['xs:complexType'] = _.filter(schema['xs:schema']['xs:complexType'], function(complexType) {
		var filter = complexType.$.name.indexOf('CHOICE') !== -1 || used[complexType.$.name];
		if (!filter) {
			log.debug('Remove type %s that is not referenced in elements', complexType.$.name);
		}
		return filter;
	});

	// last minute fix for all types
	_.each(complexTypes, function(complexType) {
		fixComplexType(complexType, groupsMap);
	});

	// only keep the first element at the root of the schema.
	// xsd2json will use it as the root of the schema.
	// other elements were potentially useful only to be references in complex types.
	schema['xs:schema']['xs:element'] = [elements[0]];

	return schema;
};