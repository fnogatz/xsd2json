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
	//child['xs:choice'] = _.union(parent['xs:choice'], child['xs:choice']);
	child['xs:attribute'] = _.union(parent['xs:attribute'], child['xs:attribute']);
	child['xs:group'] = _.union(parent['xs:group'], child['xs:group']);
	delete child['xs:complexContent'];
}

/**
 * A few bug escaping on simpleTypes definition
 */
function processSimpleTypes(schema) {
	_.each(schema['xs:schema']['xs:simpleType'], function(simpleType){
		if(simpleType['xs:union']) {
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

/**
 * Apply a few transformations to all complexTypes of a schema that make life easier for ulterior processing
 */
function fixComplexType(complexType, groupsMap) {
	log.debug('Fix complex type %s', complexType.$.name);
	// when a complex type extends a simple type, simple treat the simple type as a value attribute
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
				type: simpleContentBase
			}
		});
		delete complexType['xs:simpleContent'];
	}

	// when a complex extends another complex type that was not found at least put its own content
	var complexContentExtension = _.deepGet(complexType, 'xs:complexContent.0.xs:extension.0');
	if (complexContentExtension) {
		log.debug('Complex type merging for %s failed. Preserve its own content.', complexType.$.name);
		_.merge(complexType, complexContentExtension);
		delete complexType['xs:complexContent'];
		delete complexType.$.base;
	}

	// lastly merge elements from a group
	_.each(complexType['xs:group'], function(groupRef) {
		log.debug('Merge group %s into %s', groupRef.$.ref, complexType.$.name);
		var group = groupsMap[groupRef.$.ref];
		_.deepSet(complexType, 'xs:sequence.0.xs:element', _.union(_.deepGet(complexType, 'xs:sequence.0.xs:element'), _.deepGet(group, 'xs:sequence.0.xs:element')));
		_.deepSet(complexType, 'xs:all.0.xs:element', _.union(_.deepGet(complexType, 'xs:all.0.xs:element'), _.deepGet(group, 'xs:all.0.xs:element')));
		complexType['xs:attribute'] = _.union(complexType['xs:attribute'], group['xs:attribute']);
	});
	delete complexType['xs:group'];
}

/**
 * Split the complex types that have a choice at their root
 */
function splitChoices(complexTypes) {
	var complexTypesSplitted = [];
	_.each(complexTypes, function(complexType) {
		// Either directly a choice
		var choices = _.deepGet(complexType, 'xs:choice.0.xs:sequence');
		// Or a choice inside an extension of another type
		var extendedChoices = _.deepGet(complexType, 'xs:complexContent.0.xs:extension.0.xs:choice.0.xs:sequence');
		if (choices) {
			_.each(choices, function(choice, choiceKey) {
				var choiceComplexType = _.cloneDeep(complexType);
				delete choiceComplexType['xs:choice'];
				choiceComplexType['xs:sequence'] = [choice];
				choiceComplexType.$.name = choiceComplexType.$.name + 'CHOICE' + choiceKey;
				complexTypesSplitted.push(choiceComplexType);
			});
		} else if (extendedChoices) {
			_.each(extendedChoices, function(choice, choiceKey) {
				var choiceComplexType = _.cloneDeep(complexType);
				var extension = _.deepGet(choiceComplexType, 'xs:complexContent.0.xs:extension.0');
				delete extension['xs:choice'];
				extension['xs:sequence'] = [choice];
				choiceComplexType.$.name = choiceComplexType.$.name + 'CHOICE' + choiceKey;
				complexTypesSplitted.push(choiceComplexType);
			});
		} else {
			complexTypesSplitted.push(complexType);
		}
	});

	return complexTypesSplitted;
}

/**
 * Recursively traverse the schema looking for referenced element. Simply duplicate the element definition in this case.
 */
function fixElementRefs(elementRefs, element) {
	if (element.$ && element.$.ref && elementRefs[element.$.ref]) {
		log.debug('Merge referenced element %s into other element', element.$.ref);
		console.log(elementRefs[element.$.ref]);
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
function usedTypes(schema, element, types){
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

	var toMerge = complexTypesToMerge(complexTypesMap);
	while (toMerge.length > 0) {
		toMerge = complexTypesToMerge(complexTypesMap);
		_.each(toMerge, mergeComplexTypes);
	}

	// filter out the complex types that not actually used
	var used = {};
	usedTypes(schema, schema, used);
	schema['xs:schema']['xs:complexType'] = _.filter(schema['xs:schema']['xs:complexType'], function(complexType){
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