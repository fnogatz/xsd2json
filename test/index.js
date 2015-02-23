var xml2js = require('xml2js');
var JaySchema = require('jayschema');
require('should');
var _ = require('lodash');

var js = new JaySchema();

var xsd2json = require('../');
var exampleInstance = require('./resources/example.json');
var exampleInstance2 = require('./resources/example2.json');
var schemaResult = require('./resources/schema.json');

describe('xsd2json', function() {
	var schemaWithInclusions;
	it('should merge a XSD with all its includes', function(done) {
		xsd2json.mergeInclusions('./', './test/resources/chapter04ord1.xsd', function(err, schema) {
			schemaWithInclusions = schema;
			//console.log(JSON.stringify(schemaWithInclusions, null, 2));
			done(err);
		});
	});

	var mergedSchema;
	var mergedSchemaStr;
	it('should merge extensions of complex types', function() {
		mergedSchema = xsd2json.mergeExtensions(schemaWithInclusions);
		//console.log(JSON.stringify(mergedSchema, null, 2));
		var builder = new xml2js.Builder();
		mergedSchemaStr = builder.buildObject(mergedSchema);
	});

	var jsonSchema;
	it('should generate a json schema', function(done) {
		this.timeout(20000);
		xsd2json.xsd2jsonWrapper(mergedSchemaStr, function(err, schema) {
			jsonSchema = JSON.parse(schema);
			done(err);
		});
	});

	it('should cure a few defaults of the json schema', function() {
		xsd2json.postProcessing(jsonSchema);
		//console.log(JSON.stringify(jsonSchema, null, 2));
		jsonSchema.should.eql(schemaResult);

		// do not authorize additional properties in the following test. It prevents some ambiguities.
		_.each(jsonSchema.definitions, function(definition) {
			if (!definition.anyOf) definition.additionalProperties = false;
		});
	});

	it('should validate the example document against the produced schema', function() {
		var errors = js.validate(exampleInstance, jsonSchema);
		if (errors.length > 0) {
			//console.log('Validation errors');
			//console.log(JSON.stringify(errors, null, 2));
		}
		errors.should.have.length(0);
	});

	it('should reject the example document with complex object in anySimpleType element', function() {
		var example = _.cloneDeep(exampleInstance);
		example.anySimpleTypeElement = {};

		var errors = js.validate(example, jsonSchema);
		errors.should.have.length(1);
	});

	it('should validate the alternative of the root choice', function() {
		var errors = js.validate(exampleInstance2, jsonSchema);
		if (errors.length > 0) {
			console.log('Validation errors');
			console.log(JSON.stringify(errors, null, 2));
		}
		errors.should.have.length(0);
	});
});