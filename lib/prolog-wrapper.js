var spawn = require('child_process').spawn;

var log = require('winston').loggers.get('xsd2json');

/**
 * Wrap the original xsd2json prolog programm in a function call
 *
 * @param [string] xsd - The XSD schema as a string
 * @param [function] callback - The callback will be run with and optional error and the JSON schema result as a string
 */
exports.xsd2jsonWrapper = function(xsd, callback) {
	log.debug('Spawn prolog xsd2json program');
	//var swipl = spawn();
	var swipl = spawn('swipl', ['-q', '-f', '../prolog-xsd2json/cli.pl', '--'], {
		cwd: __dirname,
		env:{LANG:"C_UTF8"}
	});

	swipl.stdout.setEncoding('utf8');
	swipl.stderr.setEncoding('utf8');
	swipl.stdin.setEncoding('utf8');

	var result = "";
	swipl.stdout.on('data', function(data) {
		result += data;
	});

	var stderr = "";
	swipl.stderr.on('data', function(data) {
		log.debug('Spawned prolog xsd2json program wrote to stderr: %s', data);
		stderr += data;
	});

	swipl.on('close', function(code) {
		log.debug('Spawned prolog xsd2json program terminated');
		if (code !== 0) return callback(new Error('swipl returned with code ' + code + ' and write to stderr: ' + stderr));
		callback(null, result);
	});

	swipl.stdin.write(xsd);
	swipl.stdin.end();
};
