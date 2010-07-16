var Buffer = require('buffer').Buffer;

Buffer.prototype.type = 'buffer';
Number.prototype.type = 'number';
String.prototype.type = 'string';
Array.prototype.type = 'array';

var CH_0 = 48;
var CH_9 = 57;
var CH_COLON = 58;
var CH_D = 100;
var CH_E = 101;
var CH_I = 105;
var CH_L = 108;

module.exports = {
    parse: function(buffer) {
	function parseValue() {
	    if (buffer[0] == CH_L) {
		var list = [];
		buffer = buffer.slice(1, buffer.length);
		while(buffer[0] != CH_E && buffer.length > 0) {
		    list.push(parseValue());
		}
		buffer = buffer.slice(1, buffer.length);
		return list;
	    } else if (buffer[0] == CH_D) {
		var dict = {};
		buffer = buffer.slice(1, buffer.length);
		while(buffer[0] != CH_E && buffer.length > 0) {
		    var k = parseValue();
		    var v = parseValue();
		    dict[k] = v;
		}
		buffer = buffer.slice(1, buffer.length);
		return dict;
	    } else if (buffer[0] == CH_I) {
		var i;
		for(i = 1; buffer[i] != CH_E && i < buffer.length; i++) {
		}
		var num = new Number(buffer.slice(1, i).toString());
		buffer = buffer.slice(i + 1, buffer.length);
		return num;
	    } else if (buffer[0] >= CH_0 && buffer[0] <= CH_9) {
		var i;
		for(i = 1; buffer[i] != CH_COLON && i < buffer.length; i++) {
		}
		var len = new Number(buffer.slice(0, i).toString());
		var s = buffer.slice(i + 1, i + 1 + len);
		buffer = buffer.slice(i + 1 + len, buffer.length);
		return s;
	    } else
		throw('Cannot parse ' + buffer);
	}

	return parseValue();
    },
    dumpFile: function(path) {
	var fs = require('fs');
	var sys = require('sys');
	fs.readFile(path,
		    function(err, data) {
			if (err)
			    throw err;

			var contents = module.exports.parse(data);
			sys.puts(JSON.stringify(contents));
			if (module.exports.toBuffer(contents).toString() != data.toString()) {
			    sys.puts("Expected: " + data.slice(0, 1024));
			    sys.puts("Encoded: " + module.exports.toBuffer(contents).slice(0, 1024));
			}
		    });
    },
    write: function(obj, dest) {
	switch(obj.type) {
	case 'number':
	    dest.write('i');
	    dest.write(obj.toString());
	    dest.write('e');
	    break;
	case 'string':
	    if (obj === '')
		dest.write('0:');
	    else {
		var buf = new Buffer(obj, 'binary');
		dest.write(buf.length.toString());
		dest.write(':');
		dest.write(buf);
	    }
	    break;
	case 'buffer':
	    dest.write(obj.length.toString());
	    dest.write(':');
	    dest.write(obj);
	    break;
	case 'array':
	    dest.write('l');
	    obj.forEach(function(el) {
		module.exports.write(el, dest);
	    });
	    dest.write('e');
	    break;
	default:
	    dest.write('d');
	    for(var k in obj) {
		module.exports.write(k, dest);
		module.exports.write(obj[k], dest);
	    }
	    dest.write('e');
	    break;
	}
    },
    toBuffer: function(obj, len) {
	len = len || 128;
	var buf = new Buffer(len);
	var offset = 0;

	try {
	    module.exports.write(obj,
				 { write: function(s) {
				     if (s.type === 'string')
					 offset += buf.write(s, offset, 'binary');
				     else if (s.type === 'buffer') {
					 if (s.length > 0) {
					     s.copy(buf, offset, 0, s.length);
					     offset += s.length;
					 }
				     }
				     else
					 throw 'Cannot write ' + s.type;
				 } });
	    return buf.slice(0, offset);
	} catch (e) {
	    if (e.message === 'Offset is out of bounds' ||
		e.message === 'targetStart out of bounds')
		// retry with larger buffer
		return module.exports.toBuffer(obj, len * 2);
	    else {
		console.log(JSON.stringify(e));
		throw e;
	    }
	}
    },
    encode: function(obj) {
	var result = '';
	var dest = {
	    write: function(s) {
		result += s;
	    }
	};
	module.exports.write(obj, dest);
	return result;
    }
};
