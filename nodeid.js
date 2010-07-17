var Buffer = require('buffer').Buffer;

function hexChar(s) {
    var i = s.charCodeAt(0);
    if (i >= 48 && i <= 57)
	return i - 48;
    if (i >= 97 && i <= 102)
	return i - 97 + 10;
    throw 'No hexadecimal character: ' + s;
}

module.exports = {
    generateRandom: function() {
	var result = new Buffer(20);
	for(var i = 0; i < 20; i++)
	    result[i] = Math.floor(Math.random() * 256);
	return result;
    },

    parseMagnet: function(url) {
	var m;
	if ((m = url.toLowerCase().match(/^magnet:\?xt=urn:btih:([0-9a-fA-F]+)/))) {
	    var bytes = [];
	    for(i = 0; i < m[1].length; i += 2) {
		var a = hexChar(m[1][i]);
		var b = hexChar(m[1][i + 1]);
		bytes.push((a << 4) | b);
	    }
	    //console.log("parseMagnet "+m[1]+" -> "+JSON.stringify(bytes));
	    if (bytes.length == 20)
		return new Buffer(bytes);
	}
    },

    toString: function(nodeid) {
	var result = '';
	for(var i = 0; i < nodeid.length; i++) {
	    var c = nodeid[i].toString(16).toString();
	    if (c.length < 2)
		result += '0';
	    result += c;
	}
	return result;
    },

    distance: function(nodeidA, nodeidB) {
	for(var i = 0; i < 20; i++) {
	    //console.log(nodeidA[i] + ' <=> ' + nodeidB[i]);
	    if (nodeidA[i] != nodeidB[i]) {
		var mask = 0x80;
		for(var j = 0; j < 8; j++) {
		    //console.log(nodeidA[i]+' & '+mask+' != '+nodeidB[i]+' & '+mask+' = '+((nodeidA[i] & mask) != (nodeidB[i] & mask)));
		    if ((nodeidA[i] & mask) != (nodeidB[i] & mask))
			return 160 - (i * 8 + j);

		    mask = (mask >> 1) | 0x80;
		}
		throw 'NodeIDs differ, but not differing bits found!';
	    }
	}

	return 0;  // no distance returned before
    },
    testDistance: function() {
	var assert = require('assert');
	var nid0 = new Buffer([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
	var nid1 = new Buffer([128, 128, 127, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
	var nid2 = new Buffer([128, 128, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
	var nid3 = new Buffer([128, 128, 129, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
	assert.equal(module.exports.distance(nid1, nid2), 144);
	assert.equal(module.exports.distance(nid0, nid1), 160);
	assert.equal(module.exports.distance(nid2, nid3), 137);
    }
};
