var NodeId = require('./nodeid');

var db = [];

function nodeEq(node) {
    return function(node1) {
	return node.addr == node1.addr &&
	    node.port == node1.port &&
	    node.nodeid == node1.nodeid;
    };
}

module.exports = {
    seen: function(node) {
	var ary = db[node.nodeid[0]];
	if (!ary)
	    ary = db[node.nodeid[0]] = [];

	node.lastSeen = Date.now();
	if (!ary.some(nodeEq(node)))
	    ary.push(node);
    },
    findNode: function(node) {
	var ary = db[node.nodeid[0]];
	return ary ? ary.filter(nodeEq(node))[0] : null;
    },
    nearest: function(nodeid) {
	result = [];
	var n = nodeid[0];
	for(var i = 0; i < 256; i++) {
	    if (db[n + i]) {
		db[n + i].forEach(function(node) {
		    result.push(node);
		});
	    }
	    if (i != 0 && db[n - i]) {
		db[n - i].forEach(function(node) {
		    result.push(node);
		});
	    }

	    if (result.length >= 8)
		break;
	}

	result = result.sort(function(nodeA, nodeB) {
	    var distA = NodeId.distance(nodeid, nodeA);
	    var distB = NodeId.distance(nodeid, nodeB);
	    return distA - distB;
	});
	return result.slice(0, 8);
    }
};
