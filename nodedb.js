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
	oldNodes = ary.filter(nodeEq(node));
	if (oldNodes[0]) {
	    oldNodes[0].lastSeen = Date.now();
	} else {
	    node.lastSeen = Date.now();
	    ary.push(node);
	}
    },
    findNode: function(node) {
	var ary = db[node.nodeid[0]];
	return ary ? ary.filter(nodeEq(node))[0] : null;
    },
    nearest: function(nodeid, amount) {
	result = [];
	var n = nodeid[0];
	for(var i = 0; result.length < amount && i < 256; i++) {
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
	}

	result = result.sort(function(nodeA, nodeB) {
	    var distA = NodeId.distance(nodeid, nodeA);
	    var distB = NodeId.distance(nodeid, nodeB);
	    return distA - distB;
	});
	return result.slice(0, amount);
    }
};

var purgeN = 160;
function purge(n) {
    var ary = db[purgeN];
    if (ary) {
	var now = Date.now();
	var before = ary.length;
	db[purgeN] = ary.filter(function(node) {
				    return node.lastSeen > now - 600000 ||
					(node.lastReply && node.lastReply > now - 900000);
				});
	var after = db[purgeN].length;
	if (before != after)
	    console.log("purged from bucket " + purgeN + ": " +
			before + " - " + (before - after) + " = " + after);
    }
}

setInterval(function() {
		try {
		    purge();
		} catch (x) {
		    console.log(x.stack);
		}

		purgeN--;
		if (purgeN < 0)
		    purgeN = 160;
	    }, 1000);
