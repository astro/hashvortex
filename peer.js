var Buffer = require('buffer').Buffer;
var KRPC = require('./krpc');
var NodeId = require('./nodeid');
var NodeDB = require('./nodedb');


process.on('uncaughtException',
	   function(e) {
	       console.log(e.stack);
	   });

function decodeNodes(buf) {
    var result = [];
    for(var i = 0; i < buf.length; i += 26) {
	var nodeid = buf.slice(i, i + 20);
	var addr = buf[i + 20] + '.' +
	    buf[i + 21] + '.' +
	    buf[i + 22] + '.' +
	    buf[i + 23];
	var port = (buf[i + 24] << 8) | buf[i + 25];
	result.push({ addr: addr,
		      port: port,
		      nodeid: nodeid
		    });
    }
    return result;
}

function encodeNodes(nodes) {
    var result = new Buffer(nodes.length * 26);
    var i = 0;
    nodes.forEach(function(node) {
	m = node.addr.match(/^(\d+)\.(\d+)\.(\d+)\.(\d+)$/);
	node.nodeid.copy(result, i, 0, 20);
	result[i + 20] = Number(m[1]);
	result[i + 21] = Number(m[2]);
	result[i + 22] = Number(m[3]);
	result[i + 23] = Number(m[4]);
	result[i + 24] = node.port >> 8;
	result[i + 25] = node.port & 0xff;
	i += 26;
    });
    return result;
}

var peers = [];
function myNearestNodeId(nodeid) {
    var minDistance = 160;
    var result = null;
    peers.forEach(function(peer) {
	var distance = NodeId.distance(nodeid, peer.nodeid);
	if (distance < minDistance) {
	    minDistance = distance;
	    result = peer.nodeid;
	}
    });
    return result;
}

var node = new KRPC.Node(10000);
var queryCnt = 0, replyCnt = 0;
var token = new Buffer('a');
var targets = [new Buffer([87, 106, 131, 203, 0, 80]),
	       new Buffer([87, 106, 131, 203, 0, 87])];
node.on('query', function(addr, port, pkt, reply) {
    var t1 = Date.now();
    queryCnt++;

    switch(pkt.q.toString()) {
    case 'ping':
	if (pkt.a.nodeid)
	    reply({ id: myNearestNodeId(pkt.a.nodeid) });
	break;
    case 'find_node':
	if (pkt.a.id && myNearestNodeId(pkt.a.id) == pkt.a.id)
	    return;  // don't reply to self

	if (pkt.a.target) {
	    console.log('find_node ' + NodeId.toString(pkt.a.target));
	    var nodes = NodeDB.nearest(pkt.a.target);
	    reply({ id: myNearestNodeId(pkt.a.target),
		    nodes: encodeNodes(nodes) });
	}
	break;
    case 'get_peers':
	console.log('get_peers ' + NodeId.toString(pkt.a.info_hash));
	reply({ id: pkt.a.info_hash || nodeid,
		token: token,
		values: targets });
	break;
    case 'announce_peer':
	if (pkt.a.info_hash) {
	    console.log('announce_peer ' + NodeId.toString(pkt.a.info_hash));
	    reply({ id: pkt.a.info_hash });
	}
	break;
    }

    var t2 = Date.now();
    console.log('Query from ' +
		addr + ':' + port + ': ' +
		pkt.q + ' (' +
		(t2 - t1) + ' ms)');
});
node.on('reply', function(addr, port, pkt) {
    /*console.log('Reply from ' + addr + ':' + port + ': ' +
		JSON.stringify(pkt));*/
    replyCnt++;

    if (pkt.r.nodes) {
	var dests = decodeNodes(pkt.r.nodes);
	dests.forEach(NodeDB.seen);
    }

    var node = NodeDB.findNode({ addr: addr,
				 port: port,
				 nodeid: pkt.r.id
			       });
    if (node)
	node.lastReply = Date.now();
});
setInterval(function() {
    console.log(queryCnt + ' queries/s, ' + replyCnt + ' replies/s');
    queryCnt = 0;
    replyCnt = 0;
}, 1000);

function Peer(nodeid) {
    this.nodeid = nodeid;
    this.settleInterval = 300;

    this.settle();
}

Peer.prototype = {
    settle: function() {
	var dests = NodeDB.nearest(this.nodeid);
	if (dests.length < 1)
	    dests = [{ addr: 'router.bittorrent.com',
		       port: 6881
		     }];
	var self = this;
	var now = Date.now();
	dests.forEach(function(dest) {
	    if (!dest.lastQuery ||
		dest.lastQuery <= now - 300000) {
		console.log(now + ': find_node ' +
			    dest.addr + ':' + dest.port +
			    ' (' + (dest.nodeid ? NodeId.distance(self.nodeid, dest.nodeid) : '?') +
			    '/' + dest.lastQuery + ')');
		dest.lastQuery = now;
		node.send(dest.addr, dest.port,
			  { t: 'foo',
			    y: 'q',
			    q: 'find_node',
			    a: { id: self.nodeid,
				 target: self.nodeid
			       }
			  });
	    }
	});

	if (this.settleInterval < 60000)
	    this.settleInterval = Math.round(this.settleInterval * 1.5);
	setTimeout(function() {
	    self.settle();
	}, this.settleInterval);
    }
};


/*peers.push(new Peer(NodeId.parseMagnet("magnet:?xt=urn:btih:8874e48d1b71415af0585a424d239f324b4342eb&dn=The+Twilight+Saga+Eclipse+TS+XViD+-+IMAGiNE&tr=http%3A%2F%2Fdenis.stalker.h3q.com%3A6969%2Fannounce&tr=http%3A%2F%2Ftracker.mytorrenttracker.com%3A6099%2Fannounce"), node));
peers.push(new Peer(NodeId.parseMagnet("magnet:?xt=urn:btih:0777769ef49ebfef7212c71a09b67eb68bcc602d&dn=Hot+Tub+Time+Machine+%282010%29+R5+XviD-MAX&tr=http%3A%2F%2Ftracker.prq.to%2Fannounce&tr=http%3A%2F%2Ftracker.mytorrenttracker.com%3A6099%2Fannounce"), node));*/

var http = require('http');
var tpb = http.createClient(80, 'thepiratebay.org');
var request = tpb.request('GET', '/top/all',
			  {'host': 'thepiratebay.org'});
request.end();
var body = '';
request.on('response', function (response) {
    response.setEncoding('utf8');
    response.on('data', function (chunk) {
	body += chunk;
    });
    response.on('end', function() {
	var re = /(magnet:.+?\&)/g;
	var m;
	var delay = 0;
	while((m = re.exec(body))) {
	    var link = m[1];
	    setTimeout(function() {
		peers.push(new Peer(NodeId.parseMagnet(link)));
	    }, delay);
	    delay += 2500;
	}
    });
});
