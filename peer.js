var Buffer = require('buffer').Buffer;
var KRPC = require('./krpc');
var NodeId = require('./nodeid');
var NodeDB = require('./nodedb');


/*process.on('uncaughtException',
	   function(e) {
	       console.log(e);
	   });*/

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

var node = new KRPC.Node(10000);
var queryCnt = 0, replyCnt = 0;
var token = new Buffer('a');
var targets = [new Buffer([87, 106, 131, 203, 0, 80]),
	       new Buffer([87, 106, 131, 203, 0, 87])];
node.on('query', function(addr, port, pkt, reply) {
    console.log('Query from ' +
		addr + ':' + port + ': ' +
		pkt.q);
    queryCnt++;

    switch(pkt.q.toString()) {
    case 'ping':
	reply({ id: nodeid });
	break;
    case 'find_node':
	reply({ id: nodeid,
		nodes: '' });
	break;
    case 'get_peers':
	reply({ id: nodeid,
		token: token,
		values: targets });
	break;
    case 'announce_peer':
	reply({ id: nodeid });
	break;
    }
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

    var self = this;
    setInterval(function() {
	self.settle();
    }, 1000);
}

Peer.prototype = {
    settle: function() {
	var dests = NodeDB.nearest(this.nodeid);
	if (dests.length < 1)
	    dests = [{ addr: 'router.bittorrent.com',
		       port: 6881
		     }];
	var nodeid = this.nodeid;
	var now = Date.now();
	dests.forEach(function(dest) {
	    if (!dest.lastQuery ||
		dest.lastQuery <= now - 300000) {
		console.log(now + ': find_node ' +
			    dest.addr + ':' + dest.port +
			    ' (' + dest.lastQuery + ')');
		dest.lastQuery = now;
		node.send(dest.addr, dest.port,
			  { t: 'foo',
			    y: 'q',
			    q: 'find_node',
			    a: { id: nodeid,
				 target: nodeid
			       }
			  });
	    }
	});
    }
};

//var nodeid = NodeId.parseMagnet("magnet:?xt=urn:btih:8874e48d1b71415af0585a424d239f324b4342eb&dn=The+Twilight+Saga+Eclipse+TS+XViD+-+IMAGiNE&tr=http%3A%2F%2Fdenis.stalker.h3q.com%3A6969%2Fannounce&tr=http%3A%2F%2Ftracker.mytorrenttracker.com%3A6099%2Fannounce");
var nodeid = NodeId.parseMagnet("magnet:?xt=urn:btih:0777769ef49ebfef7212c71a09b67eb68bcc602d&dn=Hot+Tub+Time+Machine+%282010%29+R5+XviD-MAX&tr=http%3A%2F%2Ftracker.prq.to%2Fannounce&tr=http%3A%2F%2Ftracker.mytorrenttracker.com%3A6099%2Fannounce");
console.log("nodeid: " + JSON.stringify(nodeid));
new Peer(nodeid, node);
