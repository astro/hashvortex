var KRPC = require('./krpc');
var NodeId = require('./nodeid');

var node = new KRPC.Node(10000);
node.on('query', function(addr, port, pkt) {
    console.log('Query from ' + addr + ':' + port + ': ' +
		JSON.stringify(pkt));
});
node.on('reply', function(addr, port, pkt) {
    console.log('Reply from ' + addr + ':' + port + ': ' +
		JSON.stringify(pkt));
});

node.send('router.bittorrent.com', 6881,
	  { t: 'foo',
	    y: 'q',
	    q: 'ping',
	    a: { id: NodeId.generateRandom() }
	  });
