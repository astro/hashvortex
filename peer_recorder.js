var dgram = require('dgram');
var PORT = 30000;

var stats = {};
function incStats(what) {
    var i = stats[what] || 0;
    stats[what] = i + 1;
}
var t = 0;
setInterval(
    function() {
        stats.time = t;
	console.log(JSON.stringify(stats));
	t++;
	stats = {};
    }, 1000);

function listenOn(i) {
    var sock = dgram.createSocket('udp4');
    sock.on('message', function(msg, rinfo) {
	incStats("pkt-" + i);
    });
    sock.on('error', function(e) {
	throw e;
    });
    sock.bind(PORT + i);
}


for(var i = 0; i < 8; i++) {
    listenOn(i);
}
