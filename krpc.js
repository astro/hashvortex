var sys = require('sys');
var dgram = require('dgram');
var EventEmitter = require('events').EventEmitter;
var BEnc = require('./benc');

var Node = function(port) {
    EventEmitter.call(this);
    var self = this;
    this.sock = dgram.createSocket(function(msg, rinfo) {
	self.onMsg(msg, rinfo);
    });
    this.sock.addListener("error", function(e) {
	throw e;
    });
    this.sock.bind(port, '0.0.0.0');
}
sys.inherits(Node, EventEmitter);

Node.prototype.onMsg = function(msg, rinfo) {
    console.log("connection: " + rinfo.address + ":"+ rinfo.port);
    console.log("server got: " + msg);

    try {
	var pkt = BEnc.parse(msg);

	if (pkt.y.toString() == 'q' &&
	    pkt.q && pkt.a) {
	    var reply = this.emit('query', rinfo.address, rinfo.port, pkt);
	    if (typeof reply == 'object') {
		var rpkt = { t: pkt.t,
			     q: 'r',
			     r: reply };
		this.send(rinfo.address, rinfo.port, rpkt);
	    }
	} else if (pkt.y.toString() == 'r' &&
		   pkt.r) {
	    this.emit('reply', rinfo.address, rinfo.port, pkt);
	}
    } catch(e) {
	console.log(e.stack());
    }
};

Node.prototype.send = function(addr, port, pkt) {
    var buf = BEnc.toBuffer(pkt);
    this.sock.send(port, addr, buf, 0, buf.length);
};

module.exports = {
    Node: Node
};
