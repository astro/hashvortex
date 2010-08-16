var net = require('net');
var Buffer = require('buffer').Buffer;

var port = parseInt(process.argv[2]);

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

var clients = {
    'AG': 'Ares',
    'A~': 'Ares',
    'AR': 'Arctic',
    'AV': 'Avicora',
    'AX': 'BitPump',
    'AZ': 'Azureus',
    'BB': 'BitBuddy',
    'BC': 'BitComet',
    'BF': 'Bitflu',
    'BG': 'BTG (uses Rasterbar libtorrent)',
    'BR': 'BitRocket',
    'BS': 'BTSlave',
    'BX': '~Bittorrent X',
    'CD': 'Enhanced CTorrent',
    'CT': 'CTorrent',
    'DE': 'DelugeTorrent',
    'DP': 'Propagate Data Client',
    'EB': 'EBit',
    'ES': 'electric sheep',
    'FT': 'FoxTorrent',
    'GS': 'GSTorrent',
    'HL': 'Halite',
    'HN': 'Hydranode',
    'KG': 'KGet',
    'KT': 'KTorrent',
    'LH': 'LH-ABC',
    'LP': 'Lphant',
    'LT': 'libtorrent',
    'lt': 'libTorrent',
    'LW': 'LimeWire',
    'MO': 'MonoTorrent',
    'MP': 'MooPolice',
    'MR': 'Miro',
    'MT': 'MoonlightTorrent',
    'NX': 'Net Transport',
    'PD': 'Pando',
    'qB': 'qBittorrent',
    'QD': 'QQDownload',
    'QT': 'Qt 4 Torrent example',
    'RT': 'Retriever',
    'S~': 'Shareaza alpha/beta',
    'SB': '~Swiftbit',
    'SS': 'SwarmScope',
    'ST': 'SymTorrent',
    'st': 'sharktorrent',
    'SZ': 'Shareaza',
    'TN': 'TorrentDotNET',
    'TR': 'Transmission',
    'TS': 'Torrentstorm',
    'TT': 'TuoTu',
    'UL': 'uLeecher!',
    'UT': 'uTorrent',
    'VG': 'Vagaa',
    'WT': 'BitLet',
    'WY': 'FireTorrent',
    'XL': 'Xunlei',
    'XT': 'XanTorrent',
    'XX': 'Xtorrent',
    'ZT': 'ZipTorrent'
};

var banner = "\x13BitTorrent protocol";

net.createServer(
    function (stream) {
	incStats("Connect");
	var timeout = setTimeout(
	    function() {
		stream.end();
	    }, 5000);

	stream.on(
	    'data',
	    function(data) {
		if (data.length < 68) {
		    incStats("No BT");
		} else if (data.slice(0, 20) != banner) {
		    incStats("No BT banner");
		} else {
		    //var infoHash = data.slice(28, 48);
		    var peerId = data.slice(48, 68);
		    var client = null;
		    if (peerId[0] == 77 &&  // M
			peerId[2] == 45 &&  // -
			peerId[4] == 45 &&  // -
			peerId[6] == 45) {  // -
			client = "Mainline";
		    } else if (peerId[0] == 45) {  // -
			client = clients[peerId.slice(1, 3).toString('ascii')];
		    }
		    if (client)
			incStats(client);
		    else
			incStats("Unknown");
		}

		stream.end();
	    });
	stream.on(
	    'error',
	    function(e) {
		console.log("error: "+e);
		stream.end();
	    });
	stream.on(
	    'end',
	    function() {
		incStats("Disconnect");
		clearTimeout(timeout);
	    });
}).listen(port);
