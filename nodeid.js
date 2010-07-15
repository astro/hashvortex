var Buffer = require('buffer').Buffer;

module.exports = {
    generateRandom: function() {
	var result = new Buffer(20);
	for(var i = 0; i < 20; i++)
	    result[i] = Math.floor(Math.random() * 256);
	return result;
    }
};
