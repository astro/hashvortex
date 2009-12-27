set xdata time
set timefmt "%s"
set xlabel "Time"
set ylabel "Pkt/s"
set samples 10000
plot 'Ping.data' using 1:2 \
     title 'Ping' \
     with lines, \
     'FindNode.data' using 1:2 \
     title 'FindNode' \
     with lines, \
     'GetPeers.data' using 1:2 \
     title 'GetPeers' \
     with lines, \
     'AnnouncePeer.data' using 1:2 \
     title 'AnnouncePeer' \
     with lines
