set xdata time
set timefmt "%s"
set xlabel "Time"
set ylabel "Pkt/s"
set samples 10000
plot 'spoofer.data' using 1:2 \
     title 'Ping' \
     with lines, \
     'spoofer.data' using 1:3 \
     title 'FindNode' \
     with lines, \
     'spoofer.data' using 1:4 \
     title 'GetPeers' \
     with lines, \
     'spoofer.data' using 1:5 \
     title 'AnnouncePeer' \
     with lines
