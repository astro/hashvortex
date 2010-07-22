set xdata time
set timefmt "%s"
set xlabel "Time"
set ylabel "Pkt/s"
set samples 100
plot 'trackerspoofer.data' using 1:2 \
     title 'Ping' \
     with lines, \
     'trackerspoofer.data' using 1:3 \
     title 'FindNodes' \
     with lines, \
     'trackerspoofer.data' using 1:4 \
     title 'GetPeers' \
     with lines, \
     'trackerspoofer.data' using 1:5 \
     title 'AnnouncePeer' \
     with lines
