set term postscript eps enhanced color
set output "results2.eps"

set title "Amazing title"
set xlabel "Request Rate"
set ylabel "Response Time"
set y2label "Percentage of errors"

set xrange [0:1500]
set yrange [0:70]
set y2range [0:100]

plot "experiment.dat" using 1:3 axis x1y1 with linespoints title "Response Time",\
     "experiment.dat" using 1:4 axis x1y2 with linespoints title "% Errors"

