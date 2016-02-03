set term postscript eps enhanced color
set output "results1.eps"
set title "Amazing title"
set xlabel "Request Rate"
set ylabel "Reply Rate"
set xrange [0:1500]
set yrange [0:900]

plot "experiment.dat" using 1:2 with linespoints title "Experiment"

