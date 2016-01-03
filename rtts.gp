#!/usr/bin/env gnuplot
set datafile separator ','
set terminal svg enhanced mouse standalone size 1280,960
set output 'rtts.svg'
#set terminal pngcairo size 1280,960
#set output 'rtts.png'
set xdata time
set timefmt '%H:%M:%S'
set format x '%H:%M:%S'
set format y '%.1s %c'
set lmargin 10
set xrange ['15:12:40.288':'15:12:40.917']
set multiplot layout 1,1 title 'rtts.csv'
do for [i=2:2] {
    plot 'rtts.csv' using 1:i w lines lt 3 t column(i)
}
