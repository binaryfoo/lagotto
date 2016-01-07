#!/usr/bin/env gnuplot
name = 'rtts'
csv = name.'.csv'
set datafile separator ','
set terminal svg enhanced mouse standalone size 1280,960; set output name.'.svg'
#set terminal pngcairo size 1280,960; set output name.'.png'
set xdata time
set timefmt '%H:%M:%S'
set format x '%H:%M:%S'
set format y '%.1s %c'
set lmargin 10
set xrange ['15:12:40.288':'15:12:40.917']
set multiplot layout 1,1 title 'rtts'

plot csv using 1:2 w lines lt 2 t column(2)
