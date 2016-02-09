#!/bin/sh

# intended use: put on a machine, with access to a docker daemon instead of installing gnuplot directly
# why? instead of backporting gnuplot to host distro

docker run --rm --volume $PWD:/mnt/work -w /mnt/work wcurrie/gnuplot "$@"
