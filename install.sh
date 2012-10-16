#!/bin/sh

make clean
make PRODUCTION=1

sudo -u prokka  mkdir -p ~prokka/bin ~prokka/user-files/uploads ~prokka/user-files/status ~prokka/user-files/output
sudo -u prokka cp -f main runner ~prokka/bin
sudo -u prokka cp -r static ~prokka
sudo -u prokka cp -r templates ~prokka

