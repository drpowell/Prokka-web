#!/bin/sh

make clean
cabal exec make PRODUCTION=1

sudo -u prokka  mkdir -p ~prokka/bin ~prokka/user-files/uploads ~prokka/user-files/status ~prokka/user-files/output
sudo -u prokka cp -f prokka-web prokka-runner ~prokka/bin
sudo -u prokka cp -f zip-output.sh ~prokka
sudo -u prokka cp -r static ~prokka
sudo -u prokka cp -r templates ~prokka

sudo -u prokka killall prokka-runner prokka-web
