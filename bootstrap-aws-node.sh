#!/bin/sh -e

apt-get update
apt-get install -y screen tcpdump build-essential

wget http://github.com/ry/node/tarball/v0.1.102
tar xvfz ry-node-*
cd ry-node-*/
./configure &&make JOBS=3&&make install