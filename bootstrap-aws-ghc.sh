#!/bin/sh -e

apt-get update
apt-get install -y ghc6 git-core cabal-install libghc6-quickcheck2-dev libghc6-deepseq-dev libghc6-network-dev libghc6-network-bytestring-dev libghc6-http-dev libev-dev libssl-dev

cabal update
cabal install hopenssl hlibev binary-strict
