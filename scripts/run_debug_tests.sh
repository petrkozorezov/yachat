#!/bin/bash
ROOT=$(dirname $0)/..
cd ${ROOT}
ERL_AFLAGS="-config default" \
make eunit
