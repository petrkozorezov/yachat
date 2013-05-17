#!/bin/bash
ROOT=$(dirname $0)/..
ERL_LIBS=${ROOT}/deps erl -pa ebin -boot start_sasl -sname yachat@localhost -setcookie yachat -config default -s yachat -s reloader $@
