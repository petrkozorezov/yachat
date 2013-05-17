#!/bin/bash
ROOT=$(dirname $0)/..
ERL_AFLAGS="-lager handlers '[{lager_console_backend, debug}]'" \
ERL_LIBS=${ROOT}/deps:${ROOT}/apps \
erl  -pa ebin -boot start_sasl -sname yachat@localhost -setcookie yachat -s lager -s reloader $@
