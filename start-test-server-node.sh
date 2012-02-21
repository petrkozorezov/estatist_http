#!/bin/sh
export ERL_LIBS="./deps/:../:${ERL_LIBS}"
exec erl -pa ebin -boot start_sasl -s estatist_http -sname estatist_http@`hostname` -config example
