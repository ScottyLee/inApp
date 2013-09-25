#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname iap_verify_dev \
    -s iap_verify \
    -s reloader
