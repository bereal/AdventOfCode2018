#!/bin/sh

if [ -z "$1" ]; then
    echo "Usage: $0 <Day>" >&2
    exit 1
fi

runhaskell Day$1.hs
