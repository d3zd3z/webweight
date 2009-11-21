#! /bin/sh

# Directly invoke the program's main.
java -cp src/:lib/'*' clojure.main \
  -e "(use 'org.davidb.webweight.main)" \
  -e '(-main)' "$@"
