#! /bin/sh

# Copy jar files so that the ./run.sh script will work to get a REPL.

mvn clojure:run -Dscript=src/util/clojure/setup_repl.clj
