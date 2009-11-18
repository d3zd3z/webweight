#! /bin/sh

# Copy jar files so that the ./run.sh script will work to get a REPL.

mvn clojure:run -P setup-repl
