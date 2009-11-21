#! /bin/bash

# Extract arguments that should be given to Java rather than to the
# repl.

java_opts=()

for arg; do
	case "$arg" in
		-D*)
			java_opts+=("$arg")
			shift
			;;
		*)
			break
			;;
	esac
done

# This runs a REPL with the environment.
rlwrap java "${java_opts[@]}" \
  -cp repl/:src/:test/:lib/'*' \
  clojure.main "$@"

# To set this up, you need to use maven to assemble all of the dependencies.
# The setup-repl.sh should be wiling to do this for you.
