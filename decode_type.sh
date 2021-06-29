ML_FILE=$1
make -C decoder > /dev/null
make build > /dev/null
ocamlc -w +A -rectypes -thread unix.cma threads.cma -I _build/install/default/lib/fuse/ -i $ML_FILE  | sed 's/Fuse.//g' |  ./decoder/rosetta
