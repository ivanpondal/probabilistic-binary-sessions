ML_FILE=$1
make -C decoder > /dev/null
make build > /dev/null
dune exec -- ocaml-print-intf $ML_FILE | ./decoder/rosetta
