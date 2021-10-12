ML_FILE=$1
make build > /dev/null
dune exec -- ocaml-print-intf $ML_FILE