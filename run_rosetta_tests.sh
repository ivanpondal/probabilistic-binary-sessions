function diff_testcase() {
    local expected_pretty_print=$1
    local input_ml=$2
    diff=$(colordiff -y <(cat $expected_pretty_print) <(./decode_type.sh $input_ml))
    diff_result=$?

    if [[ $diff_result == 1 ]];
    then
        tput bold
        tput setaf 1
        echo "Found differences for input \"$input_ml\""
        tput sgr0
        echo "expected vs actual pretty printing:"
        echo "$diff"
    else
        tput bold
        tput setaf 2
        echo "\"$input_ml\" is ok!"
        tput sgr0
    fi
}

function get_expected_pretty_print() {
    local input_ml=$1
    echo -n $input_ml | rev | cut -c9- | rev | xargs -I{} echo "{}_expected_pp"
}

function run_compare(){
    local input_ml=$1
    diff_testcase $(get_expected_pretty_print $input_ml) $input_ml
}

export -f run_compare
export -f diff_testcase
export -f get_expected_pretty_print

ls -1 test/rosetta/resources/*_test.ml | xargs -I{} bash -c "run_compare {}"