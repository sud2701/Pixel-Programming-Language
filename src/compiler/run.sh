#!/bin/sh

output=$(python3 lexical_analyzer_1.py)
echo $output
swipl -q -f ../runtime/parser_.pl -g "program(P, ${output}, []), program_eval(P, [], Env)." -t halt