#!/bin/sh

output=$(python lexical_analyzer_1.py)
echo $output
swipl -q -f parser_.pl -g "program(P, ${output}, []), program_eval(P, [], Env)." -t halt