# erl default
This projects aims to include lots of helper for debugging, developing and testing your erlang project.

## user_default
user_default is a file where all the exported functions can be called without the module prefix from the shell.
For documentation of user_default run help() with the code loaded.

## xref 
* ./priv/xref - a script that peeks in rebar.config and runs xref with the help of that specification
* ./priv/xref components - a script that, given a spec, groups the moduls in your callgraph and checks that 
call between groups only can be done according to the spec. 
The grouping is done by prefixes in the module names. 