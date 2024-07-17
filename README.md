## Lua to Windows Batch Translator

[![Donate via Stripe](https://img.shields.io/badge/Donate-Stripe-green.svg)](https://buy.stripe.com/00gbJZ0OdcNs9zi288)<br>

Batch scripting is a train wreck.
Logical operators are missing.
Calling internal functions is broken.
Calling external scripts is broken.
Local variable scope is broken.
So I'm making a fix for it.

Features:
- global variables are transpiled into environment variables
- local variables are transpiled into ... setlocal variables, which operate like Lua global variables, but are not environment variables.
- global scope functions work
- if-conditions work.  'and's, 'or's, 'not's, elseifs and else
- arithmetic and concatenation operations work with assignment
- select('#', ...) and select(n, ...) works when used for command-line arguments

depends upon:
- https://github.com/thenumbernine/lua-parser
- https://github.com/thenumbernine/lua-ext

Still TODO:
- support for inline and lambda functions 
-  keep track of what vars are in scope when a function is moved 
-  local functions won't respect the local scope
- scope of local variables isn't respected
- breaking apart boolean expressions of statements into preceding temporary statements, and using them in either conditions or assignment
-  boolean operations are based on short-circuiting, so assigning to a boolean operation still needs to be implemented
-  in fact, merging the assignment and if-conditions, it might be best to use temp registers for bool expressions (and all expressions, for that matter)
- heterogeneous tables -- non-numbers as keys
- assigning functions to variables
