Compiler macros are often used to lay down code in the calling word. You'll
see something like:

::

  : foo &+ , &. , ; compile-only

This is fine, but sometimes you need to deal with other macros:

::

  : bar 1 1 , , 2 1 , , &foo do ; compile-only

This gets messy quickly. And it's not flexible. What if you want *foo* and
*bar* to work interactively as well?

::

  : foo &+ .word &. .word ; immediate
  : bar 1 .data 2 .data &foo .macro ; immediate

Still messy, but it does solve the flexiblity problem. You just need to know
the class of each function.

The word *`* was written to address the clarity and flexibility issue. It is
class-aware, so lays down the proper code and the proper class handler for
it. With *`* the above examples can be rewritten:

::

  : foo ` + ` . ; immediate
  : bar ` 1 ` 2 ` foo ; immediate
