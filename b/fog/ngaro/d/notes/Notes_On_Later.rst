One of the more interesting flow control words in Retro is *later*. This is a
word that returns control to the caller, then regains control when the caller
is finished.

A simple example:

::

  : test ( - ) 1 . later 2 . ;
  : b ( - ) test 3 . ;

Run *b*, and you should see:

::

  1 3 2

What happens here:

::

  b:
    call test
      test displays 1
      test returns to b
    display 3
    execution of b ends
      control passes back to test
      test displays 2
      and returns
  back to the interpreter

*later* can be very useful in cases where you need to ensure cleanups are done
when a function returns. For example, when parsing, you may want to disable the
whitespace filter, but restore it when your word is done. The simple way:

::

  : foo ( "- ) whitespace off 32 accept whitespace on ;

Or, factoring out the filter:

::

  : no-ws  ( - ) whitespace off ;
  : yes-ws ( - ) whitespace on ;
  : foo ( "- ) no-ws 32 accept yes-ws ;

But it's cleaner with *later* added:

::

  : disable-ws ( - ) whitespace off later whitespace on ;
  : foo ( "- ) disable-ws 32 accept ;

The use of *later* also allows passing control back and forth between two words:

::

   : foo ( - ) 1 . later 2 . later 3 . later 4 . ;
   : bar ( - ) foo 5 . later 6 . later 7 . ;

When *bar* is run:

::

  1 5 2 6 3 7 4

Play with *later* and see what kind of interesting bits of flow control you can
achieve with it. It's a powerful word, and definitely worth study.
