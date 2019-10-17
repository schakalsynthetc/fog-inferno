#!/! /c/fith.m
#
#
#

Xt :adt{t: array of byte; it.left,right: list of array of byte; };

# possible to initialize t array of byte in declaration?

## fith #
X :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## leaf #
Y :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## wires #
Z :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## pointer space #
P :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## walkable qidspace #
Q :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## reference map for qidspace #
R :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## unit support #
U :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## vector support #
V :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## abstract data structure support #
E :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## data representation support #
F :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## identity support #
I :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## journal support #
J :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## key cryptography support #
K :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## location support #
L :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## medium support #

M :module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

## network transit support #

N:module{t:adt{t: array of byte ; 
	it : list of array of byte ;
		left: list of array of byte ;
		right: list of array of byte ;
	};
	init: fn (it: ref Xt, argv: list of string): ref Xt ;
	walk: fn (it: ref Xt, argv: list of string): ref Xt ;
	eval: fn (it: ref Xt, argv: list of string): ref Xt ;
	quit: fn (it: ref Xt, argv: list of string): ref Xt ;
	oops: fn (it: ref Xt, argv: list of string): ref Xt ;
} ;

####bye#

