#!/! fiqh.m

Fiqh: module { PATH: con "/fiqh/a/Inferno/b/fiqh.dis" ;

	init: fn(nil: ref Draw->Context, argv: list of sting) ;
	eval: fn(t: array of byte, argv: list of string) ;
	walk: fn(t: array of byte, argv: list of string) ;
	quit: fn(t: array of byte, argv: list of string) ;

	Qt: adt {t: array of byte ; p:int; q:int; r:int; }
		# fiqh object pointer
		# these point in both smalltalk and inferno address spaces
	Qr: adt { t: array of byte; it:Qt; q:Qt; r:string; }
		# typed named reference to an object
		# type directs how to resolve it 
		# t, it, q can be nil for a bare textual symbol
		# r can be nil for an anonymous pointer

	Object: adt {
		t:Qr; it:Qt; q,r: list of Qr ; 
		x: fn(x: Qr, argv: list of string): (Qr,Qr) ;
	}
		# t is class, qr are inst vars x msg send returning (oops, value)

	Messsage: adt {t: array of byte ; it:Qt; x:Qr; argc:int; argv: list of Qr ; }
		# "it" is receiver, "x" is selector

	# shared primitive types: integers, byte arrays, word arrays
	ByteArray:  adt {t:con 16rEE010000; it:Qt; c:int;v: array of byte ;}
	WordArray:  adt {t:con 16rEE040000; it:Qt; c:int;v: array of int  ;}
	FloatArray: adt {t:con 16rEE060000; it:Qt; c:int;v: array of real ;}

	ByteSymbol: adt {t:con 16rEE250000; it:Qt; q:Qr; r: string ;}
	ByteString: adt {t:con 16rEE270000; it:Qt; q:Qr; r: string ;}

	# SmallInteger

	# arrays of objects, "it" is the smalltalk-side array object
	Array: adt {t:con 16rEE050000; it:Qt; c:int;v: array of Qr ;}
	Association: adt {t:con 16rEE070000; it:Qt; left,right: Qr ;}

	# module functions 
	# allocate Qt and Qr
	# resolve Qr on smalltalk side
	# create and destroy objects
	# execute message sends
	# callback for smalltalk to resolve Qr pointing into inferno side

	# file io and dis module init from smalltalk
	# translate smalltalk class definitions to dis modules
}

