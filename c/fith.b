#!/! /c/fith.b
#
#
#
implement FITH;
include "sys.m"; sys: Sys;
include "draw.m";
include "daytime.m"; daytime: Daytime;
include "arg.m"; arg: Arg;

FITH: module {t:adt{t: array of byte ; 
	it : list of array of byte ; 
		left: list of array of byte ;
		right: list of array of byte ;
	}; 
	# t, this fith image, left and right context images ##
	init: fn (nil: ref Draw->Context, argv: list of string);
	walk: fn (it: ref Fith, argv: list of string) : ref Fith ;
	eval: fn (it: ref Fith, argv: list of string) : ref Fith ;
	quit: fn (it: ref Fith, argv: list of string) ;
	oops: fn (it: ref Fith, argv: list of string) :string;
}

# import /a/fith/wire/a

# touch a-fith-wire-a.m
# touch a-fith-leaf-a.m
# touch a-fith-unit-a.m
# touch a-fith-vector-a.m
# touch a-fith-map-a.m

# touch fith-inferno.m

# file /a/fith/.../a yields module via shebang call

# shebang '#!/! /path/a' dispatches to Xa.dis (/$A/$B/Xa.dis)
# and so on for a,b,c,d,e,f thus /path/t -> /b/dis/Xt.dis
# and /path/x -> Xx.dis, /path/y -> Xy.dis /path/z -> Xz.dis
# Xt.dis interprets shebang file body

# Xa.dis yields 0.dis 0.sbl 0.asm 0.m 0.b from shebang file body
# /path/a mk and compile /path/c/<what>

# Xa.dis is the fish mk and compiler driver
# Xb.dis runtime interpreter for '#!/! /path/b' compiled file
# Xc.dis preproc and compile for '#!/! /path/c' source file
# Xd.dis preproc compile and runtime for '#!/! /path/d' doc file
# Xe, Xf runtime elements and forms syscalls and library

# Xx.dis runtime dispatch for '#!/! /path/x' verb file, probably a derivative of sh
# Xy.dis runtime for fix namespace, srv, mount, bind and '#!/! /path/y' archive files
# Xz.dis runtime for wire graph, chan, srv, plumbing, network and '#!/! /path/z' channel def files

# Xp runtime pointer resolver
# Xq runtime qid resolver
# Xr runtime ref resolver

# Xi runtime identity verify, login, etc service and library

# Xl runtime location service and library
# Xn runtime network transport method library

# Xh runtime venti accessor and library
# Xj runtime journal, log, timekeeping, calendrics, scheduling library
# Xk runtime cryptography library

# Xm runtime medium attach/detach/map library
# 	extended Xm modules for public media formats

# Xw runtime fiqh window system and terminal library
#	includes fiqt scene viewport and render

# Xo runtime fiqt effect and object (attached to scene leaf) geometry and morphing library

# load modules X,Y,Z

load X "/a/fith/subtype/b/X.dis";
load Y "/a/fith/subtype/b/Y.dis";
load Z "/a/fith/subtype/b/Z.dis";

### init ## entry point from limbo #
init (nil: ref Draw->Context, argv: list of string)
{
	sys = load Sys Sys->PATH;
	arg = load Arg Arg->PATH;
	daytime = load Daytime Daytime->PATH;
	
}

### quit ## ok exit point back to ordinary limbo #
quit (it: ref Fith, argv: list of string)
{ }

### oops ## error exit point back to ordinary limbo #
oops (it: ref Fith, argv: list of string): string
{ }

### argv ## arguments vector #
walkargv (it: ref Fith, argv: list of string): ref Fith { }
evalargv (it: ref Fith, argv: list of string): ref Fith { }

### env ## environment map #
walkenv (it: ref Fith, argv: list of string): ref Fith { }
evalenv (it: ref Fith, argv: list of string): ref Fith { }

### wdir ## the leaf that is here #
walkwdir (it: ref Fith, argv: list of string): ref Fith { }
evalwdir (it: ref Fith, argv: list of string): ref Fith { }

### stdin ## standard io #
walkstdin (it: ref Fith, argv: list of string): ref Fith { }
evalstdin (it: ref Fith, argv: list of string): ref Fith { }

### stdout ## standard io #
walkstdout (it: ref Fith, argv: list of string): ref Fith { }
evalstdout (it: ref Fith, argv: list of string): ref Fith { }

### stderr ## standard io #
walkstderr (it: ref Fith, argv: list of string): ref Fith { }
evalstderr (it: ref Fith, argv: list of string): ref Fith { }

### initial walk eval and ok : x,argv,wdir,env ; then loop : stdio ; then quit ; env,wdir,argv,y #

# argv: parse arguments as fith
# wdir: here leaf, may be synthetic
# env: alpha-reduce env variables named in argv if any
# wdir before env because vars may shadow leaf elements

# oops raises string $status, quit raises nil

# normal stdio walk does fish interactive or fix noninteractive

# argv walk handles shebang
# new x context (emu host or remote call) starts shebang-to-shebang comms
# /b/<context>/! for host exec contexts
