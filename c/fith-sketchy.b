# fith image
#
#
#
implement FITH;
include "sys.m"; sys: Sys;
include "arg.m"; arg: Arg;
include "draw.m";
include "daytime.m"; daytime: Daytime;

FITH: module
{ 

	init: fn (nil: ref Draw->Context, argv: list of string);
	walk: fn (it: ref Fith, argv: list of string) : ref Fith ;
	eval: fn (it: ref Fith, argv: list of string) : ref Fith ;
	quit: fn (it: ref Fith, argv: list of string) : ref Fith ;

	oops: fn (it: ref Fith, argv: list of string) : ref Fith ;

	Xp: adt {
		now: int; pid: int; seq: int ;	
	};

	Xq: adt {
		it: array of byte ; left: ref Xq; right: ref Xq ;
	};
	Xr: adt {
		it: string ; left: ref Xq; right: ref Xq ;
	};
		
	Fith: adt {
		t: byte ; tt: int ;
		ndata: int; data: array of byte ; 
		left: list of array of byte  ; 
		right: chan of string ;
		p: Xp ; q: Xq ; r: Xr ;
		x:string; y:string; z:string;
	};
};

stdin,stdout,stderr: ref Sys->FD ;

init (nil: ref Draw->Context, argv: list of string)
{
	sys = load Sys Sys->PATH;
	arg = load Arg Arg->PATH;
	daytime = load Daytime Daytime->PATH;
	
	arg->setusage("fith");
	
	arg->init(argv);
	
	argv=arg->argv(); 

	stderr=sys->fildes(2);
	stdout=sys->fildes(1);
	stdin=sys->fildes(0);
	
	if (argv!=nil) for (; argv!=nil; argv=tl argv){

	} else {}
	
	it: Fith ; it.t=byte 0; it.tt=0;
	
	p: Xp ; q: Xq ; r: Xr ;
	it.p=p; it.q=q; it.r=r;
	it.p.now=daytime->now();
	it.p.pid=0;
	it.p.seq=0;
	
	sys->fprint (stderr, " /%0.2X/%0.4X/%0.8X/%0.8X/%0.8x/t :init: \n",
		int it.t, it.tt, it.p.pid, it.p.now, it.p.seq
	); 
	walk(ref it, argv);
}

walk (it: ref Fith, argv: list of string) : ref Fith
{
	n:int; data := array[8192] of byte ;
	
	it.p.now = daytime->now(); 
	
	sys->fprint(stderr, " /%0.8X/%0.8X/%0.8X/ :walk: ", 
		it.p.pid, it.p.now, it.p.seq
	);
	
	n=sys->read(stdin, data, 8192); 
	
	if (n<0) {
		it.p.now = daytime->now(); 
		sys->fprint(stderr, ":read: %r ;oops;\n /%0.8X/%0.8X/%0.8X/ :oops: \n",
				it.p.pid, it.p.now, it.p.seq
		); 
		
	} else if (n==0) {
		return quit (it,argv); 
	} else {
		it.p.seq++; it.ndata=n; it.data=data; 	
	}
	
	
return eval (it,argv); }

eval (it: ref Fith, argv: list of string) : ref Fith
{
	n:int; data := array[8192] of byte ;
	
	it.p.now = daytime->now(); 
	
	n=sys->write(stdout, data, 8192); 
	
	if (n<0) {
		it.p.now = daytime->now(); 
		sys->fprint(stderr, ":write: %r ;oops;\n /%0.8X/%0.8X/%0.8X/ :oops: \n",
				it.p.pid, it.p.now, it.p.seq
		); 
	} else { 		
		it.p.now = daytime->now(); 
		sys->fprint(stderr, ";eval;\n");
	} 
	
return walk (it,argv); }

quit (it: ref Fith, argv: list of string) : ref Fith
{
	it.p.now = daytime->now(); 
	sys->fprint(stderr, ";quit; /%0.8X/%0.8X/%0.8x/t\n\n",
		it.p.pid, it.p.now, it.p.seq
	); 
	return it;
}

oops (it: ref Fith, argv: list of string) : ref Fith
{
	it.p.now = daytime->now(); 
	sys->fprint(stderr, ";oops; /%0.8X/%0.8X/%0.8x/t oops\n\n",
		it.p.pid, it.p.now, it.p.seq
	); 
	it.y = "oops" ;
	return it;
}
