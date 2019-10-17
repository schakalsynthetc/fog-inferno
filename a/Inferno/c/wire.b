implement X;
include "sys.m"; sys:Sys;
include "draw.m"; draw:Draw;

X: module { PATH: con "/b/dis/Inferno/!.dis";	
	t,z,y,x: byte ;
	
	p:int; q:int; r:string;
	
	init: fn (nil: ref Draw->Context, argv: list of string) ;
	eval: fn (nil: ref Draw->Context, argv: list of string) ;
	walk: fn (nil: ref Draw->Context, argv: list of string) ;
	quit: fn (nil: ref Draw->Context, argv: list of string) ;
	
	XA,XB,XX, LEFT,RIGHT :string;
	
	stdin,stdout,stderr: ref Sys->FD ;
	
}; init(nil: ref Draw->Context, argv: list of string){ 
	it:string;
	sys=load Sys Sys->PATH;
	
	t=byte 16rE9 ; z=y=x=byte 0 ;
	
	stdin=sys->fildes(0); stdout=sys->fildes(1); stderr=sys->fildes(2); 
	sys->fprint(stderr, "init ");
	for (i:=0; argv!=nil && i<5; argv=tl argv) {it=hd argv; case i {
			0=>XB=it; 1=>XA=it; 2=>LEFT=it; 3=>XX=it; 4=>RIGHT=it;
	} ;i++; }
	sys->fprint(stderr, ":%s%s: %s %s %s : ",XA,XB, LEFT, XX, RIGHT);

return eval(nil,argv);}eval(nil: ref Draw->Context, argv: list of string){

	if (p==0) { } else {p++; sys->fprint(stderr,"eval "); }
	
return walk(nil,argv);}walk(nil: ref Draw->Context, argv: list of string){ 
	it:string;
	sys->fprint(stderr,"walk ");
	if (argv!=nil) { 
		XX=hd argv;
		for (i:=0;argv!=nil;argv=tl argv) {it=hd argv; sys->fprint(stderr,"\n :%d: %s ; ",i++,it); }
	}
	
return quit(nil,argv);

} quit (nil: ref Draw->Context, argv: list of string) { 
	sys->fprint(stderr,"quit ");
	it:string; if(argv!=nil)for(i:=0;argv!=nil;argv=tl argv){it=hd argv; 
		sys->fprint(stderr,"\n :%d: %s ; ",i++,it); 
	}
} oops (nil: ref Draw->Context, argv: list of string) { 
	sys->fprint(stderr,"oops "); 
	it:string; if(argv!=nil)for(i:=0;argv!=nil;argv=tl argv){it=hd argv; 
		sys->fprint(stderr,"\n :%d: %s ; ",i++,it); 
	}
}
