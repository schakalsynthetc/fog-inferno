implement Qlines;
include "sys.m";  sys:Sys; read,write,fildes,open,print,fprint,FD,OREAD: import sys;
include "draw.m";
include "arg.m"; arg:Arg;
include "env.m"; env:Env;
include "string.m"; str:String;

Qlines: module { init: fn(nil: ref Draw->Context, argv: list of string); };

usage()
{ sys->fprint(sys->fildes(2), "usage: qlines  [-m] [-n nlines] [-o offset] [files...]\n"); raise "fail:usage"; }

pline,pchar,nchan,multi,nlines,offset: int ; stderr, stdout: ref FD;

init (nil: ref Draw->Context, argv: list of string)
{
	sys = load Sys Sys->PATH;
	arg = load Arg Arg->PATH;
          env = load Env Env->PATH;
	str = load String String->PATH;
	stderr = fildes(2);
	stdout = fildes(1);
	
	charoffset:int;
	pline=pchar=nchan=multi=nlines=offset=charoffset=0;
	
	arg->init(argv);
	while((c := arg->opt()) != 0) case c {
		'm' => multi=1;
		'n' => s := arg->arg(); (nlines, nil) = str->toint(s, 10);
		'o' => s := arg->arg(); (offset, nil) = str->toint(s, 10);
		* => usage();
	}
	argvtoo: list of string ; argvtoo=argv=arg->argv();
	if(len argv == 0)
		usage();
	
	prline:string; prline=sys->sprint("qlines init %d", len argv);
	for(; argv != nil; argv = tl argv){
		prline += sys->sprint(" %s", hd argv);
	}
	sys->fprint(stderr, "%s\n", prline);
	
	argv=argvtoo;

	offset0:=offset;
	
	for(; argv != nil; argv = tl argv){
		fd := open(hd argv, OREAD); if(fd==nil){
			fprint(stderr, "qlines oops errant open %s '%r'\n", hd argv); exit;
		}
		nchan++; 
		offset=offset0;
		if (offset>0) charoffset=drops (fd, hd argv); 
		sys->fprint(stderr, "qlines walk %d %s\n  qline init %d %d %d %d\n", nchan, hd argv, nchan, offset0, charoffset, 0);
		pline=pchar=0; 
		charoffset=lines (fd, hd argv);
	}
	sys->fprint(stderr, "qlines quit\n");
}

walk (fd: ref FD, file: string, toss: int): int
{
	n, m: int; buf:= array[8192] of byte;
	
	n=m=0; for (;;){
		c:= array[1] of byte; 
		n=read(fd,c,1);
		if (n<0){ fprint(stderr, "qline oops errant read %s '%r'\n", file); exit; }
		if (n==0){ pline++; pchar += m; break; }
		buf[m++]=c[0];
		if (int c[0] == '\n') {pline++; pchar += m; break;}
	}
	
	if (toss==0) { 
		if (m==0) 
			sys->fprint(stderr, "  qline quit %d %d %d %d\n", nchan, pline, pchar, m);
		else 
			eval ((nchan, pline, pchar, m), buf,m);
	}
	
	return m;
}

eval (q: (int,int,int,int), buf: array of byte, nbuf: int): string
{ 
	m:int; (nchan,pline,pchar,m)=q; m=nbuf;
	
	sys->fprint(stderr, "  qline walk %d %d %d %d\n", nchan, pline, pchar, m);
	write (stdout,buf,m); 
	# env->setenv("it", string buf[0:m]);
	
	return nil; 
}

drops (fd: ref FD, file: string): int
{ p,m:int; p=m=0; do {
	m=walk(fd,file,1); if (m==0) break; p+=m;
} while (--offset>0); return p; }

lines (fd: ref FD, file: string): int
{ p,m:int; p=m=0; do {
	m=walk(fd, file,0); if (m==0) break; p+=m; 
}while (multi || --nlines>0); return p; }
