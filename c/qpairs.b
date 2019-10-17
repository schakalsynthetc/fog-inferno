implement Getline;
include "sys.m";  sys:Sys; read,write,fildes,open,print,fprint,FD,OREAD: import sys;
include "draw.m";
include "arg.m"; arg:Arg;
include "string.m"; str:String;

Getline: module { init: fn(nil: ref Draw->Context, argv: list of string); };

usage()
{ sys->fprint(sys->fildes(2), "usage: infixes [files...]\n"); raise "fail:usage"; }

showdepth,pline,pchar,nchan,multi,nlines,offset: int ; stderr, stdout: ref FD;

pairs: array of byte ;

init (nil: ref Draw->Context, argv: list of string)
{
	sys = load Sys Sys->PATH;
	arg = load Arg Arg->PATH;
	str = load String String->PATH;
	stderr = fildes(2);
	stdout = fildes(1);
	
	showdepth=1; 
	
	charoffset:int;
	pline=pchar=nchan=multi=nlines=offset=charoffset=0;
	
	pairs=array[2] of byte;  pairs[0]=byte '{'; pairs[1]=byte '}';
	
	arg->init(argv);
	while((c := arg->opt()) != 0) case c {
		'm' => multi=1;
		'n' => s := arg->arg(); (nlines, nil) = str->toint(s, 10);
		'o' => s := arg->arg(); (offset, nil) = str->toint(s, 10);
		'd' => s := arg->arg(); (showdepth, nil) = str->toint(s, 10);
		* => usage();
	}
	argvtoo: list of string ; argvtoo=argv=arg->argv();
	if(len argv == 0)
		usage();
	
	prline:string; prline=sys->sprint("infix blocks init %d", len argv);
	for(; argv != nil; argv = tl argv){
		prline += sys->sprint(" %s", hd argv);
	}
	sys->fprint(stdout, ": %s ", prline);
	
	argv=argvtoo;

	for(; argv != nil; argv = tl argv){
		fd := open(hd argv, OREAD); if(fd==nil){
			fprint(stderr, "qlines oops errant open %s '%r'\n", hd argv); exit;
		}
		nchan++; pline=pchar=0; (pline,pchar)=lines (fd, hd argv);
		sys->fprint(stdout, "; %d %d %d %s ;\n",nchan,pline,pchar, hd argv);
	}
	sys->fprint(stdout, ";\n");
}

walk (fd: ref FD, file: string, stack: int): (int,int)
{
	quoting:int; n, m: int; buf:= array[8192] of byte;
	
	quoting=0; n=m=0; for (;;){
		c:= array[1] of byte; 
		n=read(fd,c,1);
		if (n<0){ fprint(stderr, "qline oops errant read %s '%r'\n", file); exit; }
		if (n==0){ break; }
		buf[m++]=c[0];
		
		if (quoting==0 && (int c[0]=='{' || int c[0]=='[' || int c[0]=='(')) { 
			++stack;
			sys->fprint (stdout,
				": %0.2X :%0.7d:%0.7d: %d : open ;\n", 
				nchan, ++pline, pchar, int c[0]
			); break; 
		}
		if (quoting==0 && (int c[0]=='}' || int c[0]==']' || int c[0]==')')) { 
			if (--stack<0) stack=0;
			sys->fprint (stdout,
				": %0.2X :%0.7d:%0.7d: %d : close ;\n", 
				nchan, ++pline, pchar, int c[0]
			); break; 
		}
		if (quoting==0 && (int c[0]==',' || int c[0]==';')) { 
			sys->fprint (stdout,
				": %0.2X :%0.7d:%0.7d: %d : infix ;\n", 
				nchan, ++pline, pchar, int c[0]
			); break; 
		}
		
		if (int c[0]=='"') { 
			if (quoting==0) quoting=1; else quoting=0; 
			# sys->fprint (stdout,
			#	": %0.2X :%0.7d:%0.7d: %d : quote %d ;\n", 
			#	nchan, pline, pchar, int c[0], quoting
			#); 
		} 
	}
	
	pchar += m;
	
	return (m,stack);
}

lines (fd: ref FD, file: string): (int,int)
{ m,stack:int; stack=0; do { (m,stack)=walk(fd, file,stack); } while (m!=0); return (pline,pchar); }
