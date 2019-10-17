implement Mods;
include "sys.m";  sys:Sys; read,write,fildes,open,print,fprint,FD,OREAD: import sys;
include "draw.m";
include "arg.m"; arg:Arg;
include "string.m"; str:String;

Mods: module { init: fn(nil: ref Draw->Context, argv: list of string); };

usage()
{ sys->fprint(sys->fildes(2), "usage: mods [start] [nsteps] [mod .. mod]\n"); raise "fail:usage"; }

start,nsteps: int; stderr, stdout: ref FD;

init(nil: ref Draw->Context, argv: list of string)
{
	sys = load Sys Sys->PATH;
	arg = load Arg Arg->PATH;
	str = load String String->PATH;
	
	stderr = fildes(2);
	stdout = fildes(1);
	
	arg->init(argv); while((c:=arg->opt())!=0) case c { * => usage();} argv = arg->argv();
	
	err:string;
	
	if (argv==nil) usage(); (start, err) = str->toint(hd argv, 10); argv=tl argv;
	if (argv==nil) usage(); (nsteps, err) = str->toint(hd argv, 10); argv=tl argv;

	steps := array [len argv] of int; 
	
	for (i:=0; argv!=nil; argv=tl argv) { (steps[i++], err) = str->toint(hd argv, 10); }
	
	walk (start, nsteps, steps);
}

walk (start: int, nsteps: int, steps: array of int): int
{
	p, i, steplen: int ; line: string;

	steplen=len steps;
	
	for (p=start; p<nsteps; p++) {
		line = sys->sprint("%7d",p);
		for (i=0; i<steplen; i++){
				line += sys->sprint(" %7d", p % steps[i]);
		}
		sys->fprint(stdout, "%s\n", line);
	}
	return p;
}

