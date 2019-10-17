implement Xelt;
include "sys.m"; sys: Sys ;
include "draw.m";
include "sh.m";
include "arg.m"; arg: Arg ;

Xelt: module { init: fn (nil: ref Draw->Context, args: list of string); } ;

DISPATH: con "/w/eb/x/ml/unit.dis" ;

stdin,stdout,stderr  : ref Sys->FD ;

die (status: string, msg: string) 
{ sys->fprint(stderr, "xelt: %s\n", msg); raise status; }

init (nil: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH ;
	stdin  = sys->fildes(0);
	stdout = sys->fildes(1);
	stderr = sys->fildes(2);
	xs,left : list of string ; n: int ;
	argv0,tag,id,classes : string ;
	
	argv0 = hd args; (n,left) = sys->tokenize (hd args,"/"); 
	for (xs = left; tl xs != nil; xs = tl xs) classes+= "/"+ hd xs ;
	args = tl args ;

	if (argv0 == DISPATH) { 
		tag=hd args; args=tl args; 
		sys->fprint(stderr, "%dt not shebang : %s/  %s :\n", n, classes, tag);
	} else {
		tag = argv0; 
		sys->fprint(stderr, "%dt shebang : %s/  %s :\n", n, classes, tag);
	}
	if (args != nil) { id=hd args; args=tl args; }

	if (args==nil) {
		# id is pid, class is argv0, body is stdin 
	}else{}

	sys->fprint(stdout, "<%s id='%s' class='%s'", tag, id, classes);
	
	if (args==nil) sys->fprint (stdout, "/>"); else {
		sys->fprint(stdout, ">");
		for (; args != nil; args = tl args) sys->fprint(stdout, "%s ", hd args);
		sys->fprint(stdout, "</%s> ", tag);
	}
}
