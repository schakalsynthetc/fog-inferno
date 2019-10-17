#!/! /w/eb/x/ml/elt.b
#
# < cdata elt id attrs ; e=tagname
#
implement XeXML;
include "sys.m"; sys: Sys ;
include "draw.m";
include "sh.m";
include "arg.m"; arg: Arg ;

XeXML : module { init: fn (ctxt: ref Draw->Context, args: list of string); e: string; };

Xelt : adt { tag:string; attrs: list of string ; ndata:int; data:string; } ;

stdin  : ref Sys->FD;
stdout : ref Sys->FD;
stderr : ref Sys->FD;

die (status: string, msg: string)
{ sys->fprint(stderr, "xelt: %s\n", msg); raise status; }

init (ctxt: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;

	stdin  = sys->fildes(0);
	stdout = sys->fildes(1);
	stderr = sys->fildes(2);

	it:Xelt;

	argv0 := hd args ; args = tl args;

	# if (elt == "shebang") {
	#	(n,elts) := sys->tokenize(shebang, "/");
	#	for (eltl := ""; tl elts != nil; elts = tl elts) eltl += hd elts + "/";
	#	elt = hd elts;
	#	sys->fprint(stderr, "%d path elements in shebang: %s\n", n, eltl);
	# }
	
	propen(it);
	prtag(it);
	prattrs(it);
	prdata(it);
	prclose(it);
	
	if (args != nil) {
		sys->fprint(stdout, ">");
		for (; args != nil; args = tl args) sys->fprint(stdout, "%s ", hd args);
		sys->fprint(stdout, "</%s>", elt);
	} else { sys->fprint(stdout, "/>"); }
}

# returns (tag,path)

findtag (it: string): (string,string)
{ 
	yleft:string; y:string; yc:int; ys: list of string ;	
	
	(yc,ys)=sys->tokenize(it,"/"); for (; ys != nil; ys=tl ys){ 
		y=hd ys; yleft+="/"+y; 
	};
}

prtag (it: string)
{ }
