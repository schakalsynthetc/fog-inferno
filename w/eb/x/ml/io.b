implement WeXt;
include "sys.m"; sys: Sys;
include "draw.m";
include "string.m"; str: String;

WeXt: module { init: fn(nil: ref Draw->Context, argv: list of string); };

usage () {sys->fprint(sys->fildes(2), 
	"usage: wext [id] [class..class]\n"
); raise "fail:usage"; }

init (nil: ref Draw->Context, argv: list of string)
{
	sys = load Sys Sys->PATH;
	str = load String String->PATH;

	count := Sys->ATOMICIO; offset := big 0; unit := 1;

	tag,id,classes : string ;
	
	if (argv!=nil) { tag = hd argv; argv = tl argv; } else usage();
	if (argv!=nil) { id = hd argv; argv = tl argv; } else usage();
	for (; argv != nil; argv = tl argv) classes += hd argv + " ";

	fd := sys->fildes(0); buf := array [unit*count] of byte ;
	
	if (0 < (n := sys->read (fd, buf, len buf))) {
		sys->fprint(sys->fildes(1), 
			"<%s id='%s' class='%s'>\n", tag, id, classes);
		sys->write(sys->fildes(1),buf,n);
		sys->fprint(sys->fildes(1),"</%s>\n", tag);
	}else if (n<0) { 
		sys->fprint (sys->fildes(2), "oops errant read : '%r' "); raise "fail:error";
	} else {
		sys->fprint (sys->fildes(2), "oops short read : %d < %d ", n, count); raise "fail:error";
	}
}
