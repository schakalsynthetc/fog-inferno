implement Jsxn;
include "sys.m"; sys: Sys ;
include "draw.m";
include "sh.m";
include "arg.m"; arg: Arg ;

Jsxn: module
{ init: fn(ctxt: ref Draw->Context, args: list of string); };

stdout: ref Sys->FD;
stderr: ref Sys->FD;

die (status: string, msg: string)
{ sys->fprint(stderr, "jsxn: %s\n", msg); raise status; }

init(ctxt: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	stdout = sys->fildes(1);
	stderr = sys->fildes(2);

	if (args==nil) die ("nothing", "nothing to do"); else verb (tl args);
}

verb (args: list of string)
{
	if (args==nil)
		die("empty", "empty value");
	if (hd args == "list")
		prlist (stdout, tl args);
	if (hd args == "object")
		prdict (stdout, tl args);
	if (hd args == "var")
		prvar (stdout, tl args);
}

prvar(fd: ref Sys->FD, args: list of string)
{
	if (args==nil) {
		die("usage", "usage: json var [name] list|object [value..value]");
	} else {
		sys->fprint(fd, "var %s = ", hd args); verb (tl args);
	}
}

prlist(fd: ref Sys->FD, args: list of string)
{
	sys->fprint(fd, "[");
	for (; args!=nil; args = tl args) {
		sys->fprint(fd, "%s", hd args);
		if (tl args != nil) sys->fprint(fd, ",");
	} sys->fprint(fd, "];\n"); exit;
}

prdict (fd: ref Sys->FD, args: list of string)
{
	sys->fprint(fd, "{\n"); while (args!=nil) {
		sys->fprint(fd, "  '%s' : ", hd args);
		args = tl args;

		if (args != nil) {
			sys->fprint(fd, "\"%s\"", hd args);
		} else die ("incomplete", "incomplete (unbalanced) dictionary");

		if (tl args != nil)
			sys->fprint(fd, ",\n");
		else
			sys->fprint(fd, "\n");
			
		args = tl args;
	} sys->fprint(fd, "};\n"); exit;
}
