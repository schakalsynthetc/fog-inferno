implement Extension;

include "sys.m";
sys: Sys;
include "draw.m";
include "math.m";
math: Math;
include "string.m";
str: String;

include "sh.m";

include "bufio.m";
bufio: Bufio;
Iobuf: import bufio;

include "cell.m";
cell: SCell;
Cell: import cell;
Pair: import cell;
Env: import cell;

include "scheme.m";
scheme: Scheme;
eval: import scheme;

include "builtin.m";
builtin: BuiltIn;
closeinport: import builtin;

include "sform.m";

stdout:  ref Iobuf = nil;

ctxt: ref Draw->Context;

include "extension.m";

init(drawctxt: ref Draw->Context, s: Sys, sch: Scheme, c: SCell,
	b: BuiltIn, m: Math, st: String, bi: Bufio)
{
	ctxt = drawctxt;
	sys = s;
	scheme = sch;
	cell = c;
	builtin = b;
	math = m;
	str = st;
	bufio = bi;

	e := cell->globalenv;
	e = ref Env("<-=", cell->BuiltIn, nil, lsend) :: e;
	e = ref Env("=<-", cell->BuiltIn, nil, lrecv) :: e;
	e = ref Env("alt", cell->SpecialForm, nil, lalt) :: e;
	e = ref Env("channel", cell->BuiltIn, nil, lchannel) :: e;
	e = ref Env("close-inout-port", cell->BuiltIn, nil, closeinoutport) :: e;
	e = ref Env("open-inout-file", cell->BuiltIn, nil, openinoutfile) :: e;
	e = ref Env("open-input-string", cell->BuiltIn, nil, openinstr) :: e;
	e = ref Env("popen", cell->BuiltIn, nil, popen) :: e;
	e = ref Env("quit", cell->BuiltIn, nil, quit) :: e;
	e = ref Env("sleep", cell->BuiltIn, nil, lsleep) :: e;
	e = ref Env("spawn", cell->SpecialForm, nil, lspawn) :: e;
	cell->globalenv = e;
	l := e;
	while(l != nil) {
		x := hd l;
		if(x.ilk == cell->BuiltIn || x.ilk == cell->SpecialForm)
			x.val = ref Cell.Internal(x.name, x);
		l = tl l;
	}
}

lalt(args: ref Cell, env: list of ref Env): (int, ref Cell)
{
	x := args;
	i := 0;
	while(x != nil && !cell->isnil(x)) {
		++i;
		x = cell->lcdr(x);
	}
	ca := array[i] of chan of ref Cell;
	x = args;
	i = 0;
	while(x != nil && !cell->isnil(x)) {
		y := cell->lcar(x);
		if (y != nil && !cell->isnil(y)) {
			(r, nil) := eval(y, env);
			pick z := r {
			Channel =>
				ca[i++] = z.ch;
			}
		}
		x = cell->lcdr(x);
	}
	(idx, val) := <- ca;
	ic := ref Cell.Number(big idx, big 1, real idx, cell->Integer|cell->Exact);
	return (0, cell->lcons(ic, cell->lcons(val, ref Cell.Link(nil))));
}

lchannel(args: ref Cell, nil: list of ref Env): (int, ref Cell)
{
	if(args == nil || cell->isnil(args)) {
		c := chan of ref Cell;
		return (0, ref Cell.Channel(c));
	}	
	x := cell->lcar(args);
	if(x == nil || cell->isnil(x)) {
		c := chan of ref Cell;
		return (0, ref Cell.Channel(c));
	}
	pick y := x {
	Number =>
		c := chan [int y.i] of ref Cell;
		return (0, ref Cell.Channel(c));
	}
	return (0, ref Cell.Link(nil));
}

closeinoutport(args: ref Cell, env: list of ref Env): (int, ref Cell)
{
	return closeinport(args, env);
}

openinoutfile(args: ref Cell, nil: list of ref Env): (int, ref Cell)
{
	x := cell->lcar(args);
	if(x == nil) {
		cell->error("wrong number of arguments to open-input-file\n");
		return (0, nil);
	}
	pick y := x {
	String =>
		b := bufio->open(y.str, Bufio->ORDWR);
		if(b == nil) {
			cell->error(sys->sprint("Cannot open %s: %r\n", y.str));
			return (0, nil);
		}
		return (0, ref Cell.Port(b, Bufio->ORDWR));
	* =>
		cell->error("non-string argument to open-input-file\n");
	}
	return (0, nil);
}

openinstr(args: ref Cell, nil: list of ref Env): (int, ref Cell)
{
	x := cell->lcar(args);
	if(x == nil) {
		cell->error("wrong number of arguments to open-input-file\n");
		return (0, nil);
	}
	pick y := x {
	String =>
		b := bufio->sopen(y.str);
		if(b == nil) {
			cell->error(sys->sprint("Cannot open %s: %r\n", y.str));
			return (0, nil);
		}
		return (0, ref Cell.Port(b, Bufio->OREAD));
	* =>
		cell->error("non-string argument to open-input-string\n");
	}
	return (0, nil);
}

popen(args: ref Cell, nil: list of ref Env): (int, ref Cell)
{
	cmd: string;
	r: ref Cell;

	r = nil;
	x := cell->lcar(args);
	if(x == nil) {
		cell->error("wrong number of arguments to popen\n");
		return (0, nil);
	}
	pick name := x {
	String =>
		cmd = name.str;
	* =>
		cell->error("non-string argument to popen\n");
		return (0, nil);
	}
	infds := array[2] of ref Sys->FD;
	outfds := array[2] of ref Sys->FD;
	sys->pipe(infds);
	sys->pipe(outfds);
	spawn startshell(cmd, outfds[0], infds[1]);
	outfds[0] = nil;
	infds[1] = nil;
	rb := bufio->fopen(infds[0], Bufio->OREAD);
	tb := bufio->fopen(outfds[1], Bufio->OWRITE);
	rc := ref Cell.Port(rb, Bufio->OREAD);
	tc := ref Cell.Port(tb, Bufio->OWRITE);
	return (0, cell->lcons(rc, cell->lcons(tc, ref Cell.Link(nil))));
}

quit(nil: ref Cell, nil: list of ref Env): (int, ref Cell)
{
	exit;
}

lrecv(args: ref Cell, nil: list of ref Env): (int, ref Cell)
{
	x := cell->lcar(args);
	pick y := x {
	Channel =>
		r := <- y.ch;
		return (0, r);
	}
	cell->error("recv must have a channel argument\n");
	return (0, nil);
}

lsend(args: ref Cell, nil: list of ref Env): (int, ref Cell)
{
	x := cell->lcar(args);
	cdrarg := cell->lcdr(args);
	if (cdrarg == nil || cell->isnil(cdrarg)) {
		cell->error("wrong number of arguments in lsend\n");
		return (0, nil);
	}
	y := cell->lcar(cell->lcdr(args));
	pick z := y {
	Channel =>
		z.ch <- = x;
		return (0, x);
	}
	cell->error("send must have a channel argument\n");
	return (0, nil);
}
	

lsleep(args: ref Cell, nil: list of ref Env): (int, ref Cell)
{
	x := cell->lcar(args);
	if (x == nil) {
		cell->error("wrong number of argument in sleep\n");
		return (0, nil);
	}
	pick y := x {
	Number =>
		sys->sleep(int y.i);
	}
	return (0, nil);
}

startshell(cmd: string, infd: ref Sys->FD, outfd: ref Sys->FD)
{
	sh := load Sh Sh->PATH;
	if(sh == nil) {
		sys->print("loading sh failed: %r\n");
		exit;
	}
	sys->pctl(Sys->NEWFD, 2 :: infd.fd :: outfd.fd :: nil);
	sys->dup(infd.fd, 0);
	sys->dup(outfd.fd, 1);
	infd = nil;
	outfd = nil;
	sh->init(ctxt, "sh" :: "-c" :: cmd :: nil);
	cell->error(sys->sprint("child shell returned: %r\n"));
}

seval(args: ref Cell, env: list of ref Env)
{
	if (args == nil || cell->isnil(args)) {
		cell->error("Empty spawn");
		exit;
	}
	eval(cell->lcar(args), env);
	exit;
}

lspawn(args: ref Cell, env: list of ref Env): (int, ref Cell)
{
	spawn seval(args, env);
	return (0, ref Cell.Link(nil));
}
