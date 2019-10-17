Scheme: module
{
	PATH: con "/dis/scheme.dis";

	init: fn(nil: ref Draw->Context, nil: list of string);
	eval: fn(c: ref Cell, env: list of ref Env): (ref Cell, list of ref Env);
	readcell: fn(b: ref Iobuf, env: list of ref Env): ref Cell;
	printcell: fn(x: ref Cell, b: ref Iobuf, disp: int);
	printenv: fn(env: list of ref Env);
	scannum: fn(s: string, radix: int): ref Cell;
	reduce: fn(n, m: big): (big, big);
};

