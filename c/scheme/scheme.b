#
# TODO:
# Sec 6.4 control features
# proper tail recursion
# define-syntax
#
implement Scheme;

include "sys.m";
sys: Sys;

include "bufio.m";
bufio: Bufio;
Iobuf: import bufio;

include "string.m";
str: String;

include "math.m";
math: Math;

include "draw.m";

include "cell.m";
cell: SCell;
Cell: import cell;
Pair: import cell;
Env: import cell;

include "sform.m";
sform: SForm;

include "builtin.m";
builtin: BuiltIn;

include "extension.m";
extension: Extension;

include "scheme.m";

stdin, stdout: ref Iobuf = nil;

ctxt: ref Draw->Context;

init(drawctxt: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;
	bufio = load Bufio Bufio->PATH;
	str = load String String->PATH;
	math = load Math Math->PATH;
	cell = load SCell SCell->PATH;
	sform = load SForm SForm->PATH;
	builtin = load BuiltIn BuiltIn->PATH;
	extension = load Extension Extension->PATH;

	ctxt = drawctxt;
	stdin = bufio->fopen(sys->fildes(0), Bufio->OREAD);
	stdout = bufio->fopen(sys->fildes(1), Bufio->OWRITE);

	cell->init(sys);

	e := ref Env("nil", cell->Variable, ref Cell.Link(nil), nil) :: nil;
	e = ref Env("else", cell->Variable, ref Cell.Boolean(1), nil) :: nil;


	cell->globalenv = e;
	sform->init(sys, load Scheme SELF, cell);

	e = cell->globalenv;
	cell->nullenvironment = e;

	builtin->init(sys, load Scheme SELF, cell, math, str,
		 bufio, stdin, stdout);

	e = cell->globalenv;
	b := bufio->open("/lib/scheme/library.scm", Bufio->OREAD);
	if(b == nil) {
		cell->error("Can't open library code\n");
	}
	else {
		while(1) {
			c := readcell(b, e);
			if(c == nil)
				break;
			(nil, e) = eval(c, e);
		}
	}
	b = nil;
	cell->reportenv = e;
	cell->globalenv = e;
	extension->init(ctxt, sys, load Scheme SELF, cell, builtin, math, str, bufio);

	e = cell->globalenv;

	while(1) {
		sform->resetbody();
		sys->print("> ");
		c := readcell(stdin, cell->globalenv);
		(r, ge) := eval(c, cell->globalenv);
		cell->globalenv = ge;
		printcell(r, stdout, 0); stdout.flush(); sys->print("\n");
	}
}

readcell(b: ref Iobuf, env: list of ref Env): ref Cell
{
	c: int;

	while(1) {
		do {
			c = b.getc();
		} while(c != Bufio->EOF && str->in(c, " \t\n"));
		case c {
		Bufio->EOF or ')' =>
			return nil;
		';' =>
			do {
				c = b.getc();
			} while(c != Bufio->EOF && c != '\n');
		'(' =>
			return readlist(b, env);
		'"' =>
			return readstring(b);
		'\'' =>
			return cell->lcons(ref Cell.Symbol("quote",
				cell->lookupsym("quote", cell->globalenv)),
				cell->lcons(readcell(b, env), ref Cell.Link(nil)));
		'`' =>
			return cell->lcons(ref Cell.Symbol("quasiquote",
				cell->lookupsym("quasiquote", cell->globalenv)),
				cell->lcons(readcell(b, env), ref Cell.Link(nil)));
		',' =>
			c = b.getc();
			if(c == '@')
				return cell->lcons(ref Cell.Symbol("unquote-splicing",
					cell->lookupsym("unquote-splicing", cell->globalenv)),
					cell->lcons(readcell(b, env), ref Cell.Link(nil)));
			else {
				b.ungetc();
				return cell->lcons(ref Cell.Symbol("unquote",
					cell->lookupsym("unquote", cell->globalenv)),
					cell->lcons(readcell(b, env), ref Cell.Link(nil)));
			}
		'+' or '-' or '.' or '0' to '9' =>
			b.ungetc();
			return readnumber(b, 0);
		'#' =>
			c = b.getc();
			case c {
			'b' or 'B' or 'o' or 'O' or 'd' or 'D' or 'x' or 'X' 
			or 'e' or 'E' or 'i' or 'I' =>
				b.ungetc();
				return readnumber(b, '#');
			'f' =>
				return ref Cell.Boolean(0);
			't' =>
				return ref Cell.Boolean(1);
			'\\' =>
				return readchar(b);
			'(' =>
				return readvector(b, env);
			}
		* =>
			b.ungetc();
			return readsymbol(b, env);
		}
	}
	return nil;
}

readchar(b: ref Iobuf): ref Cell
{
	lexeme: string;

	lexeme[0] = b.getc();
	i := 1;
	do {
		lexeme[i] = b.getc();
	} while(!str->in(lexeme[i++], " \t\n\r();"));
	b.ungetc();
	lexeme = lexeme[:len lexeme -1];
	if(len lexeme == 1)
		return ref Cell.Char(lexeme[0]);
	case str->tolower(lexeme) {
	"space" =>
		return ref Cell.Char(' ');
	"newline" =>
		return ref Cell.Char('\n');
	"return" =>
		return ref Cell.Char('\r');
	"tab" =>
		return ref Cell.Char('\t');
	"backspace" =>
		return ref Cell.Char('\b');
	"bell" or "alert" =>
		return ref Cell.Char('\a');
	"quote" =>
		return ref Cell.Char('\'');
	"doublequote" =>
		return ref Cell.Char('\"');
	"null" =>
		return ref Cell.Char(0);
	"backslash" =>
		return ref Cell.Char('\\');
	"escape" =>
		return ref Cell.Char(16r1b);
	"formfeed" =>
		return ref Cell.Char('\v');
	}
	return nil;
}

readvector(b: ref Iobuf, env: list of ref Env): ref Cell
{
	l: list of ref Cell;

	l = nil;
	while(1) {
		c := readcell(b, env);
		if(c == nil)
			break;
		l = c :: l;
	}
	v := array [len l] of ref Cell;
	for(i := len l - 1; i >= 0; --i) {
		v[i] = hd l;
		l = tl l;
	}
	return ref Cell.Vector(v);
}

readlist(b: ref Iobuf, env: list of ref Env): ref Cell
{
	c := readcell(b, env);
	if(c == nil)
		return ref Cell.Link(nil);
	pick x := c {
	Symbol =>
		if(x.sym == ".") {
			cdr := readcell(b, env);
			if(readcell(b, env) != nil) {
				cell->error("malformed improper list\n");
				return nil;
			}
			return cdr;
		}
	}
	p := Pair(nil, nil);
	p.car = c;
	p.cdr = readlist(b, env);
	return ref Cell.Link(ref p);
}

readstring(b: ref Iobuf): ref Cell
{
	x: string;

	i := 0;
	esc := 0;
loop:
	while(1) {
		x[i] = b.getc();
		case x[i] {
		'"' =>
			if(!esc)
				break loop;
			else {
				++i;
				esc = 0;
			}
		'\\' =>
			if(esc) {
				++i;
				esc = 0;
			}
			else
				esc = 1;
		't' =>
			if(esc) {
				x[i] = '\t';
				esc = 0;
			}
			++i;
		'n' =>
			if(esc) {
				x[i] = '\n';
				esc = 0;
			}
			++i;
		'r' =>
			if(esc) {
				x[i] = '\r';
				esc = 0;
			}
			++i;
		'b' =>
			if(esc) {
				x[i] = '\b';
				esc = 0;
			}
			++i;
		'a' =>
			if(esc) {
				x[i] = '\a';
				esc = 0;
			}
			++i;
		'v' =>
			if(esc) {
				x[i] = '\v';
				esc = 0;
			}
			++i;
		* =>
			esc = 0;
			++i;
		}

	}
	return ref Cell.String(x[:i]);
}

readnumber(b: ref Iobuf, seed: int): ref Cell
{
	s: string;

	# Get the string
	i := -1;
	if(seed != 0)
		s[++i] = seed;
	do {
		++i;
		s[i] = b.getc();
	} while(str->in(s[i], "-+.#oOxXiIsSlL0-9A-Fa-f/"));
	b.ungetc();
	if(s[:i] == ".")
		return ref Cell.Symbol(".", nil);
	else if(s[:i] == "+")
		return ref Cell.Symbol("+", cell->lookupsym("+", cell->globalenv));
	else if(s[:i] == "-")
		return ref Cell.Symbol("-", cell->lookupsym("-", cell->globalenv));
	--i;
	return scannum(s, 10);
}

readsymbol(b: ref Iobuf, env: list of ref Env): ref Cell
{
	x: string;

	i := 0;
	do {
		x[i] = b.getc();
	} while(!str->in(x[i++], " \t\n()"));
	b.ungetc();
	e := cell->lookupsym(x[:i-1], env);
#	if(e != nil && (e.ilk == cell->SpecialForm || e.ilk == cell->BuiltIn))
#		return ref Cell.Internal(x[:i-1], e);
	return ref Cell.Symbol(x[:i-1], e);
}

bugger: int;

eval(c: ref Cell, env: list of ref Env): (ref Cell, list of ref Env)
{
	z: ref Cell;
	r: ref Cell;
	lenv: list of ref Env;

	tailcont := 0;
	lenv = env;
	do {
		if(c == nil || cell->isnil(c))
			return (c, env);
#printcell(c, stdout, 0);
		pick c2 := c {
		Continuation =>
			lenv = c2.env;
			c = c2.exp;
		}
		pick x := c {
		Link =>
			if(x.next == nil)
				return (ref Cell.Link(nil), lenv);
			(r, lenv) = eval(x.next.car, lenv);
			if(r == nil) {
				cell->error("Undefined operation: ");
				printcell(x.next.car, stdout, 0);
				stdout.putc('\n');
				return (nil, lenv);
			}
			pick y := r {
			Internal =>
				e := cell->lookupsym(y.sym, lenv);
				if(e == nil)
					return (nil, lenv);
				case e.ilk {
				cell->BuiltIn =>
					l := evallist(x.next.cdr, lenv);
					(tailcont, z) = e.handler(l, lenv);
					if(tailcont == 2)
						return (z, lenv);
					else if(tailcont == 0) {
						if(z == nil || cell->isnil(z))
							return (z, lenv);
						pick v := z {
						Environment =>
							return (ref Cell.Link(nil), v.env);
						* =>
							return (z, lenv);
						}
					}
					else
						c = z;
				cell->SpecialForm =>
					(tailcont, z) = e.handler(x.next.cdr, lenv);
					if(tailcont == 0) {
						if(z == nil || cell->isnil(z))
							return (z, lenv);
						pick v := z {
						Environment =>
							return (ref Cell.Link(nil), v.env);
						* =>
							return (z, lenv);
						}
					}
					else
						c = z;
				}
			Symbol =>
				e := cell->lookupsym(y.sym, lenv);
				if(e == nil)
					return (nil, lenv);
				case e.ilk {
				cell->Variable =>
					# return eval(e.val);
					c = e.val;
					tailcont = 1;
				}
			Lambda =>
				l := evallist(x.next.cdr, lenv);
				lenv = cell->listappend(y.env, lenv);
				p := y.formals;
				q := l;
				dorest := 0;
				while(p != nil && q != nil) {
					fname := "";
					pick fp := p {
					Link =>
						if(fp.next != nil && fp.next.car != nil) {
							pick ffp := fp.next.car {
							Symbol =>
								fname = ffp.sym;
							* =>
								cell->error("non-symbol in formals\n");
								return (nil, lenv);
							}
							p = fp.next.cdr;
						}
						else
							p = nil;
					Symbol =>
						fname = fp.sym;
						dorest = 1;
						p = nil;
					* =>
						p = nil;
					}
					pick vp := q {
					Link =>
						if(vp.next != nil) {
							if(dorest) {
								(nil, lenv) = cell->ldefine(fname, vp, lenv);
								q = nil;
							}
							else {
								(nil, lenv) = cell->ldefine(fname, vp.next.car, lenv);
								q = vp.next.cdr;
							}
						}
						else {
							if(dorest) {
								(nil, lenv) = cell->ldefine(fname,
									 ref Cell.Link(nil), lenv);
							}
							q = nil;
						}
					* =>
						q = nil;
					}
				}
				if(p != nil || q != nil) {
					cell->error("wrong number of arguments for lambda\n");
					return (nil, lenv);
				}
				exp := y.exp_list;
				r: ref Cell;
				r = ref Cell.Link(nil);
				sform->startbody();
				while(exp != nil) {
					pick ep := exp {
					Link =>
						if(ep.next != nil) {
							(r, lenv) = eval(ep.next.car, lenv);
							if(r == nil) {
								return (nil, lenv);
							}
							exp = ep.next.cdr;
						}
						else
							exp = nil;
					* =>
						cell->error("malformed expression list\n");
						sform->resetbody();
						return (nil, lenv);
					}
				}
				sform->resetbody();
				return (r, env);
			* =>
				cell->error("non-lambda and non-symbol in eval\n");
				return (nil, lenv);
			}
		Symbol =>
	#		if(x.env == nil)
				s := cell->lookupsym(x.sym, lenv);
	#		else
	#			s = x.env;
			if(s == nil)
				return (nil, lenv);
			else
				return (s.val, lenv);
		* =>
			return (c, lenv);
		}
	} while (tailcont != 0) ;
	return (nil, lenv);
}

evallist(c: ref Cell, env: list of ref Env): ref Cell
{
	if(c == nil || cell->isnil(c))
		return c;
	pick x := c {
	Link =>
		if(x.next == nil)
			return ref Cell.Link(nil);
		(vc, e) := eval(x.next.car, env);
		if(vc == nil)
			return nil;
		vl := evallist(x.next.cdr, e);
		if(vl == nil)
			return nil;
		y := Pair(vc, vl);
		return ref Cell.Link(ref y);
	* =>
		cell->error("non-list in evallist\n");
	}
	return nil;
}

printlist(plist: ref Pair, b: ref Iobuf, disp: int)
{
	x: ref Pair;

	b.puts("(");
	x = plist;
	while(x != nil) {
		printcell(x.car, b, disp);
		y := x.cdr;
		if(y == nil) {
			cell->error("unexpected end of list\n");
			break;
		}
		pick z := y {
		Link =>
			x = z.next;
			if(x != nil)
				b.puts(" ");
		* =>
			b.puts(" . ");
			printcell(z, b, disp);
			x = nil;
		}
	}
	b.puts(")");
	b.flush();
}

printvector(v: array of ref Cell, b: ref Iobuf, disp: int)
{
	b.puts("#(");
	if(len v == 0) {
		b.puts(")");
		b.flush();
		return;
	}
	i := 0;
	while(1) {
		printcell(v[i], b, disp);
		if(++i >= len v)
			break;
		b.putc(' ');
	}
	b.puts(")");
	b.flush();
}

printcell(x: ref Cell, b: ref Iobuf, disp: int)
{
	if(x == nil) {
		b.puts("nil");
		b.flush();
		return;
	}
	pick y := x {
	Boolean =>
		if(y.b == 0)
			b.puts("#f");
		else
			b.puts("#t");
	Symbol =>
		b.puts(sys->sprint("%s", y.sym));
	Internal =>
		b.puts(sys->sprint("%s", y.sym));
	String =>
		if(disp)
			b.puts(y.str);
		else {
			b.putc('"');
			for(i := 0; i < len y.str; ++i)
				if(y.str[i] == '"')
					b.puts("\\\"");
				else
					b.putc(y.str[i]);
			b.putc('"');
		}
	Char =>
		if(disp)
			b.putc(y.c);
		else
		case y.c {
		'\n' =>
			b.puts("#\\newline");
		'\t' =>
			b.puts("#\\tab");
		'\r' =>
			b.puts("#\\return");
		'\b' =>
			b.puts("#\\backspace");
		'\a' =>
			b.puts("#\\bell");
		'\v' =>
			b.puts("#\\formfeed");
		* =>
			b.puts(sys->sprint("#\\%c", y.c));
		}
	Number =>
		case (y.ilk & ~cell->Exact) {
		cell->Integer =>
			if(!(y.ilk & cell->Exact))
				b.puts("#i");
			b.puts(sys->sprint("%bd", y.i));
		cell->Rational =>
			if(!(y.ilk & cell->Exact))
				b.puts("#i");
			b.puts(sys->sprint("%bd/%bd", y.i, y.j));
		* =>
			b.puts(sys->sprint("%.#g", y.r));
		}
	Link =>
		printlist(y.next, b, disp);
	Lambda =>
		b.puts("[lambda expression]");
	Port =>
		case y.dir {
		-1 =>
			b.puts("[closed port]");
		Bufio->OREAD =>
			b.puts("[input port]");
		Bufio->OWRITE =>
			b.puts("[output port]");
		}
	Vector =>
		printvector(y.v, b, disp);
	Environment =>
		b.puts("[environment]");
	Channel =>
		b.puts("[channel]");
	Promise =>
		b.puts("[promise]");
	Continuation =>
		b.puts("[continuation]");
	}
	b.flush();
}

scannum(s: string, radix: int): ref Cell
{
	n2: big;

	exact := cell->Exact;
	ilk := 0;
	sign := big 1;
	l := len s;

	# parse the prefix
	j := 0;
prefixlp:
	for(k := 0; j < l && k < 2; ++k) {
		if(s[j] != '#')
			break;
		++j;
		if (j >= l)
			break;
		case s[j] {
		'b' or 'B' =>
			radix = 2;
		'o' or 'O' =>
			radix = 8;
		'd' or 'D' =>
			radix = 10;
		'x' or 'X' =>
			radix = 16;
		'e' or 'E' =>
			exact = cell->Exact;
		'i' or 'I' =>
			exact = 0;
		* =>
			exact = 0;
			break prefixlp;
		}
		++j;
	}

	for(k = j; k < l; ++k)
		if (s[k] == '#')
			s[k] = '0';

	if(j >= l)
		j = l-1;
	# Get the initial sign
	if(s[j] == '+') {
		sign = big 1;
	}
	else if(s[j] == '-') {
		sign = big -1;
	}

	# Try to classify the number (ugly ad hoc)
	# As a first cut, just integers and reals
	if(radix == 10)
		(s1,s2) := str->splitl(s[j:], ".eEsSfFdDlL");
	else
		s2 = nil;
	if(s2 == nil) {
		(s1, s2) = str->splitl(s[j:], "/");
		(n1, rs) := str->tobig(s1, radix);
		if (rs != nil && (!str->in(rs[0], " \n\r\t\f\v)/") || rs == s1))
			return ref Cell.Boolean(0);
		if(s2 != nil) {
			ilk = cell->Rational;
			(n2, rs) = str->tobig(s2[1:], radix);
			if (rs != nil && (!str->in(rs[0], " \n\r\t\f\v)/") || rs == s1))
				return ref Cell.Boolean(0);
		}
		else {
			ilk = cell->Integer;
			n2 = big 1;
		}
		if (n2 == big 0)
			return ref Cell.Number(big 0, big 1, real 0, cell->Integer|exact);
		if(n2 != big 1)
			(n1, n2) = reduce(n1, n2);
		if(n2 == big 1)
			ilk = cell->Integer;
		else
			ilk = cell->Rational;
		return ref Cell.Number(n1, n2, real n1 / real n2, ilk|exact);
	}
	else {
		for(m := j; m < len s; ++m) {
			case s[m] {
			's' or 'S' or 'f' or 'F' or 'd' or 'D' or 'l' or 'L' =>
				s[m] = 'e';
			}
		}
		if(s[j] != '.')
			(n, rs) := str->toreal(s[j:], 10);
		else if(len s[j:] <= 1 || s[j+1] < '0' || s[j+1] > '9')
			return ref Cell.Boolean(0);
		else
			(n, rs) = str->toreal("0" + s[j:], 10);
		if (rs != nil && (!str->in(rs[0], " \n\r\t\f\v)/") || rs == s1))
			return ref Cell.Boolean(0);
		if(n > real 18446744073709551615)
			return ref Cell.Number(big 0, big 1, n, cell->Real);
		return ref Cell.Number(big n, big 1, n, cell->Real);
	}
}

printenv(env: list of ref Env)
{
	sys->print("\n***Env: ");
	for(p := env; p != nil; p = tl p) {
		sys->print("%s:", (hd p).name);
		printcell((hd p).val, stdout, 0);
		sys->print(" ");
	}
	sys->print("\n");
}

# Basically Euclid's gcd algorithm
reduce(n, m: big): (big,big)
{
	j := n;
	k := m;
	while(1) {
		r := j % k;
		if(r == big 0) {
			x := n / k;
			y := m / k;
			if (y < big 0) {
				x = -x;
				y = -y;
			}
			return (x, y);
		}
		j = k;
		k = r;
	}
}

