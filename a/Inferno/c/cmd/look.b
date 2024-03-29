#
#	initially generated by c2l
# 	subject to the Lucent Public License 1.02
#

implement Look;

include "draw.m";

Look: module
{
	init: fn(nil: ref Draw->Context, argl: list of string);
};

include "sys.m";
	sys: Sys;
include "libc.m";
	libc: Libc;
	isdigit, isspace, isupper, isalnum, tolower: import libc;
include "math.m";
	math: Math;
include "bufio.m";
	bufio: Bufio;
	Iobuf: import bufio;
include "arg.m";
	arg: Arg;

filename := "/lib/words";
dfile: ref Iobuf;
bout, bin: ref Iobuf;
debug: int = 0;
fold, direc, exact, iflag, range: int;
rev: int = 1;	# -1 for reverse-ordered file, not implemented
tab:= '\t';
cmptyp: int;
entry : string;
word : string;
key : string;
orig : string;
targ : string;
latin_fold_tab := array[64] of {
	# 	Table to fold latin 1 characters to ASCII equivalents
	# 	based at Rune value 0xc0
	# 
	#	 À    Á    Â    Ã    Ä    Å    Æ    Ç
	#	 È    É    Ê    Ë    Ì    Í    Î    Ï
	#	 Ð    Ñ    Ò    Ó    Ô    Õ    Ö    ×
	#	 Ø    Ù    Ú    Û    Ü    Ý    Þ    ß
	#	 à    á    â    ã    ä    å    æ    ç
	#	 è    é    ê    ë    ì    í    î    ï
	#	 ð    ñ    ò    ó    ô    õ    ö    ÷
	#	 ø    ù    ú    û    ü    ý    þ    ÿ
	# 
	'a',	'a',	'a',	'a',	'a',	'a',	'a',	'c',	
	'e',	'e',	'e',	'e',	'i',	'i',	'i',	'i',
	'd',	'n',	'o',	'o',	'o',	'o',	'o',	0,
	'o',	'u',	'u',	'u',	'u',	'y',	0,	0,
	'a',	'a',	'a',	'a',	'a',	'a',	'a',	'c',
	'e',	'e',	'e',	'e',	'i',	'i',	'i',	'i',
	'd',	'n',	'o',	'o',	'o',	'o',	'o',	0,
	'o',	'u',	'u',	'u',	'u',	'y',	0,	'y',
};

init(nil: ref Draw->Context, argl: list of string)
{
	sys = load Sys Sys->PATH;
	libc = load Libc Libc->PATH;
	math = load Math Math->PATH;
	bufio = load Bufio Bufio->PATH;
	arg = load Arg Arg->PATH;

	n: int;

	bin = bufio->fopen(sys->fildes(0), Sys->OREAD); 
	bout = bufio->fopen(sys->fildes(1), Sys->OWRITE);
	cmptyp = 0;
	arg->init(argl);
	while((c := arg->opt()) != 0)
		case c {
		'd' =>
			direc++;
		'f' =>
			fold++;
		'i' =>
			iflag++;
		'n' =>
			cmptyp = 1;
		't' =>
			tab = (arg->earg())[0];
		'x' =>
			exact++;
		'r' =>
			range++;
			orig = arg->earg();
			targ = rcanon(orig);
		* =>
			sys->fprint(sys->fildes(2), "%s: bad option %c\n", arg->progname(), c);
			sys->fprint(sys->fildes(2), "usage: %s -[dfinx] [-t c] [string] [file]\n", arg->progname());
			exit;
		}
	argl = arg->argv();
	if(!iflag)
		if(len argl >= 1){
			orig = hd argl;
			argl = tl argl;
			key = rcanon(orig);
		}else
			iflag++;
	if(len argl < 1){
		direc++;
		fold++;
	}else
		filename = hd argl;
	if(debug)
		sys->fprint(sys->fildes(2), "orig %s key %s %s\n", orig, key, filename);
	dfile = bufio->open(filename, Sys->OREAD);
	if(dfile == nil){
		sys->fprint(sys->fildes(2), "look: can't open %s\n", filename);
		exit;
	}
	if(!iflag) 
		if(!locate() && !range && exact)
			exit;
	do{
		if(iflag){
			bout.flush();
			if((orig = bin.gets('\n')) == nil)
				exit;
			key = rcanon(orig);
			if(!locate())
				continue;
		}
		if(range){
			if(compare(key, word) <= 0 && compare(word, targ) <= 0)
				bout.puts(entry);
		}else if(!exact || !compare(word, key))
			bout.puts(entry);
		while((entry = dfile.gets('\n')) != nil){
			word = rcanon(entry);
			if(range)
				n = compare(word, targ);
			else
				n = compare(key, word);
			if(debug)
				sys->print("compare %d\n", n);
			case(n){
			-2 =>
				if(range){
					bout.puts(entry);
					continue;
				}
			-1 =>
				if(exact)
					break;
				if(!exact || !compare(word, key))
					bout.puts(entry);
				continue;
			0 =>
				if(!exact || !compare(word, key))
					bout.puts(entry);
				continue;
			}
			break;
		}
	}while(iflag);
	bout.flush();
	exit;
}

locate(): int
{
	top, bot, mid: big;
	c: int;
	n: int;

	bot = big 0;
	top = dfile.seek(big 0, 2);
	for(;;){
		mid = (top+bot)/ big 2;
		if(debug)
			sys->fprint(sys->fildes(2), "locate %bd %bd %bd\n", top, mid, bot);
		dfile.seek(mid, 0);
		do
			c = dfile.getc();
		while(c >= 0 && c != '\n');
		mid = dfile.offset();
		if((entry = dfile.gets('\n')) == nil)
			break;
		word = rcanon(entry);
		if(debug)
			sys->fprint(sys->fildes(2), "mid %bd key: %s entry: %s\n", mid, key, word);
		n = compare(key, word);
		if(debug)
			sys->fprint(sys->fildes(2), "compare: %d\n", n);
		case(n){
		-2 or -1 or 0 =>
			if(top <= mid)
				break;
			top = mid;
			continue;
		1 or 2 =>
			bot = mid;
			continue;
		}
		break;
	}
	if(debug)
		sys->fprint(sys->fildes(2), "locate %bd %bd %bd\n", top, mid, bot);
	bot = dfile.seek(bot, 0);
	while((entry = dfile.gets('\n')) != nil){
		word = rcanon(entry);
		if(debug)
			sys->fprint(sys->fildes(2), "seekbot %bd key: %s entry: %s\n", bot, key, word);
		n = compare(key, word);
		if(debug)
			sys->fprint(sys->fildes(2), "compare: %d\n", n);
		case(n){
		-2 =>
			return 0;
		-1 =>
			if(exact)
				return 0;
			return 1;
		0 =>
			return 1;
		1 or 2 =>
			continue;
		}
	}
	return 0;
}

compare(s, t: string): int
{
	if(cmptyp)
		return ncomp(s, t);
	else
		return acomp(s, t);
}

# 
#  *	acomp(s, t) returns:
#  *		-2 if s strictly precedes t
#  *		-1 if s is a prefix of t
#  *		0 if s is the same as t
#  *		1 if t is a prefix of s
#  *		2 if t strictly precedes s
#  
acomp(s, t: string): int
{
	cs, ct, l: int;

	if(len s > len t)
		l = len t;
	else
		l = len s;
	if(s == t)
		return 0;
	for(i := 0; i < l; i++) {
		cs = int s[i];
		ct = int  t[i];
		if(cs != ct)
			break;
	}
	if(i == len s)
		return -1;
	if(i == len t)
		return 1;
	if(cs < ct)
		return -2;
	return 2;
}

rcanon(old: string): string
{
	r: int;

	if(old[len old - 1] == '\n')
		old = old[0: len old - 1];
	new := old;
	for(i := 0; i < len old; i++){
		r = old[i];
		if(r == tab){
			new = new[:i];
			break;
		}
		if(16rc0 <= r && r <= 16rff && latin_fold_tab[r-16rc0])
			r = latin_fold_tab[r-16rc0];
		if(direc)
			if(!(isalnum(r) || r == ' ' || r == '\t'))
				continue;
		if(fold)
			if(isupper(r))
				r = tolower(r);
		new[i] = r;
	}
	return new;
}

sgn(v: int): int
{
	n : int;
	if(v < 0)
		n = -1;
	else if(v > 0)
		n = 1;
	else
		n = 0;
	return n;
}

ncomp(s: string, t: string): int
{
	is, it, js, jt: string;
	a, b, ssgn, tsgn: int;
	i,j: int;

	while(len s > 0 && isspace(s[0]))
		s = s[1: ];
	while(len t > 0 && isspace(t[0]))
		t = t[1: ];
	ssgn = tsgn = -2*rev;
	if(len s > 0 && s[0] == '-'){
		s = s[1: ];
		ssgn = -ssgn;
	}
	if(len t > 0 && t[0] == '-'){
		t = t[1: ];
		tsgn = -tsgn;
	}
	for(i = 0; i < len s && isdigit(s[i]); i++)
		;
	is = s[0:i];
	js = s[i:];
	for(i = 0; i < len t && isdigit(t[i]); i++)
		;
	it = t[0:i];
	jt = t[i:];
	a = 0;
	i = len is;
	j = len it;
	if(ssgn == tsgn){
		while(j > 0 && i > 0)
			if(b = it[--j] - is[--i])
				a = b;
	}
	while(i > 0)
		if(is[--i] != '0')
			return -ssgn;
	while(j > 0)
		if(it[--j] != '0')
			return tsgn;
	if(a)
		return sgn(a)*ssgn;
	s = js;
	if(len s > 0 && s[0] == '.')
		s = s[1: ];
	t = jt;
	if(len t > 0 && t[0] == '.')
		t = t[1: ];
	if(ssgn == tsgn)
		while((len s > 0 && isdigit(s[0])) && (len t > 0 && isdigit(t[0]))){
			if(a = t[0] - s[0])
				return sgn(a)*ssgn;
			s = s[1:];
			t = t[1:];
		}
	for(; len s > 0 && isdigit(s[0]); s = s[1:])
		if(s[0] != '0')
			return -ssgn;
	for(; len t > 0 && isdigit(t[0]); t = t[1:])
		if(t[0] != '0')
			return tsgn;
	return 0;
}
