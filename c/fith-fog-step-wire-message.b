step (status: string, msg: string)
{ 
	Xstatus:=" :oops: /"+pid+"/p : /"+progname+"/"+status+"/t "; 
	sys->fprint(stderr, "%s : %s ;\n", Xstatus, msg); 
	raise Xstatus; 
}

# wire message format ##

: z init,quit,walk,eval,oops t 
: y namespace path wdir leaf file point t 
: x left prog mod func argv t
	: r map t 
	: q closure t 
	: p pgrp pid threadextent timestamp t
		: t ; errstr or informative log message

closure encompasses r,q,p also maybe hijkl and abcdef

