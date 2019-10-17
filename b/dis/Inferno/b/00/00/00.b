#!/! /b/dis/Inferno/b/00/00/00.b

## head #
implement B;
include "sys.m"    ; sys: Sys;
include "draw.m"   ;
include "string.m" ; str: String;
include "arg.m"    ; arg: Arg;
## globals #

Xargv : list of string ;

## module #

B : module {

    init : fn (nil: ref Draw->Context, argv: list of string) ;
    eval : fn (nil: ref Draw->Context, argv: list of string) ;
    walk : fn (nil: ref Draw->Context, argv: list of string) ;
    quit : fn (nil: ref Draw->Context, argv: list of string) ;

    oops : fn (nil: ref Draw->Context, argv: list of string) ;

	argv : list of string ;

} ; init (nil: ref Draw->Context, argv: list of string) {

	sys=load Sys Sys->PATH ;
	str=load String String->PATH ; if(str==nil) 
		fail (sys->sprint("cannot load %s: %r", String->PATH));
	arg=load Arg Arg->PATH ; if(arg==nil) 
		fail (sys->sprint("cannot load %s: %r", Arg->PATH));

	arg->init(argv);
	arg->setusage ("os [-d dir] [-m mount] [-n] [-N nice] [-b] command [arg...]");

	nice:=0; nicearg:string; workdir:=""; mntpoint:=""; foreground:=1;

	# while ((opt:=arg->opt())!=0) case opt {
	# 	'd' => workdir = arg->earg();
	# 	'm' => mntpoint = arg->earg();
	# 	'n' => nice=1;
	# 	'N' => nice=1; nicearg=sys->sprint(" %q", arg->earg());
	# 	'b' => foreground=0;
	# 	 *  => arg->usage();
	# }

	sys->fprint(sys->fildes(2), "#!/! %d\n", len argv);

	Xargv=argv; it:string; i:=0; while (Xargv!=nil){
		it=hd Xargv; sys->fprint(sys->fildes(2), "%64s\n",it); Xargv=tl Xargv ;i++;
	}; Xargv=argv;

	argv=arg->argv(); if(argv==nil) return quit (nil,argv); arg=nil;

	sys->pctl(Sys->FORKNS, nil);
	sys->bind("#p", "/prog", Sys->MREPL);		# don't worry if it fails
	sys->bind("#p", "/b/00/p", Sys->MREPL);		# don't worry if it fails
	sys->bind("/cmd","/b/00/z", Sys->MREPL); # FIXME: should be from kernel device

	if (mntpoint==nil) { mntpoint="/b/00/z";
		if (sys->stat(mntpoint+"/clone").t0 == -1)
		if (sys->bind("#C","/",Sys->MBEFORE)<0) fail (sys->sprint("bind #C /: %r"));
	}

	cfd := sys->open(mntpoint+"/clone", sys->ORDWR); if (cfd==nil) 
		fail (sys->sprint("cannot open /cmd/clone: %r"));
	buf := array [32] of byte ; if((n:=sys->read(cfd,buf,len buf))<=0)
		fail (sys->sprint("cannot read /cmd/clone: %r"));

	dir := mntpoint +"/"+ string buf [0:n] ;

	wfd := sys->open(dir+"/wait", Sys->OREAD);

	if (nice && sys->fprint(cfd,"nice%s",nicearg) < 0)
		sys->fprint(sys->fildes(2), "warn: cannot set nice priority: %r\n");
	if (workdir != nil && sys->fprint(cfd,"dir %s",workdir) < 0)
		fail(sys->sprint("warn: cannot set cwd %q: %r", workdir));
	if (foreground && sys->fprint(cfd, "killonclose") < 0)
		sys->fprint(sys->fildes(2), "warn: cannot write killonclose: %r\n");

	if (sys->fprint(cfd, "exec %s", str->quoted(argv)) < 0)
		fail (sys->sprint("cannot exec: %r"));
	if (foreground) {
		if ((tocmd := sys->open(dir+"/data", sys->OWRITE)) == nil)
			fail(sys->sprint("canot open/writing %s/data : %r", dir));
		if ((fromcmd := sys->open(dir+"/data", sys->OREAD)) == nil)
			fail(sys->sprint("cannot open/reading %s/data : %r", dir));
		if ((errcmd := sys->open(dir+"/stderr", sys->OREAD)) == nil)
			fail(sys->sprint("cannot open/reading %s/stderr : %r", dir));

		spawn copy (sync:=chan of int, nil, sys->fildes(0), tocmd); pid := <-sync; tocmd=nil;
		spawn copy (sync,nil,errcmd,sys->fildes(2)); epid := <-sync; sync=nil; errcmd=nil;
		spawn copy (nil, done:=chan of int, fromcmd, sys->fildes(1)); fromcmd=nil;

		# cfd is still open, so if we're killgrp'ed and we're on a platform
		# (e.g. windows) where the fromcmd read is uninterruptible,
		# cfd will be closed, so the command will be killed (due to killonclose), and
		# the fromcmd read should complete, allowing that process to be killed.

	<-done; kill(pid); kill(epid); }

	if (wfd!=nil) {
		status := array [1024] of byte;
		n=sys->read (wfd, status, len status);
		if (n<0) fail(sys->sprint("wait error: %r"));
		s := string status [0:n] ; if (s!=nil) {
			# pid user sys real status
			flds := str->unquoted(s);
			if (len flds < 5) fail(sys->sprint("wait error: odd status: %q", s));
			s = hd tl tl tl tl flds;
			if (0) sys->fprint(sys->fildes(2), "WAIT: %q\n", s);
			if (s!=nil) raise "fail:host: "+s;
		}
	}
} eval (nil: ref Draw->Context, argv: list of string) { 
} walk (nil: ref Draw->Context, argv: list of string) {
} quit (nil: ref Draw->Context, argv: list of string) {

	it:string; i:=0; while (argv!=nil) {
		it=hd argv; sys->fprint(sys->fildes(1), "%64s\n",it); argv=tl argv ;i++;
	}
	sys->fprint(sys->fildes(2),"quit\n"); return;

} oops (nil: ref Draw->Context, argv: list of string) { 

	sys->fprint(sys->fildes(2),"oops :\n"); 
	it:string; i:=0; while (argv!=nil) {
		it=hd argv; sys->fprint(sys->fildes(1), "%64s\n",it); argv=tl argv ;i++;
	}
	raise "fail:oops:";
}

## methods #

kill (pid: int)
	{ fd:=sys->open("#p/"+string pid+"/ctl",sys->OWRITE); sys->fprint(fd,"kill"); }
fail (msg: string)
	{ oops(nil, msg :: tl Xargv); }

copy (sync, done: chan of int, f, t: ref Sys->FD)
{
	if (sync!=nil) sync <-= sys->pctl (0,nil);
	buf := array [8192] of byte; for(;;){
	r := sys->read (f, buf, len buf) ; if (r<=0) break;
	w := sys->write (t, buf, r)      ; if (w!=r) break;
	}
	if (done!=nil) done <-= 1;
}

