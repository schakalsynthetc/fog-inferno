implement Catty;
include "sys.m"; sys: Sys; stderr: ref Sys->FD;
include "bufio.m";
include "draw.m"; draw : Draw;
include "cache.m";
include "contents.m";
include "httpd.m"; Private_info: import Httpd;
include "cgiparse.m"; cgiparse: CgiParse;
include "string.m"; str: String;

Catty: module
{ init: fn(g: ref Private_info, req: Httpd->Request); };

init(g: ref Private_info, req: Httpd->Request) 
{	
	sys=load Sys Sys->PATH; stderr=sys->fildes(2);	
	cgiparse=load CgiParse CgiParse->PATH; if(cgiparse==nil) {
		sys->fprint(sys->fildes(2), "echo: cannot load %s: %r\n", CgiParse->PATH); return;
	}

	str=load String String->PATH; if(str==nil)fail(sys->sprint("cannot load %s: %r", String->PATH));

	send(g, cgiparse->cgiparse(g, req));
}

send(g: ref Private_info, cgidata: ref CgiData ) 
{	
	bufio := g.bufio; Iobuf: import bufio;
	if(cgidata==nil){ g.bout.flush(); return; }
	
	g.bout.puts(cgidata.httphd);
	
	# cgidata.method, cgidata.uri cgidata.search
	# client date version cgidata.remote tmstamp version
	while( cgidata.header != nil ){
		(tag, val) := hd cgidata.header;
		g.bout.puts( tag + " " + val + "\r\n" );
		cgidata.header = tl cgidata.header;
	}

	if (cgidata.form != nil){	
		i:=0; while(cgidata.form!=nil){(tag,val):=hd cgidata.form; cgidata.form=tl cgidata.form; i++; }
	}	
	g.bout.flush();
}

xinit (nil: ref Draw->Context, args: list of string)
{
	nice:=0; nicearg: string; workdir:=""; mntpoint:=""; foreground:=1;

#		case opt {
#		'd' => workdir = arg->earg();
#		'm' => mntpoint = arg->earg();
#		'n' => nice = 1;
#		'N' => nice = 1; nicearg = sys->sprint(" %q", arg->earg());
#		'b' =>foreground = 0;
#		* =>arg->usage();

	sys->pctl(Sys->FORKNS, nil);
	sys->bind("#p", "/prog", Sys->MREPL);		# don't worry if it fails
	if(mntpoint==nil){
		mntpoint = "/cmd";
		if(sys->stat(mntpoint+"/clone").t0 == -1)
		if(sys->bind("#C", "/", Sys->MBEFORE) < 0) fail(sys->sprint("bind #C /: %r"));
	}

	cfd := sys->open(mntpoint+"/clone", sys->ORDWR); if(cfd==nil) 
		fail (sys->sprint("cannot open /cmd/clone: %r"));
	buf := array[32] of byte; n := sys->read(cfd,buf,len buf); if(n<=0)
		fail (sys->sprint("cannot read /cmd/clone: %r"));

	dir := mntpoint + "/" + string buf[0:n];
	wfd := sys->open (dir + "/wait", Sys->OREAD);

	if(nice && sys->fprint(cfd, "nice %s", nicearg) < 0)
		sys->fprint(sys->fildes(2), "os: warning: can't set nice priority: %r\n");
	if(workdir != nil && sys->fprint(cfd, "dir %s", workdir) < 0)
		fail(sys->sprint("cannot set cwd %q: %r", workdir));
	if(foreground && sys->fprint(cfd, "killonclose") < 0)
		sys->fprint(sys->fildes(2), "os: warning: cannot write killonclose: %r\n");
	if(sys->fprint(cfd, "exec %s", str->quoted(args)) < 0)
		fail(sys->sprint("cannot exec: %r"));

	if(foreground){
		if((tocmd := sys->open(dir+"/data", sys->OWRITE)) == nil)
			fail(sys->sprint("canot open %s/data for writing: %r", dir));
		if((fromcmd := sys->open(dir+"/data", sys->OREAD)) == nil)
			fail(sys->sprint("cannot open %s/data for reading: %r", dir));
		if((errcmd := sys->open(dir+"/stderr", sys->OREAD)) == nil)
			fail(sys->sprint("cannot open %s/stderr for reading: %r", dir));

		spawn copy (sync := chan of int, nil, sys->fildes(0), tocmd); pid := <-sync; tocmd=nil;
		spawn copy (sync, nil, errcmd, sys->fildes(2)); epid := <-sync; sync=nil; errcmd=nil;
		spawn copy (nil, done := chan of int, fromcmd, sys->fildes(1)); fromcmd=nil;

		# cfd is still open, so if we're killgrp'ed and we're on a platform
		# (e.g. windows) where the fromcmd read is uninterruptible,
		# cfd will be closed, so the command will be killed (due to killonclose), and
		# the fromcmd read should complete, allowing that process to be killed.

		<-done; kill(pid); kill(epid);
	}

	if (wfd != nil){
		status := array[1024] of byte;
		n = sys->read(wfd, status, len status);
		if (n < 0) fail(sys->sprint("wait error: %r"));
		s := string status[0:n]; if(s!=nil){
			# pid user sys real status
			flds := str->unquoted(s);
			if (len flds < 5) fail(sys->sprint("wait error: odd status: %q", s)) ;
			s = hd tl tl tl tl flds;
			if (0) sys->fprint(sys->fildes(2), "WAIT: %q\n", s) ;
			if (s != nil) raise "fail:host: " + s ;
		}
	}
}

copy(sync, done: chan of int, f, t: ref Sys->FD)
{
	if (sync!=nil) sync <-=sys->pctl(0,nil) ;
	buf := array[8192] of byte; for (;;) {
		r:=sys->read(f,buf,len buf); if(r<=0)break;
		w:=sys->write(t,buf,r); if(w!=r)break;
	}
	if (done!=nil) done <-=1 ;
}

pctlfile(pid: int): string
{ return "#p" + string pid + "/ctl" ; } 

kill(pid: int)
{ fd := sys->open(pctlfile(pid), sys->OWRITE); sys->fprint(fd, "kill"); }

fail(msg: string)
{ sys->fprint(sys->fildes(2), "os: %s\n", msg); raise "fail:"+msg; }

