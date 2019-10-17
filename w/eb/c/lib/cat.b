implement Cat;
include "sys.m"; sys: Sys ;
include "draw.m"; draw: Draw ;
include "bufio.m";
include "cache.m";
include "contents.m";
include "httpd.m"; Private_info: import Httpd ;
include "cgiparse.m"; cgiparse: CgiParse ;

stderr: ref Sys->FD;

Cat: module { init: fn (g: ref Private_info, req: Httpd->Request); };

init (g: ref Private_info, req: Httpd->Request) 
{	
	sys = load Sys Sys->PATH;
	cgiparse = load CgiParse CgiParse->PATH;

	stderr = sys->fildes(2);	

	if (cgiparse==nil) {sys->fprint (stderr, "echo: cannot load %s: %r\n",CgiParse->PATH); return;}
	walk (g, cgiparse->cgiparse(g,req)) ;
}

walk (g: ref Private_info, cgidata: ref CgiData) 
{	
	bufio := g.bufio; Iobuf: import bufio ;
	if (cgidata==nil) {g.bout.flush();return;}
	g.bout.puts(cgidata.httphd); cat (g, cgidata.uri) ;g.bout.flush();
}

cat (g: ref Private_info, file: string)
{
	bufio:=g.bufio; Iobuf: import bufio;
	buf := array [Sys->ATOMICIO] of byte;
	if ((fd:=sys->open(file,Sys->OREAD)==nil){
		g.bout.puts(sys->sprint("oops open %s '%r' \n", file)); return;
	}
	while((n:=sys->read(fd, buf, len buf))>0){ if (n<0){
			g.bout.puts(sys->sprint("oops read %s '%r' \n", file));
		} else g.bout.puts (string buf[0:n]);
	}
}
