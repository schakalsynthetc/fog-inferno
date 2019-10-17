#include <u.h>
#include <libc.h>

#define SYSHOST "Linux"
#define OBJTYPE "386"

char* t (int argc, char **argv);

/* read /b/../t if it exists, or synthesize one */

char* t (int argc, char **argv)
{ fprint (1,"#!/! /b/%s/%s/t\n",OBJTYPE,SYSHOST); return nil; }

void main (int argc, char **argv)
{ if (argc==1) exits (t(argc,argv)); else {
	int i=0; fprint (1,"#!/! %d\n",argc); for(i=0;i<argc;i++)fprint (1,"%64s\n",argv[i]);
} exits(nil); }
