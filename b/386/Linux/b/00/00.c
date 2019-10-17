#include <u.h>
#include <libc.h>

#define SYSHOST "Linux"
#define OBJTYPE "386"

void main (int argc, char **argv)
{ int i=0;

	if (argc==1) {
		fprint (1,"#!/b/00/00/00 386/Linux\n"); exits(nil);
	} else {
		fprint (1,"#!/!\n");
		for (i=1;i<argc;i++) {
			fprint (1,":%3d :%64s ;\n");
		}
	}
	exits(nil);
}
