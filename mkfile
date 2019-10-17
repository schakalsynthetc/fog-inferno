ROOT=/
SYNC=tet:/y/00

%/sync: $ROOT/%
	rsync -avz $ROOT/$stem/ $SYNC/$stem

install:V:
	rsync -az $ROOT/b/386/Linux/b/ /b/386/Linux/b
	rm -rf /b/00/00/*; rsync -az /b/386/Linux/b/00/00/ /b/00/00
	rsync -az $ROOT/b/386/Linux/x/ /b/386/Linux/x
	rm -rf /x/00/00/*; rsync -az /b/386/Linux/x/00/00/ /x/00/00
