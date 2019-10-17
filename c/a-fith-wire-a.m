# WireT ## t signature of wire, handles io ref of fid,qid,path ###
# WireU ## t, chan and/or fid and/or mem ###
# WireV ## t, in, out, err triple of WireU ###

# WireX ## t, it, read, write, seek, tell verbs ###
# WireY ## t, it, memory buffer blocks and maybe log, and leaf ###
# WireZ ## t, it, z is wire (synonym for Wire) ###

# WireP ## t, it, fith extent of wired thing ###
# WireQ ## t, it, fith index space of wired thing ###
# WireR ## t, it, fith reference map of wired thing ###

# WireW ## t, it, double buffer for presentation form if needed ###
# w is synonym for y if we don't need to interpret presentation forms

# WireE ## t, it, canonical type read and write fn on stream ###
# WireF ## t, it, form read and write to buffer from e fn calls ###

# WireY ## t, it, memory buffer blocks and maybe log, and leaf ###
# always a leaf, synthetic if not fix leaf
# 0: 8k block triple ; t:signature; 
# t:path: nil ; if not extant in fix

# wire leaf t -> 
#	fix leaf t (on storage), 
#	fish leaf t (live in process memory), 
#	fith leaf t (in image qidspace),
#	fiqh leaf t (realized on a live terminal),
#	fiqt leaf t (realized as fiqt space, scene or object)
# in any case there's always a WireY (Leaf)

# Y without Z,X is block buffer without access method

# all fid and fid metadata (dir, stat, pathname) is on Z with chan
# Z abstracts over chan and fid, there's always a chan
# chan of (t, string, int, array of byte) tag,err,ndata,data

# X of XYZ has the open,close,read,write,rstat,wstat,rcursor,wcursor implementations
# X of XYZ has but defers to Y via Z for rstat,wstat,rcursor,wcursor implementations
# don't bother with pread,pwrite but have a save-excursion for cursor
# cursor knows the full Xp,Xq,Xr
# x walk mutable referent, otherwise immutable
# define (Us,Gs,Rs,Fs) (unit, group, record, everything) for read and write

# Wire IO always in units of (Us,Gs,Rs,Fs)
# (1 bytes, 16 bytes, 8k bytes, 64k bytes) if undefined, (byte, Xt, block, slice)
# seps definition is 'min bytes, max bytes, delimiter rune'
# if delimiter undefined then read at least min at most max bytes

