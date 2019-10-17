#!/! /c/walkpath.b
#
#
#
parsepath (it: string, infix: string): (int, string, list of string)
{ 
	left: list of string; n:int; y:string; ys: list of string ;	
	
	(n,ys)=sys->tokenize(it,infix); 
	
	for (; ys != nil; ys=tl ys){ 
		y=hd ys; 
		left=left::y; 
	};
	
	return (n,y,left);
}

