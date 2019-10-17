typedef struct uPoint uPoint ;
typedef struct Xp Xp ;

typedef unsigned char u8  ; typedef u8  * v8  ;
typedef unsigned long u32 ; typedef u32 * v32 ;

#define tBLOCK 8192
#define tSLICE 65536  

struct uPoint {
	u8 xt [tBLOCK] ; u8 yt [tBLOCK] ;
	u8 xp [tBLOCK] ; u8 yp [tBLOCK] ;
	u8 xq [tBLOCK] ; u8 yq [tBLOCK] ;
	u8 xr [tBLOCK] ; u8 yr [tBLOCK] ;
};

struct Xp { Xp * left ; uPoint * p,q ; Xp * right ; };

/* ngaro thread
   xt, yt = state block
   xp, yp = input and output byte buffers
   xq, yq = data and return stacks (1k each) 
   xr, yr = dictionary mapper (1k pairs)
 */

