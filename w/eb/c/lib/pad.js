padn = function (x,n) {
    var buf=[]; var s=String(x); var i;

    //do{
    //	buf[i] = Math.floor(x % 10);
    //	i += 1;
    //}while(x = Math.floor(x / 10));

    for (i = 0; i < n - s.length; ++i){ buf[i]=" ".charCodeAt(0); }
    for (i; i < n; ++i){ buf[i] = s.charCodeAt(i - (n - s.length)); }
    buf[n] = " ".charCodeAt(0);
    return buf;
}
