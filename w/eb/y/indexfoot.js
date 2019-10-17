// indexfoot.js / comes at the bottom of the index page ///

function hello(it,msg) {
	document.getElementById(it).innerHTML=msg; return it;
}

hello("#hello","hello foot");
