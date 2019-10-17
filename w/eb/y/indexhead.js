// indexhead.js / comes at the top of the index page ///

function hello(it,msg) {
	document.getElementById(it).innerHTML=msg; return it;
}

hello("#hello","hello head");
