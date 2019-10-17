
function xjson (url, jobj) { 
	var req = new XMLHttpRequest();
	req.open("GET", url, true);
	req.responseType="json";
	req.onreadystatechange = function(){
  		var done=4, ok=200;
  		if (req.readyState===done && req.status===ok) {
  			jobj = JSON.parse(req.response);
  		}
	};
	req.send(null);
	return req;
}
