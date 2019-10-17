
var MaxNumTouches = 4; //multitouch limit 

var mire = document.getElementById( 'mire' ); //the canvas, our touch area
var btnPlay = document.getElementById( "BtnPlay" );
var btnConn = document.getElementById( "BtnConn" );
var canvasWH = document.getElementById( 'canvasWH' ); //input field for touch area resizing

// display widgets for our data
var tx=[]; var ty=[]; var ta=[]; var tr=[];
for( var i=0; i<MaxNumTouches; i++ ){
	tx[i]=document.getElementById('t' + i + 'x' );
	ty[i]=document.getElementById('t' + i + 'y' );
	ta[i]=document.getElementById('t' + i + 'a' );
	tr[i]=document.getElementById('t' + i + 'r' );
}

var bPlaying = false; // are we streaming data to the server

var zeroTime = 0; // the time in system ticks at which streaming started

var MCJCONNECTING = 0; // possible websocket connection states
var MCJOPEN = 1;
var MCJCLOSING = 2;
var MCJCLOSED = 3;
var connectState = MCJCLOSED; //websocket connection states

var webSocket; // our communication socket

drawMire(); // paint our touch screen area with guide lines and circles

window.addEventListener( 'load', installTouchEvents, false ); //start sampling

myVar = setInterval( myTimer, 1000 ); // our transmission pulsation

function drawMire(){
	var canvas = document.getElementById('mire');
	var context = canvas.getContext('2d');
	var centerX = canvas.width / 2;
	var centerY = canvas.height / 2;
	var radius = canvas.width / 2;
	context.lineWidth = 3;
	context.strokeStyle = '#808055';
	for(var i=0; i<=10; i+=2){
		context.beginPath();
		context.arc(centerX,centerY,radius*i/10,0,2*Math.PI,false); context.stroke();
		context.closePath();
	}
	context.strokeStyle = '#805580';
	for( var i=0; i<=10; i++ ){
		var x = canvas.width * i / 10;
		context.beginPath();context.moveTo(x,0);context.lineTo(x,canvas.height-1);
		context.stroke();
		context.beginPath();context.moveTo(0,x);context.lineTo(canvas.width-1,x);
		context.stroke();
	}
	context.strokeStyle = '#558080';
	for( var i=0; i<=360; i+=30 ){
		var ang = i * Math.PI / 180;
		var x = Math.cos(ang) * radius + centerX ;
		var y = Math.sin(ang) * radius + centerY ;
		context.beginPath();context.moveTo(centerX,centerY);context.lineTo(x,y);
		context.stroke();
	}
}

function resizeMire(){
	var wh=canvasWH.value; mire.style.width=wh; mire.style.height=wh;
}

//For the touchstart event, it is a list of the touch points that became active with the current event.
//For the touchmove event, it is a list of the touch points that have changed since the last event.
//For the touchend event, it is a list of the touch points that have been removed from the surface.
function printTouch(e){
	var canvasW=this.width; var canvasH=this.height;
	var touches=e.changedTouches; var n=touches.length;
	for( var i=0; i<MaxNumTouches; i++ ){ if(i<n) {
			var xx=parseInt(touches[i].clientX) - this.offsetLeft;
			var yy=parseInt(touches[i].clientY) - this.offsetTop;
			yy = canvasH - 1 - yy; //flip
			xx = 2 * xx / canvasW - 1; yy = 2 * yy / canvasH - 1;
			var aa = Math.atan2(yy,xx)*180 / Math.PI;
			var rr = Math.sqrt(xx*xx + yy*yy);
			tx[i].value=small(xx); ty[i].value=small(yy);
			ta[i].value=small(aa); tr[i].value=small(rr);
		}
	}
	e.preventDefault();
}

//==============================================================================
function installTouchEvents(){
    mire.addEventListener( 'touchstart', printTouch, false );
    mire.addEventListener( 'touchmove',  printTouch, false );
    mire.addEventListener( 'touchend',   printTouch, false );
}

function Play(){ if(bPlaying){ btnPlay.value="     play     "; bPlaying=false; } else { btnPlay.value="     Stop     "; bPlaying=true; }}

function Connect(){
	if( (connectState==MCJOPEN) || (connectState==MCJCONNECTING) ){
		connectState=MCJCLOSING; btnConn.value="Disonnecting...";
		webSocket.close();
		btnPlay.value = "     Play     "; bPlaying = false; return;
	}
	if( connectState != MCJCLOSED ){ return; }
	connectState=MCJCONNECTING; btnConn.value = "Connecting...";
	serverIP = document.getElementById( "serverIP" ).value;
	webSocket = new WebSocket("ws:" + serverIP);
	
	webSocket.onopen = function(){
		connectState=MCJOPEN; PutLabelsToServer(); btnConn.value="Disconnect";
	}	
	webSocket.onclose=function(){
		connectState=MCJCLOSED;
		btnConn.value="  Connect  "; 
		btnPlay.value="     Play     "; 
		bPlaying=false;
	}
	webSocket.onerror=function(evt){};
	webSocket.onmessage=function(evt){};
}

function sendit(msg){ if(connectState==MCJOPEN){ webSocket.send(msg); }}

//============
function small(a){ return(Math.round(a*1000)/1000); }

//============
// because the header doesnt seem to end with a CRLF we add one
function PutLabelsToServer(){
	var msg = 
		"\r\n"    + 
		"["       + "\t" +
		"HEADERS" + "\t" +
		0         + "\t";
	for( var i=0; i<MaxNumTouches; i++ ){	
		msg=msg + 
			"t" + i + "x" + "\t" +
			"t" + i + "y" + "\t" +
			"t" + i + "a" + "\t" +
			"t" + i + "r" + "\t";
	}
	msg=msg + "]"; sendit(msg); zeroTime=new Date().getTime();
}

function PutDataToServer(){
	var t=new Date().getTime() - zeroTime;
	var msg = "["+"\t"+"DATA"+"\t"+t+"\t" ;
	for (var i=0; i<MaxNumTouches; i++) {	
		msg=msg + 
			tx[i].value + "\t" +
			ty[i].value + "\t" +
			ta[i].value + "\t" +
			tr[i].value + "\t" ;
	}; msg=msg + "]"; sendit(msg);
}

function ChangeRate(){
	clearInterval(myVar);
	var SamplingRate=document.getElementById("SamplingRate").value;
	var interval=1000/SamplingRate;
	myVar=setInterval(myTimer,interval);
}

function myTimer(){
	if(bPlaying){ PutDataToServer(); }


	var anim=[ 
		"-------<","------<-","-----<--","----<---","---<----","--<-----","-<------","<-------"
	];
	var mark=anim[i%8];document.getElementById("counter").innerHTML=mark;
	i++;
}
