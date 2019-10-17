
var bPlaying = false;
var i = 0;
var zeroTime = 0;

var MCJCONNECTING = 0;
var MCJOPEN = 1;
var MCJCLOSING = 2;
var MCJCLOSED = 3;

var connectState = MCJCLOSED;

var webSocket;

var gx = document.getElementById( 'gx' );
var gy = document.getElementById( 'gy' );
var gz = document.getElementById( 'gz' );
var ax = document.getElementById( 'ax' );
var ay = document.getElementById( 'ay' );
var az = document.getElementById( 'az' );
var rz = document.getElementById( 'rx' );
var ry = document.getElementById( 'ry' );
var rz = document.getElementById( 'rz' );

function Play()
{
  var btn = document.getElementById( "BtnPlay" );
  if( bPlaying )
  {
    btn.value = "     Play     ";
    bPlaying = false
  }
  else
  {
    btn.value = "     Stop     ";
    bPlaying = true;
  }
}

if (window.DeviceOrientationEvent) 
{
	handleOrientationEvent = function(e) {
		gy.value = small( e.alpha );
		gx.value = small( e.beta );
		gz.value = small( e.gamma );
    };
	window.addEventListener('deviceorientation', handleOrientationEvent, false);
}
if (window.DeviceMotionEvent) 
{
	handleDeviceMotionEvent = function(e) {
		var acc = e.acceleration;
		ax.value = small( acc.x );
		az.value = small( acc.y ); //phone had sky vector = Z, Daz has Y up
		ay.value = small( acc.z );
		var rot = e.rotationRate;
		rx.value = small( rot.alpha );
		rz.value = small( rot.beta );
		ry.value = small( rot.gamma );
    };
	window.addEventListener( 'devicemotion', handleDeviceMotionEvent, false );
}

var dynForm = document.getElementById( "dynForm" );

function Connect()
{
	if( ( connectState == MCJOPEN ) || ( connectState == MCJCONNECTING ) )
	{
		connectState = MCJCLOSING;
		var btn = document.getElementById( "BtnCon" );
		btn.value = "Disonnecting...";
		webSocket.close();
		return;
	}
	if( connectState != MCJCLOSED )
	{
		return;
	}
	connectState = MCJCONNECTING;
	var btn = document.getElementById( "BtnCon" );
	btn.value = "Connecting...";
	serverIP = document.getElementById( "serverIP" ).value;
	webSocket = new WebSocket( "ws:" + serverIP );
	
	webSocket.onopen = function(){
		connectState = MCJOPEN;
		PutLabelsToServer();
		var btn = document.getElementById( "BtnCon" );
		btn.value = "Disconnect";
	}
	
	webSocket.onclose = function(){
		connectState = MCJCLOSED;
		var btn = document.getElementById( "BtnCon" );
		btn.value = "  Connect  ";
	}
	webSocket.onerror = function(evt){
	}
	webSocket.onmessage = function(evt){
	}
}

function sendit( msg )
{
	if( connectState == MCJOPEN )
	{
		webSocket.send( msg );
	}
}

function small(a)
{
return(Math.round(a*1000)/1000);
}

function PutLabelsToServer()
{
	sendit(
		"\r\n" + //because the header doesnt seem to end with a CRLF
		"["      + "\t" +
		"HEADERS" + "\t" +
		0      + "\t" +
		"ori.x"  + "\t" +
		"ori.y"  + "\t" +
		"ori.z"  + "\t" +
		"acc.x"  + "\t" +
		"acc.y"  + "\t" +
		"acc.z"  + "\t" + 
		"rrt.x"  + "\t" +
		"rrt.y"  + "\t" +
		"rrt.z"  + "\t" +
		"]"
	);
	zeroTime = new Date().getTime();
}
function PutDataToServer()
{
	var t = new Date().getTime() - zeroTime;
	sendit(
		"["      + "\t" +
		"DATA"   + "\t" +
		t        + "\t" +
		gx.value + "\t" +
		gy.value + "\t" +
		gz.value + "\t" +
		ax.value + "\t" +
		ay.value + "\t" +
		az.value + "\t" + 
		rx.value + "\t" +
		ry.value + "\t" +
		rz.value + "\t" +
		"]" 
	);
}

myVar = setInterval( myTimer, 1000 );

function ChangeRate()
{
	clearInterval( myVar );
	var SamplingRate = document.getElementById( "SamplingRate" ).value;
	var interval = 1000 / SamplingRate;
	myVar = setInterval( myTimer, interval );
}

function myTimer() 
{
	if( bPlaying )
	{
		PutDataToServer();
	}
	var anim = [ 
	"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*", 
	"&nbsp;&nbsp;&nbsp;&nbsp;*&nbsp;", 
	"&nbsp;&nbsp;&nbsp;*&nbsp;&nbsp;", 
	"&nbsp;&nbsp;*&nbsp;&nbsp;&nbsp;", 
	"&nbsp;*&nbsp;&nbsp;&nbsp;&nbsp;", 
	"*&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"  
	];
	var mark = anim[i%6];
	document.getElementById( "counter" ).innerHTML = mark;
	i++;
}
