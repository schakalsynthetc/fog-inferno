<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">

<html onclick="keepFocusInTextbox(event)">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<title>javascript hell</title>

<script type="text/javascript">
var 
histList=[""], histPos=0, 
_scope = {}, _win, // a top-level context
question, _in, _out, tooManyMatches=null, lastError=null;

function refocus()
{ _in.blur(); _in.focus(); } 
// Needed for Mozilla to scroll correctly.

function init() {
  _in = document.getElementById("input");
  _out = document.getElementById("output");
  _win = window;
  if (opener && !opener.closed) {
    println("Using bookmarklet version of shell: commands will run in opener's context.", "message");
    _win = opener;
  }
  initTarget();
  recalculateInputHeight();
  refocus();
}

function initTarget() {
  _win.Shell = window;
  _win.print = shellCommands.print;
}

// Unless the user is selected something, refocus the textbox.
// (requested by caillon, brendan, asa)
function keepFocusInTextbox(e) {
  var g = e.srcElement ? e.srcElement : e.target; // IE vs. standard
  while (!g.tagName) g = g.parentNode;
  var t = g.tagName.toUpperCase();
  if (t=="A" || t=="INPUT") return;
  if (window.getSelection) { // Mozilla
    if (String(window.getSelection())) return;
  }
  else if (document.getSelection) { // Opera? Netscape 4?
    if (document.getSelection()) return;
  }
  else { // IE
    if ( document.selection.createRange().text ) return;
  }
  refocus();
}

function inputKeydown(e) {
  // Use onkeydown because IE doesn't support onkeypress for arrow keys

  //alert(e.keyCode + " ^ " + e.keycode);

  if (e.shiftKey && e.keyCode == 13) { // shift-enter
    // don't do anything; allow the shift-enter to insert a line break as normal
  } else if (e.keyCode == 13) { // enter
    // execute the input on enter
    try { go(); } catch(er) { alert(er); };
    setTimeout(function() { _in.value = ""; }, 0); // can't preventDefault on input, so clear it later
  } else if (e.keyCode == 38) { // up
    // go up in history if at top or ctrl-up
    if (e.ctrlKey || caretInFirstLine(_in))
      hist(true);
  } else if (e.keyCode == 40) { // down
    // go down in history if at end or ctrl-down
    if (e.ctrlKey || caretInLastLine(_in))
      hist(false);
  } else if (e.keyCode == 9) { // tab
    tabcomplete();
    setTimeout(function() { refocus(); }, 0); // refocus because tab was hit
  } else { }

  setTimeout(recalculateInputHeight, 0);
  
  //return true;
};

// IE doesn't support selectionStart/selectionEnd
function caretInFirstLine(textbox)
{
  // IE doesn't support selectionStart/selectionEnd
  if (textbox.selectionStart == undefined) return true;
  var firstLineBreak = textbox.value.indexOf("\n");
  return ((firstLineBreak == -1) || (textbox.selectionStart <= firstLineBreak));
}
function caretInLastLine(textbox)
{
  if (textbox.selectionEnd == undefined) return true;
  var lastLineBreak = textbox.value.lastIndexOf("\n");
  return (textbox.selectionEnd > lastLineBreak);
}

function recalculateInputHeight()
{
  var rows = _in.value.split(/\n/).length
    + 1 // prevent scrollbar flickering in Mozilla
    + (window.opera ? 1 : 0); // leave room for scrollbar in Opera
  
  if (_in.rows != rows) // without this check, it is impossible to select text in Opera 7.60 or Opera 8.0.
    _in.rows = rows;
}

function println(s, type)
{
  if((s=String(s))) {
    var newdiv = document.createElement("div");
    newdiv.appendChild(document.createTextNode(s));
    newdiv.className = type;
    _out.appendChild(newdiv);
    return newdiv;
  }
}

function printWithRunin(h, s, type)
{
  var div = println(s, type);
  var head = document.createElement("strong");
  head.appendChild(document.createTextNode(h + ": "));
  div.insertBefore(head, div.firstChild);
}

var shellCommands = {
load : function load(url) {
  var s = _win.document.createElement("script");
  s.type = "text/javascript";
  s.src = url;
  _win.document.getElementsByTagName("head")[0].appendChild(s);
  println("Loading " + url + "...", "message");
},

clear : function clear() {
  var CHILDREN_TO_PRESERVE = 3;
  while (_out.childNodes[CHILDREN_TO_PRESERVE]) 
    _out.removeChild(_out.childNodes[CHILDREN_TO_PRESERVE]);
},

print : function print(s) { println(s, "print"); },

// the normal function, "print", shouldn't return a value
// (suggested by brendan; later noticed it was a problem when showing others)
pr : function pr(s) { 
  shellCommands.print(s); // need to specify shellCommands so it doesn't try window.print()!
  return s;
},

props : function props(e, onePerLine) {
  if (e === null) { println("props called with null argument", "error"); return; } 
  if (e === undefined) { println("props called with undefined argument", "error"); return; }
  var ns = ["Methods", "Fields", "Unreachables"];
  var as = [[], [], []]; // array of (empty) arrays of arrays!
  var p, j, i; // loop variables, several used multiple times
  var protoLevels = 0;
  for (p = e; p; p = p.__proto__) { for (i=0; i<ns.length; ++i) as[i][protoLevels] = []; ++protoLevels; }
  for(var a in e) {
	// Shortcoming: doesn't check that VALUES are the same in object and
	// prototype.

    var protoLevel = -1;
    try { for (p = e; p && (a in p); p = p.__proto__)
        ++protoLevel; 
	} catch(er) { 
		protoLevel = 0; 
	} // "in" operator throws when param to props() is a string

    var type = 1; try { if ((typeof e[a]) == "function") type = 0; } catch (er) { type = 2; }
    as[type][protoLevel].push(a);
  }

  function times(s, n) { return n ? s + times(s, n-1) : ""; }

  for (j=0; j<protoLevels; ++j)
    for (i=0;i<ns.length;++i)
      if (as[i][j].length) 
        printWithRunin(
          ns[i] + times(" of prototype", j), 
          (onePerLine ? "\n\n" : "") + as[i][j].sort().join(onePerLine ? "\n" : ", ") + (onePerLine ? "\n\n" : ""), 
          "propList"
        );
},

blink : function blink(node) {
  if (!node)                     throw("blink: argument is null or undefined.");
  if (node.nodeType == null)     throw("blink: argument must be a node.");
  if (node.nodeType == 3)        throw("blink: argument must not be a text node");
  if (node.documentElement)      throw("blink: argument must not be the document object");
  function setOutline(o) { return function() {
      if (node.style.outline != node.style.bogusProperty) { node.style.outline = o; } 
		// browser supports outline (Firefox 1.1 and newer, CSS3, Opera 8).
      else if (node.style.MozOutline != node.style.bogusProperty) { node.style.MozOutline = o; } 
		// browser supports MozOutline (Firefox 1.0.x and older)
      else { node.style.border = o; }
        // browser only supports border (IE). border is a fallback because it moves things around.
  }} 
  function focusIt(a) { return function() { a.focus(); } }
  if (node.ownerDocument) {
    var windowToFocusNow = (node.ownerDocument.defaultView || node.ownerDocument.parentWindow); // Moz vs. IE
    if (windowToFocusNow) setTimeout(focusIt(windowToFocusNow.top), 0);
  }
  for(var i=1;i<7;++i) setTimeout(setOutline((i%2)?'3px solid red':'none'), i*100);
  setTimeout(focusIt(window), 800);
  setTimeout(focusIt(_in), 810);
},

scope : function scope(sc) {
  if (!sc) sc = {};
  _scope = sc;
  println("Scope is now " + sc + ".  If a variable is not found in this scope, window will also be searched.  New variables will still go on window.", "message");
},

mathHelp : function mathHelp() {
  printWithRunin("Math constants", 
	"E, LN2, LN10, LOG2E, LOG10E, PI, SQRT1_2, SQRT2", "propList");
  printWithRunin("Math methods", 
	"abs, acos, asin, atan, atan2, ceil, cos, exp, floor, log, max, min, pow, random, round, sin, sqrt, tan", "propList");
},
  ans : undefined
};

function hist(up) {
  // histList[0] = first command entered, [1] = second, etc.
  // type something, press up --> thing typed is now in "limbo"
  // (last item in histList) and should be reachable by pressing 
  // down again.

  var L = histList.length; if (L == 1) return;

  if (up) {
    // Save this entry in case the user hits the down key.
    if (histPos == L-1) { histList[histPos] = _in.value; }

    if (histPos > 0) {
      histPos--;
      // Use a timeout to prevent up from moving cursor within new text
      // Set to nothing first for the same reason
      setTimeout(
        function() {
          _in.value = ''; 
          _in.value = histList[histPos];
          var caretPos = _in.value.length;
          if (_in.setSelectionRange) 
            _in.setSelectionRange(caretPos, caretPos);
        }, 0);
    }
  } 
  else // down
  {
    if (histPos < L-1) { histPos++; _in.value = histList[histPos]; }
    else if (histPos == L-1) {
      // Already on the current entry: clear but save
      if (_in.value) {
        histList[histPos] = _in.value;
        ++histPos;
        _in.value = "";
      }}}}

function tabcomplete() {
  /*
   * Working backwards from s[from], find the spot
   * where this expression starts.  It will scan
   * until it hits a mismatched ( or a space,
   * but it skips over quoted strings.
   * If stopAtDot is true, stop at a '.'
   */
  function findbeginning(s, from, stopAtDot) {
    /* *  Complicated function. Return true if s[i] == q BUT ONLY IF s[i-1] is not a backslash.  */
    function equalButNotEscaped(s,i,q)
    {
      if(s.charAt(i) != q) return false; // not equal go no further
      if(i==0) return true; // beginning of string
      if(s.charAt(i-1) == '\\') return false; // escaped?
      return true;
    }

    var nparens = 0;
    var i; for(i=from; i>=0; i--) {
      if(s.charAt(i) == ' ') break;
      if(stopAtDot && s.charAt(i) == '.') break;
      if(s.charAt(i) == ')') nparens++;
      else if(s.charAt(i) == '(') nparens--;
      if(nparens < 0) break;
      // skip quoted strings
      if(s.charAt(i) == '\'' || s.charAt(i) == '\"') {
        //dump("skipping quoted chars: ");
        var quot = s.charAt(i); i--;
        while(i >= 0 && !equalButNotEscaped(s,i,quot)) { i--; }
          //dump(s.charAt(i)); //dump("\n");
      }
    }
    return i;
  }

  // XXX should be used more consistently (instead of using selectionStart/selectionEnd throughout code)
  // XXX doesn't work in IE, even though it contains IE-specific code
  function getcaretpos(inp) {
    if(inp.selectionEnd != null) return inp.selectionEnd;
    if(inp.createTextRange) {
      var docrange = _win.Shell.document.selection.createRange();
      var inprange = inp.createTextRange();
      if (inprange.setEndPoint) {
        inprange.setEndPoint('EndToStart', docrange);
        return inprange.text.length;
      }
    }

    return inp.value.length; // sucks, punt
  }

  function setselectionto(inp,pos) {
    if(inp.selectionStart) { inp.selectionStart = inp.selectionEnd = pos; }
    else if(inp.createTextRange) {
      var docrange = _win.Shell.document.selection.createRange();
      var inprange = inp.createTextRange();
      inprange.move('character',pos);
      inprange.select();
    } else { // err...
    /*
      inp.select();
      if(_win.Shell.document.getSelection())
        _win.Shell.document.getSelection() = "";
        */
    }
  }
    // get position of cursor within the input box
    var caret = getcaretpos(_in);
    if(caret) {
      //dump("----\n");
      var dotpos, spacepos, complete, obj;
      //dump("caret pos: " + caret + "\n");
      // see if there's a dot before here
      dotpos = findbeginning(_in.value, caret-1, true);
      //dump("dot pos: " + dotpos + "\n");
      if(dotpos == -1 || _in.value.charAt(dotpos) != '.') {
        dotpos = caret;
		//dump("changed dot pos: " + dotpos + "\n");
      }
      // look backwards for a non-variable-name character
      spacepos = findbeginning(_in.value, dotpos-1, false);
      //dump("space pos: " + spacepos + "\n");
      // get the object we're trying to complete on
      if(spacepos == dotpos || spacepos+1 == dotpos || dotpos == caret)
      {
        // try completing function args
        if(_in.value.charAt(dotpos) == '(' ||
 (_in.value.charAt(spacepos) == '(' && (spacepos+1) == dotpos))
        {
          var fn,fname;
  var from = (_in.value.charAt(dotpos) == '(') ? dotpos : spacepos;
          spacepos = findbeginning(_in.value, from-1, false);

          fname = _in.value.substr(spacepos+1,from-(spacepos+1));
  //dump("fname: " + fname + "\n");
          try { 
			with (_win.Shell._scope) with (_win) with (Shell.shellCommands) fn = eval(fname);
          } catch(er) { return; } //dump('fn is not a valid object\n');
          if(fn == undefined) { return; } //dump('fn is undefined');
          if(fn instanceof Function) {
            // Print function definition, including argument names, but not function body
            if(!fn.toString().match(/function .+?\(\) +\{\n +\[native code\]\n\}/))
              println(fn.toString().match(/function .+?\(.*?\)/), "tabcomplete");
          }
          return;
        } else obj = _win;
      } else {
        var objname = _in.value.substr(spacepos+1,dotpos-(spacepos+1));
        //dump("objname: |" + objname + "|\n");
        try {
          with (_win.Shell._scope) with (_win) obj = eval(objname);
        } catch(er) { printError(er); return; }
        if(obj == undefined) {
          // sometimes this is tabcomplete's fault, so don't print it :(
          // e.g. completing from "print(document.getElements"
          // println("Can't complete from null or undefined expression " + objname, "error");
          return;
        }
      }
      //dump("obj: " + obj + "\n");
      // get the thing we're trying to complete
      if(dotpos == caret) {
        if(spacepos+1 == dotpos || spacepos == dotpos) { return; }
          // nothing to complete //dump("nothing to complete\n");
        complete = _in.value.substr(spacepos+1,dotpos-(spacepos+1));
      } else { complete = _in.value.substr(dotpos+1,caret-(dotpos+1)); }
      //dump("complete: " + complete + "\n");
      // ok, now look at all the props/methods of this obj
      // and find ones starting with 'complete'
      var matches = [];
      var bestmatch = null;
      for(var a in obj)
      {
        //a = a.toString();
        //XXX: making it lowercase could help some cases,
        // but screws up my general logic.
        if(a.substr(0,complete.length) == complete) {
          matches.push(a);
          ////dump("match: " + a + "\n");
          // if no best match, this is the best match
          if(bestmatch == null) { bestmatch = a; }
          else {
            // the best match is the longest common string
            function min(a,b) { return ((a<b)?a:b); }
            var i;
            for(i=0; i< min(bestmatch.length, a.length); i++)
            { 
				if(bestmatch.charAt(i) != a.charAt(i)) break; 
			}
            bestmatch = bestmatch.substr(0,i);
            ////dump("bestmatch len: " + i + "\n");
          }
          ////dump("bestmatch: " + bestmatch + "\n");
        }
      }
      bestmatch = (bestmatch || "");
      ////dump("matches: " + matches + "\n");
      var objAndComplete = (objname || obj) + "." + bestmatch;
      //dump("matches.length: " + matches.length + ", tooManyMatches: " + tooManyMatches + ", objAndComplete: " + objAndComplete + "\n");
      if(matches.length > 1 && (tooManyMatches == objAndComplete || matches.length <= 10)) {

        printWithRunin("Matches: ", matches.join(', '), "tabcomplete");
        tooManyMatches = null;
      }
      else if(matches.length > 10) {
        println(matches.length + " matches.  Press tab again to see them all", "tabcomplete");
        tooManyMatches = objAndComplete;
      } else { tooManyMatches = null; }
      if(bestmatch != "") {
        var sstart;
        if(dotpos == caret) { sstart = spacepos+1; } else { sstart = dotpos+1; }
        _in.value = _in.value.substr(0, sstart)
                  + bestmatch
                  + _in.value.substr(caret);
        setselectionto(_in,caret + (bestmatch.length - complete.length));
      }
    }
}

function printQuestion(q) { println(q, "input"); }
function printAnswer(a) { if (a !== undefined) { println(a, "normalOutput"); shellCommands.ans = a; } }
function printError(er) { 
  var lineNumberString;
  lastError = er; 
	// for debugging the shell
  if (er.name) {
    // lineNumberString should not be "", to avoid a very wacky bug in IE 6.
    lineNumberString = (er.lineNumber != undefined) ? (" on line " + er.lineNumber + ": ") : ": ";
    println(er.name + lineNumberString + er.message, "error"); // Because IE doesn't have error.toString.
  } else println(er, "error"); 
	// Because security errors in Moz /only/ have toString.
}

function go(s) {
  _in.value = question = s ? s : _in.value;
  if (question == "") return;
  histList[histList.length-1] = question;
  histList[histList.length] = "";
  histPos = histList.length - 1;
  // Unfortunately, this has to happen *before* the JavaScript is run, so that 
  // print() output will go in the right place.
  _in.value='';
  recalculateInputHeight();
  printQuestion(question);
  if (_win.closed) { printError("Target window has been closed."); return; }
  try { ("Shell" in _win) } catch(er) {
    printError("The JavaScript Shell cannot access variables in the target window.  The most likely reason is that the target window now has a different page loaded and that page has a different hostname than the original page.");
    return;
  }
  if (!("Shell" in _win)) initTarget(); // silent
  // Evaluate Shell.question using _win's eval (this is why eval isn't in the |with|, IIRC).
  _win.location.href = "javascript:try{ Shell.printAnswer(eval('with(Shell._scope) with(Shell.shellCommands) {' + Shell.question + String.fromCharCode(10) + '}')); } catch(er) { Shell.printError(er); }; setTimeout(Shell.refocus, 0); void 0";
}
</script>

<!-- for http://ted.mielczarek.org/code/mozilla/extensiondev/ -->
<script type="text/javascript" src="chrome://extensiondev/content/rdfhistory.js"></script>
<script type="text/javascript" src="chrome://extensiondev/content/chromeShellExtras.js"></script>

<style type="text/css">
body { 
	margin: 0 0 0 0;
	background: black url(/m/img/bg/tet/bg1.jpeg); 
	color: #aaa; 
	font-family: 'Consolas'; font-size: 12pt; font-weight: bold;
}

#output { 
	border: 0; width: 100%; margin: 0pt; 
	white-space: pre-line;
	/* Preserve line breaks, but wrap too if browser supports it */
}

h3 { margin-top: 0; margin-bottom: 0em; }
h3 + div { margin: 0; }

.banner { 
	font-family: sans-serif; 
	font-size: xx-large; 
	margin: 0 0 0 0;
	color: #99b;
}

form { }

#input { 
	width: 100%; overflow: auto; border: 0; border-top: 1px solid #444;
}

.inputLine { color: #aac; font: inherit; background: black; margin: 0pt; }
.input { background: black ; color: #99c; font: inherit; }
.normalOutput { background: black; color: #aaa; }
.print { background: black ; color: #a8a6a8; }
.error { background: black ; color: red; }
.propList { background: black ; color: #90ac98; }
.message { background: black ; color: #a8ac90; }
.tabcomplete { background: black ; color: #9090ac; }

#helpjunk { display: none; }

</style>
</head>

<body onload="init()">
<h3 class="banner">javascript hell</h3>
<div id="it"></div>
<div id="output">
<!-- <div id="helpjunk">
<a accesskey="M" href="javascript:go('scope(Math); mathHelp();');" title="Accesskey: M">Math</a> 
<a accesskey="H" href="http://www.squarefree.com/shell/?ignoreReferrerFrom=shell1.4" title="Accesskey: H">help</a> ans print(string) <a accesskey="P" href="javascript:go('props(ans)')" title="Accesskey: P">props(object)</a> <a accesskey="B" href="javascript:go('blink(ans)')" title="Accesskey: B">blink(node)</a> <a accesskey="C" href="javascript:go('clear()')" title="Accesskey: C">clear()</a> load(scriptURL) scope(object)
</div> -->
</div>
<div> <textarea id="input" class="inputLine" wrap="off" onkeydown="inputKeydown(event)" rows="1"> </textarea> </div>

</body>
</html>
