/**********************************************************************
 * Ngaro Virtual Machine
 * Copyright (C) 2008 - 2011, Charles Childers
 **********************************************************************/


/**********************************************************************
 * Symbolic constants for each instruction.
 **********************************************************************/
  const VM_NOP = 0;       const VM_LIT = 1;         const VM_DUP = 2;
  const VM_DROP = 3;      const VM_SWAP = 4;        const VM_PUSH = 5;
  const VM_POP = 6;       const VM_LOOP = 7;        const VM_JUMP = 8;
  const VM_RETURN = 9;    const VM_GT_JUMP = 10;    const VM_LT_JUMP = 11;
  const VM_NE_JUMP = 12;  const VM_EQ_JUMP = 13;    const VM_FETCH = 14;
  const VM_STORE = 15;    const VM_ADD = 16;        const VM_SUB = 17;
  const VM_MUL = 18;      const VM_DIVMOD = 19;     const VM_AND = 20;
  const VM_OR = 21;       const VM_XOR = 22;        const VM_SHL = 23;
  const VM_SHR = 24;      const VM_ZERO_EXIT = 25;  const VM_INC = 26;
  const VM_DEC = 27;      const VM_IN = 28;         const VM_OUT = 29;
  const VM_WAIT = 30;



/**********************************************************************
 * Some constants useful to us for dealing with the VM settings.
 *
 * If you have performance issues, try modifying CYLES_PER
 **********************************************************************/
  var   IMAGE_SIZE    = 256000;         /* Amount of simulated RAM    */
  var   DATA_DEPTH    =    128;         /* Depth of data stack        */
  var   ADDRESS_DEPTH =   1000;         /* Depth of the stacks        */
  var   CYCLES_PER    =   4000;         /* Instructions to run per    */
                                        /* clock cycle                */
  var   TERM_WIDTH    =     80;         /* Width of virtual terminal  */
  var   TERM_HEIGHT   =     22;         /* Height of virtual terminal */
  var   FB_WIDTH      =    590;         /* Canvas Width               */
  var   FB_HEIGHT     =    290;         /* Canvas Height              */



/**********************************************************************
 * Internal registers, flags, and variables
 **********************************************************************/
  var sp = 0, rsp = 0, ip = 0;
  var run = 0;
  var data    = new Array(DATA_DEPTH);
  var address = new Array(ADDRESS_DEPTH);
  var ports   = new Array(64);
  var portHandlers = new Array(64);
  var image   = new Array(IMAGE_SIZE);
  var interval;
  var devOutput = "";
  var output = document.getElementById("output");
  var lastKey = " ";
  var width = 0;
  var mx, my, mb;
  var fbraw, fb;



/**********************************************************************
 * getImage()
 * Load a saved image
 **********************************************************************/
function getImage()
{
  clearDisplay();
  stopVM();
  try
  {
    x = localStorage['retroImage'].split(';');
    for (ip = 0; ip < IMAGE_SIZE; ip++)
    {
      image[ip] = parseInt(x[ip]);
    }
    ip = 0;
    sp = 0;
    rsp = 0;
  }
  catch (e)
  {
    alert("Sorry, unable to find a saved image");
  }
  startVM();
}


/**********************************************************************
 * saveImage()
 * Save the current image
 **********************************************************************/
function saveImage()
{
  stopVM();
  try
  {
    localStorage.setItem("retroImage", image.join(";"));
  }
  catch (e)
  {
    if (e == QUOTA_EXCEEDED_ERR)
      alert("Quota exceeded!");
  }
  startVM();
}


/**********************************************************************
 * pristine()
 * Load the default image
 **********************************************************************/
function pristine()
{
  clearDisplay();
  stopVM();
  loadImage();
  ip = 0;
  sp = 0;
  rsp = 0;
  startVM();
}


/**********************************************************************
 * initVM()
 * Initialize the Ngaro VM
 **********************************************************************/
function initVM()
{
  ip  = 0;
  sp  = 0;
  rsp = 0;
  ports[0] = 0;
  data[0] = 0;
  width = 0;
  mx = 0; my = 0; mb = 0;
  var i = 0;
  while (i < 64)
  {
    ports[i] = 0;
    i++;
  }
}


/**********************************************************************
 * This is a nice little hack to read key presses and store them
 * somewhere for later use. It's critical for the console emulation.
 **********************************************************************/
function readKeyboard(e)
{
  var uni = e.keyCode ? e.keyCode : e.charCode;
  lastKey = uni;
  if (uni == 8)
    return false;
}

function moveMouse(e)
{
  if (e.offsetX)
  {
    mx = e.offsetX;
    my = e.offsetY;
  }
  else if (e.layerX)
  {
    mx = e.layerX;
    my = e.layerY;
  }
  return true;
}

function setButton(e)
{
  mb = 1;
  return true;
}

function releaseButton(e)
{
  mb = 0;
  return true;
}



/**********************************************************************
 * handleDevices()
 * This handles the simulated hardware devices.
 **********************************************************************/
portHandlers[2] = function()
{
  var ch = String.fromCharCode(data[sp]);

  /* Remap select characters to HTML */
  switch (data[sp])
  {
    case 10: ch = "<br>\n"; width = 0; break;
    case 32: ch = "&nbsp;"; break;
    case 38: ch = "&amp;";  break;
    case 60: ch = "&lt;";   break;
    case 62: ch = "&gt;";   break;
  }

  /* Display the character */
  if (data[sp] < 0)
  {
    clearDisplay();
  }
  else
  {
    devOutput += ch;
    width++;
    if (width > TERM_WIDTH)
    {
      width = 0;
      devOutput += "<br>\n";
    }
  }

  if (data[sp] == 8)
    devOutput = devOutput.substr(0, devOutput.length - 2);

  sp--;
  ports[2] = 0;
}

portHandlers[4] = function()
{
  ports[4] = 0;
  saveImage();
}

portHandlers[5] = function()
{
  /* Capabilities */
  if (ports[5] == -1)
    ports[5] = IMAGE_SIZE;
  if (ports[5] == -2)
    ports[5] = -1;
  if (ports[5] == -3)
    ports[5] = FB_WIDTH;
  if (ports[5] == -4)
    ports[5] = FB_HEIGHT;
  if (ports[5] == -5)
    ports[5] = sp;
  if (ports[5] == -6)
    ports[5] = rsp;
  if (ports[5] == -7)
    ports[5] = -1;
  if (ports[5] == -8)
  {
    var foo = new Date;
    var unixtime_ms = foo.getTime();
    var unixtime = parseInt(unixtime_ms / 1000);
    ports[5] = unixtime;
  }
  if (ports[5] == -9)
  {
    if (run == 1)
      output.innerHTML = "Goodbye!";
    ip = IMAGE_SIZE;
    run = 0;
    ports[5] = 0;
  }
  if (ports[5] == -11)
    ports[5] = TERM_WIDTH;
  if (ports[5] == -12)
    ports[5] = TERM_HEIGHT;
}

portHandlers[6] = function()
{
  if (ports[6] == 1)
  {
    video_color(data[sp]); sp--;
    ports[6] = 0;
  }
  if (ports[6] == 2)
  {
    var x, y;
    y = data[sp]; sp--;
    x = data[sp]; sp--;
    video_pixel(x, y);
    ports[6] = 0;
  }
  if (ports[6] == 3)
  {
    var x, y, h, w;
    w = data[sp]; sp--;
    h = data[sp]; sp--;
    y = data[sp]; sp--;
    x = data[sp]; sp--;
    video_rect(x, y, w, h);
    ports[6] = 0;
  }
  if (ports[6] == 4)
  {
    var x, y, h, w;
    w = data[sp]; sp--;
    h = data[sp]; sp--;
    y = data[sp]; sp--;
    x = data[sp]; sp--;
    video_fillRect(x, y, w, h);
    ports[6] = 0;
  }
  if (ports[6] == 5)
  {
    var x, y, h;
    h = data[sp]; sp--;
    y = data[sp]; sp--;
    x = data[sp]; sp--;
    video_vline(x, y, h);
    ports[6] = 0;
  }
  if (ports[6] == 6)
  {
    var x, y, w;
    w = data[sp]; sp--;
    y = data[sp]; sp--;
    x = data[sp]; sp--;
    video_hline(x, y, w);
    ports[6] = 0;
  }
  if (ports[6] == 7)
  {
    var x, y, w;
    w = data[sp]; sp--;
    y = data[sp]; sp--;
    x = data[sp]; sp--;
    video_circle(x, y, w);
    ports[6] = 0;
  }
  if (ports[6] == 8)
  {
    var x, y, w;
    w = data[sp]; sp--;
    y = data[sp]; sp--;
    x = data[sp]; sp--;
    video_fillCircle(x, y, w);
    ports[6] = 0;
  }
}

portsHandler = function()
{
  if (ports[7] == 1)
  {
    sp++; data[sp] = mx;
    sp++; data[sp] = my;
    ports[7] = 0;
  }
  if (ports[7] == 2)
  {
    sp++; data[sp] = mb;
    ports[7] = 0;
  }
}

function handleDevices()
{
  if (ports[0] != 0)
    return;

  ports[0] = 1;

  /* Input */
  ports[1] = lastKey;
  lastKey = 0;

  if (ports[2] == 1)
    if (typeof portHandlers[2] == 'function')
      portHandlers[2]();

  for (var a = 3; a < 64; a++)
  {
    if (ports[a] != 0)
      if (typeof portHandlers[a] == 'function')
        portHandlers[a]();
  }
}



/**********************************************************************
 * clearDisplay()
 * This clears the display.
 **********************************************************************/
function clearDisplay()
{
  devOutput = "";
  output.innerHTML = devOutput;
  width = 0;
  fb.clearRect(0, 0, 800, 400);
}



/**********************************************************************
 * processOpcode()
 * This is the main piece of code in Ngaro. It looks up each opcode and
 * carries out the proper action. This is mostly a straight port of the
 * C implementation, so some optimization opportunities will probably
 * exist.
 **********************************************************************/
function processOpcode()
{
  var x, y, z, op;
  op = image[ip];
  switch(op)
  {
    case VM_NOP:
      break;
    case VM_LIT:
      sp++; ip++; data[sp] = image[ip];
      break;
    case VM_DUP:
      sp++; data[sp] = data[sp-1];
      break;
    case VM_DROP:
      data[sp] = 0; sp--;
      break;
    case VM_SWAP:
      x = data[sp];
      y = data[sp-1];
      data[sp] = y;
      data[sp-1] = x;
      break;
    case VM_PUSH:
      rsp++;
      address[rsp] = data[sp];
      sp--;
      break;
    case VM_POP:
      sp++;
      data[sp] = address[rsp];
      rsp--;
      break;
    case VM_LOOP:
      data[sp]--;
      if (data[sp] != 0)
      {
        ip++;
        ip = image[ip] - 1;
      }
      else
      {
        ip++;
        sp--;
      }
      break;
    case VM_JUMP:
      ip++;
      ip = image[ip] - 1;
      if (image[ip + 1] == 0) ip++;
      if (image[ip + 1] == 0) ip++;
      break;
    case VM_RETURN:
      ip = address[rsp]; rsp--;
      break;
    case VM_GT_JUMP:
      ip++
      if (data[sp-1] > data[sp])
        ip = image[ip] - 1;
      sp = sp - 2;
      break;
    case VM_LT_JUMP:
      ip++
      if (data[sp-1] < data[sp])
        ip = image[ip] - 1;
      sp = sp - 2;
      break;
    case VM_NE_JUMP:
      ip++
      if (data[sp-1] != data[sp])
        ip = image[ip] - 1;
      sp = sp - 2;
      break;
    case VM_EQ_JUMP:
      ip++
      if (data[sp-1] == data[sp])
        ip = image[ip] - 1;
      sp = sp - 2;
      break;
    case VM_FETCH:
      x = data[sp];
      data[sp] = image[x];
      break;
    case VM_STORE:
      image[data[sp]] = data[sp-1];
      sp = sp - 2;
      break;
    case VM_ADD:
      data[sp-1] += data[sp]; data[sp] = 0; sp--;
      break;
    case VM_SUB:
      data[sp-1] -= data[sp]; data[sp] = 0; sp--;
      break;
    case VM_MUL:
      data[sp-1] *= data[sp]; data[sp] = 0; sp--;
      break;
    case VM_DIVMOD:
      b = data[sp];
      a = data[sp - 1];
      x = Math.abs(b);
      y = Math.abs(a);
      q = Math.floor(y / x);
      r = y % x;
      if (a < 0 && b < 0)
      {
        r = r * -1;
      }
      if (a > 0 && b < 0)
      {
        q = q * -1;
      }
      if (a < 0 && b > 0)
      {
        r = r * -1;
        q = q * -1;
      }
      data[sp] = q;
      data[sp - 1] = r;
      break;
    case VM_AND:
      x = data[sp];
      y = data[sp-1];
      sp--;
      data[sp] = x & y;
      break;
    case VM_OR:
      x = data[sp];
      y = data[sp-1];
      sp--;
      data[sp] = x | y;
      break;
    case VM_XOR:
      x = data[sp];
      y = data[sp-1];
      sp--;
      data[sp] = x ^ y;
      break;
    case VM_SHL:
      x = data[sp];
      y = data[sp-1];
      sp--;
      data[sp] = y << x;
      break;
    case VM_SHR:
      x = data[sp];
      y = data[sp-1];
      sp--;
      data[sp] = y >>= x;
      break;
    case VM_ZERO_EXIT:
      if (data[sp] == 0)
      {
        sp--;
        ip = address[rsp]; rsp--;
      }
      break;
    case VM_INC:
      data[sp]++;
      break;
    case VM_DEC:
      data[sp]--;
      break;
    case VM_IN:
      x = data[sp];
      data[sp] = ports[x];
      ports[x] = 0;
      break;
    case VM_OUT:
      ports[data[sp]] = data[sp-1];
      sp = sp - 2;
      break;
    case VM_WAIT:
      handleDevices();
      break;
    default:
      rsp++;
      address[rsp] = ip;
      ip = image[ip] - 1;
      if (image[ip + 1] == 0) ip++;
      if (image[ip + 1] == 0) ip++;
      break;
  }
}




/**********************************************************************
 * startVM()
 **********************************************************************/
function startVM()
{
  interval = setInterval("processImage()", 10);
  run = 1;
}

function stopVM()
{
  clearInterval(interval);
  interval = null;
  run = 0;
}


/**********************************************************************
 * checkStack()
 **********************************************************************/
function checkStack()
{
  if (sp < 0 || rsp < 0)
    output.innerHTML = "Stack Underflow.<br>Press COMMAND+R or CTRL+R to reload";
  if (sp > DATA_DEPTH || rsp > DATA_DEPTH)
    output.innerHTML = "Stack Overflow.<br>Press COMMAND+R or CTRL+R to reload";
  if (sp < 0 || rsp < 0 || sp > DATA_DEPTH || rsp > ADDRESS_DEPTH)
  {
    ip = IMAGE_SIZE;
    run = 0;
  }
}


/**********************************************************************
 * processImage()
 * This runs through the image, calling processOpcode() for each
 * instruction.
 *
 * For performance reasons up to CYCLES_PER instructions will be
 * executed by this code per call.
 **********************************************************************/
function processImage()
{
  var a;

  for (a = CYCLES_PER; a > 0 && run == 1; a--)
  {
    processOpcode();
    checkStack();
    ip++;
  }

  /* Update the display */
  if (ports[3] == 0)
  {
    ports[3] = 1;
    output.innerHTML = devOutput;
    output.scrollTop = output.scrollHeight;
  }
}

/* Enable our keyboard handler */
document.onkeypress = readKeyboard;
document.onmousedown = setButton;
document.onmouseup = releaseButton;
document.onmousemove = moveMouse;
