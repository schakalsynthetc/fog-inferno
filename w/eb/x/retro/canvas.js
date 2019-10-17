/**********************************************************************
 * Ngaro VM
 * Copyright (C) 2008 - 2010, Charles Childers
 **********************************************************************/


/**********************************************************************
 * Canvas Support Code
 **********************************************************************/

function init_fb()
{
  fbraw = document.getElementById("framebuffer");
  fb = fbraw.getContext("2d");
}

function video_color(c)
{
  if (c == 0)
    fb.fillStyle = "black";
  if (c == 1)
    fb.fillStyle = "darkblue";
  if (c == 2)
    fb.fillStyle = "darkgreen";
  if (c == 3)
    fb.fillStyle = "darkcyan";
  if (c == 4)
    fb.fillStyle = "darkred";
  if (c == 5)
    fb.fillStyle = "purple";
  if (c == 6)
    fb.fillStyle = "brown";
  if (c == 7)
    fb.fillStyle = "darkgray";
  if (c == 8)
    fb.fillStyle = "gray";
  if (c == 9)
    fb.fillStyle = "blue";
  if (c == 10)
    fb.fillStyle = "green";
  if (c == 11)
    fb.fillStyle = "cyan";
  if (c == 12)
    fb.fillStyle = "red";
  if (c == 13)
    fb.fillStyle = "magenta";
  if (c == 14)
    fb.fillStyle = "yellow";
  if (c == 15)
    fb.fillStyle = "white";
  if (c < 0 || c > 15)
    fb.fillStyle = "black";
}

function video_pixel(x, y)
{
  fb.fillRect(x, y, 2, 2);
}

function video_rect(x, y, w, h)
{
  fb.strokeRect(x, y, w, h);
}

function video_fillRect(x, y, w, h)
{
  fb.fillRect(x, y, w, h);
}

function video_hline(x, y, w)
{
  fb.fillRect(x, y, w, 2);
}

function video_vline(x, y, h)
{
  fb.fillRect(x, y, 2, h);
}

function video_circle(x, y, w)
{
  fb.beginPath();
  fb.arc(x, y, w, 0, Math.PI*2, true);
  fb.closePath();
  fb.stroke();
}

function video_fillCircle(x, y, w)
{
  fb.beginPath();
  fb.arc(x, y, w, 0, Math.PI*2, true);
  fb.closePath();
  fb.fill();
}
