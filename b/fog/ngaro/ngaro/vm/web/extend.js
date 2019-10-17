portHandlers[20] = function()
{
  var s = "";
  var i = ports[20];
  ports[20] = 0;
  while (image[i] != 0)
  {
    s += String.fromCharCode(image[i]);
    i++;
  }
  eval('('+s+')');
}
