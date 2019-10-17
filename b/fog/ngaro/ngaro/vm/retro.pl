#!/usr/bin/env perl
# Ngaro VM
# Copyright (c) 2010 - 2011, Charles Childers
# ----------------------------------------------------------------------------- Dependencies
use strict;
use POSIX;
use integer;
use Switch;

package Retro;
local $| = 1;
# ----------------------------------------------------------------------------- Variables
our $data    = 0;
our $i       = 0;
our $n       = 0;
our $sp      = 0;
our $rp      = 0;
our $ip      = 0;
our @memory  = ();
our @stack   = ();
our @address = ();
our @ports   = ();
# ----------------------------------------------------------------------------- Save Image
sub rxSaveImage {
  open f, ">retroImage" or die #!;
  binmode f;
  my $i = 0;
  while ($i < @memory[3]) {
    print f pack("i", @memory[$i]);
    $i++;
  }
  close f;
}
# ----------------------------------------------------------------------------- I/O Handler
sub rxHandleDevices {
  if (@ports[0] == 0)
  {
    @ports[0] = 1;
    if (@ports[1] == 1)
    {
      $i = getc(STDIN);
      @ports[1] = ord($i);
      if (@ports[1] == 13)
      {
        @ports[1] = 10;
      }
    }
    if (@ports[2] == 1)
    {
      if (@stack[$sp] < 0)
      {
        print "...";
      }
      else
      {
        print chr(@stack[$sp]);
      }
      $sp = $sp - 1;
      @ports[2] = 0;
    }

    if (@ports[4] == 1)
    {
      rxSaveImage();
      @ports[4] = 0;
    }

    switch (@ports[5])
    {
      case  -1 { @ports[5] = 1000000; }
      case  -2 { @ports[5] = 0; }
      case  -3 { @ports[5] = 0; }
      case  -4 { @ports[5] = 0; }
      case  -5 { @ports[5] = $sp; }
      case  -6 { @ports[5] = $rp; }
      case  -7 { @ports[5] = 0; }
      case  -8 { @ports[5] = time; }
      case  -9 { $ip = 1000000; }
      case -10 { @ports[5] = 0; }
      case -11 { @ports[5] = 0; }
      case -12 { @ports[5] = 0; }
      case -13 { @ports[5] = 0; }
    }
  }
}
# ----------------------------------------------------------------------------- Process Opcodes
sub rxProcessOpcode {
  my $opcode = @memory[$ip];
  my $a = 0;
  my $b = 0;
  my $c = 0;
  my $d = 0;
  if ($opcode >= 0 && $opcode <= 30)
  {
    switch ($opcode)
    {
      case 1
      {
        $sp++; $ip++;
        @stack[$sp] = @memory[$ip];
      }
      case 2
      {
        $sp++;
        @stack[$sp] = @stack[$sp - 1];
      }
      case 3
      {
        $sp--;
      }
      case 4
      {
        $a = @stack[$sp];
        @stack[$sp] = @stack[$sp - 1];
        @stack[$sp - 1] = $a;
      }
      case 5
      {
        $rp++;
        @address[$rp] = @stack[$sp];
        $sp--;
      }
      case 6
      {
        $sp++;
        @stack[$sp] = @address[$rp];
        $rp--;
      }
      case 7
      {
        @stack[$sp]--;
        if (@stack[$sp] != 0 && @stack[$sp] > -1)
        {
          $ip++;
          $ip = @memory[$ip] - 1;
        }
        else
        {
          $ip++;
          $sp--;
        }
      }
      case 8
      {
        $ip++;
        $ip = @memory[$ip] - 1;
        if (@memory[$ip + 1] == 0) { $ip++; }
        if (@memory[$ip + 1] == 0) { $ip++; }
      }
      case 9
      {
        $ip = @address[$rp];
        $rp--;
        if (@memory[$ip + 1] == 0) { $ip++; }
        if (@memory[$ip + 1] == 0) { $ip++; }
      }
      case 10
      {
        $ip++;
        if (@stack[$sp - 1] > @stack[$sp]) { $ip = @memory[$ip] - 1; }
        $sp = $sp - 2;
      }
      case 11
      {
        $ip++;
        if (@stack[$sp - 1] < @stack[$sp]) { $ip = @memory[$ip] - 1; }
        $sp = $sp - 2;
      }
      case 12
      {
        $ip++;
        if (@stack[$sp - 1] != @stack[$sp]) { $ip = @memory[$ip] - 1; }
        $sp = $sp - 2;
      }
      case 13
      {
        $ip++;
        if (@stack[$sp - 1] == @stack[$sp]) { $ip = @memory[$ip] - 1; }
        $sp = $sp - 2;
      }
      case 14
      {
        @stack[$sp] = @memory[@stack[$sp]];
      }
      case 15
      {
        @memory[@stack[$sp]] = @stack[$sp - 1];
        $sp = $sp - 2;
      }
      case 16
      {
        @stack[$sp - 1] = @stack[$sp] + @stack[$sp - 1];
        $sp--;
      }
      case 17
      {
        @stack[$sp - 1] = @stack[$sp - 1] - @stack[$sp];
        $sp--;
      }
      case 18
      {
        @stack[$sp - 1] = @stack[$sp - 1] * @stack[$sp];
        $sp--;
      }
      case 19
      {
        $a = @stack[$sp];
        $b = @stack[$sp - 1];
        @stack[$sp] = POSIX::floor($b / $a);
        @stack[$sp - 1] = $b % $a;
      }
      case 20
      {
        $a = @stack[$sp];
        $b = @stack[$sp - 1];
        $sp--;
        @stack[$sp] = $b & $a;
      }
      case 21
      {
        $a = @stack[$sp];
        $b = @stack[$sp - 1];
        $sp--;
        @stack[$sp] = $b | $a;
      }
      case 22
      {
        $a = @stack[$sp];
        $b = @stack[$sp - 1];
        $sp--;
        @stack[$sp] = $b ^ $a;
      }
      case 23
      {
        $a = @stack[$sp];
        $b = @stack[$sp - 1];
        $sp--;
        @stack[$sp] = $b << $a;
      }
      case 24
      {
        $a = @stack[$sp];
        $b = @stack[$sp - 1];
        $sp--;
        @stack[$sp] = $b >> $a;
      }
      case 25
      {
        if (@stack[$sp] == 0)
        {
          $sp--;
          $ip = @address[$rp];
          $rp--;
        }
      }
      case 26
      {
        @stack[$sp]++;
      }
      case 27
      {
        @stack[$sp]--;
      }
      case 28
      {
        $a = @stack[$sp];
        @stack[$sp] = @ports[$a];
        @ports[$a] = 0;
      }
      case 29
      {
        @ports[@stack[$sp]] = @stack[$sp - 1];
        $sp = $sp - 2;
      }
      case 30
      {
        rxHandleDevices();
      }
    }
  }
  else                  # call (implicit)
  {
    $rp++;
    @address[$rp] = $ip;
    $ip = @memory[$ip] - 1;
    if (@memory[$ip + 1] == 0) { $ip++; }
    if (@memory[$ip + 1] == 0) { $ip++; }
  }
}
# ----------------------------------------------------------------------------- Load Image
open f, "retroImage" or die #!;
binmode f;
$i = 0;
while (($n = read(f, $data, 4)) != 0) {
  @memory[$i] = int(unpack("i", $data));
  $i++;
}
close f;
# ----------------------------------------------------------------------------- Initialize
while ($i < 1000000) {
  @memory[$i] = 0;
  $i++;
}
$i = 0;
while ($i < 12) {
  @stack[$i] = 0;
  @address[$i] = 0;
  @ports[$i] = 0;
  $i++;
}
while ($i < 128) {
  @stack[$i] = 0;
  @address[$i] = 0;
  $i++;
}
while ($i < 1024) {
  @address[$i] = 0;
  $i++;
}
# ----------------------------------------------------------------------------- main loop
while ($ip < 1000000)
{
  rxProcessOpcode();
  $ip++;
}
