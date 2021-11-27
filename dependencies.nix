{ pkgs ? import <nixpkgs> {} }:

with pkgs;
{ inherit coreutils gnugrep; }
