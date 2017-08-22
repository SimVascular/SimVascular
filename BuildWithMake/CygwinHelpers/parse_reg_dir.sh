#!/bin/sh -f
# HKLM\\SOFTWARE\\Wow6432Node\\Microsoft\\VisualStudio\\SxS\\VS7

reg query "$1" /v $2 | sed ':a;N;$!ba;s/\n//g;s/\r//g;s+\\+/+g' | awk '{gsub(/^[ \t]+/,"",$0);gsub(/^[ \t]+$/,"",$0);print}' | awk '{$1="";$2="";$3="";print}' | awk '{gsub(/^[ \t]+/,"",$0);gsub(/^[ \t]+$/,"",$0);print}'

