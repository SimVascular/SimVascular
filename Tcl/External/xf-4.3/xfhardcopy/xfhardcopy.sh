#!/bin/sh
#
# Program: xfhardcopy
#
# This file is an automatically created shell script
# for starting the application named: xfhardcopy.tcl
#
# adapt the following variables to fit your
# local site
#
# WHIS_CMD is the wish interpreter to use
WISH_CMD=/home/stone/garfield/bin/X386/epwish
#
# XF_LOAD_PATH is the path were the tcl modules
# for this application are located
if test "$XF_LOAD_PATH" = ""; then
  XF_LOAD_PATH=.:/usr/local/lib/
else
  XF_LOAD_PATH=$XF_LOAD_PATH:.:/usr/local/lib/
fi
#
#
ARGC=$#
COMMANDLINE=
while [ $ARGC -gt 0 ]; do
  C=$1
  shift
  ARGC=`expr $ARGC - 1`
  case $C in
    -xfloadpath)
      if [ $ARGC -gt 0 ]; then
        C=$1
        shift
        ARGC=`expr $ARGC - 1`
        XF_LOAD_PATH=$C:$XF_LOAD_PATH
      else
        echo "xfhardcopy.tcl: expected path for -xfloadpath"
        exit 2
      fi;;
    *)
      COMMANDLINE=$COMMANDLINE" "$C;;
  esac
done
#
export XF_LOAD_PATH
for p in `echo $XF_LOAD_PATH|awk 'BEGIN{RS=":"}{print $0}'`; do
  if test -f $p/xfhardcopy.tcl; then 
    exec $WISH_CMD -name xfhardcopy -file $p/xfhardcopy.tcl $COMMANDLINE
  fi
  (cd $p; retrv -q xfhardcopy.tcl) 2>/dev/null
  if test -f $p/xfhardcopy.tcl; then 
    $WISH_CMD -name xfhardcopy -file $p/xfhardcopy.tcl $COMMANDLINE
    (cd $p; rm -f xfhardcopy.tcl) 2>/dev/null
  fi
done
echo "Could not find: xfhardcopy.tcl"
# eof

