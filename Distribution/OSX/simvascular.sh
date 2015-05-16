#!/bin/bash -f
#/bin/tcsh -f
# limit stacksize unlimited 

if [ "$BASH_SOURCE" == "$0" ];then
  BUNDLE=`echo "$0" | sed -e 's/\/Contents\/MacOS\/.*//'`
else
  BUNDLE=`echo "$BASH_SOURCE" | sed -e 's/\/Contents\/MacOS\/.*//'`
fi

RESOURCES=$BUNDLE/Contents/Resources

echo "BUNDLE: $BUNDLE"
echo "RESOURCES: $RESOURCES"

sh $RESOURCES/simvascular