#/bin/sh
rm -f /usr/local/bin/REPLACE_SV_VERSION
rm -f /usr/local/bin/REPLACE_SV_VERSION-cmdline
rm -f /usr/local/bin/xfsvREPLACE_SV_POSTFIX
cp /usr/local/package/REPLACE_SV_VERSION/REPLACE_TIMESTAMP/generic_launch_script /usr/local/bin/REPLACE_SV_VERSION
cp /usr/local/package/REPLACE_SV_VERSION/REPLACE_TIMESTAMP/generic_launch_script /usr/local/bin/REPLACE_SV_VERSION-cmdline
cp /usr/local/package/REPLACE_SV_VERSION/REPLACE_TIMESTAMP/generic_launch_script /usr/local/bin/xfsvREPLACE_SV_POSTFIX
chmod a+rx /usr/local/bin/simvascularREPLACE_SV_POSTFIX
chmod a+rx /usr/local/bin/simvascularREPLACE_SV_POSTFIX-cmdline
chmod a+rx /usr/local/bin/xfsvREPLACE_SV_POSTFIX


