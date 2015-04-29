#/bin/sh
rm -Rf /usr/local/package/simvascular64
rm -f /usr/local/bin/simvascular64
rm -f /usr/local/bin/simvascular64-cmdline
rm -f /usr/local/bin/adaptor64
rm -f /usr/local/bin/presolver64
rm -f /usr/local/bin/postsolver64
rm -f /usr/local/bin/freeview64
rm -f /usr/local/bin/flowsolver64
cp dist_files/generic_launch_script /usr/local/bin/simvascular64
cp dist_files/generic_launch_script /usr/local/bin/simvascular64-cmdline
cp dist_files/generic_launch_script /usr/local/bin/adaptor64
cp dist_files/generic_launch_script /usr/local/bin/presolver64
cp dist_files/generic_launch_script /usr/local/bin/postsolver64
cp dist_files/generic_launch_script /usr/local/bin/freeview64
cp dist_files/generic_launch_script /usr/local/bin/flowsolver64
tar --directory=/usr/local -xvzf dist_files/simvascular*x64*.gz
chmod a+rx /usr/local/package/simvascular64
chmod a+rx /usr/local/bin/simvascular64
chmod a+rx /usr/local/bin/simvascular64-cmdline
chmod a+rx /usr/local/bin/adaptor64
chmod a+rx /usr/local/bin/presolver64
chmod a+rx /usr/local/bin/postsolver64
chmod a+rx /usr/local/bin/freeview64
chmod a+rx /usr/local/bin/flowsolver64

