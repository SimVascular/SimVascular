#/bin/sh
rm -Rf /usr/local/package/simvascular
rm -f /usr/local/bin/simvascular
rm -f /usr/local/bin/simvascular-cmdline
chmod a+rx generic_launch_script
cp generic_launch_script /usr/local/bin/simvascular
cp generic_launch_script /usr/local/bin/simvascular-cmdline
cp generic_launch_script /usr/local/bin/presolver
cp generic_launch_script /usr/local/bin/postsolver
cp generic_launch_script /usr/local/bin/solver
cp generic_launch_script /usr/local/bin/adaptor
for fn in `ls *.gz`;do (tar --directory=/usr/local -xvzf $fn); done
chmod a+rwx /usr/local/package/simvascular

