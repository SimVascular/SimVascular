
exec rm -f all-files.txt
exec find ../../Code -name "Makefile" > all-files.txt

set fp [open all-files.txt r]
while {[gets $fp line] >= 0} {
    if {[string trim $line] == ""} continue
    puts "working on $line..."
    exec sed -f ./update-lib-names-sed-script.txt $line >& $line.tmp
    exec mv -f $line.tmp $line
}
