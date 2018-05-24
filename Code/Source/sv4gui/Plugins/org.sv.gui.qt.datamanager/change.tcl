file mkdir src
file mkdir src/internal
foreach fn [glob src-original/* src-original/internal/*] {
    if [file isdirectory $fn] continue
    puts "working on $fn..."
    regsub src-original $fn src newfn
    set newfn [file join [file dirname $newfn] sv[file tail $newfn]]
    puts "newfn ($newfn)"
    exec sed -f ./sed-script.txt $fn > $newfn
}
