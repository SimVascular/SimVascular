# Find external licensed module dir
if [ -d /usr/local/sv/licensed ]; then
    dirarray=($(find /usr/local/sv/licensed -maxdepth 1 -type d | sort))
    if [ ${#dirarray[@]} -gt 0 ]; then
        licdir=${dirarray[${#dirarray[@]}-1]}
    fi
fi
