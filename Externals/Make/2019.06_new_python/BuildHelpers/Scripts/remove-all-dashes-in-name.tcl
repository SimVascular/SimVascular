foreach fn [glob *.tar.gz] {
  set newname [regsub -all -- - $fn .]
  exec mv $fn $newname 
}
