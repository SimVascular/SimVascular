https://software.intel.com/en-us/articles/numpyscipy-with-intel-mkl

https://pypi.python.org/pypi/Cython/

http://mingwpy.github.io/motivation.html

http://nipy.org/nipy/devel/install/windows_scipy_build.html

http://stackoverflow.com/questions/32228967/compile-numpy-without-intel-mkl-blas-atlas-lapack

http://www.lfd.uci.edu/~gohlke/pythonlibs/#numpy

https://www.scipy.org/scipylib/building/windows.html#microsoft-visual-c-msvc

./pip2.7.exe install Cython --install-option="--no-cython-compile"

tar xvzf ./numpy-1.11.1.tar.gz

launched in MSVC 2010 win64 compiler shell

cd [dir]

set BLAS=None
set LAPACK=None
set ATLAS=None

[fullpath]/python setup.py build

[fullpath]/python.exe setup.py install --prefix C:\\cygwin64/SV16/bin/msvc-12.5/x64/python-2.7.11

