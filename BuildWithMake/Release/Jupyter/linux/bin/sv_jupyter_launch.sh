#!/usr/local/package/simvascular/REPLACE_SV_TIMESTAMP/PythonREPLACE_PYTHON_MAJOR_VERSION.REPLACE_PYTHON_MINOR_VERSION/bin/python-wrapper

# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import os
import string
import subprocess
#import getpass

# function to check for whitespace in a string
def sv_contains_whitespace(s):
    for c in s:
        if c in string.whitespace:
            return True
    return False

# hardcoded default path for jupyter notebooks
sv_j_path = os.path.join(os.path.expanduser("~"),'sv-jupyter-notebooks')

# if path contains a space, change to different default
if(sv_contains_whitespace(sv_j_path)):
  print("Home directory contains space (sv_j_path)")
  sv_j_path = "/tmp/sv-jupyter-notebooks"
  print("Using default ",sv_j_path," instead")

# check to make sure directory exists
# and create if not
if(os.path.isdir(sv_j_path)):
   # nothing to do
   print("using directory: ",sv_j_path)
else:
   # make the dir
   print("creating directory: ",sv_j_path)
   os.mkdir(sv_j_path)

sv_notebook_dir_flag = '--notebook-dir=' + sv_j_path
subprocess.run(['/usr/local/package/simvascular/REPLACE_SV_TIMESTAMP/PythonREPLACE_PYTHON_MAJOR_VERSION.REPLACE_PYTHON_MINOR_VERSION/bin/jupyter','notebook',sv_notebook_dir_flag])
