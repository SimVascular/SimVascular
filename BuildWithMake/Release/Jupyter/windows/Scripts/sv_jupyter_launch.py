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
  sv_j_path = "C:\\sv-jupyter-notebooks"
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
subprocess.run(['jupyter.exe','notebook',sv_notebook_dir_flag])
