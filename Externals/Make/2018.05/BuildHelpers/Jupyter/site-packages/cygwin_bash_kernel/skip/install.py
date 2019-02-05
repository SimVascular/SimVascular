# This file is a modified copy of https://github.com/takluyver/bash_kernel/blob/0966dc102d7549f5c909c93de633a95b2af9f707/bash_kernel/install.py

# Copyright (c) 2015, Thomas Kluyver and contributors 
# All rights reserved.

# BSD 3-clause license:

# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import json
import os
import sys
import argparse

from jupyter_client.kernelspec import KernelSpecManager
from IPython.utils.tempdir import TemporaryDirectory
from powershell_kernel.util import get_powershell

kernel_json = {
 "argv": [sys.executable, "-m", "powershell_kernel", "-f", "{connection_file}"],
 "display_name": "PowerShell",
 "language": "powershell",
}

def install_my_kernel_spec(user=True, prefix=None, powershell_command=None):
    
    if sys.version_info >= (3, 0): 
        if powershell_command is None:
            powershell_command = get_powershell()
        kernel_json.update({'env': {'powershell_command' : powershell_command}})
        print('Using powershell_command=%r' % powershell_command)
    else:
        # python 2 cannot use env to pass values to the kernel
        # https://github.com/vors/jupyter-powershell/issues/7
        # TODO(python2): find a way to pass it
        if powershell_command is not None:
            print('Ignoring powershell_command on python2, jupyter will use default powershell_command=%r' % powershell_command)

    with TemporaryDirectory() as td:
        os.chmod(td, 0o755) # Starts off as 700, not user readable
        with open(os.path.join(td, 'kernel.json'), 'w') as f:
            json.dump(kernel_json, f, sort_keys=True)
        # TODO: Copy resources once they're specified

        print('Installing IPython kernel spec')
        KernelSpecManager().install_kernel_spec(td, 'powershell', user=user, replace=True, prefix=prefix)

def _is_root():
    try:
        return os.geteuid() == 0
    except AttributeError:
        return False # assume not an admin on non-Unix platforms

def main(argv=None):
    parser = argparse.ArgumentParser(
        description='Install KernelSpec for PowerShell Kernel'
    )
    prefix_locations = parser.add_mutually_exclusive_group()

    prefix_locations.add_argument(
        '--user',
        help='Install KernelSpec in user homedirectory',
        action='store_true'
    )
    prefix_locations.add_argument(
        '--sys-prefix',
        help='Install KernelSpec in sys.prefix. Useful in conda / virtualenv',
        action='store_true',
        dest='sys_prefix'
    )
    prefix_locations.add_argument(
        '--prefix',
        help='Install KernelSpec in this prefix',
        default=None
    )
    prefix_locations.add_argument(
        '--powershell-command',
        help='Command to run powershell (by default "powershell" on windows and "pwsh" on unix)',
        default=None
    )

    args = parser.parse_args(argv)

    user = False
    prefix = None
    powershell_command = None
    if args.sys_prefix:
        prefix = sys.prefix
    elif args.prefix:
        prefix = args.prefix
    elif args.user or not _is_root():
        user = True

    if args.powershell_command:
        powershell_command = args.powershell_command

    install_my_kernel_spec(user=user, prefix=prefix, powershell_command=powershell_command)

if __name__ == '__main__':
    main()
