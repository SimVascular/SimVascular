# -*- coding: utf-8 -*-
# Copyright (c) 2011, Wojciech Bederski (wuub.net)
# All rights reserved.
# See LICENSE.txt for details.
from __future__ import absolute_import, unicode_literals, print_function, division

import subprocess
import os
import sys
import re
import signal
from subprocess import Popen
from codecs import getencoder, getincrementaldecoder

PY3 = sys.version_info[0] == 3

if os.name == 'posix':
    POSIX = True
    import fcntl
    import select
else:
    POSIX = False

class SubprocessRepl(object):
    def __init__(self, cmd):
        self.encoder = getencoder('utf8')
        self.decoder = getincrementaldecoder('utf8')()
        self.popen = Popen(cmd, bufsize=1,
            stderr=subprocess.STDOUT, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        if POSIX:
            flags = fcntl.fcntl(self.popen.stdout, fcntl.F_GETFL)
            fcntl.fcntl(self.popen.stdout, fcntl.F_SETFL, flags | os.O_NONBLOCK)

    def is_alive(self):
        return self.popen.poll() is None

    def read_bytes(self):
        out = self.popen.stdout
        if POSIX:
            while True:
                i, _, _ = select.select([out], [], [])
                if i:
                    return out.read(4096)
        else:
            # this is windows specific problem, that you cannot tell if there
            # are more bytes ready, so we read only 1 at a times

            while True:
                byte = self.popen.stdout.read(1)
                if byte == b'\r':
                    # f'in HACK, for \r\n -> \n translation on windows
                    # I tried universal_endlines but it was pain and misery! :'(
                    continue
                return byte

    def write(self, command):
        (bytes, how_many) = self.encoder(command)
        si = self.popen.stdin
        si.write(bytes)
        si.flush()
    
    def reset_decoder(self):
        self.decoder = getincrementaldecoder('utf8')()

    def read(self):
        """Reads at least one decoded char of output"""
        while True:
            bs = self.read_bytes()
            if not bs:
                return None
            try:
                output = self.decoder.decode(bs)
            except Exception as e:
                output = "■"
                self.reset_decoder()
            if output:
                return output
  