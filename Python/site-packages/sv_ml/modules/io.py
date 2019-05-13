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
#
import yaml
import csv
import os
import json

def mkdir(fn):
    if not os.path.exists(os.path.abspath(fn)):
        os.mkdir(os.path.abspath(fn))

def read_lines(fn):
    f = open(fn,'r').readlines()
    f = [s.replace('\n','') for s in f]
    return f


def load_yaml(fn):
    """loads a yaml file into a dict"""
    with open(fn,'r') as file_:
        try:
            return yaml.load(file_)
        except RuntimeError as e:
            print("failed to load yaml fille {}, {}\n".format(fn,e))

def save_yaml(fn, data):
    with open(fn,'w') as file_:
        yaml.dump(data,file_, default_flow_style=False)

def write_csv(filename,dict):
    with open(filename,'w') as f:
        w = csv.DictWriter(f,dict.keys())
        w.writeheader()
        w.writerow(dict)

def read_csv(filename):
    with open(filename,'r') as f:
        w = csv.DictReader(f)
        d = [row for row in w]
        if len(d)==1:
            return d[0]
        else:
            return d

def write_json(data,fn):
    with open(fn,'w') as f:
        json.dump(data,f, indent=2)

def load_json(fn):
    with open(fn,'r') as f:
        return json.load(f)
