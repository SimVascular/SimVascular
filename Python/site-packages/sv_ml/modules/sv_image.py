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
import sv_ml.modules.vascular_data as sv
import numpy as np
import os

class Image(object):
    def __init__(self, filename):

        if not os.path.exists(filename):
            raise RuntimeError("path {} does not exist".format(filename))

        if ".mha" in filename:
            self.image = sv.read_mha(filename)

        elif ".vti" in filename:
            self.image = sv.read_vti(filename)

        elif ".dcm" in filename:
            d = os.path.dirname(os.path.abspath(filename))
            self.image = sv.read_dicom(d)

        elif os.path.isdir(filename):
            #assumed dicom dir
            self.image = sv.read_dicom(filename)

        else:
            raise RuntimeError("unrecognized image format {}".format(filename))

    def set_spacing(self, spacing):
        self.spacing = spacing

    def set_reslice_ext(self, ext):
        self.ext = [ext-1]*2

    def get_reslice(self, p, n, v):
        return sv.getImageReslice(self.image, self.ext,
            p, n, v, self.spacing, asnumpy=True)

    def get_reslices(self, plist, nlist, vlist):
        reslices = []
        for p,n,v in zip(plist,nlist,vlist):
            x = self.get_reslice(p,n,v)
            reslices.append(x.copy())

        return np.array(reslices)
