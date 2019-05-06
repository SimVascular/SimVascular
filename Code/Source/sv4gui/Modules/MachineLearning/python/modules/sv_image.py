import modules.vascular_data as sv
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
