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
#import joblib
import numpy as np
import os
#import skimage.filters as filters

from sv_ml.modules.vessel_regression import pred_to_contour
import sv_ml.modules.vessel_regression as vessel_regression

from sv_ml.base.preprocessor import AbstractPreProcessor
from sv_ml.base.postprocessor import AbstractPostProcessor
EPS = 1e-5

def gauss_filter(img, sigma=3, trunc=4):
    b = sigma*trunc
    x = np.arange(-b,b+1)

    X,Y = np.meshgrid(x,x)

    d = (X**2+Y**2)/(2*sigma**2)

    f = 1.0/(2*np.pi*sigma**2)*np.exp(-d)

    H,W = img.shape
    im_pad = np.zeros((H+2*b, W+2*b))
    im_pad[b:H+b,b:W+b] = img

    out = np.zeros((H,W))
    #print(f.shape)
    for i in range(b,H+b):
        for j in range(b,W+b):
            out[i-b,j-b] = np.sum(im_pad[i-b:i+b+1,j-b:j+b+1]*f)

    return out

class BasePreProcessor(AbstractPreProcessor):
    def __call__(self, x):
        if not 'IMAGE_TYPE' in self.config:
            mu  = 1.0*np.mean(x)
            sig = 1.0*np.std(x)+EPS
            x_   = (x-mu)/sig
        else:
            # if self.config['IMAGE_TYPE'] == 'EDGE':
            #     x_ = filters.sobel(x)
            #     ma = np.amax(x_)
            #     mi = np.amin(x_)
            #     x_ = (x_-mi)/(ma-mi+EPS)
            #
            # if self.config['IMAGE_TYPE'] == 'HESSIAN':
            #     x_ = filters.gaussian(x, sigma=self.config['BLUR_SIGMA'])
            #     x_ = filters.sobel(x_)
            #     x_ = filters.sobel(x_)
            #     mu  = 1.0*np.mean(x_)
            #     sig = 1.0*np.std(x_)+EPS
            #     x_   = (x_-mu)/sig
            #     c = self.config['CLIP_VAL']
            #     x_[x_>c] = c
            #     x_[x_<-c] = -c
            #
            # if self.config['IMAGE_TYPE'] == 'CLIP':
            #     mu  = 1.0*np.mean(x)
            #     sig = 1.0*np.std(x)+EPS
            #     x_   = (x-mu)/sig
            #     c = self.config['CLIP_VAL']
            #     x_[x_>c] = c
            #     x_[x_<-c] = -c

            if self.config['IMAGE_TYPE'] == 'BLUR':
                #x_ = filters.gaussian(x, sigma=self.config['BLUR_SIGMA'])
                x_  = gauss_filter(x, sigma=self.config['BLUR_SIGMA'])
                mu  = 1.0*np.mean(x_)
                sig = 1.0*np.std(x_)+EPS
                x_   = (x_-mu)/sig

        x_ = x_.reshape(self.config['INPUT_DIMS'])

        return x_.copy()

class BasePostProcessor(AbstractPostProcessor):
    def setup(self):
        self.scale = self.config['CROP_DIMS']*self.config['SPACING']/2

    def __call__(self,y):
        c = pred_to_contour(y)
        return c*self.scale

class PointPostProcessor(AbstractPostProcessor):
    def setup(self):
        self.scale = self.config['CROP_DIMS']*self.config['SPACING']/2

    def __call__(self,y):
        c = vessel_regression.point_pred_to_contour(y)
        return c*self.scale
