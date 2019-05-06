#import joblib
import numpy as np
import os
import skimage.filters as filters

from modules.vessel_regression import pred_to_contour
import modules.vessel_regression as vessel_regression

from base.preprocessor import AbstractPreProcessor
from base.postprocessor import AbstractPostProcessor
EPS = 1e-5

class BasePreProcessor(AbstractPreProcessor):
    def __call__(self, x):
        if not 'IMAGE_TYPE' in self.config:
            mu  = 1.0*np.mean(x)
            sig = 1.0*np.std(x)+EPS
            x_   = (x-mu)/sig
        else:
            if self.config['IMAGE_TYPE'] == 'EDGE':
                x_ = filters.sobel(x)
                ma = np.amax(x_)
                mi = np.amin(x_)
                x_ = (x_-mi)/(ma-mi+EPS)

            if self.config['IMAGE_TYPE'] == 'HESSIAN':
                x_ = filters.gaussian(x, sigma=self.config['BLUR_SIGMA'])
                x_ = filters.sobel(x_)
                x_ = filters.sobel(x_)
                mu  = 1.0*np.mean(x_)
                sig = 1.0*np.std(x_)+EPS
                x_   = (x_-mu)/sig
                c = self.config['CLIP_VAL']
                x_[x_>c] = c
                x_[x_<-c] = -c

            if self.config['IMAGE_TYPE'] == 'CLIP':
                mu  = 1.0*np.mean(x)
                sig = 1.0*np.std(x)+EPS
                x_   = (x-mu)/sig
                c = self.config['CLIP_VAL']
                x_[x_>c] = c
                x_[x_<-c] = -c

            if self.config['IMAGE_TYPE'] == 'BLUR':
                x_ = filters.gaussian(x, sigma=self.config['BLUR_SIGMA'])
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
