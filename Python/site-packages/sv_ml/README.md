# SimVascular Machine Learning Segmentation

This folder contains the python code related to the machine learning 2D segmentation in SimVascular.

Other files related to this code are in
```
/Code/Source/sv4gui/Modules/MachineLearning
```
These files mainly wrap the C++-Python interface to load the python machine learning code and expose it to the SimVascular interface which is in C++.

and
```
/Code/Source/sv4gui/Plugins/org.sv.gui.qt.segmentation
```
These are the main segmentation code files. They connect the machine learning code (and other segmentation methods) to the buttons in the SimVascular interface and manage the created data objects.

## Machine Learning Python Code
The main interface to the SimVascular machine learning code is in the file `sv_wrapper.py`.
This file will load a neural network and exposes methods to specify a medical image volume and vessel path points to segment.

The folder `/config` contains configuration files for the neural network.

`/factories` contains several factory classes to load image preprocessors, neural networks and output postprocessors.

`/components` contains the classes for the neural network itself, these wrap the raw Tensorflow code.

`/base` contains several abstract classes used in the other files.

### How does segmentation work?
The SV python machine learning code uses a convolutional network to produce 2D cross-sectional lumen segmentations.
The input to the neural network is a 2D image of a vessel cross-section extracted from the perpendicular plane
to the vessel pathline at a given point.
The cross-sectional images are extracted using the VTK image reslice function.
The Neural network outputs a vector of 30 numbers indicating the distance of the vessel lumen from the vessel pathline
at angular intervals.
To get the final lumen segmentation these distances are postprocessed into 3D coordinates using the pathline information.

For more details the paper can be viewed [here](https://www.dropbox.com/s/bzzrsepbyzg7l6f/cvet_2020.pdf?dl=0)

For details on the training and testing data used see the paper and the repository [here](https://github.com/gmaher/vascular_data)
### Example use in Python for a single path point
```python
import os
import sys

SV_PATH       = '/Path/to/SimVascular/Python/site-packages'
sys.path.append(SV_PATH)

from sv_ml import sv_wrapper

NET_FN = "googlenet_c30_train300k_aug10_clean"
IMAGE_FN = "/path/to/Image/image_name.vti" #also supports .mha and DICOM

sw = sv_wrapper.SVWrapper(NET_FN)

sw.set_image(IMAGE_FN)

import json

d = {
    "p":[0.0,0.0,0.0], #path point
    "n":[1.0,0.0,0.0], #normal to the cross-sectional plane
    "tx":[0.0,1.0,0.0] #tangent to the cross-sectional plane
}

d_s = json.dumps(d)

ctr_pts_s = sw.segment(d_s)

ctr_pts = json.loads(ctr_pts_s)

print(type(ctr_pts))
print(ctr_pts.keys())
print(ctr_pts['points'])
```
