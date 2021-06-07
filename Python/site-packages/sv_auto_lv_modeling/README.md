# Automating Model Generation For Image-Based Cardiac Flow Simulation

This repository contains the source code for our paper:

Kong, F., and Shadden, S. C. (August 7, 2020). "Automating Model Generation for Imagebased Cardiac Flow Simulation." ASME. J Biomech Eng. doi: https://doi.org/10.1115/1.4048032

The code repository consists of two parts

* Deep-learning based automatic segmentation of 3D CT/MR image data using an ensemble of 2D UNets
* Down-stream automatic model generation from segmentations for LV CFD simulations 

## Dependencies

* Segmentation and Model Generation
    * SimVascular
       * Tensorflow (V 1.14)
       * VTK
       * Numpy
* Model Registration
    * [SimpleElastix](https://github.com/SuperElastix/SimpleElastix) (Registration)<sup>1</sup>
       * SimpleITK 
<sup>1</sup>  Install SimpleElastix commit 8244e0001f4137514b0f545f1e846910b3dd7769.

## Segmentation Usage 

The segmentation models can generate segmentations for LV blood pool, LV myocardium, LA, RA, RV blood pool, aorta and pulmonary artery.
### Input Requirements
The preferred input format of the image volumes is **.nii.gz or nii**. VTK image volumes (.vti) are also accepted; however they should be reoriented to have an orientation matrix of identity. This is because the segmnetation method requires identity-oriented image volumes while the version of VTK within SimVascular does not include orientation matrix with VTI images.
The directory containing the input image data should be organized as follows:
```
image_dir
     |__ patient_id (optional)
         |__ image_volume0.nii.gz
         |__ image_volume1.nii.gz
         |__ image_volume2.nii.gz
         |__ ...
```
### Trained Models
We used the image and ground truth data provided by [MMWHS](http://www.sdspeople.fudan.edu.cn/zhuangxiahai/0/mmwhs/) to train our models. 
Our segmentation models were trainined simultaneously on CT and MR data and trained weights are [here](https://drive.google.com/open?id=162Xr5OezSZL-0K3aoYO7WnHWuGTEXkkj). 

### Prediction
To generate segmentations for 3D CT or MR image volumes:
```
python Segmentation/prediction.py \
    --pid patient_id \ # Patient ID.
    --image image_dir \ # the images should be saved in proper format in a folder named as patient_id within image_dir. 
    --output output_dir \
    --model weight_dir \
    --view 0 1 2 \ # Use models trained on axial (0), coronal (1) and/or sagittal (2) view[s].
    --modality ct # Image modality, ct or mr.
```
A shell script (`segmentation.sh`) is provided for ease of use. Patient ID is optional.

## LV Modeling Usage

The model construction pipeline takes in the generated segmentation and output reconstructed LV surface meshes for CFD simulations. The pipeline consists of the following four steps: 1) Construct LV surface meshes from segmentation results; 2) Register the surface meshes to get consistent mesh topology; 3) Obtain volumetric mesh using SimVascular; 4) Interpolate the registered surface meshes to obtain sufficient temporal resolution.

### 1.  Construct LV Surface Meshes with Tagged Boundary Faces
* Update `surfaces.sh` with correct file and folder names.
* Run the shell script to generate a LV surface mesh for each segmentation file in a folder.   
    ```
    sv_python_dir=/usr/local/bin
    model_script=Modeling/main.py
    dir=./examples/ct_test_seg
    for file in ${dir}/*.nii.gz; do echo ${file} &&  ${sv_python_dir}/simvascular --python -- ${model_script} --input_dir ${dir} --output_dir ${output_dir} --seg_name ${file##*/} --edge_size 2.5; done
    ```
* Use `--disable_SV` to turn off SimVascular (no remeshing would be performed). 
    ```
    for file in ${dir}/*.nii.gz; do echo ${file} &&  ${sv_python_dir}/simvascular --python -- ${model_script} --input_dir ${dir} --output_dir ${output_dir} --seg_name ${file##*/} --edge_size 2.5 --disable_SV; done
    ```

### 2.  Construct Point Corresponded LV Meshes from 4D Images
Building point-corresponded LV meshes require segmentations from all time frames. One surface mesh will be created at one time frame and propagated to the others by registering the corresponding segmentations. 
* Update `surface_registration.sh` with correct file and folder names. Specify the time phase id to construct LV surface mesh.
* Run `elastix_main.py` through the shell script.

### 3.  Volumetric Meshing using SimVascular 
* Update `volume_mesh.sh` with correct file/folder names and mesh edge size.

*  Run `volume_mesh_main.py` through the shell script.
   

### 4.  Generate Mesh Motion File for [svFSI](https://github.com/SimVascular/svFSI)
* Run `simulation.sh` to generate mesh motion file.
  Input: Surface mesh from segmented geometry with the same connectivity.
  Output: Displacement files for all the surfaces in this [format](https://simvascular.github.io/docssvFSI.html#app_app_prescribed_wall_motion).

## Acknowledgement
This work was supported by the NSF, Award #1663747. 

