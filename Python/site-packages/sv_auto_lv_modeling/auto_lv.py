import os
import sys
import glob
from typing import List
from auto_lv.segmentation.prediction import seg_main
from auto_lv.modeling.surface_main import build_lv_model_from_image
from auto_lv.modeling.volume_mesh_main import create_volume_mesh

class Segmentation(object):

    def __init__(self):
        self.size = 256
        self.n_chanel = 1
    def set_modality(self, modality='ct'):
        if modality not in ['ct', 'mr']:
            raise ValueError('Incorrect image modality type: {}. Modality should be ct or mr'.format(modality))
        self.modality = [modality]
    def set_patient_id(self, p_id: str):
        self.p_id = p_id
    def set_image_directory(self, data_dir: str):
        self.data_dir = data_dir
    def set_output_directory(self, output_dir: str):
        self.output_dir = output_dir
    def set_model_directory(self, model_dir: List[str]):
        models = []
        if type(model_dir) != list:
            model_dir = [model_dir]
        for mdl_dir in model_dir:
            print(mdl_dir)
            print(os.path.join(mdl_dir, '*.hdf5'))
            models += glob.glob(os.path.join(mdl_dir, '*.hdf5'))
        if len(models)==0:
            raise ValueError('No models (.hdf5) found in directory {}.'.format(model_dir))
        self.model_dir = model_dir
    def set_view(self, views: List[int]):
        for v in views:
            if v > 2:
                raise ValueError('Invalid view id {}. View id should be 0 (axial), 1 (coronal), or 2 (sagittal).'.format(v))
        self.view_ids = views
    def generate_segmentation(self):
        attr = ['modality', 'p_id', 'data_dir', 'output_dir', 'model_dir', 'view_ids']
        attr_func = ['set_modality', 'set_patient_id', 'set_image_directory', 'set_output_directory', 'set_model_directory', 'set_view']
        attr_name = ['Modality', 'Patient ID', 'Image directory', 'Output directory', 'Network directory', 'View IDs']
        for a, n, f in zip(attr, attr_name, attr_func):
            if not hasattr(self, a):
                raise RuntimeError("{} has not been set. Run '{}'.".format(n, f)) 
        ims = glob.glob(os.path.join(self.data_dir, self.p_id, '*.nii.gz')) + \
                glob.glob(os.path.join(self.data_dir, self.p_id, '*.nii')) + \
                glob.glob(os.path.join(self.data_dir, self.p_id, '*.vti'))
        if len(ims) == 0:
            raise ValueError("No image files of nii.gz, nii or vti found in directory {}.".format(os.path.join(self.data_dir, self.p_id)))
        seg_main(self.size, self.modality, self.p_id, self.data_dir, self.output_dir, self.model_dir, self.view_ids, self.n_chanel)

class Modeling(object):

    def __init__(self):
        pass
    def set_segmentation_directory(self, seg_dir: str):
        ims = glob.glob(os.path.join(seg_dir, '*.nii.gz')) + \
                glob.glob(os.path.join(seg_dir, '*.nii')) + \
                glob.glob(os.path.join(seg_dir, '*.vti'))
        if len(ims) == 0:
            raise ValueError("No segmentation files of nii.gz, nii or vti found in directory {}.".format(data_dir))
        self.seg_dir = seg_dir
    def set_output_directory(self, output_dir: str):
        self.output_dir = output_dir
    def set_max_edge_size(self, edge_size: float):
        self.edge_size = edge_size
    def generate_lv_modes(self):
        attr = ['seg_dir', 'output_dir', 'edge_size']
        attr_func = ['set_segmentation_directory', 'set_output_directory', 'edge_size']
        attr_name = ['Segmentation directory', 'Output directory', 'Edge size']
        for a, n, f in zip(attr, attr_name, attr_func):
            if not hasattr(self, a):
                raise RuntimeError("{} has not been set. Run '{}'.".format(n, f)) 
        seg_fns = glob.glob(os.path.join(self.seg_dir, '*.nii.gz')) + \
                glob.glob(os.path.join(self.seg_dir, '*.nii')) + \
                glob.glob(os.path.join(self.seg_dir, '*.vti'))
        for seg_fn in seg_fns:
            poly_fn = os.path.join(self.output_dir, os.path.basename(seg_fn)+'.vtp')
            build_lv_model_from_image([seg_fn], [poly_fn], edge_size=self.edge_size, use_SV=True)

class VolumeMesh(object):

    def __init__(self):
        pass
    def set_output_directory(self, output_dir: str):
        self.output_dir = output_dir
    def set_max_edge_size(self, edge_size: float):
        self.edge_size = edge_size
    def set_surface_model_filename(self, filename: str):
        self.poly_fn = filename
    def generate_volume_mesh(self):
        attr = ['poly_fn', 'edge_size', 'output_dir']
        attr_func = ['set_surface_model_filename', 'set_max_edge_size', 'set_output_directory']
        attr_name = ['Surface filename', 'Edge size', 'Output directory']
        for a, n, f in zip(attr, attr_name, attr_func):
            if not hasattr(self, a):
                raise RuntimeError("{} has not been set. Run '{}'.".format(n, f)) 
        create_volume_mesh(self.poly_fn, self.edge_size, self.output_dir)

if __name__ == '__main__':
    seg = Segmentation()
    seg.set_image_directory('/Users/fanweikong/Documents/Segmentation/Auto-LV-Modeling/01-Images')
