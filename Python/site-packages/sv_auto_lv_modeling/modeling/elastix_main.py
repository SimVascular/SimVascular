import os
import glob
import sys
sys.path.append(os.path.join(os.path.dirname(
__file__), "src"))

import numpy as np
import io_utils
from models import LeftVentricle
from registration import Registration
from utils import natural_sort
import time

def registration(lvmodel, START_PHASE, image_fns, output_dir, mask_fns):
    """
    Registration of surface mesh point set using Elastix
    Performs 3D image registration and move points based on the computed transform
    Cap the surface mesh with test6_2()
    """
    # compute volume of all phases to select systole and diastole:
    TOTAL_PHASE = len(image_fns)
    ids = list(range(START_PHASE,TOTAL_PHASE)) + list(range(0,START_PHASE))
    #ids = [9, START_PHASE]
    reg_output_dir = os.path.join(output_dir, "registration")
    try:
        os.makedirs(reg_output_dir)
    except Exception as e: print(e)

    register = Registration()
    # Only need to register N-1 mesh
    fixed_im_fn = image_fns[START_PHASE]
    fixed_mask_fn = mask_fns[START_PHASE]
    fn_poly = os.path.join(output_dir, os.path.basename(fixed_im_fn)+'.vtp')
    lvmodel.write_surface_mesh(fn_poly)
    volume = list()
    volume.append([START_PHASE,lvmodel.get_volume()])
    image_output_dir = os.path.join(reg_output_dir, "images")
    for index in ids[:-1]:
        print("REGISTERING FROM %d TO %d " % (START_PHASE, (index+1)%TOTAL_PHASE))
    
        #ASSUMING increment is 1
        moving_im_fn = image_fns[(index+1)%TOTAL_PHASE]
        
        register.update_moving_image(moving_im_fn)
        register.update_fixed_image(fixed_im_fn)
        register.update_fixed_mask(fixed_mask_fn)

        try:
            os.makedirs(os.path.join(image_output_dir))
        except Exception as e: print(e)
        fn_out = os.path.join(os.path.join(reg_output_dir), "verts.pts")

        fn_paras = os.path.join(reg_output_dir, str(START_PHASE)+"to"+str((index+1)%TOTAL_PHASE)+'.txt')
        new_lvmodel = register.polydata_image_transform(lvmodel, fn_out, fn_paras)
        register.write_parameter_map(fn_paras)

        #ASSUMING increment is 1
        fn_poly = os.path.join(output_dir, os.path.basename(moving_im_fn)+'.vtp')
        new_lvmodel.write_surface_mesh(fn_poly)
        volume.append([(index+1)%TOTAL_PHASE,new_lvmodel.get_volume()])

    np.save(os.path.join(output_dir, "volume.npy"), volume)
    return

if __name__=='__main__':
    start = time.time()
    import argparse
    parser = argparse.ArgumentParser()
    
    parser.add_argument('--image_dir', help='Path to the ct/mr images or segmentation results')
    parser.add_argument('--mask_dir', help='Path to the mask file')
    parser.add_argument('--surface_dir', help='Path to the unregistered surface meshes')
    parser.add_argument('--output_dir', help='Path to the registered surface meshes')
    parser.add_argument('--start_phase', type=int, help='Phase ID of the surface mesh used as the registration target')
    parser.add_argument('--edge_size', type=float, help='Maximum edge size of the surface mesh')
    parser.add_argument('--image_file_extension', default='nii.gz', help='Extension of the images or segmentation results')
    args = parser.parse_args()
    
    #
    image_fns = natural_sort(glob.glob(os.path.join(args.image_dir, '*.'+args.image_file_extension)))
    mask_fns = natural_sort(glob.glob(os.path.join(args.mask_dir, '*.'+args.image_file_extension)))
    surface_fns = natural_sort(glob.glob(os.path.join(args.surface_dir, '*.vtp')))
    lvmodel = LeftVentricle(io_utils.read_vtk_mesh(surface_fns[args.start_phase]), edge_size=args.edge_size )
    registration(lvmodel, args.start_phase,image_fns,  args.output_dir, mask_fns)
    end = time.time()
    print("Time spent in elastix_main.py: ", end-start)
