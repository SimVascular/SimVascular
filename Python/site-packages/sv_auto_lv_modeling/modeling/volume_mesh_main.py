import os
import sys
sys.path.append(os.path.join(os.path.dirname(__file__),"src"))
import argparse
import numpy as np
import glob
import meshing
import models
import io_utils
import time

def create_volume_mesh(poly_fn, edge_size, output_dir):
    lvmodel = models.LeftVentricle(io_utils.read_vtk_mesh(poly_fn))
    output_vol = os.path.join(output_dir, 'mesh-complete')
    lvmodel.remesh(edge_size, poly_fn, poly_fn=None, ug_fn=output_vol, mmg=False)
    lvmodel.write_mesh_complete(output_vol)

if __name__ == '__main__':
    from utils import natural_sort
    start = time.time()
    parser = argparse.ArgumentParser()
    
    parser.add_argument('--input_dir', help="Path to the surface meshes")
    parser.add_argument('--output_dir', help="Path to the volume meshes")
    parser.add_argument('--model_out', help="Name format of surface")
    parser.add_argument('--edge_size', type=float, help="Maximum edge size of the volumetric mesh.")
    parser.add_argument('--phase', default=-1, type=int, help="Id of the phase to generate volume mesh")
    args = parser.parse_args()
    
    input_dir = args.input_dir

    if args.phase == -1:
        try:
            volume_fn = np.load(os.path.join(input_dir, "volume.npy"))
            phase = volume_fn[:,0][int(np.argmax(volume_fn[:,1]))]
        except:
            print("Mesh volumes not found, the first model will be meshed")
            phase = 0
    else:
        phase = args.phase
    surface_fns = natural_sort(glob.glob(os.path.join(args.input_dir, '*.vtp')))
    poly_fn = surface_fns[int(phase)]
    
    create_volume_mesh(poly_fn, args.edge_size, args.output_dir)
    end = time.time()
    print("Time spent in volume_mesh_main.py: ", end-start)
    print("Mesh generated for ", poly_fn)
