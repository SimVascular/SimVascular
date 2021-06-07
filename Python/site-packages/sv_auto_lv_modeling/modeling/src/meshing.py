import os
import vtk
import sv

def mesh_polydata(fn, args, fns_out=None):
    """
    Use SimVascular to mesh a file containing a VTK PolyData and write the volumetric mesh to disk
    Args:
        fn: file name of the VTK PolyData
        args: meshing options, dic
        fns_out: file names of the output mesh (poly_fn, ug_fn)
    Returns:
        return (surface, volumetric)
    """    

    mesher = sv.meshing.TetGen()
    mesher.load_model(fn)
    mesher.set_walls([1])
    face_ids = mesher.get_model_face_ids()
    options = sv.meshing.TetGenOptions(**args)
    options.no_merge = True
    options.no_bisect = True
    options.optimization = 3
    options.quality_ratio = 1.4
    mesher.generate_mesh(options)
    volumetric = mesher.get_mesh()
    surface = mesher.get_surface()
    return (surface, volumetric)

def remesh_polydata(poly, hmin, hmax,write_fn=None):
    """
    Use SimVascular MMG remesh to remesh a surfac mesh
    
    Args:
        poly: input polydata
        hmin: min edge size
        hmax: max edge size
        write: output filename 
    Returns:
        remeshed: remeshed polydata
    """
    remeshed = sv.mesh_utils.remesh(poly, hmin=hmin, hmax=hmax)
    if write_fn is not None:
        write_vtk_polydata(remeshed, write_fn)
    return remeshed

