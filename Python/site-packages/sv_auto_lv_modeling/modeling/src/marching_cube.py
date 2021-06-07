"""
Use marching cube algorithm to create iso-surface of label map

@author Fanwei Kong
"""
import vtk
import utils
import numpy as np

def marching_cube(label, tol):
    """
    Args:
        label: numpy array of label map
        tol: threshold value for iso-surface
    Returns
        mesh: tuple containing outputs of marching cube algorithm
    """
    from skimage import measure

    verts, faces, normals, values = measure.marching_cubes_lewiner(label, tol)
    
    return (verts, faces, normals, values)

def vtk_marching_cube(vtkLabel, tol, smooth=None, band=None):
    """
    Use the VTK discreate marching cube implementation to create the surface mesh

    Args:
        vtkLabel: vtk structured array containing the label map
        tol: threshold value for iso-surface
        smooth: smoothing iterations
    Returns:
        mesh: vtk PolyData of the surface mesh
    """
    contour = vtk.vtkDiscreteMarchingCubes()
    contour.SetInputData(vtkLabel)
    contour.SetValue(0, tol)
    contour.Update()

    mesh = contour.GetOutput()
    
    if smooth is not None:
        if band is not None:
            mesh = utils.windowed_sinc_smooth_vtk_polydata(mesh, smooth, band)
        else:
            mesh = utils.smooth_vtk_polydata(mesh, smooth)
    return mesh

def vtk_continuous_marching_cube(vtkLabel, tol, smooth=None, band=None):
    """
    Use the VTK marching cube implementation to create the surface mesh

    Args:
        vtkLabel: vtk structured array containing the label map
        tol: threshold value for iso-surface
        smooth: smoothing iterations
    Returns:
        mesh: vtk PolyData of the surface mesh
    """
    contour = vtk.vtkMarchingCubes()
    contour.SetInputData(vtkLabel)
    contour.SetValue(0, tol)
    contour.Update()

    mesh = contour.GetOutput()

    if smooth is not None:
        if band is not None:
            mesh = utils.windowed_sinc_smooth_vtk_polydata(mesh, smooth, band)
        else:
            mesh = utils.smooth_vtk_polydata(mesh, smooth)
    return mesh

def vtk_marching_cube_multi(vtkLabel, bg_id, smooth=None, band=None):
    """
    Use the VTK marching cube to create isosrufaces for all classes excluding the background

    Args:
        labels: vtk image contraining the label map
        bg_id: id number of background class
        smooth: smoothing iteration
    Returns:
        mesh: vtk PolyData of the surface mesh
    """
    from vtk.util.numpy_support import vtk_to_numpy
    ids = np.unique(vtk_to_numpy(vtkLabel.GetPointData().GetScalars()))
    ids = np.delete(ids, np.where(ids==bg_id))

    #smooth the label map
    #vtkLabel = utils.gaussianSmoothImage(vtkLabel, 2.)

    contour = vtk.vtkDiscreteMarchingCubes()
    contour.SetInputData(vtkLabel)
    for index, i in enumerate(ids):
        print("Setting iso-contour value: ", i)
        contour.SetValue(index, i)
    contour.Update()
    mesh = contour.GetOutput()

    if smooth is not None:
        if band is not None:
            mesh = utils.windowed_sinc_smooth_vtk_polydata(mesh, smooth, band)
        else:
            mesh = utils.smooth_vtk_polydata(mesh, smooth)

    return mesh

def vtk_marching_cube_union(vtkLabel, bg_id, smooth=True):
    """
    Use the VTK marching cube to create isosrufaces for all classes excluding the background

    Args:
        labels: vtk image contraining the label map
        bg_id: id number of background class
        smooth: whether to smooth
    Returns:
        model: vtk PolyData of the surface mesh
    """
    from vtk.util.numpy_support import vtk_to_numpy
    ids = np.unique(vtk_to_numpy(vtkLabel.GetPointData().GetScalars()))
    ids = np.delete(ids, np.where(ids==bg_id))
    
    model = vtk.vtkPolyData()
    from utils import boolean_vtk_polydata
    for index, i in enumerate(ids):
        mesh = vtk_marching_cube(vtkLabel, i, False)
        if model.GetNumberOfCells()==0:
            model.ShallowCopy(mesh)
        print("Processing iso-contour value: ", i)
        model = boolean_vtk_polydata(model, mesh, 'union')
        
    if smooth:
        model = utils.smooth_vtk_polydata(model)

    return model
    
    
