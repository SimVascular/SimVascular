import numpy as np
import vtk
import skimage
from skimage.measure import grid_points_in_poly
import re
import scipy
from scipy.interpolate import UnivariateSpline
from vtk import vtkImageExport
from vtk.util import numpy_support
from scipy.ndimage import rotate

import os

def random_rotate(image_pair):
    angle = np.random.randint(360)
    return_images = []
    for i,im in enumerate(image_pair):
        return_images.append(rotate(im,angle,axes=(1,0),reshape=False))

    return tuple(return_images)

def qc_deviation(c):
    p = np.mean(c,axis=0)

    c_cent = c-p
    b = np.sqrt(np.sum(c_cent**2,axis=1))
    r = np.mean(b)

    v = np.sqrt(np.mean((b-r)**2))/(r+1e-3)

    return v

def mkdir(fn):
    if not os.path.exists(os.path.abspath(fn)):
        os.mkdir(os.path.abspath(fn))

def window_image(image,center,window):
    start_ = center-float(window)/2
    end_   = center+float(window)/2
    x = image.copy()

    x = (1.0*x)/window + (0.5-float(center)/window)
    x[image <= start_] = 0.0
    x[image > end_] = 1.0
    return x

def smoothContour(c, num_modes=10):
    if len(c) < 3:
        return np.array([[0.0,0.0],[0.0,0.0]]).T
    x = c[:,0]
    y = c[:,1]
    mu = np.mean(c,axis=0)

    x = x-mu[0]
    y = y-mu[1]

    xfft = np.fft.fft(x)
    yfft = np.fft.fft(y)

    xfft[num_modes:] = 0
    yfft[num_modes:] = 0

    sx = 2*np.fft.ifft(xfft)+mu[0]
    sy = 2*np.fft.ifft(yfft)+mu[1]

    return np.array([np.real(sx),np.real(sy)]).T

def crop_center(img,cropx,cropy):
    y,x = img.shape
    startx = x//2-(cropx//2)
    starty = y//2-(cropy//2)
    return img[starty:starty+cropy,startx:startx+cropx]

def vtk_image_to_numpy(im):

    H,W,D = im.GetDimensions()
    sc = im.GetPointData().GetScalars()
    a = numpy_support.vtk_to_numpy(sc)
    a = a.reshape(H, W, D)

    assert a.shape==im.GetDimensions()
    return a

def normalizeContour(c,p,t,tx, as_list=False):
    """
    uses simvascular path info to transform contour into local 2d coordinates

    args:
        c (np array, (num points x 3)) - 3d contour to transform
        p (np array 1x3) - 3d origin of contour
        t (np array 1x3) - normal vector of 3d contour
        tx (np array 1x3) - vector in 3d contour plane

    returns:
        res (np array, (num points x 3)) - 3d contour
    """

    c = list(c)

    ty = np.cross(t,tx)
    ty = ty/np.linalg.norm(ty)

    c_p = [np.array(k)-np.array(p) for k in c]

    if as_list:
        res = [[k.dot(tx), k.dot(ty)] for k in c_p]
    else:
        res = np.array([(k.dot(tx), k.dot(ty)) for k in c_p])
    #print '{}\n{}\n{}\n{}\n{}\n{}\n{}\n'.format(p,t,tx,ty,c,c_p,res)
    return res

def interpContour(c,num_pts=15, k=1):
    c = smoothContour(c, num_modes=5)
    angles = np.arctan2(c[:,1],c[:,0])
    angles = angles/np.pi
    bbox = [-1.0,1.0]

    delta = 2.0/num_pts
    new_angles = np.linspace(-0.95,0.95,num_pts)
    inds = angles.argsort()
    angles=angles[inds]
    x = c[:,0]
    y = c[:,1]
    x = x[inds]
    y = y[inds]

    #angles = np.r_[angles[-1]-2, angles, angles[0]+2]
    #x = np.r_[x[-1],x,x[0]]
    #y = np.r_[y[-1],y,y[0]]

    #angles = np.r_[angles[-1]-2, angles]
    #x = np.r_[x[-1],x]
    #y = np.r_[y[-1],y]

    angles = angles[1:-1]
    x = x[1:-1]
    y = y[1:-1]

    x_spline = scipy.interpolate.UnivariateSpline(angles,x,s=0,k=k)
    y_spline = scipy.interpolate.UnivariateSpline(angles,y,s=0,k=k)

    new_c = np.zeros((num_pts,2))
    new_c[:,0] = x_spline(new_angles)
    new_c[:,1] = y_spline(new_angles)
    return new_c.copy()

def normalize_grps(grp_dict,path_info):
    norm_grps = []

    for i in sorted(grp_dict.keys()):

            vecs = path_info[i]
            vecs_g = grp_dict[i]['points']
            p = vecs[:3]
            t = vecs[3:6]
            tx= vecs[6:]
            c = grp_dict[i]['contour']

            contour_norm = normalizeContour(c,p,t,tx)
            norm_grps.append(contour_norm)
    means = [np.mean(c,axis=0) for c in norm_grps]

    norm_grps = [norm_grps[i]-means[i] for i in range(len(norm_grps))]
    return norm_grps, means

def parseGroupFile(fn):
    """
    parses a simvascular groups file
    point_number:(points:9x1 array, contour:3xN array)
    """
    f = open(fn).readlines()
    f = [i.replace('\r\n','') for i in f]
    f = [i.replace('\n','') for i in f]
    nrmExpr = 'nrm {.*}'
    posExpr = 'pos {.*}'
    xhatExpr = 'xhat {.*}'

    group = {}
    for i in range(len(f)):
        if 'xhat' in f[i]:
            #print f[i]
            group_num = int(f[i-1])

            s = f[i]

            xhat_string = re.search(xhatExpr, s).group()
            xhat_string = xhat_string.split('}')[0]
            xhat = [float(x) for x in xhat_string[6:].split(' ')]

            # pos_string = re.search(posExpr, s).group()
            # pos_string = pos_string.split('}')[0]
            # pos = [float(x) for x in pos_string[5:].split(' ')]

            nrm_string = re.search(nrmExpr, s).group()
            nrm_string = nrm_string.split('}')[0]
            nrm = [float(x) for x in nrm_string[5:].split(' ')]


            group[group_num] = {}
            group[group_num]['contour'] = []
            j = i+1
            while f[j] != '':

                tup = [float(x) for x in f[j].split(' ')]
                group[group_num]['contour'].append(tup)
                j = j+1

            group[group_num]['contour'] = np.array(group[group_num]['contour'])

            pos = np.mean(group[group_num]['contour'],axis=0)
            group[group_num]['points'] = list(pos) + nrm + xhat
            i = j

    return group

def parsePathFile(fn):
    """
    parses a simvascular 2.0 path file
    str(path_id):(points:Lx9 array, name:grp_name)
    """
    f = open(fn).readlines()

    paths={}

    expr1 = ['set ', 'gPathPoints', '(',')','{','}',',name','\n']
    expr2 = ['{','}','p ','t ', 'tx ', '(', '\\\n',' ']

    for i in range(len(f)):
        if ',name' in f[i]:
            s = f[i]
            s = multi_replace(s,expr1)

            s = s.split(' ')
            if not s[0] in paths:
                paths[s[0]] = {}
                paths[s[0]]['name'] = s[1]
            else:
                paths[s[0]]['name'] = s[1]

        if ',splinePts' in f[i]:
            j = i+1
            key = multi_replace(f[i],expr1).split(',')[0]
            if not key in paths:
                paths[key] = {}
                paths[key]['points'] = []
            else:
                paths[key]['points'] = []

            while 'tx' in f[j]:
                s = f[j]
                s = multi_replace(s,expr2).replace(')',',').split(',')[:-1]
                s = [float(x) for x in s]
                paths[key]['points'].append(s)

                j = j+1
            paths[key]['points'] = np.array(paths[key]['points'])

    return paths

def reinterp_grps(grps_list, num_pts=20):
    reinterp_list = [
        interpContour(c,num_pts) for c in grps_list
    ]
    return reinterp_list

def contourToSeg(contour, origin, dims, spacing):
    '''
    Converts an ordered set of points to a segmentation
    (i.e. fills the inside of the contour), uses the point in polygon method

    args:
    	@a contour: numpy array, shape = (num points, 2), ordered list of points
    	forming a closed contour
    	@a origin: The origin of the image, corresponds to top left corner of image
    	@a dims: (xdims,ydims) dimensions of the image corresponding to the segmentation
    	@a spacing: the physical size of each pixel
    '''
    #print contour
    dims_ = (int(dims[0]),int(dims[1]))
    d = np.asarray([float(dims[0])/2,float(dims[1])/2])

    seg = np.zeros(dims_)

    origin_ = np.asarray([origin[0],origin[1]])
    spacing_ = np.asarray([spacing[0],spacing[1]])
    a = skimage.measure.grid_points_in_poly(dims_,
        (contour[:,:2]-origin_)/spacing_+d)
    seg[a] = 1.0
    return np.flipud(seg.T)

def loft_path(grps_list, grp_points, num_new_points, means, k=1):
    """
    Note grp_points and path_points should be sorted
    """
    xsplines = []
    ysplines = []

    gpts = np.asarray(grp_points)
    gpts = 1.0*gpts/num_new_points

    mx = [m[0] for m in means]
    my = [m[1] for m in means]

    mxspline = UnivariateSpline(gpts,mx,k=k,s=0)
    myspline = UnivariateSpline(gpts,my,k=k,s=0)

    d = 1.0/num_new_points
    ppts = np.arange(0,1,d)

    num_contour_pts = len(grps_list[0])

    for i in range(num_contour_pts):
        x = [g[i,0] for g in grps_list]
        y = [g[i,1] for g in grps_list]

        x_s = UnivariateSpline(gpts,x,k=k,s=0)
        y_s = UnivariateSpline(gpts,y,k=k,s=0)

        xsplines.append(x_s)
        ysplines.append(y_s)

    new_grps = []
    for p in ppts:
        cpts = [[xsplines[i](p),ysplines[i](p)]
                 for i in range(num_contour_pts)]
        new_grps.append(np.asarray(cpts))

    for i,p in enumerate(ppts):
        new_grps[i] += np.asarray([mxspline(p),myspline(p)])

    return new_grps, xsplines,ysplines,mxspline,myspline

def get_segs(path_points, grp_dict, dims, spacing, num_contour_points):

    if len(grp_dict.keys()) == 0: return None

    if np.amax(grp_dict.keys()) >= len(path_points): return None

    norm_grps, means = normalize_grps(grp_dict,path_points)
    for i in range(len(norm_grps)):
        #norm_grps[i][:,0] += spacing[0]
        norm_grps[i][:,1] -= spacing[1]
    if len(norm_grps) > 3:

        interp_grps = reinterp_grps(norm_grps, num_contour_points)
        import pdb; pdb.set_trace()

        segs = [contourToSeg(c,-means[i],dims,spacing) for i,c in
                 enumerate(norm_grps)]

        # segs = [contourToSeg(c,means[i],dims,spacing) for i,c in
        #           enumerate(norm_grps)]

        return segs,norm_grps,interp_grps,means

    else: return None

def loft_path_segs(interp_grps, means, grp_dict, dims, spacing):

    ngroup_points_between = np.amax(grp_dict.keys())-np.amin(grp_dict.keys())

    lofted_grps,xsplines,ysplines,mx,my = loft_path(interp_grps,
                sorted(grp_dict.keys()), ngroup_points_between, means)

    N = len(lofted_grps)

    new_means = np.asarray([[mx(1.0*i/N), my(1.0*i/N)] for i in range(N)])

    origin = [0.0,0.0]
    segs = [contourToSeg(c,-new_means[i],dims,spacing) for i,c in
           enumerate(lofted_grps)]

    return segs,lofted_grps

def multi_replace(s,exprlist):

    for e in exprlist:
        s = s.replace(e,'')
    return s

def resample_image(vtk_im,min_):
    resample = vtk.vtkImageResample();
    spacing = vtk_im.GetSpacing()

    resample.SetInputData(vtk_im)
    for i in range(3):
        resample.SetAxisOutputSpacing(i,min_)
    resample.Update()
    return resample.GetOutput()

def eccentricity(contour):
	'''
	calculates the ratio between minor and major axis of contour

	args:
		@a contour: list of points, shape = (N,2)
	'''
	origin = np.mean(contour,axis=0)
	xcomp = contour[:,0]-origin[0]
	ycomp = contour[:,1]-origin[1]
	dists = np.sqrt(xcomp**2 + ycomp**2)
	dmax = np.max(dists)
	dmin = np.min(dists)
	return dmin/dmax

def anomaly(c,y):
    return eccentricity(c) < ELLIPTICITY or np.sum(y) < GT_CUTOFF

def read_mha(img_fn):
    reader = vtk.vtkMetaImageReader()

    reader.SetFileName(img_fn)
    reader.Update()
    return reader.GetOutput()

def read_vti(img_fn):
    reader = vtk.vtkXMLImageDataReader()

    reader.SetFileName(img_fn)
    reader.Update()
    return reader.GetOutput()

def read_dicom(dicom_dir):
    # fnames = os.listdir(dicom_dir)
    # fnames = [f for f in fnames if ".dcm" in f]
    # fnames = sorted(fnames)
    # fnames = [os.path.join(dicom_dir,f) for f in fnames]

    reader = vtk.vtkDICOMImageReader()
    reader.SetDirectoryName(dicom_dir)
    reader.Update()

    return reader.GetOutput()

def getImageReslice(img, ext, p, n, x, spacing, asnumpy=False):
    """
    gets slice of an image in the plane defined by p, n and x

    args:
        @a img: vtk image (3 dimensional)
        @a ext: extent of the reslice plane [Xext,Yext]
        @a p ((x,y,z)): origin of the plane
        @a n ((x,y,z)): vector normal to plane
        @a x ((x,y,z)): x-axis in the plane

    returns:
        ret (itk image): image in the slice plane
    """
    reslice = vtk.vtkImageReslice()
    reslice.SetInputData(img)
    reslice.SetInterpolationModeToLinear()

    #Get y axis and make sure it satisfies the left hand rule
    tr = vtk.vtkTransform()
    tr.RotateWXYZ(-90,n)
    y = tr.TransformPoint(x)

    reslice.SetResliceAxesDirectionCosines(
        x[0],x[1],x[2],y[0],y[1],y[2],n[0],n[1],n[2])
    reslice.SetResliceAxesOrigin(p[0],p[1],p[2])

    #delta_min = min(img.GetSpacing())
    #delta_min = 0.025
    px = spacing*ext[0]
    py = spacing*ext[1]

    reslice.SetOutputSpacing((spacing,spacing,spacing))
    reslice.SetOutputOrigin(-0.5*px,-0.5*py,0.0)
    reslice.SetOutputExtent(0,ext[0],0,ext[1],0,0)

    reslice.Update()
    #print asnumpy
    if asnumpy:
         return VTKSPtoNumpy(reslice.GetOutput())
    else:
        return reslice.GetOutput()

def VTKSPtoNumpy(vol):
    '''
    Utility function to convert a VTK structured points (SP) object to a numpy array
    the exporting is done via the vtkImageExport object which copies the data
    from the supplied SP object into an empty pointer or array

    C/C++ can interpret a python string as a pointer/array

    This function was shamelessly copied from
    http://public.kitware.com/pipermail/vtkusers/2002-September/013412.html
    args:
    	@a vol: vtk.vtkStructuredPoints object
    '''
    exporter = vtkImageExport()
    exporter.SetInputData(vol)
    dims = exporter.GetDataDimensions()
    if np.sum(dims) == 0:
        return np.zeros((1,64,64))
    if (exporter.GetDataScalarType() == 3):
    	dtype = UnsignedInt8
    if (exporter.GetDataScalarType() == 4):
    	dtype = np.short
    if (exporter.GetDataScalarType() == 5):
    	dtype = np.int16
    if (exporter.GetDataScalarType() == 10):
    	dtype = np.float32
    if (exporter.GetDataScalarType() == 11):
    	dtype = np.float64

    D = 1
    for ddd in dims: D *= ddd

    a = np.zeros(D,dtype)
    s = a.tostring()
    exporter.SetExportVoidPointer(s)
    exporter.Export()
    a = np.reshape(np.fromstring(s,dtype),(dims[2],dims[0],dims[1]))
    return a[0]

def marchingSquares(img, iso=0.0, mode='center'):
    s = img.shape
    alg = vtk.vtkMarchingSquares()

    sp = VTKNumpytoSP(img)

    alg.SetInputData(sp)
    alg.SetValue(0,iso)
    alg.Update()
    pds = alg.GetOutput()

    a = vtk.vtkPolyDataConnectivityFilter()
    a.SetInputData(pds)

    if mode=='center':
        a.SetExtractionModeToClosestPointRegion()
        a.SetClosestPoint(float(s[0])/2,float(s[1])/2,0.0)

    elif mode=='all':
        a.SetExtractionModeToAllRegions()

    a.Update()
    pds = a.GetOutput()

    if pds.GetPoints() is None:
        return np.asarray([[0.0,0.0],[0.0,0.0],[0.0,0.0]])
    else:
        pds = VTKPDPointstoNumpy(pds)
        if len(pds) <= 1:
            return np.asarray([[0.0,0.0],[0.0,0.0],[0.0,0.0]])
        return pds

def VTKPDPointstoNumpy(pd):
	'''
	function to convert the points data of a vtk polydata object to a numpy array

	args:
		@a pd: vtk.vtkPolyData object
	'''
	return numpy_support.vtk_to_numpy(pd.GetPoints().GetData())

def reorder_contour(c):
    N = len(c)
    if N <= 2:
        return c
    even_inds = np.arange(0,N,2)
    odd_inds = np.arange(1,N,2)

    even_points = np.asarray([c[i] for i in even_inds])
    odd_points = np.asarray([c[i] for i in odd_inds])

    N_even = len(even_points)
    ret = np.zeros_like(c)
    ret[:N_even] = even_points
    ret[N_even:] = np.flipud(odd_points)
    ret = ret[:-2]
    return ret.copy()

def VTKNumpytoSP(img_):
    img = img_.T

    H,W = img.shape

    sp = vtk.vtkStructuredPoints()
    sp.SetDimensions(H,W,1)
    sp.AllocateScalars(10,1)
    for i in range(H):
        for j in range(W):
            v = img[i,j]
            sp.SetScalarComponentFromFloat(i,j,0,0,v)

    return sp

from skimage.measure import grid_points_in_poly
def contourToSeg(contour, origin, dims, spacing):
    '''
    Converts an ordered set of points to a segmentation
    (i.e. fills the inside of the contour), uses the point in polygon method

    args:
    	@a contour: numpy array, shape = (num points, 2), ordered list of points
    	forming a closed contour
    	@a origin: The origin of the image, corresponds to top left corner of image
    	@a dims: (xdims,ydims) dimensions of the image corresponding to the segmentation
    	@a spacing: the physical size of each pixel
    '''
    #print contour
    dims_ = (int(dims[0]),int(dims[1]))
    d = np.asarray([float(dims[0])/2,float(dims[1])/2])

    seg = np.zeros(dims_)

    origin_ = np.asarray([origin[0],origin[1]])
    spacing_ = np.asarray([spacing[0],spacing[1]])
    a = grid_points_in_poly(dims_,
        (contour[:,:2]-origin_)/spacing_+d)
    seg[a] = 1.0
    return np.flipud(seg.T)

def denormalizeContour(c,p,t,tx):
    """
    uses simvascular path info to transform a contour from 2d to 3d

    args:
        c (np array, (num points x 2)) - contour to transform
        p (np array 1x3) - 3d origin of contour
        t (np array 1x3) - normal vector of 3d contour
        tx (np array 1x3) - vector in 3d contour plane

    returns:
        res (np array, (num points x 3)) - 3d contour
    """
    c = np.array(c)
    if c.shape[1] == 2:
        c = np.hstack((c, np.zeros((c.shape[0],1))))
    p = np.array(p)
    t = np.array(t)
    tx = np.array(tx)

    ty = np.cross(t,tx)
    ty = ty/np.linalg.norm(ty)

    res = np.array([p + k[0]*tx + k[1]*ty for k in c])
    return res[:-1]
