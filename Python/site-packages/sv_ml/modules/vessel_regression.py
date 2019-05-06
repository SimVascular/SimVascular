import numpy as np

def pred_to_contour(pred):
    #assume -1,1 extent
    Npoints  = len(pred)
    radiuses = pred
    angles   = np.linspace(-0.95,0.95, Npoints)*np.pi

    c = np.zeros((Npoints,2))

    c[:,0] = np.cos(angles)*radiuses
    c[:,1] = np.sin(angles)*radiuses

    return c

def point_pred_to_contour(pred):
    #assume -1,1 extent
    Npoints  = len(pred)-2
    p = (pred[:2]-0.5)*2
    radiuses = pred[2:]
    angles   = np.linspace(-0.95,0.95, Npoints)*np.pi

    c = np.zeros((Npoints,2))

    c[:,0] = np.cos(angles)*radiuses
    c[:,1] = np.sin(angles)*radiuses

    return c+p

def edge_fit(E, C, angles, alpha, R, Nsteps):
    na     = len(angles)
    Cnew   = np.zeros((na,2))

    coeffs = np.linspace(-alpha*R,alpha*R,2*Nsteps)

    for i,a in enumerate(angles):
        d = np.array([np.cos(a), np.sin(a)])

        c = C[i]

        v = d[np.newaxis,:]*coeffs[:,np.newaxis]

        z = v+c[np.newaxis,:]

        e = np.array([E(x) for x in z])

        k = np.argmax(e)

        Cnew[i,:] = z[k]

    return Cnew.copy()

class Interpolant(object):
    def __init__(self, image, origin, spacing, outside_value=0):
        self.image   = image
        self.extent  = image.shape
        self.origin  = origin
        self.spacing = spacing
        self.outside_value = outside_value

    def __call__(self, x):
        p     = x-self.origin

        coord = p/self.spacing
        coord = np.array([coord[1], coord[0]])

        index = coord.astype(int)

        i1    = index[0]
        i2    = i1+1
        j1    = index[1]
        j2    = j1+1

        if i2 > self.extent[0]-1 or j2 > self.extent[1]-1:
            return self.outside_value

        if i1 < 0 or j1 < 0:
            return self.outside_value

        v1    = self.image[i1,j1]
        v2    = self.image[i1,j2]
        v3    = self.image[i2,j2]
        v4    = self.image[i2,j1]

        f1 = (j2-coord[1])*v1 + (coord[1]-j1)*v2
        f2 = (j2-coord[1])*v4 + (coord[1]-j1)*v3

        f  = (i2-coord[0])*f1 + (coord[0]-i1)*f2

        return f
