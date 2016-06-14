#############################################################################
#			      NURBSSurface - 3D                             #
#		       Adam Updegrove (updega2@berkeley.edu)                #
#			       November 1, 2015                             #
#############################################################################
import sys, getopt
import numpy as np
import vtk
import imp
import re
import os

#Import SV Group readings functions
#import SV_Groups.processGroups as SVGroup
#np.set_printoptions(threshold='nan')

## @brief Function to get the zero order basis funcions for given u and p
#  @param u Desired parametric locations of basis functions [0-1]
#  @param deg Degree of bspline
#  @return The N0 or zero order basis functions and the corresponding knot vector
def getzerobasis(u,deg,ktype):
	nc = len(u)
	n = nc - 1
	p = deg
	m = p+n+1
	nk = m+1

	#Calculate the knot vector, uniform interp for middle knots
	if (ktype == 'endderiv'):
		m = m+2
	knots = getknots(u,p,ktype)

	#Get the zero order basis functions
	N0 = np.zeros((nc,m))
	for i in range(nc):
		spots = np.intersect1d( \
			np.where(u[i] >= knots)[0],np.where(u[i] <= knots[1:])[0])
		N0[i,spots] = 1.0

	return N0,knots

## @brief Function to get the p order basis funcions for given u and p
#  @param knots The knot vector for given parametric locations and degree
#  @param u Desired parametric locations of basis functions [0-1]
#  @param deg Degree of bspline
#  @return The Np or p order basis functions
def getpbasis(u,deg,ktype):
	N0,knots = getzerobasis(u,deg,ktype)
	nc = len(u)
	n = nc - 1
	p = deg
	m = p+n+1
	nk = m+1

	#Get the p level basis functions
	Np = np.empty_like(N0)
	Np[:,:] = N0
	for j in range(1,p+1):
		blength = Np.shape[1]
		Nnew = np.zeros((nc,blength-1))
		for i in range(blength-1):
			denom1 = (knots[i+j]-knots[i])
			if (denom1 == 0):
				term1 = 0.0
			else:
				term1 = (u-knots[i])/(knots[i+j]-knots[i])
			denom2 =  (knots[i+j+1]-knots[i+1])
			if (denom2 == 0):
				term2 = 0.0
			else:
				term2 = (knots[i+j+1]-u)/(knots[i+j+1]-knots[i+1])
			Nnew[:,i] = term1*Np[:,i] + term2*Np[:,i+1]
		Np = np.empty_like(Nnew)
		Np[:,:] = Nnew

	return Np,knots

## @brief Function to get the p order basis funcions for given u and p
#  @param knots The knot vector for given parametric locations and degree
#  @param u Desired parametric locations of basis functions [0-1]
#  @param deg Degree of bspline
#  @return The Np or p order basis functions
def getrbasis(u,deg,w,ktype):
	Np,knots = getpbasis(u,deg,ktype)
	nc = len(u)
	n = nc - 1
	p = deg
	m = p+n+1
	nk = m+1
	#-----------------#
	# For now using weight of one everywhere
	weight = 1.0
	if (ktype == 'endderiv'):
		nc = nc+2

	sumNp = np.zeros((nc))
	Rp = np.empty_like(Np)
	#for i in range(nc):
	#	sumNp = sumNp + Np[:,i]*w[i]
	sumNp = np.ones((nc))
	for i in range(nc):
		Rp[:,i] = np.divide(Np[:,i]*weight,sumNp[i])

	return Rp,knots

## @brief Function to set up bspline going through x,y points and of degree deg
#  @param x given values in x direction
#  @param y corresponding y values to x
#  @param u given parametric values for knot points
#  @param deg desired degree of bspline
#  @return The control points in x and y, the basis functions and the knots vector
def nurbs3d(x,y,z,u,v,w,xdeg,ydeg,kutype,kvtype,Du0,DuN,Dv0,DvN):
	#Number of given data points. This is number of output control points
	ncx = x.shape[0]
	ncy = y.shape[1]
	#Order of the bspline
	n = ncx - 1
	m = ncy - 1
	#Degree of bspline
	p = xdeg
	q = ydeg
	#Number of zero bspline basis functions
	r = p+n+1
	s = m+q+1
	#Number of knot points
	nkx = r+1
	nky = s+1

	####################Calculate control points!#######################
	#Get the p order basis functions

	#Solve the system
	Rx,xknots = getrbasis(u,xdeg,w[:,0],kutype)
	Ry,yknots = getrbasis(v,ydeg,w[0,:],kvtype)
	if (kutype == 'endderiv'):
		x,y,z,Rx = addderivdata(x.T,y.T,z.T,Rx,xknots,p,Du0,DuN)
		x = x.T
		y = y.T
		z = z.T
		ncy = ncy+2
	if (kvtype == 'endderiv'):
		x,y,z,Ry = addderivdata(x,y,z,Ry,yknots,q,Dv0,DvN)
		ncx = ncx+2
	Rxinv = np.linalg.inv(Rx)
	xu = np.empty_like(x)
	yu = np.empty_like(y)
	zu = np.empty_like(z)
	for l in range(ncx):
		xu[l,:] = np.dot(Rxinv,x[l,:])  #Control points in x direction
		yu[l,:] = np.dot(Rxinv,y[l,:])  #Control points in y direction
		zu[l,:] = np.dot(Rxinv,z[l,:])  #Control points in z direction
	Ryinv = np.linalg.inv(Ry)
	xv = np.empty_like(x)
	yv = np.empty_like(y)
	zv = np.empty_like(z)
	for i in range(ncy):
		xv[:,i] = np.dot(Ryinv,xu[:,i])
		yv[:,i] = np.dot(Ryinv,yu[:,i])
		zv[:,i] = np.dot(Ryinv,zu[:,i])
	####################Calculate control points!#######################
	return xv,yv,zv,Rx,Ry,xknots,yknots

def addderivdata(X,Y,Z,R,knots,deg,D0,Dn):
	ncx = X.shape[0]
	ncy = X.shape[1]
	#Order of the bspline
	n = ncx - 1
	#Degree of bspline
	p = deg
	#Number of zero bspline basis functions
	r = p+n+1
	#Number of knot points
	nkx = r+1

	#Make x add rows
	row2x = np.zeros((1,R.shape[1]))
	row2x[0,0] = -1.0
	row2x[0,1] = 1.0
	rownx = np.zeros((1,R.shape[1]))
	rownx[0,-2] = -1.0
	rownx[0,-1] = 1.0

	Dx = np.zeros((ncx+2,ncy))
	Dx[0,:] = X[0,:]
	Dx[1,:] = (knots[p+1]/p)*D0[0]
	Dx[2:-2,:] = X[1:-1,:]
	Dx[-2,:] = ((1- knots[r-p-1])/p)*Dn[0]
	Dx[-1,:] = X[-1,:]
	Dy = np.zeros((ncx+2,ncy))
	Dy[0,:] = Y[0,:]
	Dy[1,:] = (knots[p+1]/p)*D0[1]
	Dy[2:-2,:] = Y[1:-1,:]
	Dy[-2,:] = ((1- knots[r-p-1])/p)*Dn[1]
	Dy[-1,:] = Y[-1,:]
	Dz = np.zeros((ncx+2,ncy))
	Dz[0,:] = Z[0,:]
	Dz[1,:] = (knots[p+1]/p)*D0[2]
	Dz[2:-2,:] = Z[1:-1,:]
	Dz[-2,:] = ((1- knots[r-p-1])/p)*Dn[2]
	Dz[-1,:] = Z[-1,:]

	R = np.insert(R,1,row2x,axis=0)
	R = np.insert(R,ncx,rownx,axis=0)

	return Dx,Dy,Dz,R

## @brief Function to give u values for data x
#  @param x data
#  @param type method for u determination
#  @return The u values
def getus(x,y,z,type='equal'):
	nc = len(x)
	if (type == 'equal'):
		u = np.linspace(0,1,nc)
	elif (type == 'chord'):
		d = 0
		dists = np.zeros(nc)
		for k in range(1,nc):
			dists[k] = np.sqrt((x[k]-x[k-1])**2 +
				           (y[k]-y[k-1])**2 +
				           (z[k]-z[k-1])**2)
			d = d+dists[k]
		u = np.zeros(nc)
		u[0] = 0.0
		for k in range(1,nc-1):
			u[k] = u[k-1] + float(dists[k])/d
		u[-1] = 1.0
	elif (type == 'centripetal'):
		d = 0
		dists = np.zeros(nc)
		for k in range(1,nc):
			dists[k] = np.sqrt((x[k]-x[k-1])**2 +
				           (y[k]-y[k-1])**2 +
				           (z[k]-z[k-1])**2)
			d = d+ np.sqrt(dists[k])
		u = np.zeros(nc)
		u[0] = 0.0
		for k in range(1,nc-1):
			u[k] = u[k-1] + float(np.sqrt(dists[k]))/d
		u[-1] = 1.0
	else:
		print 'Type specified is not an option. Options are: equal, chord,centripetal'

	return u

## @brief Function to give knot values for parameters u
#  @param u parameter vector
#  @param type method for knot determination
#  @return The knot vector
def getknots(u,p,type='equal'):
	nc = len(u)
	n = nc - 1
	m = p+n+1
	nk = m+1

	if (type == 'equal'):
		knots = np.hstack((np.zeros(p),np.linspace(0,1,nk-2*p),np.ones(p)))
	elif (type == 'avg'):
		knots = np.zeros(nk)
		knots[m-p:m+1] = 1.0
		for j in range(1,n-p+1):
			for i in range(j,j+p):
				knots[j+p] = knots[j+p]+u[i]
			knots[j+p] = (1.0/p)*knots[j+p]
	elif (type == 'endderiv'):
		m = p+n+3
		knots = np.zeros(nk+2)
		knots[m-p:m+1] = 1.0
		for j in range(n-p+2):
			for i in range(j,j+p):
				knots[j+p+1] = knots[j+p+1]+u[i]
			knots[j+p+1] = (1.0/p)*knots[j+p+1]
		knots[m-p:m+1] = 1.0

	else:
		print 'Type specified is not an option. Options are: equal,avg,endderiv'

	return knots

def loft(argv):
   allpoints = 0
   putype    = 'equal'
   pvtype    = 'equal'
   kutype    = 'avg'
   kvtype    = 'avg'
   p         = 2
   q         = 2
   Du0       = [0.0]
   DuN       = [0.0]
   Dv0       = [0.0]
   DvN       = [0.0]
   try:
      opts, args = getopt.getopt(argv,"ha:u:v:x:y:p:q:o:n:r:s:",["allpoints=", \
		      "putype=","pvtype=","kutype=","kvtype=", \
		      "uorder=","vorder=","du0=","dun=","dv0=","dvn="])
   except getopt.GetoptError:
      print '-a[--allpoints]  <all points, NxMxL, N is number of pts in v,M is number of pts in u,L is 3 (X,Y,Z)>'
      print '-u[--putype] <parametrictype in u>'
      print '-v[--pvtype] <parametrictype in v>'
      print '-x[--kutype] <knottype in u>'
      print '-y[--kvtype] <knottype in v>'
      print '-p[--uorder] <degree of curve in u>'
      print '-q[--vorder] <degree of curve in v>'
      print '-o[--du0]    <end derivative of surface in u at beginning>'
      print '-n[--dun]    <end derivative of surface in u at end>'
      print '-r[--dv0]    <end derivative of surface in v at beginning>'
      print '-s[--dvn]    <end derivative of surface in v at end>'
      sys.exit(2)
   for opt, arg in opts:
      if opt == '-h':
         print '-a[--allpoints]  <all points, NxMxL, N is number of pts in v,M is number of pts in u,L is 3 (X,Y,Z)>'
	 print '-u[--putype] <parametrictype in u>'
	 print '-v[--pvtype] <parametrictype in v>'
	 print '-x[--kutype] <knottype in u>'
	 print '-y[--kvtype] <knottype in v>'
	 print '-p[--uorder] <degree of curve in u>'
	 print '-q[--vorder] <degree of curve in v>'
         print '-o[--du0]    <end derivative of surface in u at beginning>'
         print '-n[--dun]    <end derivative of surface in u at end>'
         print '-r[--dv0]    <end derivative of surface in v at beginning>'
         print '-s[--dvn]    <end derivative of surface in v at end>'
         sys.exit()
      elif opt in ("-a", "--allpoints"):
         allpoints = arg
      elif opt in ("-u", "--putype"):
         putype = arg
      elif opt in ("-v", "--pvtype"):
         pvtype = arg
      elif opt in ("-x", "--kutype"):
         kutype = arg
      elif opt in ("-y", "--kvtype"):
         kvtype = arg
      elif opt in ("-p", "--uorder"):
         p = int(arg)
      elif opt in ("-q", "--vorder"):
         q = int(arg)
      elif opt in ("-o", "--du0"):
         Du0 = [float(i) for i in arg]
      elif opt in ("-n", "--dun"):
         DuN = [float(i) for i in arg]
      elif opt in ("-r", "--dv0"):
         Dv0 = [float(i) for i in arg]
      elif opt in ("-s", "--dvn"):
         DvN = [float(i) for i in arg]
   print 'Parametric Type (u) is: ', putype
   print 'Parametric Type (v) is: ', pvtype
   print 'Knot Type (u) is:       ', kutype
   print 'Knot Type (v) is:       ', kvtype
   print 'Surface Degree (u) is:  ', p
   print 'Surface Degree (v) is:  ', q
   if (Du0 == [0.0]):
	print 'Beg Derivative (u) is:  ', 'default'
   else:
   	print 'Beg Derivative (u) is:  ', Du0
   if (DuN == [0.0]):
	print 'End Derivative (u) is:  ', 'default'
   else:
   	print 'End Derivative (u) is:  ', DuN
   if (Dv0 == [0.0]):
	print 'Beg Derivative (v) is:  ', 'default'
   else:
   	print 'Beg Derivative (v) is:  ', Dv0
   if (DvN == [0.0]):
	print 'End Derivative (v) is:  ', 'default'
   else:
   	print 'End Derivative (v) is:  ', DvN
   print ''

   return nurbs_func(allpoints,putype,pvtype,kutype,kvtype,p,q,Du0,DuN,Dv0,DvN)

## @brief Function to convert the knots and parametric values into the input
#         for opencascade data structures. This is knots and multiplicity.
#         Knots contains the knot values, but does not keep any repeated knots.
#         The multiplicity vector keeps the number of times each value in the
#	  knot vector repeats. This is 1 in most cases.
#  @param uKnots, the knots in the u direction to be processed
#  @param uKnots, the knots in the v direction to be processed
#  @param uDeg,   the degree of the surface in the u direction
#  @param vDeg,   the degree of the surface in the v direction
#  @return uKnew, the knot vector with unrepeating values in the u direction
#  @return vKnew, the knot vector with unrepeating values in the v direction
#  @return uMnew, the mult vector containing the repition of knot values in u
#  @return vMnew, the mult vector containing the repition of knot values in v
def convertToKnotsMults(uKnots,vKnots,uDeg,vDeg):

	#Take interior u knots
	uKlength = len(uKnots)-2*uDeg
	uKnew    = np.zeros(uKlength)
	for i in range(uKlength):
		uKnew[i] = uKnots[i+uDeg]

	#Take interior v knots
	vKlength = len(vKnots)-2*vDeg
	vKnew    = np.zeros(vKlength)
	for i in range(vKlength):
		vKnew[i] = vKnots[i+vDeg]

	#U mult is multiplicity of knot vector u
	uMlength = uKlength
	uMnew    = np.zeros(uMlength)
	for i in range(uMlength):
		if (i==0 or i==uMlength-1):
			uMnew[i] = uDeg+1
		else:
			uMnew[i] = 1

	#V mult is multiplicity of knot vector v
	vMlength = vKlength
	vMnew    = np.zeros(vMlength)
	for i in range(vMlength):
		if (i==0 or i==vMlength-1):
			vMnew[i] = vDeg+1
		else:
			vMnew[i] = 1

	return uKnew,vKnew,uMnew,vMnew

## @brief Function to calculate the normal to three points. This is used for
#	  the first and last segmentations to specify as an end derivative.
#  @param Xpts, x coordinate for three points
#  @param Ypts, y coordinate for three points
#  @param Zpts, z coordinate for three points
#  @param direction, direction in which the normal should face. The dot
#	  product of this and the normal calculated will be > 0.
#  @return perp, the normal to the given three points
def calc_seg_perp_vector(Xpts,Ypts,Zpts,direction):
  	math = vtk.vtkMath()

	#Initiate vecs for cross product
	vec1 = [Xpts[1]-Xpts[0],Ypts[1]-Ypts[0],Zpts[1]-Zpts[0]]
	vec2 = [Xpts[2]-Xpts[0],Ypts[2]-Ypts[2],Zpts[2]-Zpts[0]]
	perp = [0.0,0.0,0.0]
	dirl = [0.0,0.0,0.0]
	for i in range(3):
		dirl[i] = direction[i]

	math.Cross(vec1,vec2,perp)
	dotval = math.Dot(perp,dirl)
	if (dotval < 0):
		for i in range(3):
			perp[i] = -1.0*perp[i]

	return perp


#################################TESTING#####################################
## @brief Function that takes all inputs and calls the code to interpolate
#	  a 3D bspline surface.
#  @note  The book used for this work is:
#         Piegl, Les A., and Wayne Tiller. The NURBS Book. 2nd ed. Berlin: Springer, 1997. Print.
#	  Chapters: 3 and 4 for curves and surfaces, 9 for surface fitting
#  @param allpoints, all the segmentation points to interpolate in m,n,l array.
#	  m is number of segs, n is number of points per seg, and l is 3 for
#	  each coordinate direction.
#  @param putype, the type of parametrization to use in u direction. Can be
#         'equal', 'chord', or 'centripetal'. 'Equal' does not always guarantee a
#	  invertible system. 'chord' or 'centripetal' do, and they are better for
#	  unevenly spaced input data as the parametrization is based off the
#	  distance of the input points.
#  @param pvtype, the type of parametrization to use in v direction. Same options
#	  available as in the u direction.
#  @param kutype, the type of knot spacing to use in u dierction. Can be 'equal',
#	  'avg', or 'endderiv'. 'Equal' does not guarantee an invertible system.
#	  'avg' is better for unevenly spaced data, and 'endderiv' allows the
#	  input of end derivative constraints.
#  @param p, the order of the surface in the u direction
#  @param q, the order of the surface in the v direction
#  @param Du0, If 'endderiv' is specified for the kutype, then Du0 is the
#	  vector derivative for the beginning of the interpolation in u.
#  @param DuN, If 'endderiv' is specified for the kutype, then DuN is the
#	  vector derivative for the end of the interpolation in u.
#  @param Dv0, If 'endderiv' is specified for the kvtype, then Dv0 is the
#	  vector derivative for the beginning of the interpolation in v.
#  @param DvN, If 'endderiv' is specified for the kvtype, then DvN is the
#	  vector derivative for the end of the interpolation in v.
def nurbs_func(allpoints,putype,pvtype,kutype,kvtype,p,q,Du0,DuN,Dv0,DvN):

	#Pull in coordinates from input. Repeat first points to complete surface
	X         = np.zeros((allpoints.shape[0],allpoints.shape[1]+1))
	Y         = np.zeros((allpoints.shape[0],allpoints.shape[1]+1))
	Z         = np.zeros((allpoints.shape[0],allpoints.shape[1]+1))
	X[:,:-1]  = allpoints[:,:,0]
	X[:,-1]   = allpoints[:,0,0]
	Y[:,:-1]  = allpoints[:,:,1]
	Y[:,-1]   = allpoints[:,0,1]
	Z[:,:-1]  = allpoints[:,:,2]
	Z[:,-1]   = allpoints[:,0,2]

	#Set up degree, number of control points for inputs
	ncx = X.shape[1]
	ncy = X.shape[0]
	n = ncx-1
	m = ncy-1
	r = p+n+1
	s = q+m+1
	nkx = r+1
	nky = s+1

	if (kutype != 'endderiv' and ncx <= p):
		print 'Need more input points or lower degree of surface'
		return 0
	if (kvtype != 'endderiv' and ncy <= q):
		print 'Need more input points or lower degree of surface'
		return 0

	#Get parametric vectors and weights
	u = getus(X[0,:],Y[0,:],Z[0,:],putype)
	v = getus(X[:,0],Y[:,0],Z[:,0],pvtype)
	W = np.ones((ncx,ncy))

	#Set up derivates at ends (default is normal direction from last seg))
	if (Du0 == [0.0]):
		Du0 = np.array([X.T[1,:]-X.T[0,:],Y.T[1,:]-Y.T[0,:],Z.T[1,:]-Z.T[0,:]])
	if (DuN == [0.0]):
		DuN = np.array([X.T[-1,:]-X.T[-2,:],Y.T[-1,:]-Y.T[-2,:],Z.T[-1,:]-Z.T[-2,:]])
	if (Dv0 == [0.0]):
		Dv0 = np.array([X[1,:]-X[0,:],Y[1,:]-Y[0,:],Z[1,:]-Z[0,:]])
		#Dv0 = calc_seg_perp_vector(X[0,0:3],Y[0,0:3],Z[0,0:3],Dv0tmp[0,:])
	if (DvN == [0.0]):
		DvN = np.array([X[-1,:]-X[-2,:],Y[-1,:]-Y[-2,:],Z[-1,:]-Z[-2,:]])
		#DvN = calc_seg_perp_vector(X[-1,0:3],Y[-1,0:3],Z[-1,0:3],DvNtmp[0,:])

	Xu,Yu,Zu,Nx,Ny,xknots,yknots = nurbs3d(X,Y,Z,u,v,W,p,q,kutype,kvtype,\
			Du0,DuN,Dv0,DvN)

	#Convert knots and parameters into opencascade understandable arrays
	uK,vK,uM,vM = convertToKnotsMults(xknots,yknots,p,q)

	return Xu,Yu,Zu,uK,vK,uM,vM,p,q



#################################TESTING#####################################

