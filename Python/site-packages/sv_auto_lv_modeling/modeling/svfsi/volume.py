import vtk
import numpy as np
import glob
import matplotlib.pyplot as plt

Dir = "../../05-VolMesh/BD9702/mesh-complete/Debug/"


N = len(glob.glob1(Dir,"*.vtp"))

Vol = np.zeros(N)
for i in range(N):
    fname = Dir + 'debug'+str(i).zfill(5)+'.vtp'
    reader = vtk.vtkXMLPolyDataReader()
    reader.SetFileName(fname)
    reader.Update()
    polydata = reader.GetOutput()
    mass = vtk.vtkMassProperties()
    mass.SetInputData(polydata)
    mass.Update()
    Vol[i] = mass.GetVolume()

dt = 1.25*3/(N-1)
q = np.zeros(N)
q[0] = 0.5*(Vol[1] - Vol[-2])/dt
q[N-1] = q[0]
for i in range(1,N-1):
    q[i] = 0.5*(Vol[i+1]-Vol[i-1])/dt

t = np.linspace(0,300,301)*dt

np.savez("flowrate",t=t,q=q,v=Vol)

