""" 
Functions to plot the mesh surfaces

@author Fanwei Kong
"""
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection

def plot_surface(verts, faces, shape):
    """
    This function plot a mesh surface

    Args:
        verts: verts output from marching cube algorithm
        faces: faces output from marching cube algorithm
        shape: dimension of the image
    Returns:
        None
    """    
    fig = plt.figure(figsize=(10, 10))
    ax = fig.add_subplot(111, projection='3d')

    # Fancy indexing: `verts[faces]` to generate a collection of triangles
    mesh = Poly3DCollection(verts[faces])
    mesh.set_edgecolor('k')
    ax.add_collection3d(mesh)

    ax.set_xlim(0, shape[0])
    ax.set_ylim(0, shape[1])
    ax.set_zlim(0, shape[2])
#    plt.tight_layout()
    plt.show()

def point_cloud3D(points):
    """
    This function plots a 3d scatter plot

    Args:
        points: coordinates of the points
    """

    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax.scatter(points[:,0], points[:,1], points[:,2], c='r', marker='o')

    plt.show()

