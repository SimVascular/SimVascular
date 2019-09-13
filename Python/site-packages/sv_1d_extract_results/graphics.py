# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"""
This module is used to display the 1D vessel network.

The network is displayed in 3D representing nodes as spheres and 
segments as cylinders.

Segments can be selected using the left mouse button. The segment is 
highlighted in red and its name is printed.

"""

from os import path
import logging
from .manage import get_logger_name

try:
    import vtk
    from vtk.util.colors import tomato
except ImportError:
    pass

class GeometryColors(object):
    SEGMENT = [0.0, 1.0, 0.0]
    SEGMENT_HIGHLIGHT = [1.0, 0.0, 0.0]
    NODE = [0.0, 0.0, 1.0]

class MouseSegmentInteractorStyle(vtk.vtkInteractorStyleTrackballCamera):
    """
    The MouseSegmentInteractorStyle class defines a custom graphics
    interactor used to select segments.

    Attributes:
        picked_actor (vtkActor): The current selected actor.
        last_picked_actor (vtkActor): The last selected actor.
        graphics (Graphics): The Graphics object.
    """
 
    def __init__(self,parent=None):
        self.picked_actor = None
        self.last_picked_actor = None
        self.graphics = None

        # Add event handling functions.
        self.AddObserver("LeftButtonPressEvent", self.leftButtonPressEvent)
        self.AddObserver("KeyPressEvent", self.onKeyPressEvent)
        self.AddObserver("CharEvent", self.onCharEvent)
 
    def leftButtonPressEvent(self,obj,event):
        """ 
        Process a left mouse button press.

        A left mouse button press is used to select a segment actor.
        """
        clickPos = self.GetInteractor().GetEventPosition()
        picker = vtk.vtkCellPicker()
        picker.Pick(clickPos[0], clickPos[1], 0, self.renderer)
        
        # Get selecected geometry. 
        self.picked_actor = picker.GetActor()
        if self.picked_actor == None:
            self.OnLeftButtonDown()
            return

        graphics = self.graphics

        # Identify the selected segment.
        for seg_name,seg_actor in self.graphics.segment_actors.items():
            if self.picked_actor == seg_actor:
              graphics.logger.info(" ")
              graphics.logger.info("Selected segment %s" % seg_name)
              graphics.picked_segment = seg_name
        #__for seg_name,seg_actor in self.graphics.segment_actors.items()

        if self.last_picked_actor:
            self.last_picked_actor.GetProperty().SetColor(*GeometryColors.SEGMENT)

        self.picked_actor.GetProperty().SetColor(*GeometryColors.SEGMENT_HIGHLIGHT)
        self.last_picked_actor = self.picked_actor
        
        self.OnLeftButtonDown()
        return

    def onCharEvent(self, renderer, event):
        """
        Process an on char event.

        This is used to prevent passing the shortcut key 'w' to vtk which we use
        to write selected results and vtk uses to switch to wireframe display. 
        """
        key = self.GetInteractor().GetKeySym()
        if (key != 'w'): 
            self.OnChar()

    def onKeyPressEvent(self, renderer, event):
        """
        Process a key press event.
        """
        key = self.GetInteractor().GetKeySym()

        ## Save the selected segment.
        #
        if key == 's' and self.graphics.picked_segment and \
          (self.graphics.picked_segment not in self.graphics.picked_segments):
            self.graphics.logger.info("Save segment %s" % self.graphics.picked_segment)
            self.graphics.picked_segments.append(self.graphics.picked_segment)
            self.graphics.picked_actors.append(self.picked_actor)
            self.last_picked_actor = None

        ## Remove selected segments.
        #
        elif (key == 'u') and (len(self.graphics.picked_segments) != 0):
            segment_name = self.graphics.picked_segments.pop()
            actor = self.graphics.picked_actors.pop()
            actor.GetProperty().SetColor(*GeometryColors.SEGMENT)
            self.graphics.window.Render()
            self.graphics.logger.info("Remove segment %s" % segment_name) 

        elif (key == 'w') and (len(self.graphics.picked_segments) != 0):
            self.graphics.solver.write_selected_segments(self.graphics.picked_segments) 

    #__def onKeyPressEvent

#__class MouseSegmentInteractorStyle


class Graphics(object):
    """ 
    The Graphics class is used to display the solver mesh in the graphics window.

    Attributes:
        segment_actors (dict{str,vtkActor}): A map between segment names and their
          actor geometry.}
        picked_segment (str): The name of the currently selected segment. 
        picked_segments (list[str]): The names of saved selected segments.
        picked_actors (list[vtkActor]): The acrtors of saved selected segments.
    """
    def __init__(self, params):
        self.params = params
        self.renderer = None
        self.window = None
        self.interactor = None
        self.segment_actors = {}
        self.solver = None

        self.picked_segment = None
        self.picked_segments = []
        self.picked_actors = []

        self.colors = vtk.vtkNamedColors()
        self.logger = logging.getLogger(get_logger_name())
        self.initialize_graphics()

    def create_graphics_geometry(self, poly_data):
        """ 
        Create geometry for display.
        """
        mapper = vtk.vtkPolyDataMapper()
        mapper.SetInputData(poly_data)
        actor = vtk.vtkActor()
        actor.SetMapper(mapper)
        return actor

    def initialize_graphics(self):
        """ 
        Create renderer and graphics window.
        """
        self.renderer = vtk.vtkRenderer()
        self.window = vtk.vtkRenderWindow()
        self.window.AddRenderer(self.renderer)
        self.renderer.SetBackground(1.0, 1.0, 1.0)
        self.window.SetSize(1000, 1000)

        # Create a trackball interacter to transoform the geometry using the mouse.
        self.interactor = vtk.vtkRenderWindowInteractor()
        self.interactor.SetRenderWindow(self.window)

        style = MouseSegmentInteractorStyle() 
        style.renderer = self.renderer 
        style.graphics = self
        self.interactor.SetInteractorStyle(style)
        style.SetCurrentRenderer(self.renderer)

    def add_sphere(self, center, color):
        """
        Add a sphere geometry to th graphics scene.
        """
        sphere = vtk.vtkSphereSource()
        sphere.SetCenter(center[0], center[1], center[2])
        sphere.SetRadius(0.1)
        poly_data = sphere.GetOutputPort()
        self.add_graphics_geometry(poly_data, color, True)

    def add_cyl(self, pt1, pt2, name):
        """
        Add a cylinder geometry defined by two points to the graphics scene.

        Arguments:
            pt1 (list[float]): The 3D coordinates defining the first end point.
            pt2 (list[float]): The 3D coordinates defining the second end point.
            name (str): The cylinder (segment) name.
        """
        cyl = vtk.vtkCylinderSource()
        cyl.SetRadius(0.05)
        cyl.SetResolution(15)
        x = [0,0,0]
        y = [0,0,0]
        z = [0,0,0]
        vtk.vtkMath.Subtract(pt2, pt1, x)
        length = vtk.vtkMath.Norm(x)
        vtk.vtkMath.Normalize(x)

        arbitrary = [0,0,0]
        arbitrary[0] = vtk.vtkMath.Random(-10,10)
        arbitrary[1] = vtk.vtkMath.Random(-10,10)
        arbitrary[2] = vtk.vtkMath.Random(-10,10)
        vtk.vtkMath.Cross(x, arbitrary, z)
        vtk.vtkMath.Normalize(z)

        vtk.vtkMath.Cross(z, x, y)
        matrix = vtk.vtkMatrix4x4()
        matrix.Identity()

        for i in range(3):
            matrix.SetElement(i, 0, x[i])
            matrix.SetElement(i, 1, y[i])
            matrix.SetElement(i, 2, z[i])

        transform = vtk.vtkTransform()
        transform.Translate(pt1) 
        transform.Concatenate(matrix); 
        transform.RotateZ(-90.0)      
        transform.Scale(1.0, length, 1.0)
        transform.Translate(0, 0.5, 0)  

        transformPD = vtk.vtkTransformPolyDataFilter()
        transformPD.SetTransform(transform)
        transformPD.SetInputConnection(cyl.GetOutputPort())

        mapper = vtk.vtkPolyDataMapper()
        actor = vtk.vtkActor()
        mapper.SetInputConnection(cyl.GetOutputPort())
        actor.SetUserMatrix(transform.GetMatrix())
        actor.SetMapper(mapper)
        actor.GetProperty().SetColor(*GeometryColors.SEGMENT)
        self.renderer.AddActor(actor)
        self.segment_actors[name] = actor

    def add_graphics_geometry(self, poly_data, color):
        """
        Add a vtkPolydata object to the graphics scene.
        """
        gr_geom = self.create_graphics_geometry(poly_data)
        gr_geom.GetProperty().SetColor(color[0], color[1], color[2])
        gr_geom.GetProperty().SetPointSize(20)
        self.renderer.AddActor(gr_geom)
        self.window.Render()

    def add_graphics_points(self, poly_data, color):
        """
        Add a set of points, represented as spheres, to the graphics scene.

        Arguments:
            poly_data (vtkPolydata): The set of points. 
        """
        ball = vtk.vtkSphereSource()
        ball.SetRadius(self.params.node_sphere_radius)
        ball.SetThetaResolution(12)
        ball.SetPhiResolution(12)
        balls = vtk.vtkGlyph3D()
        balls.SetInputData(poly_data)
        balls.SetSourceConnection(ball.GetOutputPort())

        mapBalls = vtk.vtkPolyDataMapper()
        mapBalls.SetInputConnection(balls.GetOutputPort())
        ballActor = vtk.vtkActor()
        ballActor.SetMapper(mapBalls)
        ballActor.GetProperty().SetColor(*GeometryColors.NODE)
        ballActor.GetProperty().SetSpecularColor(1, 0, 0)
        ballActor.GetProperty().SetSpecular(0.3)
        ballActor.GetProperty().SetSpecularPower(20)
        ballActor.GetProperty().SetAmbient(0.2)
        ballActor.GetProperty().SetDiffuse(0.8)
        ballActor.PickableOff()
        self.renderer.AddActor(ballActor)

        self.window.Render()

    def add_graphics_edges(self, poly_data, names, color):
        """
        Add a set of edges, represented as cylinders, to the graphics scene.

        Arguments:
            poly_data (vtkPolydata): The set of edges. 
            names (list[str]): The names of the cylinders (segments). 
        """
        pt1 = [0,0,0]
        pt2 = [0,0,0]
        points = poly_data.GetPoints();
        poly_data.GetLines().InitTraversal()
        idList = vtk.vtkIdList()
        n = 0

        while poly_data.GetLines().GetNextCell(idList):
            node1 = idList.GetId(0)
            node2 = idList.GetId(1)
            points.GetPoint(node1,pt1)
            points.GetPoint(node2,pt2)
            self.add_cyl(pt1, pt2, names[n])
            n += 1
        #__while poly_data.GetLines().GetNextCell(idList)

        self.window.Render()

    def show(self):
        self.interactor.Start()


