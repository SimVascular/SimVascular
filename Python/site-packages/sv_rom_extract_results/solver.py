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
This module is used to read a 1D solver input (.in) and simulation results (.dat) files.

Functions are defined to write data to CSV format files and create time plots.

----------------------
 1D Solver Input File
----------------------
A solver input file uses keyword statements to defined a simulation. For example

    MODEL SU201_2005_RPA1

    NODE 0 26.904272079467773 21.049297332763672 22.75612449645996

    JOINT J0 1 IN0 OUT0
    JOINTINLET IN0 1 0
    JOINTOUTLET OUT0 2 1 6

    SEGMENT Group0_Seg0 0 25.861634854053026 259 0 1 15.693287129713616 7.8512026064629215 0.0 MAT1 NONE 0.0 0 0 NOBOUND NONE

The NODE statements define the network geometry and the SEGMENT statements its connectivity.



"""
import pdb
from os import path
import logging
from .manage import get_logger_name
from .node import Node
from .segment import Segment 
from .parameters import Parameters
from collections import namedtuple

try:
    import vtk
except ImportError:
    print("vtk not found.")

try:
    from matplotlib import pyplot as plt
except ImportError:
    print("[sv_rom_extract_results] matplotlib is not installed")

class Solver(object):
    """
    The Solver class reads and stores solver input and results. 

    The solver input file is read to extract node and segment geometry that can be optionally
    displayed. Segments can also be selected. 
    
    Attributes:
        read_segment_names (list[string]): List of segment names to read data for.
        write_segment_names (list[string]): List of segment names to convert. This is the
            same as read_segment_names[] except when interactively selecting segment names 
            to convert.
    """

    class StatementKeywords(object):
        MODEL = 'MODEL'
        NODE = 'NODE'
        SEGMENT = 'SEGMENT'
        SOLVEROPTIONS = 'SOLVEROPTIONS'

    class SegmentFields(object):
        """ 
        This class defines the location of segment fields in the solver file SEGMENT statement.
        """
        NAME = 1 
        ID = 2 
        LENGTH = 3
        NUM_ELEMS = 4
        INLET_NODE = 5
        OUTLET_NODE = 6
        INLET_AREA = 7
        OUTLET_AREA = 8
        INFLOW_VALUE = 9
        MATERIAL = 10
        LOSS_TYPE = 11
        BRANCH_ANGLE = 12
        UPSTREAM_SEGMENT_ID = 13
        BRANCH_SEGMENT_ID = 14
        BC_TYPE = 15
        DATA_TABLE_NAME = 16

    class BcTypes(object):
        """ 
        This class defines boundary condition types.
        """
        AREA = "AREA"
        CORONARY = "CORONARY"
        FLOW = "FLOW"
        IMPEDANCE = "IMPEDANCE"
        NONE = "NOBOUND"
        PRESSURE = "PRESSURE"
        PRESSURE_WAVE = "PRESSURE_WAVE"
        PULMONARY ="PULMONARY"
        RCR = "RCR"
        RESISTANCE = "RESISTANCE"
        RESISTANCE_TIME = "RESISTANCE_TIME"
        WAVE = "WAVE"

    def __init__(self, params):
        self.params = params
        self.nodes = None
        self.segments = None
        self.times = None
        self.data_index_min = None
        self.data_index_max = None
        self.read_segment_names = None
        self.write_segment_names = None
        self.graphics = None
        self.logger = logging.getLogger(get_logger_name())

        try:
            self.points = vtk.vtkPoints()
            self.vertices = vtk.vtkCellArray()
        except:
            self.points = None 
            self.vertices = None 
            self.params.display_geometry = False

        self.points_polydata = None
        self.lines_polydata = None
        self.lines_segment_names = None

    def read_solver_file(self):
        """ 
        Read in a 1D solver .in file.
        """
        self.logger.info("---------- Read solver file ----------")
        #self.logger.info("Number of points: %d" % num_points)
        self.nodes = []
        self.segments = {}
        file_name = self.params.results_directory + "/" + self.params.solver_file_name

        with open(file_name) as fp:
            line = fp.readline()
            cnt = 1
            while line:
                line = fp.readline()
                tokens = line.split()
                if len(tokens) == 0: 
                    continue
                if tokens[0] == self.StatementKeywords.NODE:
                    self.add_node(tokens)
                elif tokens[0] == self.StatementKeywords.SEGMENT:
                    self.add_segment(tokens)
                elif tokens[0] == self.StatementKeywords.SOLVEROPTIONS:
                    self.add_solver_options(tokens)
                elif tokens[0] == self.StatementKeywords.MODEL:
                    self.params.model_name = tokens[1]
            #__while line
        #__with open(self.params.solver_file_name) as fp:

        self.logger.info("Model name: %s" % self.params.model_name)
        self.logger.info("Number of nodes: %d" % len(self.nodes))
        self.logger.info("Number of segments: %d" % len(self.segments))
        self.logger.info("Number of time steps: %d" % self.params.num_steps)
        self.logger.info("Time step: %g" % self.params.time_step)

        if self.params.time_range == None:
            self.params.time_range = [0.0, self.params.times[-1]]

        # If displaying geometry then create the vtkPolyData 
        # objects used to display nodes and segments.
        #
        if self.params.display_geometry:
            # Create a points polydata object
            self.points_polydata = vtk.vtkPolyData()
            self.points_polydata.SetPoints(self.points)
            self.points_polydata.SetVerts(self.vertices)

            # Create a lines polydata object
            self.lines_polydata = vtk.vtkPolyData()
            self.lines_polydata.SetPoints(self.points)
            lines = vtk.vtkCellArray()
            self.lines_segment_names = []
            for key,segment in self.segments.items():
                line = vtk.vtkLine()
                line.GetPointIds().SetId(0, segment.node1)
                line.GetPointIds().SetId(1, segment.node2)
                lines.InsertNextCell(line)
                self.lines_segment_names.append(key)
            self.lines_polydata.SetLines(lines)

    def add_solver_options(self, tokens):
        """ 
        Add solver options.
        """
        time_step = float(tokens[1])
        self.params.time_step = time_step
        save_freq = int(tokens[2])
        num_steps = int(tokens[3])
        self.params.num_steps = num_steps 

        ## Set the time data for the given time range.
        #
        # This creates an array of time values and determines the subset 
        # of segment data to read in for the given time range 
        # (data_index_min, self.data_index_max).
        #
        self.logger.info("Number of time steps: %d" % num_steps) 
        self.logger.info("Save frequency: %d" % save_freq) 
        min_time = self.params.time_range[0]
        max_time = self.params.time_range[1]
        self.data_index_min = None
        self.data_index_max = None
        self.times = []
        self.time_indices = []
        n = 0
        for j, i in enumerate(range(0,num_steps+1,save_freq)):
            time = i*time_step
            if time >= min_time and time <= max_time:
                self.times.append(time)
                if time > 0.0:
                    self.time_indices += [j - 1]
                if self.data_index_min == None:
                    self.data_index_min = n
                self.data_index_max = n
            n += 1
        #__for i in range(0,num_steps+1,save_freq):
        self.logger.info("Number of time values: %d" % len(self.times)) 
        self.logger.info("Time values: %s" % ','.join(map(str,self.times)))
        self.logger.info("Data imin:%d  imax:%d" % (self.data_index_min, self.data_index_max)) 

    def add_node(self, tokens):
        """ 
        Add a simulation node.
        """
        id = tokens[1]
        x = float(tokens[2])
        y = float(tokens[3])
        z = float(tokens[4])
        self.nodes.append(Node(id,x,y,z))

        if self.params.display_geometry:
            id = self.points.InsertNextPoint([x, y, z])
            self.vertices.InsertNextCell(1)
            self.vertices.InsertCellPoint(id)

    def add_segment(self, tokens):
        """ 
        Add a simulation segment.
        """
        fields = self.SegmentFields
        name = tokens[fields.NAME]
        id = tokens[fields.ID]
        node1 = int(tokens[fields.INLET_NODE])
        node2 = int(tokens[fields.OUTLET_NODE])
        bc_type = tokens[fields.BC_TYPE]
        self.segments[name] = Segment(id, name, node1, node2, bc_type)
        #self.logger.info("Add segment name: %s" % name) 

    def read_segment_data(self):
        """ 
        Read in segment data.

        A subset of segment data files can be read in by

           1) Giving segment names, stored in self.params.segment_names.

           2) Setting the use outlet segments flag, if self.params.outlet_segments = true.

        All segment data files are read if self.params.all_segments = true. 

        """
        self.read_segment_names = []

        if not self.params.data_names:
            self.logger.warning("No data names given for reading data.")
            return

        if self.params.all_segments:
            self.params.segment_names = []
            for name,segment in self.segments.items():
                self.read_segment_names.append(name)

        elif self.params.outlet_segments:
            self.params.segment_names = []
            for name,segment in self.segments.items():
                if segment.bc_type != self.BcTypes.NONE:
                    self.read_segment_names.append(name)

        elif self.params.segment_names:
            for segment_name in self.params.segment_names:
                self.read_segment_names.append(segment_name)

        if not  self.read_segment_names:
            self.logger.warning("No segment names given for reading data.")
            return

        self.logger.info("Read data for segment names: %s" % ','.join(self.read_segment_names))
        self.write_segment_names = []
        data_names = self.params.data_names
        for segment_name in self.read_segment_names:
            self.read_segment_data_file(segment_name, data_names)
            self.write_segment_names.append(segment_name)

    def read_segment_data_file(self, segment_name, data_names):
        """ 
        Read in a segment data from a .dat file.

        The results are read in for the names of data given in data_names[].

        Arguments:
            segment_name (str): The name of the segment to read data for.
            data_names (list[str]): The list of data names for read.
        """
        #self.logger.info("---------- Read segment data file ----------")
        #self.logger.info("Segment name: %s" % segment_name) 

        if not segment_name in self.segments:
            msg = "No segment named: %s" % segment_name
            self.logger.error(msg)
            raise Exception(msg)

        sep = Parameters.FILE_NAME_SEP
        ext = Parameters.DATA_FILE_EXTENSION 
        results_dir = self.params.results_directory 
        model_name = self.params.model_name 

        imin = self.data_index_min
        imax = self.data_index_max
        segment = self.segments[segment_name]
        segment.data = {}

        for data_name in data_names:
            #self.logger.info("Data name: %s" % data_name) 
            file_name = results_dir + "/" + model_name + segment_name + sep + data_name + ext
            num_rows = 0
            data = []

            with open(file_name) as fp:
                line = fp.readline()
                cnt = 1
                while line:
                    line = fp.readline()
                    tokens = line.split()
                    num_rows += 1
                    if len(tokens) == 0:
                        continue
                    values = [float(v) for v in tokens]
                    num_cols = len(values)
                #__while line
                self.logger.info("Read %d data values for %s" % (len(values), data_name))
                data.append(values[imin:imax+1])
            #__with open(file_name) as fp

            segment.data[data_name] = data
        #__for data_name in self.params.data_names:

    def write_selected_segments(self, segment_names):
        """
        Write segment data to a file for segments selected interactively.
        """
        self.write_segment_names = segment_names
        self.write_segment_data()

    def write_segment_data(self):
        """
        Write segment data to a file.
        """
        if self.write_segment_names == None:
          return

        segments = self.segments
        segment_names = self.write_segment_names 
        data_names = self.params.data_names
        times = self.times

        self.logger.info("---------- Write segment data ----------")
        self.logger.info("Data names: %s" % ','.join(self.params.data_names))
        self.logger.info("Segment names: %s" % ','.join(segment_names))
        self.logger.info("File format: %s" % self.params.output_format)
        self.logger.info("self.params.output_directory: %s" % self.params.output_directory) 
        self.logger.info("self.params.output_file_name: %s" % self.params.output_file_name) 
        file_format = self.params.output_format
        output_dir = self.params.output_directory 
        output_file_name = self.params.output_file_name 
        ext = "." + file_format 
        sep = "_"

        ## Check that segements have data.
        #
        no_data = False
        for i,name in enumerate(segment_names):
            segment = segments[name]
            if segment.data == None:
               self.logger.error("Segment '%s' does not have data." % name) 
               no_data = True
        #__for i,name in enumerate(segment_names)

        for data_name in data_names:
            self.logger.info("Data name: %s" % data_name) 
            file_name = output_dir + "/" + output_file_name + sep + data_name + ext

            with open(file_name, "w") as fp:
                fp.write("time,")
                for i,name in enumerate(segment_names):
                    #self.logger.info("Segment name: %s" % name) 
                    fp.write(name)
                    if i != len(segment_names)-1:
                        fp.write(",")
                fp.write("\n")

                for i,time in enumerate(times):
                    #self.logger.info("%d time: %g" % (i,time))
                    fp.write(str(time) + ",")
                    for j,name in enumerate(segment_names):
                        segment = segments[name]
                        if segment.data == None:
                            continue 
                        data_list = segment.data[data_name]
                        data = data_list[-1]
                        fp.write(str(data[i]))
                        if j != len(segment_names)-1:
                            fp.write(",")
                    #__for j,name in enumerate(segment_names)
                    fp.write("\n")
                #__for i,time in enumerate(times):
        #__for data_name in data_names

    def plot_results(self):
        """ 
        Plot results.
        """
        self.logger.info("---------- Plot results ----------")
        title = self.params.solver_file_name
        min_time = self.params.time_range[0]
        max_time = self.params.time_range[1]
        times = self.times
        self.logger.info("Min time: %f" % min_time)
        self.logger.info("Max time: %f" % max_time)

        for data_name in self.params.data_names:
            self.logger.info("Data name: %s" % data_name)
            plot_values = [] 
            plot_names = [] 
            fig, ax = plt.subplots()
            ylabel = data_name

            for j,name in enumerate(self.read_segment_names):
                segment = self.segments[name]
                data_list = segment.data[data_name]
                data = data_list[-1]
                values = []
                plot_times = [] 
                for i,time in enumerate(times):
                    values.append(data[i])
                    plot_times.append(time)
                #__for i,time in enumerate(times)
                plot_values.append(values)
                plot_names.append(name) 
                ax.plot(plot_times, plot_values[j], label=plot_names[j])
            #__for j,name in enumerate(self.params.segments)

            ax.set(xlabel='time (s)', ylabel=ylabel, title=title)
            ax.grid()
            chartBox = ax.get_position()
            ax.set_position([chartBox.x0, chartBox.y0, chartBox.width*0.6, chartBox.height])
            ax.legend(loc='upper center', bbox_to_anchor=(1.45, 0.8), shadow=True, ncol=1)
        #__for data_name in self.params.data_names

        # Set the figure window position.
        plt.get_current_fig_manager().window.wm_geometry("+200+100")

        # Add key events.
        cid = plt.gcf().canvas.mpl_connect('key_press_event', self.press_key)

        ## If displaying geometry then don't block.
        if self.params.display_geometry:
            plt.ion()
            plt.show()
            plt.pause(0.001)
        else:
            plt.show()

    def plot_segment(self, segment_name, data_name):
        """ Plot results for a segment.

        matplotlib can't plot after vtk grabs the event queue so forget this.
        """
        return
        self.logger.info("---------- Plot segment ----------")
        title = segment_name 
        min_time = self.params.time_range[0]
        max_time = self.params.time_range[1]

        segment = self.segments[segment_name]
        if not segment:
            self.logger.error("No segment names: %s" % segment_name)
            return

        self.logger.info("Segment name: %s" % segment_name)
        self.logger.info("Data name: %s" % data_name)
        self.logger.info("Min time: %f" % min_time)
        self.logger.info("Max time: %f" % max_time)

        for data_name in self.params.data_names:
            self.logger.info("Data name: %s" % data_name)
            times = self.params.times
            plot_values = [] 
            plot_names = [] 
            fig, ax = plt.subplots()
            ylabel = data_name
            data_list = segment.data[data_name]
            data = data_list[-1]

            values = []
            plot_times = [] 
            for i,time in enumerate(times):
                if (time > min_time) and (time <= max_time):
                    values.append(data[i])
                    plot_times.append(time)
            #__for i,time in enumerate(times)
            plot_values.append(values)
            ax.plot(plot_times, values, label=data_name)

            ax.set(xlabel='time (s)', ylabel=ylabel, title=title)
            ax.grid()
            #chartBox = ax.get_position()
            #ax.set_position([chartBox.x0, chartBox.y0, chartBox.width*0.6, chartBox.height])
            #ax.legend(loc='upper center', bbox_to_anchor=(1.45, 0.8), shadow=True, ncol=1)
        #__for data_name in self.params.data_names

        # Set the figure window position.
        plt.get_current_fig_manager().window.wm_geometry("+200+100")

        # Add key events.
        cid = plt.gcf().canvas.mpl_connect('key_press_event', self.press_key)

        ## If displaying geometry then don't block.
        if self.params.display_geometry:
            plt.ion()
            plt.show()
            plt.pause(0.001)
        else:
            plt.show()

    def press_key(self, event):
        """ 
        Key press event handler for plots.

        Keys:
           q: key to quit.
        """
        if event.key == 'q':
            plt.close(event.canvas.figure)
