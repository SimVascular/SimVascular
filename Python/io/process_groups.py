#!/usr/bin/python

#-------------------------------------------------------------------------------------
# Python script to manipulate SV Groups and Paths for parametrized geometry processing
# Authors:      Debanjan Mukherjee, Adam Updegrove
# Institute:    University of California, Berkeley
#
# Latest:       Summer 2015
# TODO:         1. Create a separate script file
#               2. Create a function to write out an edited group file
#-------------------------------------------------------------------------------------

#
# Import all necessary modules
#
try:
    import numpy as np
    #import sys
except ImportError:
    print "Could Not Import Usual Modules"
try:
    import re
except ImportError:
    print "could Not Import Module re For Regex Manipulations"
#try:
#    import Tkinter
#except ImportError:
#    print "Could Not Import Module Tkinter For Python Tcl Support"
#try:
#    from vtk import *
#except ImportError:
#    print "Could Not Import Module vtk"

#---------------------------
# END MODULE IMPORT SEGMENT
#---------------------------

#-----------------------------------------
# BEGIN DEFINITIONS FOR A GROUP DATA CLASS
#-----------------------------------------
class SimVascularGroup:

    def __init__(self, a_GroupName):

        self.m_GroupName          = a_GroupName
        self.m_SegmentList        = []

    def printSelf(self):
	for Seg in self.m_SegmentList:
	    print '----------Segment',Seg.m_SegmentID,'----------'
	    print 'Number of Points:',len(Seg.m_SegmentPoints)
	    print 'Center X:        ',Seg.m_SegmentDataDict['centerX']
	    print 'Center Y:        ',Seg.m_SegmentDataDict['centerY']
	    print 'Radius:          ',Seg.m_SegmentDataDict['radius']
	    print 'Path ID:         ',Seg.m_SegmentDataDict['pathID']
	    print 'Position: ',Seg.m_SegmentDataDict['pos']
	    print 'Normal:   ',Seg.m_SegmentDataDict['normal']
	    print 'Xhat:     ',Seg.m_SegmentDataDict['xhat']
	    print 'Points:'
	    for point in Seg.m_SegmentPoints:
		print point
	    print '-------------------------------'



#----------------------------------------------------------------------
# BEGIN DEFINITIONS FOR A SEGMENT DATA CLASS (FOR POTENTIAL OOP WRAPPING)
#----------------------------------------------------------------------
class SimVascularSeg:

    def __init__(self, a_SegmentID):

        self.m_SegmentID          = a_SegmentID
        self.m_SegmentDataDict    = {}
        self.m_SegmentPoints      = []

#--------------------------------------------
# END OF DEFINITIONS FOR THE GROUP DATA CLASS
#--------------------------------------------

#
# Function to handle a group file in terms of group data objects
# All segment groups in a file are converted into an array of group
# objects, which can later be processed for geometry information
#
def processSimVascularGroupFile(a_Filename,a_Groupname):

    grpFileObj  = open(a_Filename, 'r')
    Group       = SimVascularGroup(a_Groupname)
    segCount    = 0
    lineCount   = 0
    segPtDataStart  = {}
    segPtDataEnd    = {}

    for line in grpFileObj:

        lineCount = lineCount + 1

        #
        # the first line of the group data is the header for the group
        #
        if line.startswith('/group/'):

            segCount    = segCount + 1
	    segID       = int(line[line.rfind('/')+1:].isdigit())
            segObject   = SimVascularSeg(segID)

        #
        # the second line of the group data is skipped since it contains the ID again
        # the third line of the group data is now read to create the group object's data
        # dictionary, from all the various data entries for each group
        #
        elif line.startswith('center_x') or line.startswith('num_smooth'):

            lineList    = re.split(' |{|}|\n',line)
            lineList    = [item for item in lineList if item != '']

            segObject.m_SegmentDataDict['centerX'] = 0.0
            segObject.m_SegmentDataDict['centerY'] = 0.0
            segObject.m_SegmentDataDict['radius'] = 0.0
            segObject.m_SegmentDataDict['pathId'] = 0.0
            segObject.m_SegmentDataDict['posId'] = 0.0
            segObject.m_SegmentDataDict['pos'] = [0.0,0.0,0.0]
            segObject.m_SegmentDataDict['normal'] = [0.0,0.0,0.0]
            segObject.m_SegmentDataDict['xhat'] = [0.0,0.0,0.0]
	    if ('center_x' in lineList):
            	centerX     				  = lineList[lineList.index('center_x')+1]
            	segObject.m_SegmentDataDict['centerX']    = float(centerX)
	    if ('center_y' in lineList):
            	centerY     = lineList[lineList.index('center_y')+1]
            	segObject.m_SegmentDataDict['centerY']    = float(centerY)
	    if ('radius' in lineList):
            	radius      = lineList[lineList.index('radius')+1]
            	segObject.m_SegmentDataDict['radius']     = float(radius)
	    if ('pathId' in lineList):
            	pathID      = lineList[lineList.index('pathId')+1]
            	segObject.m_SegmentDataDict['pathID']     = int(pathID)
	    if ('posId' in lineList):
            	posID       = lineList[lineList.index('posId')+1]
            	segObject.m_SegmentDataDict['posID']      = int(posID)

	    if ('pos' in lineList):
            	pos     = np.array([lineList[lineList.index('pos')+1],\
                                lineList[lineList.index('pos')+2],\
                                lineList[lineList.index('pos')+3]])
            	segObject.m_SegmentDataDict['pos']    = pos

	    if ('nrm' in lineList):
            	normal  = np.array([lineList[lineList.index('nrm')+1],\
                                lineList[lineList.index('nrm')+2],\
                                lineList[lineList.index('nrm')+3]])
            	segObject.m_SegmentDataDict['normal'] = normal

	    if ('xhat' in lineList):
            	xhat    = np.array([lineList[lineList.index('xhat')+1],\
                                lineList[lineList.index('xhat')+2],\
                                lineList[lineList.index('xhat')+3]])
            	segObject.m_SegmentDataDict['xhat']   = xhat


            Group.m_SegmentList.append(segObject)

            segPtDataStart[segCount] = lineCount

        elif line.startswith(''):

            segPtDataEnd[segCount] = lineCount

    #
    # now each group segment coordinates will be stored for each group object
    # in the original data structure this is a single numpy matrix with
    # coordinates stored in [x,y,z] format per row
    #
    addPointsCount = 0
    for seg in Group.m_SegmentList:
	addPointsCount   = addPointsCount + 1
        startReadingFrom = segPtDataStart[addPointsCount]
        stopReadingAt    = segPtDataEnd[addPointsCount] - 1

        for i, innerLine in enumerate(open(a_Filename)):

            if i in xrange(startReadingFrom, stopReadingAt):

                coordsList = innerLine.split()
		seg.m_SegmentPoints.append(coordsList)

    return Group

def resampleSegPoints(a_SimVascularSeg,a_numSamplePts):
    print 'Need to do!'

def convertGroupPointsToNumpy(a_SimVascularGroup,a_numSamplePts):
    # Converts all the points from a single simvascular group into one data
    # structure as a 3 dimensional numpy array
    segList     = a_SimVascularGroup.m_SegmentList
    numSegs     = len(segList)
    numPts      = len(segList[0].m_SegmentPoints)
    resample    = 0

    for i in range(1,numSegs):
	numPtsInSeg = len(segList[i].m_SegmentPoints)
	if (numPtsInSeg != numPts):
	    resample = 1

    # If not same number of points in all segs, resample to numSamplePts
    if (resample == 1):
	for i in range(numSegs):
	    segList[i].m_SegmentPoints = \
	    	resampleSegPoints(segList[i].m_SegmentPoints,a_numSamplePts)

	numPts = a_numSamplePts

    allPoints  = np.zeros((numSegs,numPts,3)) #3 for x,y,z

    for i in range(numSegs):
	pList        = segList[i].m_SegmentPoints
	for j in range(numPts):
	    point             = pList[j]
	    allPoints[i,j,0]  = point[0]
	    allPoints[i,j,1]  = point[1]
	    allPoints[i,j,2]  = point[2]

    return allPoints


#
# Finally, to test out the capabilities, provide an executable to run the functions
# on a set of files
#
if __name__=="__main__":

    groupFile   = 'cyl_groups/Cyl_1'
    cyl1 = processSimVascularGroupFile(groupFile,"Cyl_1")
    cyl1.printSelf()
    allPoints = convertGroupPointsToNumpy(cyl1)
