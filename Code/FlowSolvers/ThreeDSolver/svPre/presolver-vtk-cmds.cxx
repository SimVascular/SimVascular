/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2013 Open Source Medical
 * Software Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *=========================================================================*/

#include "cvFlowsolverOptions.h"

#include "cmd.h"

#include "SimVascular.h"
#include "cvSolverIO.h"
#include "vtkSmartPointer.h"

#ifdef SV_USE_ZLIB
#include "simvascular_zlib.h"
#else
#include <stdlib.h>
#define gzopen fopen
#define gzprintf fprintf
#define gzFile FILE*
#define gzclose fclose
#define gzgets fgets
#define gzeof feof
#endif

#include <vector>
using namespace std;

//
// some helpful global variables
//
extern char* oformat;

extern int numNodes_;
extern int numElements_;
extern int numMeshEdges_;
extern int numMeshFaces_;
extern int numSolnVars_;
extern int numBoundaryFaces_;
extern int** boundaryElements_;
extern double* nodes_;
extern int* elements_;
extern int* boundaryElementsIds_;
extern int* xadj_;
extern int xadjSize_;
extern int* adjncy_;
extern int adjncySize_;
extern int* iBC_;
extern int* iBCB_;
extern double* BCB_;
extern double init_p_;
extern double init_v_[3];
extern double* soln_;
extern double* dispsoln_;
extern double* acc_;

extern int DisplacementNumElements_;
extern int* DisplacementConn_[3];
extern int DisplacementNumNodes_;
extern int* DisplacementNodeMap_;
extern double* DisplacementSolution_;
extern double Displacement_Evw_;
extern double Displacement_nuvw_;
extern double Displacement_thickness_;
extern double Displacement_kcons_;
extern double Displacement_pressure_;

#if(VER_VARWALL == 1)
extern double* gBC_;
extern double* wallpropsoln_;
extern double* ThicknessSolution_;
extern double* EvwSolution_;
#endif

int writeGEOMBCDAT(char* filename);
int writeRESTARTDAT(char* filename);
int readRESTARTDAT(char* infile, int readSoln, int readDisp, int readAcc);

int parseNum(char *cmd, int *num);
int parseFile(char *cmd);
int NWgetNextNonBlankLine(int *eof);
int NWcloseFile();
int setNodesWithCode(char *cmd, int val);
int setBoundaryFacesWithCode(char *cmd, int setSurfID, int surfID, int setCode,
		int code, double value);
int parseDouble(char *cmd, double *num);
int parseDouble2(char *cmd, double *num);
int parseDouble3(char *cmd, double *v1, double *v2, double *v3);
int parseCmdStr(char *cmd, char *mystr);
int parseCmdStr2(char *cmd, char *mystr);
int parseNum2(char *cmd, int *num);
int check_node_order(int n0, int n1, int n2, int n3, int elementId, int *k0,
		int *k1, int *k2, int *k3);
//int fixFreeEdgeNodes(char *cmd);
void siftDownEdges(int **edges, int root, int bottom, int array_size);
//int createMeshForDispCalc(char *cmd);
void siftDownKentriesCalcMesh(int **ids, int root, int bottom, int array_size);
int create_bct(BCTData& bct,char *faceFile, char *flowFile,double rho, double mu, int shape,
        double period, int pointNum,int modeNum, int preserve, int flip);
extern gzFile fp_;
extern char buffer_[MAXCMDLINELENGTH];

#include "vtkPoints.h"
#include "vtkUnstructuredGrid.h"
#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkIdList.h"
#include "vtkCellType.h"
#include "vtkPointData.h"
#include "vtkGeometryFilter.h"
#include "vtkCleanPolyData.h"
#include "vtkCellData.h"
#include "vtkCellArray.h"
#include "vtkExtractEdges.h"

#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkUnstructuredGridWriter.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkPolyDataWriter.h"
#include "vtkXMLUnstructuredGridReader.h"
#include "vtkUnstructuredGridReader.h"
#include "vtkXMLPolyDataReader.h"
#include "vtkPolyDataReader.h"
#include "vtkAppendPolyData.h"

extern double rho_;
extern double mu_;
extern int bctShape_;
extern double bctPeriod_;
extern int bctPointNum_;
extern int bctModeNum_;
extern int bctPreserve_;
extern int bctFlip_;
extern int bctMerge_;
extern int bctNodeNumTotal_;
extern int bctPointNumMax_;
extern vector<BCTData> vbct;

// =========
//   Cross
// =========

inline
void Cross(double ax, double ay, double az, double bx, double by, double bz,
		double *prodx, double *prody, double *prodz) {
	(*prodx) = ay * bz - az * by;
	(*prody) = -(ax * bz - az * bx);
	(*prodz) = ax * by - ay * bx;
	return;
}

// =======
//   Dot
// =======

inline
double Dot(double ax, double ay, double az, double bx, double by, double bz) {
	double product;

	product = ax * bx + ay * by + az * bz;
	return product;
}

int cmd_mesh_vtu(char *cmd) {

	int i;

	// enter
	debugprint(stddbg, "Entering cmd_mesh_vtu.\n");

	// parse command string for filename
	char meshfn[MAXPATHLEN];
	meshfn[0] = '\0';
	parseCmdStr(cmd, meshfn);

	// check file exists
	FILE* fp = NULL;
	if ((fp = fopen(meshfn, "r")) == NULL) {
		fprintf(stderr, "ERROR: could not open file (%s).", meshfn);
		return CV_ERROR;
	} else {
		fclose(fp);
	}

	vtkXMLUnstructuredGridReader* reader = vtkXMLUnstructuredGridReader::New();
	reader->SetFileName(meshfn);
	reader->Update();

	vtkUnstructuredGrid* ug = NULL;
	ug = reader->GetOutput();
	if (ug == NULL) {
		fprintf(stderr, "ERROR: problems parsing file (%s).", meshfn);
		return CV_ERROR;
	}

	// never run scalar problems anymore

	numSolnVars_ = 5;

	// set num nodes and elements

	numNodes_ = ug->GetNumberOfPoints();
	numElements_ = ug->GetNumberOfCells();

	debugprint(stddbg, "  Number of Nodes (%i)\n", numNodes_);
	debugprint(stddbg, "  Number of Elements (%i).\n", numElements_);

	// find the number of edges in the entire mesh

	vtkExtractEdges* extractEdges = vtkExtractEdges::New();
	extractEdges->SetInputDataObject(ug);
	extractEdges->Update();
	numMeshEdges_ = extractEdges->GetOutput()->GetNumberOfCells();
	extractEdges->Delete();

	// find exterior surface

	vtkGeometryFilter* surfFilt = vtkGeometryFilter::New();
	surfFilt->MergingOff();
	surfFilt->SetInputDataObject(ug);
	surfFilt->Update();
	vtkCleanPolyData* cleaner = vtkCleanPolyData::New();
	cleaner->PointMergingOff();
	cleaner->PieceInvariantOff();
	cleaner->SetInputDataObject(surfFilt->GetOutput());
	cleaner->Update();

	vtkPolyData *bdryPD = cleaner->GetOutput();

	bdryPD->GetPointData()->SetActiveScalars("GlobalNodeID");
	bdryPD->GetCellData()->SetActiveScalars("GlobalElementID");

	numBoundaryFaces_ = bdryPD->GetNumberOfCells();

	// calculate number of total mesh faces
	numMeshFaces_ = (numElements_ * 4 - numBoundaryFaces_) / 2
			+ numBoundaryFaces_;

	debugprint(stddbg, "  Number of Boundary Faces (%i)\n", numBoundaryFaces_);
	debugprint(stddbg, "  Number of Mesh faces (%i).\n", numMeshFaces_);

	int eof = 0;
	int n0, n1, n2, n3;
	int elementId;

	//
	// nodes
	//

	if (numNodes_ == 0) {
		fprintf(stderr,
				"ERROR:  Must specify number of nodes before you read them in!\n");
		return CV_ERROR;
	}

	nodes_ = new double[3 * numNodes_];

	double pt[3];
	int nodeId;

	ug->GetPointData()->SetActiveScalars("GlobalNodeID");
	ug->GetCellData()->SetActiveScalars("GlobalElementID");

	for (i = 0; i < numNodes_; i++) {
		ug->GetPoints()->GetPoint(i, pt);
		nodeId = ug->GetPointData()->GetScalars()->GetTuple1(i);
		nodes_[0 * numNodes_ + nodeId - 1] = pt[0];
		nodes_[1 * numNodes_ + nodeId - 1] = pt[1];
		nodes_[2 * numNodes_ + nodeId - 1] = pt[2];
		debugprint(stddbg, "  Node (%i) : %lf %lf %lf\n", nodeId, pt[0], pt[1],
				pt[2]);
	}

	// do work
	if (numElements_ == 0) {
		fprintf(stderr,
				"ERROR:  Must specify number of elements before you read them in!\n");
		return CV_ERROR;
	}

	vtkIdList* ptids = vtkIdList::New();
	ptids->Allocate(10, 10);
	ptids->Initialize();

	elements_ = new int[4 * numElements_];

	vtkCellArray *cells = ug->GetCells();
	cells->InitTraversal();

	for (int i = 0; i < numElements_; i++) {

		ptids->Reset();
		cells->GetCell(5 * i, ptids);
		if (ptids->GetNumberOfIds() != 4) {
			fprintf(stderr, "ERROR:  invalid number of ids in cell (%i)!",
					ptids->GetNumberOfIds());
			return CV_ERROR;
		}
		elementId = ug->GetCellData()->GetScalars()->GetTuple1(i);
		n0 = ug->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(0));
		n1 = ug->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(1));
		n2 = ug->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(2));
		n3 = ug->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(3));

		int j0 = 0;
		int j1 = 0;
		int j2 = 0;
		int j3 = 0;

		check_node_order(n0, n1, n2, n3, elementId, &j0, &j1, &j2, &j3);

		elements_[0 * numElements_ + elementId - 1] = j0;
		elements_[1 * numElements_ + elementId - 1] = j1;
		elements_[2 * numElements_ + elementId - 1] = j2;
		elements_[3 * numElements_ + elementId - 1] = j3;

	}

	//
	// Boundary Faces
	//

	// init data first time this function is called
	if (boundaryElements_ == NULL) {
		boundaryElements_ = new int*[4];
		boundaryElements_[0] = new int[numMeshFaces_];
		boundaryElements_[1] = new int[numMeshFaces_];
		boundaryElements_[2] = new int[numMeshFaces_];
		boundaryElements_[3] = new int[numMeshFaces_];
		boundaryElementsIds_ = new int[numMeshFaces_];
	}

	cells = bdryPD->GetPolys();
	cells->InitTraversal();

	for (i = 0; i < cells->GetNumberOfCells(); i++) {
		ptids->Reset();
		cells->GetCell(4 * i, ptids);
		if (ptids->GetNumberOfIds() != 3) {
			fprintf(stderr, "ERROR:  invalid number of ids in cell (%i)!",
					ptids->GetNumberOfIds());
			return CV_ERROR;
		}
		elementId = bdryPD->GetCellData()->GetScalars()->GetTuple1(i);
		n0 = bdryPD->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(0));
		n1 = bdryPD->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(1));
		n2 = bdryPD->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(2));

		n3 = -1;

		int j0 = 0;
		int j1 = 0;
		int j2 = 0;
		int j3 = 0;

		check_node_order(n0, n1, n2, n3, elementId, &j0, &j1, &j2, &j3);

		boundaryElements_[0][i] = j0;
		boundaryElements_[1][i] = j1;
		boundaryElements_[2][i] = j2;
		boundaryElements_[3][i] = j3;

		debugprint(stddbg, "  Boundary Element (%i) (%i): %i %i %i %i\n", i,
				elementId, j0, j1, j2, j3);

		// note that I assume element numbering starts at 1,
		// whereas flow solver assumes it started at zero!!
		boundaryElementsIds_[i] = elementId - 1;

	}

	// cleanup
	ptids->Delete();
	cleaner->Delete();
	surfFilt->Delete();
	reader->Delete();

	//
	//  create some additional internal data structs
	//

	if (iBC_ == NULL) {
		iBC_ = new int[numNodes_];
		for (i = 0; i < numNodes_; i++) {
			iBC_[i] = 0;
		}
	}

	if (iBCB_ == NULL) {
		iBCB_ = new int[2 * numBoundaryFaces_];
		BCB_ = new double[numBoundaryFaces_ * 6];
		for (i = 0; i < 2 * numBoundaryFaces_; i++) {
			iBCB_[i] = 0;
		}
		for (i = 0; i < numBoundaryFaces_; i++) {
			BCB_[i] = 0.0;
		}
	}

	debugprint(stddbg, "Exiting cmd_mesh_vtu.\n");

	return CV_OK;

}

/**
 * @author Adam Updegrove
 * @author updega2@gmail.com
 * @author UC Berkeley
 * @author shaddenlab.berkeley.edu
 * @brief Command to generate adjacency from the existing vtu mesh
 * @param *char cmd; should just be the vtu file
 * @return CV_OK if function completes properly
 */
int cmd_mesh_and_adjncy_vtu(char *cmd) {

	int i;

	// enter
	debugprint(stddbg, "Entering cmd_mesh_and_adjncy_vtu.\n");

	// parse command string for filename
	char meshfn[MAXPATHLEN];
	meshfn[0] = '\0';
	parseCmdStr(cmd, meshfn);

	// check file exists
	FILE* fp = NULL;
	if ((fp = fopen(meshfn, "r")) == NULL) {
		fprintf(stderr, "ERROR: could not open file (%s).", meshfn);
		return CV_ERROR;
	} else {
		fclose(fp);
	}

	vtkXMLUnstructuredGridReader* reader = vtkXMLUnstructuredGridReader::New();
	reader->SetFileName(meshfn);
	reader->Update();

	vtkUnstructuredGrid* ug = NULL;
	ug = reader->GetOutput();
	if (ug == NULL) {
		fprintf(stderr, "ERROR: problems parsing file (%s).", meshfn);
		return CV_ERROR;
	}

	// never run scalar problems anymore

	numSolnVars_ = 5;

	// set num nodes and elements

	numNodes_ = ug->GetNumberOfPoints();
	numElements_ = ug->GetNumberOfCells();

	debugprint(stddbg, "  Number of Nodes (%i)\n", numNodes_);
	debugprint(stddbg, "  Number of Elements (%i).\n", numElements_);

	// find the number of edges in the entire mesh

	vtkExtractEdges* extractEdges = vtkExtractEdges::New();
	extractEdges->SetInputDataObject(ug);
	extractEdges->Update();
	numMeshEdges_ = extractEdges->GetOutput()->GetNumberOfCells();
	extractEdges->Delete();

	// find exterior surface

	vtkGeometryFilter* surfFilt = vtkGeometryFilter::New();
	surfFilt->MergingOff();
	surfFilt->SetInputDataObject(ug);
	surfFilt->Update();
	vtkCleanPolyData* cleaner = vtkCleanPolyData::New();
	cleaner->PointMergingOff();
	cleaner->PieceInvariantOff();
	cleaner->SetInputDataObject(surfFilt->GetOutput());
	cleaner->Update();

	vtkPolyData *bdryPD = cleaner->GetOutput();

	bdryPD->GetPointData()->SetActiveScalars("GlobalNodeID");
	bdryPD->GetCellData()->SetActiveScalars("GlobalElementID");

	numBoundaryFaces_ = bdryPD->GetNumberOfCells();

	// calculate number of total mesh faces
	numMeshFaces_ = (numElements_ * 4 - numBoundaryFaces_) / 2
			+ numBoundaryFaces_;

	debugprint(stddbg, "  Number of Boundary Faces (%i)\n", numBoundaryFaces_);
	debugprint(stddbg, "  Number of Mesh faces (%i).\n", numMeshFaces_);

	int eof = 0;
	int n0, n1, n2, n3;
	int elementId;

	//
	// nodes
	//

	if (numNodes_ == 0) {
		fprintf(stderr,
				"ERROR:  Must specify number of nodes before you read them in!\n");
		return CV_ERROR;
	}

	nodes_ = new double[3 * numNodes_];

	double pt[3];
	int nodeId;

	ug->GetPointData()->SetActiveScalars("GlobalNodeID");
	ug->GetCellData()->SetActiveScalars("GlobalElementID");

	for (i = 0; i < numNodes_; i++) {
		ug->GetPoints()->GetPoint(i, pt);
		nodeId = ug->GetPointData()->GetScalars()->GetTuple1(i);
		nodes_[0 * numNodes_ + nodeId - 1] = pt[0];
		nodes_[1 * numNodes_ + nodeId - 1] = pt[1];
		nodes_[2 * numNodes_ + nodeId - 1] = pt[2];
		debugprint(stddbg, "  Node (%i) : %lf %lf %lf\n", nodeId, pt[0], pt[1],
				pt[2]);
	}

	// do work
	if (numElements_ == 0) {
		fprintf(stderr,
				"ERROR:  Must specify number of elements before you read them in!\n");
		return CV_ERROR;
	}

	vtkIdList* ptids = vtkIdList::New();
	ptids->Allocate(10, 10);
	ptids->Initialize();

	elements_ = new int[4 * numElements_];

	vtkCellArray *cells = ug->GetCells();
	cells->InitTraversal();

	for (int i = 0; i < numElements_; i++) {

		ptids->Reset();
		cells->GetCell(5 * i, ptids);
		if (ptids->GetNumberOfIds() != 4) {
			fprintf(stderr, "ERROR:  invalid number of ids in cell (%i)!",
					ptids->GetNumberOfIds());
			return CV_ERROR;
		}
		elementId = ug->GetCellData()->GetScalars()->GetTuple1(i);
		n0 = ug->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(0));
		n1 = ug->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(1));
		n2 = ug->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(2));
		n3 = ug->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(3));

		int j0 = 0;
		int j1 = 0;
		int j2 = 0;
		int j3 = 0;

		check_node_order(n0, n1, n2, n3, elementId, &j0, &j1, &j2, &j3);

		elements_[0 * numElements_ + elementId - 1] = j0;
		elements_[1 * numElements_ + elementId - 1] = j1;
		elements_[2 * numElements_ + elementId - 1] = j2;
		elements_[3 * numElements_ + elementId - 1] = j3;

	}

	//
	// Boundary Faces
	//

	// init data first time this function is called
	if (boundaryElements_ == NULL) {
		boundaryElements_ = new int*[4];
		boundaryElements_[0] = new int[numMeshFaces_];
		boundaryElements_[1] = new int[numMeshFaces_];
		boundaryElements_[2] = new int[numMeshFaces_];
		boundaryElements_[3] = new int[numMeshFaces_];
		boundaryElementsIds_ = new int[numMeshFaces_];
	}

	cells = bdryPD->GetPolys();
	cells->InitTraversal();

	for (i = 0; i < cells->GetNumberOfCells(); i++) {
		ptids->Reset();
		cells->GetCell(4 * i, ptids);
		if (ptids->GetNumberOfIds() != 3) {
			fprintf(stderr, "ERROR:  invalid number of ids in cell (%i)!",
					ptids->GetNumberOfIds());
			return CV_ERROR;
		}
		elementId = bdryPD->GetCellData()->GetScalars()->GetTuple1(i);
		n0 = bdryPD->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(0));
		n1 = bdryPD->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(1));
		n2 = bdryPD->GetPointData()->GetScalars()->GetTuple1(ptids->GetId(2));

		n3 = -1;

		int j0 = 0;
		int j1 = 0;
		int j2 = 0;
		int j3 = 0;

		check_node_order(n0, n1, n2, n3, elementId, &j0, &j1, &j2, &j3);

		boundaryElements_[0][i] = j0;
		boundaryElements_[1][i] = j1;
		boundaryElements_[2][i] = j2;
		boundaryElements_[3][i] = j3;

		debugprint(stddbg, "  Boundary Element (%i) (%i): %i %i %i %i\n", i,
				elementId, j0, j1, j2, j3);

		// note that I assume element numbering starts at 1,
		// whereas flow solver assumes it started at zero!!
		boundaryElementsIds_[i] = elementId - 1;

	}

	//
	//  create some additional internal data structs
	//

	if (iBC_ == NULL) {
		iBC_ = new int[numNodes_];
		for (i = 0; i < numNodes_; i++) {
			iBC_[i] = 0;
		}
	}

	if (iBCB_ == NULL) {
		iBCB_ = new int[2 * numBoundaryFaces_];
		BCB_ = new double[numBoundaryFaces_ * 6];
		for (i = 0; i < 2 * numBoundaryFaces_; i++) {
			iBCB_[i] = 0;
		}
		for (i = 0; i < numBoundaryFaces_; i++) {
			BCB_[i] = 0.0;
		}
	}

	//Starting to extract the adjacency information
	//Define vars
	int numCells;
	vtkIdType cellId;
	vtkIdType meshCellId;
	vtkIdType p1, p2, p3;
	vtkIdType npts = 0;
	vtkIdType *pts = 0;
	vtkSmartPointer < vtkIdList > ptIds = vtkSmartPointer < vtkIdList > ::New();
	vtkSmartPointer < vtkIdList > cellIds = vtkSmartPointer < vtkIdList
			> ::New();

	// do work
	//
	ug->GetPointData()->SetActiveScalars("GlobalNodeID");
	ug->GetCellData()->SetActiveScalars("GlobalElementID");
	ug->BuildLinks();
	numCells = ug->GetNumberOfCells();

	xadj_ = new int[numCells + 1];
	adjncy_ = new int[4 * numCells];
	int adj = 0;
	int xcheck = 0;
	xadj_[xcheck] = 0;

	ptIds->SetNumberOfIds(3);
	for (cellId = 0; cellId < numCells; cellId++) {
		meshCellId = (int) ug->GetCellData()->GetScalars()->LookupValue(cellId + 1);
		ug->GetCellPoints(meshCellId, npts, pts);

		for (i = 0; i < npts; i++) {
			p1 = pts[i];
			p2 = pts[(i + 1) % (npts)];
			p3 = pts[(i + 2) % (npts)];

			ptIds->InsertId(0, p1);
			ptIds->InsertId(1, p2);
			ptIds->InsertId(2, p3);

			ug->GetCellNeighbors(meshCellId, ptIds, cellIds);

			if (cellIds->GetNumberOfIds() != 0) {
				adjncy_[adj++] = (int) (ug->GetCellData()->GetScalars()->GetTuple1(
						cellIds->GetId(0)) - 1);
			}

		}
		xadj_[++xcheck] = adj;
	}

	adjncySize_ = adj;
	xadjSize_ = numCells + 1;

	// cleanup
	ptids->Delete();
	cleaner->Delete();
	surfFilt->Delete();
	reader->Delete();

	debugprint(stddbg, "Exiting cmd_mesh_and_adjncy_vtu.\n");

	return CV_OK;
}

int read_variables_vtu(char* vtufn,string presName, string velName, string dispName,bool dispRequired,
		string tDName, bool tDRequired, string wpName, bool wpRequired) {

	// enter
	debugprint(stddbg, "Entering read_variables_vtu.\n");

	// check file exists
	FILE* fp = NULL;
	if ((fp = fopen(vtufn, "r")) == NULL) {
		fprintf(stderr, "ERROR: could not open file (%s).", vtufn);
		return CV_ERROR;
	} else {
		fclose(fp);
	}

	vtkXMLUnstructuredGridReader* reader = vtkXMLUnstructuredGridReader::New();
	reader->SetFileName(vtufn);
	reader->Update();

	vtkUnstructuredGrid* ug = NULL;
	ug = reader->GetOutput();
	if (ug == NULL) {
		fprintf(stderr, "ERROR: problems parsing file (%s).", vtufn);
		return CV_ERROR;
	}

	numSolnVars_ = 5;

	// read num nodes
	numNodes_ = ug->GetNumberOfPoints();

	debugprint(stddbg, "  Number of Nodes (%i)\n", numNodes_);

	if (numNodes_ == 0) {
		fprintf(stderr, "ERROR:  Needs nodes in (%s)!\n",vtufn);
		return CV_ERROR;
	}

	int size = numNodes_ * numSolnVars_;

	int numArrays = ug->GetPointData()->GetNumberOfArrays();

	string IDName="GlobalNodeID";

	string IDArrayName="", presArrayName = "", velArrayName = "", dispArrayName = "", tDArrayName="";
	string wpArrayName = "";

	for (int i = 0; i < numArrays; i++) {
		string name = ug->GetPointData()->GetArrayName(i);

		size_t found;

		if(IDName!=""){
			found = name.find(IDName);
			if (found != string::npos) {
				IDArrayName = name;
			}
		}

		if(presName!=""){
			found = name.find(presName);
			if (found != string::npos) {
				presArrayName = name;
			}
		}

		if(velName!=""){
			found = name.find(velName);
			if (found != string::npos) {
				velArrayName = name;
			}
		}

		if(dispName!=""){
			found = name.find(dispName);
			if (found != string::npos) {
				dispArrayName = name;
			}
		}

		if(tDName!=""){
			found = name.find(tDName);
			if (found != string::npos) {
				tDArrayName = name;
			}
		}

		if(wpName!=""){
			found = name.find(wpName);
			if (found != string::npos) {
				wpArrayName = name;
			}
		}
	}

	if (IDArrayName == ""){
		fprintf(stderr, "ERROR:  globalNodeID not found in (%s)!\n",vtufn);
		return CV_ERROR;
	}

	if (presArrayName != "" || velArrayName != "") {
		if(soln_==NULL){
			soln_ = new double[size];
		}
	}

	if (presArrayName != "") {
		int nodeID;
		double pres;

		vtkSmartPointer<vtkIntArray> nodeIDArray =
				vtkIntArray::SafeDownCast(ug->GetPointData()->GetArray(IDArrayName.c_str()));

		vtkSmartPointer<vtkDoubleArray> presArray =
				vtkDoubleArray::SafeDownCast(ug->GetPointData()->GetArray(presArrayName.c_str()));

		for (int i = 0; i < numNodes_; i++) {

			nodeID=nodeIDArray->GetValue(i);
			pres=presArray->GetValue(i);

			soln_[0 * numNodes_ + nodeID - 1] = pres;

			debugprint(stddbg, "  Node (%i) (p) : %lf \n", nodeID, pres);

		}

	}else{
		if(presName!=""){
			fprintf(stderr, "ERROR: pressure not found in (%s)!\n",vtufn);
			return CV_ERROR;
		}
	}

	if (velArrayName != "") {
		int nodeID;
		double vel[3];

		vtkSmartPointer<vtkIntArray> nodeIDArray =
				vtkIntArray::SafeDownCast(ug->GetPointData()->GetArray(IDArrayName.c_str()));

		vtkSmartPointer<vtkDoubleArray> velArray =
				vtkDoubleArray::SafeDownCast(ug->GetPointData()->GetArray(velArrayName.c_str()));

		for (int i = 0; i < numNodes_; i++) {

			nodeID=nodeIDArray->GetValue(i);
			velArray->GetTupleValue(i, vel);

			soln_[1 * numNodes_ + nodeID - 1] = vel[0];
			soln_[2 * numNodes_ + nodeID - 1] = vel[1];
			soln_[3 * numNodes_ + nodeID - 1] = vel[2];

			debugprint(stddbg, "  Node (%i) (v1,v2,v3) : %lf %lf %lf\n", nodeID, vel[0],vel[1],vel[2]);

		}

	}else{
		if(velName!=""){
			fprintf(stderr, "ERROR:  velocity not found in (%s)!\n",vtufn);
			return CV_ERROR;
		}
	}

	if (dispArrayName != "") {

		dispsoln_ = new double[numNodes_ * 3];
		int nodeID;
		double disp[3];

		vtkSmartPointer<vtkIntArray> nodeIDArray =
				vtkIntArray::SafeDownCast(ug->GetPointData()->GetArray(IDArrayName.c_str()));

		vtkSmartPointer<vtkDoubleArray> dispArray =
				vtkDoubleArray::SafeDownCast(ug->GetPointData()->GetArray(dispArrayName.c_str()));

		for (int i = 0; i < numNodes_; i++) {

			nodeID=nodeIDArray->GetValue(i);
			dispArray->GetTupleValue(i,disp);

			dispsoln_[0 * numNodes_ + nodeID - 1] = disp[0];
			dispsoln_[1 * numNodes_ + nodeID - 1] = disp[1];
			dispsoln_[2 * numNodes_ + nodeID - 1] = disp[2];

			debugprint(stddbg, "  Node (%i) (disp1,disp2,disp3) : %lf %lf %lf\n", nodeID, disp[0],disp[1],disp[2]);

		}
	}else{
		if(dispName!=""){
			if(dispRequired){
				fprintf(stderr, "ERROR:  displacements not found in (%s)!\n",vtufn);
				return CV_ERROR;
			}else{
				fprintf(stdout, "Warning:  displacements not found in (%s)!\n",vtufn);
			}
		}
	}

	if (tDArrayName != "") {

		vtkDataArray* nodeIDArray= ug->GetPointData()->GetArray(IDArrayName.c_str());
		vtkDataArray* tDArray= ug->GetPointData()->GetArray(tDArrayName.c_str());
//		int numComponent=vel->GetNumberOfComponents();

//		acc_ = new double[numNodes_ * numComponent];
		acc_ = new double[numNodes_ * 4];
		int nodeID;
		double* ac;

		for (int i = 0; i < numNodes_; i++) {

			nodeID = nodeIDArray->GetTuple1(i);
			ac= tDArray->GetTuple4(i);

			acc_[0 * numNodes_ + nodeID - 1] = ac[0];
			acc_[1 * numNodes_ + nodeID - 1] = ac[1];
			acc_[2 * numNodes_ + nodeID - 1] = ac[2];
			acc_[3 * numNodes_ + nodeID - 1] = ac[2];

			debugprint(stddbg, "  Node (%i) (acc0,acc1,acc2,acc3) : %lf %lf %lf %lf\n", nodeID, ac[0], ac[1],ac[2],ac[3]);

		}
	}else{
		if(tDName!=""){
			if(tDRequired){
				fprintf(stderr, "ERROR:  time derivative of solution not found in (%s)!\n",vtufn);
				return CV_ERROR;
			}else{
				fprintf(stdout, "Warning:  time derivative of solution not found in (%s)!\n",vtufn);
			}
		}
	}

#if(VER_VARWALL == 1)
	if (wpArrayName != "") {

		vtkDataArray* nodeIDArray= ug->GetPointData()->GetArray(IDArrayName.c_str());
		vtkDataArray* wpArray= ug->GetPointData()->GetArray(wpArrayName.c_str());

		wallpropsoln_ = new double[numNodes_ * 2];
		int nodeID;
		double* wp;

		for (int i = 0; i < numNodes_; i++) {

			nodeID = nodeIDArray->GetTuple1(i);
			wp= wpArray->GetTuple2(i);

			wallpropsoln_[0 * numNodes_ + nodeID - 1] = wp[0];
			wallpropsoln_[1 * numNodes_ + nodeID - 1] = wp[1];


			debugprint(stddbg, "  Node (%i) (thickness,Evw) : %lf %lf\n", nodeID, wp[0], wp[1]);

		}
	}else{
		if(wpName!=""){
			if(wpRequired){
				fprintf(stderr, "ERROR:  variable wall properties not found in (%s)!\n",vtufn);
				return CV_ERROR;
			}else{
				fprintf(stdout, "Warning:  variable wall properties not found in (%s)!\n",vtufn);
			}
		}
	}

#endif

	debugprint(stddbg, "Exiting read_variables_vtu.\n");
	return CV_OK;
}

int cmd_read_all_variables_vtu(char *cmd) {

	// enter
	debugprint(stddbg, "Entering cmd_read_all_variables_vtu.\n");

	// parse command string for filename
	char vtufn[MAXPATHLEN];
	vtufn[0] = '\0';
	parseCmdStr(cmd, vtufn);

	if(read_variables_vtu(vtufn,"pressure","velocity","displacement",false,"timeDeriv",false,"wallproperty",false)==CV_ERROR){
		return CV_ERROR;
	}
	debugprint(stddbg, "Exiting cmd_read_all_variables_vtu.\n");
	return CV_OK;
}

int cmd_read_pressure_velocity_vtu(char *cmd) {

	// enter
	debugprint(stddbg, "Entering cmd_read_pressue_velocity_vtu.\n");

	// parse command string for filename
	char vtufn[MAXPATHLEN];
	vtufn[0] = '\0';
	parseCmdStr(cmd, vtufn);

	if(read_variables_vtu(vtufn,"pressure","velocity","",false,"",false,"",false)==CV_ERROR){
		return CV_ERROR;
	}

	debugprint(stddbg, "Exiting cmd_read_pressue_velocity_vtu.\n");
	return CV_OK;
}

int cmd_read_pressure_vtu(char *cmd) {

	// enter
	debugprint(stddbg, "Entering cmd_read_pressure_vtu.\n");

    // parse command string for filename and variable name
    int n = 0;
    int end = 0;
    char ignored[MAXSTRINGLENGTH];
    ignored[0]='\0';
    cmd_token_get (&n, cmd, ignored, &end);
    char vtufn[MAXPATHLEN];
    vtufn[0]='\0';
    cmd_token_get (&n, cmd, vtufn, &end);
    char vn[MAXSTRINGLENGTH];
    vn[0]='\0';
    cmd_token_get (&n, cmd, vn, &end);

    string pres_name="pressure";
    if(vn[0]!='\0'){
    	pres_name=string(vn);
    }

    // do work
	if(read_variables_vtu(vtufn,pres_name,"","",false,"",false,"",false)==CV_ERROR){
		return CV_ERROR;
	}

	debugprint(stddbg, "Exiting cmd_read_pressure_vtu.\n");
	return CV_OK;
}

int cmd_read_velocity_vtu(char *cmd) {

	// enter
	debugprint(stddbg, "Entering cmd_read_velocity_vtu.\n");

    // parse command string for filename and variable name
    int n = 0;
    int end = 0;
    char ignored[MAXSTRINGLENGTH];
    ignored[0]='\0';
    cmd_token_get (&n, cmd, ignored, &end);
    char vtufn[MAXPATHLEN];
    vtufn[0]='\0';
    cmd_token_get (&n, cmd, vtufn, &end);
    char vn[MAXSTRINGLENGTH];
    vn[0]='\0';
    cmd_token_get (&n, cmd, vn, &end);

    string vel_name="velocity";
    if(vn[0]!='\0'){
    	vel_name=string(vn);
    }

    // do work
	if(read_variables_vtu(vtufn,"",vel_name,"",false,"",false,"",false)==CV_ERROR){
		return CV_ERROR;
	}

	debugprint(stddbg, "Exiting cmd_read_velocity_vtu.\n");
	return CV_OK;
}

int cmd_read_displacements_vtu(char *cmd) {

	// enter
	debugprint(stddbg, "Entering cmd_read_displacements_vtu.\n");

    // parse command string for filename and variable name
    int n = 0;
    int end = 0;
    char ignored[MAXSTRINGLENGTH];
    ignored[0]='\0';
    cmd_token_get (&n, cmd, ignored, &end);
    char vtufn[MAXPATHLEN];
    vtufn[0]='\0';
    cmd_token_get (&n, cmd, vtufn, &end);
    char vn[MAXSTRINGLENGTH];
    vn[0]='\0';
    cmd_token_get (&n, cmd, vn, &end);

    string disp_name="displacement";
    if(vn[0]!='\0'){
    	disp_name=string(vn);
    }

    // do work
	if(read_variables_vtu(vtufn,"","",disp_name,true,"",false,"",false)==CV_ERROR){
		return CV_ERROR;
	}

	debugprint(stddbg, "Exiting cmd_read_displacements_vtu.\n");
	return CV_OK;
}

int cmd_read_accelerations_vtu(char *cmd) {

	// enter
	debugprint(stddbg, "Entering cmd_read_accelerations_vtu.\n");

    // parse command string for filename and variable name
    int n = 0;
    int end = 0;
    char ignored[MAXSTRINGLENGTH];
    ignored[0]='\0';
    cmd_token_get (&n, cmd, ignored, &end);
    char vtufn[MAXPATHLEN];
    vtufn[0]='\0';
    cmd_token_get (&n, cmd, vtufn, &end);
    char vn[MAXSTRINGLENGTH];
    vn[0]='\0';
    cmd_token_get (&n, cmd, vn, &end);

    string tD_name="timeDeriv";
    if(vn[0]!='\0'){
    	tD_name=string(vn);
    }

    // do work
	if(read_variables_vtu(vtufn,"","","",false,tD_name,true,"",false)==CV_ERROR){
		return CV_ERROR;
	}

	debugprint(stddbg, "Exiting cmd_read_accelerations_vtu.\n");
	return CV_OK;
}

int cmd_read_varwallprop_vtu(char *cmd) {

	// enter
	debugprint(stddbg, "Entering cmd_read_varwallprop_vtu.\n");

    // parse command string for filename and variable name
    int n = 0;
    int end = 0;
    char ignored[MAXSTRINGLENGTH];
    ignored[0]='\0';
    cmd_token_get (&n, cmd, ignored, &end);
    char vtufn[MAXPATHLEN];
    vtufn[0]='\0';
    cmd_token_get (&n, cmd, vtufn, &end);
    char vn[MAXSTRINGLENGTH];
    vn[0]='\0';
    cmd_token_get (&n, cmd, vn, &end);

    string wp_name="wallproperty";
    if(vn[0]!='\0'){
    	wp_name=string(vn);
    }

    // do work
	if(read_variables_vtu(vtufn,"","","",false,"",false,wp_name,true)==CV_ERROR){
		return CV_ERROR;
	}

	debugprint(stddbg, "Exiting cmd_read_varwallprop_vtu.\n");
	return CV_OK;
}

int cmd_noslip_vtp(char *cmd) {

	// enter
	debugprint(stddbg, "Entering cmd_noslip.\n");

	// do work

	// parse command string for filename
	char polyfn[MAXPATHLEN];
	polyfn[0] = '\0';
	parseCmdStr(cmd, polyfn);

	// check file exists
	FILE* fp = NULL;
	if ((fp = fopen(polyfn, "r")) == NULL) {
		fprintf(stderr, "ERROR: could not open file (%s).", polyfn);
		return CV_ERROR;
	} else {
		fclose(fp);
	}

	// read file
	vtkPolyData* pd = NULL;
	vtkXMLPolyDataReader* reader = vtkXMLPolyDataReader::New();
	reader->SetFileName(polyfn);
	reader->Update();
	pd = reader->GetOutput();
	if (pd == NULL) {
		fprintf(stderr, "ERROR: problem parsing file (%s).", polyfn);
		return CV_ERROR;
	}
	vtkIntArray* gids = NULL;
	gids =static_cast<vtkIntArray*>(reader->GetOutput()->GetPointData()->GetArray("GlobalNodeID"));
	if (gids == NULL) {
		fprintf(stderr, "ERROR: problem finding GlobalNodeID");
		return CV_ERROR;
	}

	for (int i = 0; i < gids->GetNumberOfTuples(); i++) {
		int nodeId = gids->GetTuple1(i);
		debugprint(stddbg, "  BC (%i) Dirichlet BC on Node (%i): %i.\n", i,
				nodeId, 56);
		iBC_[nodeId - 1] = 56;
	}

	// cleanup
	reader->Delete();

	debugprint(stddbg, "Exiting cmd_noslip.\n");
	return CV_OK;
}

int cmd_prescribed_velocities_vtp(char *cmd) {

	// enter
	debugprint(stddbg, "Entering cmd_prescribed_velocities.\n");

	// do work

	// these are treated the same as noslip in geombc.dat
	cmd_noslip_vtp(cmd);

	// cleanup
	debugprint(stddbg, "Exiting cmd_prescribed_velocities.\n");
	return CV_OK;
}

int setBoundaryFacesWithCodeVTK(char *cmd, int setSurfID, int surfID,
		int setCode, int code, double value) {

	int i;

	// enter
	debugprint(stddbg, "Entering setBoundaryFacesWithCodeVTK.\n");

	// parse command string for filename
	char polyfn[MAXPATHLEN];
	polyfn[0] = '\0';
	parseCmdStr(cmd, polyfn);

	// check file exists
	FILE* fp = NULL;
	if ((fp = fopen(polyfn, "r")) == NULL) {
		fprintf(stderr, "ERROR: could not open file (%s).", polyfn);
		return CV_ERROR;
	} else {
		fclose(fp);
	}

	// read file
	vtkPolyData* pd = NULL;
	vtkXMLPolyDataReader* reader = vtkXMLPolyDataReader::New();
	reader->SetFileName(polyfn);
	reader->Update();
	pd = reader->GetOutput();
	if (pd == NULL) {
		fprintf(stderr, "ERROR: problem parsing file (%s).", polyfn);
		return CV_ERROR;
	}

	pd->GetPointData()->SetActiveScalars("GlobalNodeID");
	pd->GetCellData()->SetActiveScalars("GlobalElementID");

	vtkIdList* ptids = vtkIdList::New();
	ptids->Allocate(10, 10);
	ptids->Initialize();

	vtkCellArray *cells = pd->GetPolys();
	cells->InitTraversal();

	vtkIntArray* gids = NULL;
	gids = static_cast<vtkIntArray*>(pd->GetPointData()->GetArray(
			"GlobalNodeID"));
	if (gids == NULL) {
		fprintf(stderr, "ERROR: problem finding GlobalNodeID");
		return CV_ERROR;
	}

	// Loop on vtp faces
	int n0, n1, n2, n3;
	int elementId;
	int totCodeFaces = 0;
	int totSurfIDFaces = 0;
	for (int icell = 0; icell < cells->GetNumberOfCells(); icell++) {

		ptids->Reset();
		cells->GetCell(4 * icell, ptids);
		if (ptids->GetNumberOfIds() != 3) {
			fprintf(stderr, "ERROR:  invalid number of ids in cell (%i)!",
					ptids->GetNumberOfIds());
			return CV_ERROR;
		}
		elementId = pd->GetCellData()->GetScalars()->GetTuple1(icell);
		n0 = gids->GetTuple1(ptids->GetId(0));
		n1 = gids->GetTuple1(ptids->GetId(1));
		n2 = gids->GetTuple1(ptids->GetId(2));
		n3 = -1;

		int j0 = n0;
		int j1 = n1;
		int j2 = n2;
		int j3 = -1;

		for (i = 0; i < 4; i++) {
			if (elements_[i * numElements_ + (elementId - 1)] != j0
					&& elements_[i * numElements_ + (elementId - 1)] != j1
					&& elements_[i * numElements_ + (elementId - 1)] != j2) {
				j3 = elements_[i * numElements_ + (elementId - 1)];
				break;
			}
		}

		if (j3 < 0) {
			fprintf(stderr,
					"ERROR:  could not find nodes in element (%i %i %i %i)\n",
					elementId, n0, n1, n2);
			return CV_ERROR;
		}

		double a[3];
		double b[3];
		double c[3];
		double norm0, norm1, norm2;

		a[0] = nodes_[0 * numNodes_ + j1 - 1] - nodes_[0 * numNodes_ + j0 - 1];
		a[1] = nodes_[1 * numNodes_ + j1 - 1] - nodes_[1 * numNodes_ + j0 - 1];
		a[2] = nodes_[2 * numNodes_ + j1 - 1] - nodes_[2 * numNodes_ + j0 - 1];
		b[0] = nodes_[0 * numNodes_ + j2 - 1] - nodes_[0 * numNodes_ + j0 - 1];
		b[1] = nodes_[1 * numNodes_ + j2 - 1] - nodes_[1 * numNodes_ + j0 - 1];
		b[2] = nodes_[2 * numNodes_ + j2 - 1] - nodes_[2 * numNodes_ + j0 - 1];
		c[0] = nodes_[0 * numNodes_ + j3 - 1] - nodes_[0 * numNodes_ + j0 - 1];
		c[1] = nodes_[1 * numNodes_ + j3 - 1] - nodes_[1 * numNodes_ + j0 - 1];
		c[2] = nodes_[2 * numNodes_ + j3 - 1] - nodes_[2 * numNodes_ + j0 - 1];

		Cross(a[0], a[1], a[2], b[0], b[1], b[2], &norm0, &norm1, &norm2);
		double mydot = Dot(norm0, norm1, norm2, c[0], c[1], c[2]);

		if (mydot > 0) {
			int tmpj = j0;
			j0 = j2;
			j2 = j1;
			j1 = tmpj;
			debugprint(stddbg, "elementId %i : %i %i %i %i   (flipped0) %lf\n",
					elementId, j0, j1, j2, j3, mydot);
		} else {
			debugprint(stddbg, "elementId %i : %i %i %i %i  %lf\n", elementId,
					j0, j1, j2, j3, mydot);
		}

		// find matching element already read in
		int foundIt = 0;
		for (i = 0; i < numBoundaryFaces_; i++) {
			// Check Global Element Number
			if (boundaryElementsIds_[i] == (elementId - 1)) {
				// Check if the fourth node corresponds
				if (boundaryElements_[3][i] == j3) {
					// Set Code
					if (setCode) {
						iBCB_[i] = code;
						BCB_[1 * numBoundaryFaces_ + i] = value;
						foundIt = 1;
						totCodeFaces++;
					}
					// Set Surface Number
					if (setSurfID) {
						iBCB_[numBoundaryFaces_ + i] = surfID;
						foundIt = 1;
						totSurfIDFaces++;
					}
				}
			}
		}

		if (foundIt == 0) {
			fprintf(stderr,
					"ERROR: could not find pressure face in boundary faces!\n");
			return CV_ERROR;
		}

	}

	// Write Debug Message to Make Sure the number of Surfaces is Correct
	debugprint(stddbg, "Assigned %d Codes and %d surfIDs\n", totCodeFaces,
			totSurfIDFaces);

	// cleanup
	ptids->Delete();
	reader->Delete();

	debugprint(stddbg, "Exiting setBoundaryFacesWithCodeVTK.\n");
	return CV_OK;

}

int cmd_zero_pressure_vtp(char *cmd) {

	// enter
	debugprint(stddbg, "Entering cmd_zero_pressure.\n");

	// do work
	double pressure = 0.0;
	int setSurfID = 0;
	int surfID = 0;
	int setPressure = 1;

	// should use bits and not ints here!!
	int code = 2;

	if (setBoundaryFacesWithCodeVTK(cmd, setSurfID, surfID, setPressure, code,
			pressure) == CV_ERROR) {
		return CV_ERROR;
	}

	// cleanup
	debugprint(stddbg, "Exiting cmd_zero_pressure.\n");
	return CV_OK;
}

int cmd_pressure_vtp(char *cmd) {

	// enter
	debugprint(stddbg, "Entering cmd_pressure.\n");

	// do work
	double pressure = 0.0;

	if (parseDouble2(cmd, &pressure) == CV_ERROR) {
		return CV_ERROR;
	}

	debugprint(stddbg, "  Pressure = %lf\n", pressure);

	int setSurfID = 0;
	int surfID = 0;
	int setPressure = 1;
	// should use bits and not ints here!!
	int code = 2;

	if (setBoundaryFacesWithCodeVTK(cmd, setSurfID, surfID, setPressure, code,
			pressure) == CV_ERROR) {
		return CV_ERROR;
	}

	// cleanup
	debugprint(stddbg, "Exiting cmd_pressure.\n");
	return CV_OK;
}

int cmd_set_surface_id_vtp(char *cmd) {

	// enter
	debugprint(stddbg, "Entering cmd_set_surface_id.\n");

	// do work
	int surfID = 0;
	if (parseNum2(cmd, &surfID) == CV_ERROR) {
		return CV_ERROR;
	}

	debugprint(stddbg, "  Setting surfID to [%i]\n", surfID);

	double value = 0.0;
	int setSurfID = 1;
	int setCode = 0;
	// should use bits and not ints here!!
	int code = 0;

	if (setBoundaryFacesWithCodeVTK(cmd, setSurfID, surfID, setCode, code,
			value) == CV_ERROR) {
		return CV_ERROR;
	}

	// cleanup
	debugprint(stddbg, "Exiting cmd_set_surface_id.\n");
	return CV_OK;
}



int cmd_deformable_wall_vtp_simple(char *cmd) {

    // enter
    debugprint(stddbg, "Entering cmd_deformable_wall.\n");

    // do work
    double value = 0.0;
    int setSurfID = 0;
    int surfID = 0;
    int setCode = 1;
    // should use bits and not ints here!!
    int code = 16;

    if (setBoundaryFacesWithCodeVTK(cmd ,setSurfID, surfID, setCode, code,
            value) == CV_ERROR) {
        return CV_ERROR;
    }

    dispsoln_ = new double[3*numNodes_]();

    // cleanup
    debugprint(stddbg, "Exiting cmd_deformable_wall.\n");
    return CV_OK;
}

int cmd_fix_free_edge_nodes_vtp(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_fix_free_edge_nodes_vtp.\n");

    int i,j;

    // parse command string for filename
    char polyfn[MAXPATHLEN];
    polyfn[0] = '\0';
    parseCmdStr(cmd, polyfn);

    // check file exists
    FILE* fp = NULL;
    if ((fp = fopen(polyfn, "r")) == NULL) {
        fprintf(stderr, "ERROR: could not open file (%s).", polyfn);
        return CV_ERROR;
    } else {
        fclose(fp);
    }

    // read file
    vtkPolyData* pd = NULL;
    vtkXMLPolyDataReader* reader = vtkXMLPolyDataReader::New();
    reader->SetFileName(polyfn);
    reader->Update();
    pd = reader->GetOutput();
    if (pd == NULL) {
        fprintf(stderr, "ERROR: problem parsing file (%s).", polyfn);
        return CV_ERROR;
    }

    pd->GetPointData()->SetActiveScalars("GlobalNodeID");
    pd->GetCellData()->SetActiveScalars("GlobalElementID");

    vtkIdList* ptids = vtkIdList::New();
    ptids->Allocate(10, 10);
    ptids->Initialize();

    vtkCellArray *cells = pd->GetPolys();
    cells->InitTraversal();

    vtkIntArray* gids = NULL;
    gids = static_cast<vtkIntArray*>(pd->GetPointData()->GetArray(
            "GlobalNodeID"));
    if (gids == NULL) {
        fprintf(stderr, "ERROR: problem finding GlobalNodeID");
        return CV_ERROR;
    }

    int numEle = cells->GetNumberOfCells();
    if (numEle == 0) {
        return CV_ERROR;
    }

    int* edges[2];
    edges[0] = new int[numEle*3];
    edges[1] = new int[numEle*3];

    int n0,n1,n2;
    int elementId;
    int numEdges = 0;

    for (int icell = 0; icell < cells->GetNumberOfCells(); icell++) {

        ptids->Reset();
        cells->GetCell(4 * icell, ptids);
        if (ptids->GetNumberOfIds() != 3) {
            fprintf(stderr, "ERROR:  invalid number of ids in cell (%i)!",
                    ptids->GetNumberOfIds());
            return CV_ERROR;
        }
        elementId = pd->GetCellData()->GetScalars()->GetTuple1(icell);
        n0 = gids->GetTuple1(ptids->GetId(0));
        n1 = gids->GetTuple1(ptids->GetId(1));
        n2 = gids->GetTuple1(ptids->GetId(2));

        // stuff edges into array

        // edge 1
        if (n0 < n1) {
            edges[0][numEdges] = n0;
            edges[1][numEdges] = n1;
            numEdges++;
        } else {
            edges[0][numEdges] = n1;
            edges[1][numEdges] = n0;
            numEdges++;
        }
        // edge 2
        if (n1 < n2) {
            edges[0][numEdges] = n1;
            edges[1][numEdges] = n2;
            numEdges++;
        } else {
            edges[0][numEdges] = n2;
            edges[1][numEdges] = n1;
            numEdges++;
        }
        // edge 3
        if (n0 < n2) {
            edges[0][numEdges] = n0;
            edges[1][numEdges] = n2;
            numEdges++;
        } else {
            edges[0][numEdges] = n2;
            edges[1][numEdges] = n0;
            numEdges++;
        }
    }

    // sort edges so we can find free edges

    // debugging output
    //for (i = 0; i < numEdges; i++) {
    //    debugprint(stddbg,"org order [%i] %i %i\n",i,edges[0][i],edges[1][i]);
    //}

   int index0, index1;

    // heap sort

    int temp0,temp1;
    int array_size = numEdges;

    for (i = (array_size / 2)-1; i >= 0; i--)
      siftDownEdges(edges, i, array_size, array_size);

    for (i = array_size-1; i >= 1; i--)
    {
      temp0 = edges[0][0];
      temp1 = edges[1][0];
      edges[0][0] = edges[0][i];
      edges[1][0] = edges[1][i];
      edges[0][i] = temp0;
      edges[1][i] = temp1;
      siftDownEdges(edges, 0, i-1, array_size);
    }



    // insertion sort algorithm
/*
    for (i=1; i < numEdges; i++) {
       index0 = edges[0][i];
       index1 = edges[1][i];
       j = i;
       while ((j > 0) && (edges[0][j-1] > index0)) {
         edges[0][j] = edges[0][j-1];
         edges[1][j] = edges[1][j-1];
         j = j - 1;
       }
       edges[0][j] = index0;
       edges[1][j] = index1;
    }
*/

    // debugging output
    //for (i = 0; i < numEdges; i++) {
    //    debugprint(stddbg,"after heap [%i] %i %i\n",i,edges[0][i],edges[1][i]);
    //}

    // second pass
    // NOTE:  if you use the heap double heap sort above, this pass
    // should be unnecessary

    for (i=1; i < numEdges; i++) {
       index0 = edges[0][i];
       index1 = edges[1][i];
       j = i;
       while ((j > 0) && ((edges[0][j-1] >= index0) && (edges[1][j-1] > index1))) {
         edges[0][j] = edges[0][j-1];
         edges[1][j] = edges[1][j-1];
         j = j - 1;
       }
       edges[0][j] = index0;
       edges[1][j] = index1;
    }

    // debugging output
    //for (i = 0; i < numEdges; i++) {
    //    debugprint(stddbg,"after insert [%i] %i %i\n",i,edges[0][i],edges[1][i]);
    //}

    // fix free nodes
    for (i = 0; i < numEdges-1; i++) {
      // if edge occurs twice, not a free edge
      if ((edges[0][i] == edges[0][i+1]) && (edges[1][i] == edges[1][i+1])) {
          i++;
      } else {
          debugprint(stddbg,"  Fixing Node: %i\n",edges[0][i]);
          debugprint(stddbg,"  Fixing Node: %i\n",edges[1][i]);
          // no slip code
          // this should be a bit set instead of an int!!
          iBC_[edges[0][i] - 1] = 56;
          iBC_[edges[1][i] - 1] = 56;
      }
    }

    // cleanup
    ptids->Delete();
    reader->Delete();
    delete [] edges[0];
    delete [] edges[1];

    debugprint(stddbg,"Exiting cmd_fix_free_edge_nodes_vtp.\n");
    return CV_OK;
}

int cmd_create_mesh_deformable_vtp(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_create_mesh_deformable_vtp.\n");

    int i,j;

	// parse command string for filename
	char polyfn[MAXPATHLEN];
	polyfn[0] = '\0';
	parseCmdStr(cmd, polyfn);

	// check file exists
	FILE* fp = NULL;
	if ((fp = fopen(polyfn, "r")) == NULL) {
		fprintf(stderr, "ERROR: could not open file (%s).", polyfn);
		return CV_ERROR;
	} else {
		fclose(fp);
	}

	// read file
	vtkPolyData* pd = NULL;
	vtkXMLPolyDataReader* reader = vtkXMLPolyDataReader::New();
	reader->SetFileName(polyfn);
	reader->Update();
	pd = reader->GetOutput();
	if (pd == NULL) {
		fprintf(stderr, "ERROR: problem parsing file (%s).", polyfn);
		return CV_ERROR;
	}

	pd->GetPointData()->SetActiveScalars("GlobalNodeID");
	pd->GetCellData()->SetActiveScalars("GlobalElementID");

	vtkIdList* ptids = vtkIdList::New();
	ptids->Allocate(10, 10);
	ptids->Initialize();

	vtkCellArray *cells = pd->GetPolys();
	cells->InitTraversal();

	vtkIntArray* gids = NULL;
	gids = static_cast<vtkIntArray*>(pd->GetPointData()->GetArray(
			"GlobalNodeID"));
	if (gids == NULL) {
		fprintf(stderr, "ERROR: problem finding GlobalNodeID");
		return CV_ERROR;
	}

    int numEle = cells->GetNumberOfCells();
    if (numEle == 0) {
        return CV_ERROR;
    }

    int* ids[3];
    ids[0] = new int[numEle*3];
    ids[1] = new int[numEle*3];
    ids[2] = new int[numEle*3];

    int n0,n1,n2;
    int elementId;
    int numIds = 0;
    int numElements = 0;

	for (int icell = 0; icell < cells->GetNumberOfCells(); icell++) {

		ptids->Reset();
		cells->GetCell(4 * icell, ptids);
		if (ptids->GetNumberOfIds() != 3) {
			fprintf(stderr, "ERROR:  invalid number of ids in cell (%i)!",
					ptids->GetNumberOfIds());
			return CV_ERROR;
		}
		elementId = pd->GetCellData()->GetScalars()->GetTuple1(icell);
		n0 = gids->GetTuple1(ptids->GetId(0));
		n1 = gids->GetTuple1(ptids->GetId(1));
		n2 = gids->GetTuple1(ptids->GetId(2));

        ids[0][numIds] = n0;
        ids[1][numIds] = numElements;
        ids[2][numIds] = 0;
        numIds++;
        ids[0][numIds] = n1;
        ids[1][numIds] = numElements;
        ids[2][numIds] = 1;
        numIds++;
        ids[0][numIds] = n2;
        ids[1][numIds] = numElements;
        ids[2][numIds] = 2;
        numIds++;

        numElements++;
    }

    // sort node ids so we can find duplicates

    // debugging output
    //for (i = 0; i < numIds; i++) {
    //    debugprint(stddbg,"original:  %i %i %i %i\n",i,ids[0][i],ids[1][i],ids[2][i]);
    //}

    // heap sort

    int array_size = numIds;
    int temp0,temp1,temp2;

    for (i = (array_size / 2)-1; i >= 0; i--){
        siftDownKentriesCalcMesh(ids, i, array_size, array_size);
    }

    for (i = array_size-1; i >= 1; i--) {
          temp0 = ids[0][0];
          temp1 = ids[1][0];
          temp2 = ids[2][0];
          ids[0][0] = ids[0][i];
          ids[1][0] = ids[1][i];
          ids[2][0] = ids[2][i];
          ids[0][i] = temp0;
          ids[1][i] = temp1;
          ids[2][i] = temp2;
          siftDownKentriesCalcMesh(ids, 0, i-1, array_size);
    }

    /*
    // insertion sort algorithm
    int index0, index1, index2;

    for (i=1; i < numIds; i++) {
       index0 = ids[0][i];
       index1 = ids[1][i];
       index2 = ids[2][i];
       j = i;
       while ((j > 0) && (ids[0][j-1] > index0)) {
         ids[0][j] = ids[0][j-1];
         ids[1][j] = ids[1][j-1];
         ids[2][j] = ids[2][j-1];
         j = j - 1;
       }
       ids[0][j] = index0;
       ids[1][j] = index1;
       ids[2][j] = index2;
    }
    */

    // debugging output
    //for (i = 0; i < numIds; i++) {
    //    debugprint(stddbg,"sorted:  %i %i %i %i\n",i,ids[0][i],ids[1][i],ids[2][i]);
    //}

    // count the number of unique nodes
    int numUniqueNodes = 1;
    int index0 = ids[0][0];
    for (i = 0; i < numIds; i++) {
        if (ids[0][i] != index0) {
            numUniqueNodes++;
            index0 = ids[0][i];
        }
    }
    debugprint(stddbg,"  Number of Unique Nodes Found: %i\n",numUniqueNodes);

    // create renumbered connectivity for initial disp. calc.
    int* conn[3];
    conn[0] = new int[numElements];
    conn[1] = new int[numElements];
    conn[2] = new int[numElements];

    int* map = new int[numUniqueNodes];

    index0 = -1;
    j = 0;
    for (i = 0; i < numIds; i++) {
        if (ids[0][i] != index0) {
            map[j] = ids[0][i];
            //debugprint(stddbg,"  map[%i] %i\n",j,map[j]);
            index0 = ids[0][i];
            j++;
        }
        conn[ids[2][i]][ids[1][i]] = j - 1;
    }
    debugprint(stddbg,"  Number of Unique Nodes Found: %i\n",j);

    delete [] ids[0];
    delete [] ids[1];
    delete [] ids[2];

    // set global variables
    DisplacementNumElements_ = numElements;
    DisplacementConn_[0]     = conn[0];
    DisplacementConn_[1]     = conn[1];
    DisplacementConn_[2]     = conn[2];
    DisplacementNumNodes_    = numUniqueNodes;
    DisplacementNodeMap_     = map;

	// cleanup
	ptids->Delete();
	reader->Delete();

    debugprint(stddbg,"Exiting cmd_create_mesh_deformable_vtp.\n");
    return CV_OK;
}

int cmd_deformable_wall_vtp(char *cmd) {

    if(cmd_deformable_wall_vtp_simple(cmd)==CV_OK
            &&  cmd_fix_free_edge_nodes_vtp(cmd)==CV_OK
            &&  cmd_create_mesh_deformable_vtp(cmd)==CV_OK){
        return CV_OK;
    }else{
        return CV_ERROR;
    }
}

vtkUnstructuredGrid* createGrid(int nshgtot,double* xglobal,int neltot, int* ien){
    int i;
    vtkPoints* pts = NULL;
    vtkUnstructuredGrid* grid = NULL;

    grid = vtkUnstructuredGrid::New();
    grid->Allocate(neltot,1000);

    pts = vtkPoints::New();
    pts->Allocate(nshgtot,1000);
    pts->SetNumberOfPoints(nshgtot);

    vtkIntArray* gid = vtkIntArray::New();
    gid->SetNumberOfComponents(1);
    gid->Allocate(nshgtot,1000);
    gid->SetNumberOfTuples(nshgtot);
    gid->SetName("GlobalNodeID");

    for( i=0; i< nshgtot; i++ ) {
        pts->SetPoint(i,xglobal[0*nshgtot+i],xglobal[1*nshgtot+i],xglobal[2*nshgtot+i]);
        gid->SetTuple1(i,i+1);
    }

    grid->SetPoints(pts);
    grid->GetPointData()->AddArray(gid);

    pts->Delete();
    gid->Delete();

    vtkIdList* ptids = vtkIdList::New();
    ptids->Allocate(10,10);
    ptids->Initialize();
    ptids->SetNumberOfIds(4);

    vtkIntArray* eid = vtkIntArray::New();
    eid->SetNumberOfComponents(1);
    eid->Allocate(neltot,1000);
    eid->SetNumberOfTuples(neltot);
    eid->SetName("GlobalElementID");


    for(i=0; i< neltot; i++){
        ptids->SetId(0,ien[0*neltot+i]-1);
        ptids->SetId(1,ien[1*neltot+i]-1);
        ptids->SetId(2,ien[2*neltot+i]-1);
        ptids->SetId(3,ien[3*neltot+i]-1);
        grid->InsertNextCell(VTK_TETRA,ptids);
        eid->SetTuple1(i,i+1);
    }

    ptids->Delete();

    grid->GetCellData()->SetScalars(eid);
    grid->GetCellData()->SetActiveScalars("GlobalElementID");
    grid->GetCellData()->CopyAllOn();

    eid->Delete();

    return grid;
}

int cmd_wall_displacements_write_vtp(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_wall_displacements_write_vtp.\n");

    char outfile[MAXPATHLEN];

    // do work
    parseCmdStr(cmd,outfile);

    // some simple validity checks
    if (numNodes_ == 0 || numSolnVars_ == 0) {
        fprintf(stderr,"ERROR:  Not all required info set!\n");
        return CV_ERROR;
    }

    if (dispsoln_== NULL) {
         fprintf(stderr,"ERROR: Displacements not computed.\n");
         return CV_ERROR;
    }

    int i;

    vtkUnstructuredGrid* grid =createGrid(numNodes_,nodes_,numElements_,elements_);


    vtkDoubleArray *disp = vtkDoubleArray::New();
    disp->SetNumberOfComponents(3);
    disp->Allocate(numNodes_,10000);
    disp->SetNumberOfTuples(numNodes_);
    disp->SetName("displacement");
    for(i=0; i< numNodes_; i++){
        disp->SetTuple3(i,dispsoln_[0*numNodes_+i],dispsoln_[1*numNodes_+i],dispsoln_[2*numNodes_+i]);
    }

    grid->GetPointData()->AddArray(disp);

    disp->Delete();

    vtkGeometryFilter* surfFilt = vtkGeometryFilter::New();
    surfFilt->MergingOff();
    surfFilt->SetInputDataObject(grid);
    surfFilt->Update();
    vtkCleanPolyData* cleaner = vtkCleanPolyData::New();
    cleaner->PointMergingOff();
    cleaner->PieceInvariantOff();
    cleaner->SetInputDataObject(surfFilt->GetOutput());
    cleaner->Update();

    vtkXMLPolyDataWriter *polywriter = vtkXMLPolyDataWriter::New();
    polywriter->SetCompressorTypeToZLib();
    polywriter->EncodeAppendedDataOff();
    polywriter->SetInputDataObject(cleaner->GetOutput());
    if(outfile[0]=='\0'){
        polywriter->SetFileName("displacement.vtp");
    }else{
        polywriter->SetFileName(outfile);
    }
    polywriter->Write();
    polywriter->Delete();
    cleaner->Delete();
    surfFilt->Delete();
    grid->Delete();

    debugprint(stddbg,"Exiting cmd_wall_displacements_write_vtp.\n");

    return CV_OK;

}

#if(VER_VARWALL == 1)
int cmd_set_scalar_BCs_vtp(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_set_scalar_BCs_vtp.\n");

    // do work
    double value = 0;
    if (parseDouble2(cmd,&value) == CV_ERROR) {
        return CV_ERROR;
    }

    debugprint(stddbg,"Setting surface thickness or Evw to [%lf] \n",value);

	// parse command string for filename
	char polyfn[MAXPATHLEN];
	polyfn[0] = '\0';
	parseCmdStr(cmd, polyfn);

	// check file exists
	FILE* fp = NULL;
	if ((fp = fopen(polyfn, "r")) == NULL) {
		fprintf(stderr, "ERROR: could not open file (%s).", polyfn);
		return CV_ERROR;
	} else {
		fclose(fp);
	}

	// read file
	vtkPolyData* pd = NULL;
	vtkXMLPolyDataReader* reader = vtkXMLPolyDataReader::New();
	reader->SetFileName(polyfn);
	reader->Update();
	pd = reader->GetOutput();
	if (pd == NULL) {
		fprintf(stderr, "ERROR: problem parsing file (%s).", polyfn);
		return CV_ERROR;
	}
	vtkIntArray* gids = NULL;
	gids =static_cast<vtkIntArray*>(reader->GetOutput()->GetPointData()->GetArray("GlobalNodeID"));
	if (gids == NULL) {
		fprintf(stderr, "ERROR: problem finding GlobalNodeID");
		return CV_ERROR;
	}

    if (gBC_ == NULL) {
     gBC_ = new double [numNodes_];
     for (int i  = 0; i < numNodes_; i++) {
         gBC_[i] = -1.0;
     }
    }

	for (int i = 0; i < gids->GetNumberOfTuples(); i++) {
		int nodeId = gids->GetTuple1(i);
		gBC_[nodeId - 1] = value;
	}

	// cleanup
	reader->Delete();

    // cleanup
    debugprint(stddbg,"Exiting cmd_set_scalar_BCs_vtp.\n");
    return CV_OK;
}

// SET THICKNESS BC
int cmd_set_thickness_BCs_vtp(char *cmd){
  return cmd_set_scalar_BCs_vtp(cmd);
}

// SET ELASTIC MODULUS BC
int cmd_set_Evw_BCs_vtp(char *cmd){
  return cmd_set_scalar_BCs_vtp(cmd);
}

int cmd_set_Initial_Evw_vtp(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_set_Initial_Evw_vtp.\n");

    // do work
    double value = 0;
    if (parseDouble2(cmd,&value) == CV_ERROR) {
        return CV_ERROR;
    }

    debugprint(stddbg,"Setting initial value to [%lf] \n",value);

	// parse command string for filename
	char polyfn[MAXPATHLEN];
	polyfn[0] = '\0';
	parseCmdStr(cmd, polyfn);

	// check file exists
	FILE* fp = NULL;
	if ((fp = fopen(polyfn, "r")) == NULL) {
		fprintf(stderr, "ERROR: could not open file (%s).", polyfn);
		return CV_ERROR;
	} else {
		fclose(fp);
	}

	// read file
	vtkPolyData* pd = NULL;
	vtkXMLPolyDataReader* reader = vtkXMLPolyDataReader::New();
	reader->SetFileName(polyfn);
	reader->Update();
	pd = reader->GetOutput();
	if (pd == NULL) {
		fprintf(stderr, "ERROR: problem parsing file (%s).", polyfn);
		return CV_ERROR;
	}
	vtkIntArray* gids = NULL;
	gids =static_cast<vtkIntArray*>(reader->GetOutput()->GetPointData()->GetArray("GlobalNodeID"));
	if (gids == NULL) {
		fprintf(stderr, "ERROR: problem finding GlobalNodeID");
		return CV_ERROR;
	}

    if (gBC_ == NULL) {
     gBC_ = new double [numNodes_];
     for (int i  = 0; i < numNodes_; i++) {
         gBC_[i] = -1.0;
     }
    }

    if (EvwSolution_ == NULL) {
     EvwSolution_ = new double [numNodes_];
     for (int i  = 0; i < numNodes_; i++) {
         EvwSolution_[i] = 0.0;
     }
    }

	for (int i = 0; i < gids->GetNumberOfTuples(); i++) {
		int nodeId = gids->GetTuple1(i);
		EvwSolution_[nodeId - 1] = value;
	}

	// cleanup
	reader->Delete();

    // cleanup
    debugprint(stddbg,"Exiting cmd_set_Initial_Evw_vtp.\n");
    return CV_OK;
}

int cmd_varwallprop_write_vtp(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_varwallprop_write_vtp.\n");

    char outfile[MAXPATHLEN];

    // do work
    parseCmdStr(cmd,outfile);

    // some simple validity checks
    if (numNodes_ == 0 || numSolnVars_ == 0) {
        fprintf(stderr,"ERROR:  Not all required info set!\n");
        return CV_ERROR;
    }

    if (ThicknessSolution_== NULL && EvwSolution_ == NULL ) {
         fprintf(stderr,"ERROR: Both ThicknessSolution_ and EvwSolution_ are not computed.\n");
         return CV_ERROR;
    }

    int i;

    vtkUnstructuredGrid* grid =createGrid(numNodes_,nodes_,numElements_,elements_);

    if (ThicknessSolution_ != NULL) {
        vtkDoubleArray *thickness = vtkDoubleArray::New();
        thickness->SetNumberOfComponents(1);
        thickness->Allocate(numNodes_,10000);
        thickness->SetNumberOfTuples(numNodes_);
        thickness->SetName("thickness");
        for(i=0; i< numNodes_; i++){
            thickness->SetTuple1(i,ThicknessSolution_[i]);
        }

        grid->GetPointData()->AddArray(thickness);

        thickness->Delete();
    }

    if (EvwSolution_ != NULL) {
        vtkDoubleArray *Evw = vtkDoubleArray::New();
        Evw->SetNumberOfComponents(1);
        Evw->Allocate(numNodes_,10000);
        Evw->SetNumberOfTuples(numNodes_);
        Evw->SetName("Young_Mod");
        for(i=0; i< numNodes_; i++){
            Evw->SetTuple1(i,EvwSolution_[i]);
        }

        grid->GetPointData()->AddArray(Evw);

        Evw->Delete();
    }

    vtkGeometryFilter* surfFilt = vtkGeometryFilter::New();
    surfFilt->MergingOff();
    surfFilt->SetInputDataObject(grid);
    surfFilt->Update();
    vtkCleanPolyData* cleaner = vtkCleanPolyData::New();
    cleaner->PointMergingOff();
    cleaner->PieceInvariantOff();
    cleaner->SetInputDataObject(surfFilt->GetOutput());
    cleaner->Update();

    vtkXMLPolyDataWriter *polywriter = vtkXMLPolyDataWriter::New();
    polywriter->SetCompressorTypeToZLib();
    polywriter->EncodeAppendedDataOff();
    polywriter->SetInputDataObject(cleaner->GetOutput());
    if(outfile[0]=='\0'){
        polywriter->SetFileName("varwallprop.vtp");
    }else{
        polywriter->SetFileName(outfile);
    }
    polywriter->Write();
    polywriter->Delete();
    cleaner->Delete();
    surfFilt->Delete();
    grid->Delete();

    debugprint(stddbg,"Exiting cmd_varwallprop_write_vtp.\n");

    return CV_OK;

}
#endif


int cmd_bct_create(char *cmd) {

    debugprint(stddbg,"Entering cmd_bct_create.\n");

    char faceFile[MAXPATHLEN];
    parseCmdStr(cmd,faceFile);

    char flowFile[MAXPATHLEN];
    parseCmdStr2(cmd,flowFile);

    debugprint(stddbg,"  Create BCT for (%s) with (%s)\n",faceFile,flowFile);

    BCTData bct;

    if(create_bct(bct,faceFile,flowFile,rho_,mu_,bctShape_,bctPeriod_,bctPointNum_,bctModeNum_,bctPreserve_,bctFlip_)==CV_ERROR){
        return CV_ERROR;
    }

    bctNodeNumTotal_+=bct.pd->GetNumberOfPoints();
    if(bct.pointNum>bctPointNumMax_){
        bctPointNumMax_=bct.pointNum;
    }

    vbct.push_back(bct);

    debugprint(stddbg,"Exiting cmd_bct_create.\n");
    return CV_OK;
}

int cmd_bct_write_vtp(char *cmd) {

    debugprint(stddbg,"Entering cmd_bct_write_vtp.\n");

    char filename[MAXPATHLEN];
    char newfilename[MAXPATHLEN];

    parseCmdStr(cmd,filename);

    vtkAppendPolyData* appendFilter=NULL;

    if(bctMerge_==1 && vbct.size()>1){
        appendFilter =vtkAppendPolyData::New();

    }
    for(int n=0;n<vbct.size();n++){
        BCTData bct=vbct[n];
        vtkPolyData* pd=bct.pd;
        double* t=bct.t;
        int pointNum=bct.pointNum;
        int nodeNum=pd->GetNumberOfPoints();

        char vel_name[40];
        for(int j=0;j<pointNum;j++){
            sprintf(vel_name,"velocity_%06.4f",t[j]);
            bct.mapped_data[j]->SetName(vel_name);
            pd->GetPointData()->AddArray(bct.mapped_data[j]);
        }
        sprintf(vel_name,"velocity_%06.4f",t[pointNum]);
        vtkDoubleArray* mdata=vtkDoubleArray::New();
        mdata->DeepCopy(bct.mapped_data[0]);
        mdata->SetName(vel_name);
        pd->GetPointData()->AddArray(mdata);

        if(bctMerge_!=1 || vbct.size()==1){

            if(filename[0]=='\0'){
                strcpy(newfilename, "bct.vtp");
            }else{
                strcpy(newfilename,filename);
            }

            if(vbct.size()>1){

                if(filename[0]=='\0'){
                    strcpy(filename, "bct.vtp");
                }

                strncpy (newfilename, filename, strlen(filename)-4 );
                newfilename[strlen(filename)-4]='\0';

                char rmdr[40];
                sprintf(rmdr,"%d.vtp",n+1);
                strcat(newfilename,rmdr);

            }

            vtkXMLPolyDataWriter *polywriter = vtkXMLPolyDataWriter::New();
            polywriter->SetCompressorTypeToZLib();
            polywriter->EncodeAppendedDataOff();
            polywriter->SetInputDataObject(pd);
            polywriter->SetFileName(newfilename);
            polywriter->Write();
            polywriter->Delete();
        }else{
            appendFilter->AddInputData(pd);
        }

    }

    if(bctMerge_==1 && vbct.size()>1){
        appendFilter->Update();
        vtkXMLPolyDataWriter *polywriter = vtkXMLPolyDataWriter::New();
        polywriter->SetCompressorTypeToZLib();
        polywriter->EncodeAppendedDataOff();
        polywriter->SetInputDataObject(appendFilter->GetOutput());
        if(filename[0]=='\0'){
            strcpy(filename, "bct.vtp");
        }

        polywriter->SetFileName(filename);
        polywriter->Write();
        polywriter->Delete();

        appendFilter->Delete();

    }

    debugprint(stddbg,"Exiting cmd_bct_write_vtp.\n");
    return CV_OK;
}

