/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "SimVascular.h"
#include "cv_misc_utils.h"
#include <stdio.h>
#include <math.h>
#include "cvVTK.h"
#include "cv_spline.h"

int sys_geom_splinePtsToPathPlan (vtkPolyData *pd,int numOutputPts,
                                  char *filename, int flag, Tcl_Interp *interp)
{  
  SplinePoints *x;
  SplinePoints *y;
  int i, j;
  int num, dimensions;
  vtkFloatingPointType *pt;
  int numInputPts = 0;
  int numPaths = 1;
  FILE *outfile;

  // Get points for each path
  for (j = 0; j < numPaths; j++)
    {
      // get number of points from vtkPolyData file 
      numInputPts = pd->GetNumberOfPoints(); 

      x = sys_geom_SplinePointsInit(numInputPts, 3);
      y = sys_geom_SplinePointsInit(numOutputPts, 3);

      // Remaining lines are coordinates of points
      for (i = 0; i < numInputPts; i++) {   
         pt = pd->GetPoint(i);
         x->pts[i * 3] = pt[0];
         x->pts[i * 3 + 1] = pt[1];
         x->pts[i * 3 + 2] = pt[2];
      }
      
      if (sys_geom_SplinePath(x, 1, numOutputPts, flag, y) == CV_ERROR) {
        sys_geom_SplinePointsDelete(x);
        sys_geom_SplinePointsDelete(y);
        return CV_ERROR;
      }

      num = y->numPts;
      dimensions = y->dim;

      // if filename is NULL, then do not generate an output file
      if (filename != NULL) {
        outfile = fopen(filename, "w");
        fprintf(outfile, "pointcount=%d\nviewinterval=1.00000\nviewwindow=25.00000\nviewcount=%d\n", numOutputPts, numOutputPts);

 
        for (i = 0; i < num; i++) {
	  fprintf(outfile, "p=(%f,%f,%f) t=(%f,%f,%f) tx=(%f,%f,%f)\n", y->pts[dimensions * i], y->pts[dimensions * i + 1], y->pts[dimensions * i + 2], y->tangents[dimensions * i], y->tangents[dimensions * i + 1], y->tangents[dimensions * i + 2], y->rotVectors[dimensions * i], y->rotVectors[dimensions * i + 1], y->rotVectors[dimensions * i  + 2]);
        }
        fclose(outfile);
      }

      // return the splined path to the Tcl interpreter
      if (interp != NULL) {
        char rtnstr[2048];
        for (i = 0; i < num; i++) {
          rtnstr[0]='\0';
	  sprintf(rtnstr, "p (%f,%f,%f) t (%f,%f,%f) tx (%f,%f,%f)", y->pts[dimensions * i], y->pts[dimensions * i + 1], y->pts[dimensions * i + 2], y->tangents[dimensions * i], y->tangents[dimensions * i + 1], y->tangents[dimensions * i + 2], y->rotVectors[dimensions * i], y->rotVectors[dimensions * i + 1], y->rotVectors[dimensions * i  + 2]);
          Tcl_AppendElement(interp, rtnstr);
        }
      }
      
      sys_geom_SplinePointsDelete(x);
      sys_geom_SplinePointsDelete(y);
    }
  
  return CV_OK;
}

/***********************************************************************
 *                                                                     *
 * sys_geom_SplinePointsInit                                           *
 * -------------------------                                           *
 * Initializes the struct SplinePoints                                 *
 *                                                                     *
 ***********************************************************************/

SplinePoints *sys_geom_SplinePointsInit(int numPts, int dimensions)
{
  SplinePoints *sp;
  int i;


  sp = new SplinePoints;
  sp->pts = new vtkFloatingPointType[numPts * dimensions];
  sp->tangents = new vtkFloatingPointType[numPts * dimensions];
  sp->rotVectors = new vtkFloatingPointType[numPts * dimensions];
  sp->numPts = numPts;
  sp->dim = dimensions;

  for (i = 0; i < (numPts * dimensions); i++)
    {
      sp->pts[i] = 0;
      sp->tangents[i] = 0;
      sp->rotVectors[i] = 0;
    }

  return(sp);
}


/***********************************************************************
 *                                                                     *
 * sys_geom_SplinePointsDelete                                         *
 * ---------------------------                                         *
 * Frees up the memory associated with the struct SplinePoints         *
 *                                                                     *   
 ***********************************************************************/

void sys_geom_SplinePointsDelete(SplinePoints *sp)
{
  delete[] sp->pts;
  delete[] sp->tangents;
  delete[] sp->rotVectors;
  delete sp;
}

/***********************************************************************
 *                                                                     *
 * sys_geom_SplineInterpolate                                          *
 * --------------------------                                          *
 * Performs spline interpolation.                                      *
 *                                                                     *   
 * Assumptions:  Open spline.  Results of interpolation not clamped    *
 *               to input data.                                        *
 ***********************************************************************/

void sys_geom_SplineInterpolate(SplinePoints *input, int type, int numberOfOutputPoints, SplinePoints *output)
{

  vtkSpline *spline;
  int i, j, k;
  vtkFloatingPointType t;
  int dimensions;

  /*****************
   *     body      *
   *****************/

  switch (type) 
    {
    case 1:  /* Cardinal basis */
      spline = (vtkSpline *)vtkCardinalSpline::New();
      break;

    case 2:  /* Kochanek basis */
      spline = (vtkSpline *)vtkKochanekSpline::New();
      break;

    default:
      cerr << "   ERROR >>>> In sys_geom_SplineInterpolate(...)" << endl;
      cerr << "              Unknown interpolation method requested." << endl;
      return;
    }


  dimensions = input->dim;

  if ((dimensions > MAX_DIM) || (dimensions < 1))
    {
      cerr << "   ERROR >>>> In sys_geom_SplineInterpolate(...)" << endl;
      cerr << "              Invalid number of dimensions." << endl;
      return;
    }

  for (i = 0; i < dimensions; i++)
    {
      spline->RemoveAllPoints();

      /* Add points to spline object */
      for (j = 0; j < (input->numPts); j++)
	  spline->AddPoint(j, input->pts[dimensions * j + i]);

      /* Set spline parameters */
      spline->ClampValueOff();
      spline->ClosedOff();
      spline->Modified();

      /* Evaluate spline */
      for (k = 0; k < numberOfOutputPoints; k++)
	{
	  t = ((input->numPts - 1.0) / (numberOfOutputPoints - 1)) * k;

	  switch (type)
	    {
	    case 1:  
	      output->pts[dimensions * k + i] = ((vtkCardinalSpline *)spline)->Evaluate(t);
	      break;

	    case 2:
	      output->pts[dimensions * k + i] = ((vtkKochanekSpline *)spline)->Evaluate(t);
	      break;
	    }
	}
      
    }

  output->numPts = numberOfOutputPoints;
  output->dim = dimensions;

  /* Clean up */
  spline->Delete();
}


/***********************************************************************
 *                                                                     *
 * sys_geom_SplineGetTangents                                          *
 * --------------------------                                          *
 * Computes the spline tangents and returns the tangents and their     *
 * corresponding points.                                               *
 *                                                                     *   
 * Assumptions:  Open spline.                                          *
 * Approach is to compute spline for many, many points and calculate   *
 * the tangent using a finite difference.                              *
 ***********************************************************************/

int sys_geom_SplineGetTangents(SplinePoints *input, int type, int numberOfOutputPoints, int matchEndPoints, SplinePoints *output)
{

  SplinePoints *tmpSpline;
  int numberOfTangentPoints;
  int i, j;
  int interval;
  int dimensions;
  vtkFloatingPointType coordinate;
  vtkFloatingPointType *tmpTangent = new vtkFloatingPointType [MAX_DIM];
  vtkFloatingPointType *tmpNormTangent = new vtkFloatingPointType [MAX_DIM];
  int err;

  /*****************
   *     body      *
   *****************/


  /* Want a minimum of 500 points...this number was pulled out of my magical hat. */
  if (numberOfOutputPoints < 50)
    numberOfTangentPoints = (ceil(500.0 / numberOfOutputPoints)) * numberOfOutputPoints;
  else
    numberOfTangentPoints = 10 * numberOfOutputPoints;
    

  tmpSpline = sys_geom_SplinePointsInit(numberOfTangentPoints, input->dim);
  sys_geom_SplineInterpolate(input, type, numberOfTangentPoints, tmpSpline);
  
  if (matchEndPoints != 0)
    interval = floor(1.0*numberOfTangentPoints / (numberOfOutputPoints - 1));
  else
    interval = floor(1.0*numberOfTangentPoints / numberOfOutputPoints);
  
  /* Assume input and output have same dimensions */
  dimensions = output->dim;

 
  for (i = 0; i < (numberOfOutputPoints - 1); i++)
    { 
      for (j = 0; j < MAX_DIM; j++)
	{
	  tmpTangent[j] = 0.0;   /* Reset */
	  tmpNormTangent[j] = 0.0;
	}

      for (j = 0; j < dimensions; j++)
	{
	  coordinate  = tmpSpline->pts[dimensions * i * interval + j];

	  /* Copy point coordinates */
	  output->pts[dimensions * i + j] = coordinate;
	
	  /* Compute forward difference */
	  tmpTangent[j] = tmpSpline->pts[dimensions * (i * interval + 1) + j] - coordinate;
	}

  
      /* Normalize tangent */
      
      err = sys_geom_NormalizeVector(tmpTangent, tmpNormTangent, MAX_DIM);
      if (err == 0)
	{
	  fprintf(stderr,"Subdivision too small for computing tangents.  Decrease number of output points requested.\n");
	  return CV_ERROR;
	}

      for (j = 0; j < dimensions; j++)
	output->tangents[dimensions * i + j] = tmpNormTangent[j];
    }


  /* 
   * The last point just gets copied over and the tangent is based on the 
   * backwards difference.
   */  
  for (j = 0; j < MAX_DIM; j++)
    {
      tmpTangent[j] = 0.0;   /* Reset */
      tmpNormTangent[j] = 0.0;
    }


  for (j = 0; j < dimensions; j++)
    {

      if (matchEndPoints == 0)
	coordinate = tmpSpline->pts[dimensions * (numberOfTangentPoints - 1) + j];
      else
	coordinate = input->pts[dimensions * (input->numPts - 1) + j];
      
 
      output->pts[dimensions * (numberOfOutputPoints - 1) + j] = coordinate;
      
      tmpTangent[j] = coordinate - tmpSpline->pts[tmpSpline->dim * (tmpSpline->numPts - 2) + j];

    }
  
  /* Normalize tangent */
  err = sys_geom_NormalizeVector(tmpTangent, tmpNormTangent, MAX_DIM);
  if (err == 0)
    {
      fprintf(stderr,"Subdivision too small for computing tangents.  Decrease number of output points requested.\n");
      return CV_ERROR;
    }
  
  for (j = 0; j < dimensions; j++)
    output->tangents[dimensions * (numberOfOutputPoints - 1) + j] = tmpNormTangent[j];
  
  /* End code for handling last point */
    
  output->numPts = numberOfOutputPoints;
  output->dim = dimensions;


  /* Clean up */
  sys_geom_SplinePointsDelete(tmpSpline);
  delete[] tmpTangent;
  delete[] tmpNormTangent;

  return CV_OK;

}



/***********************************************************************
 *                                                                     *
 * sys_geom_SplineGetRotVectors                                        *
 * ----------------------------                                        *
 * Computes a rotational/orientation vector.  In this case, it is just *
 * a vector that is perpendicular to the tangent.                      *
 *                                                                     *
 * We create a vector, with most of its coordinates equal to zero and  *
 * one coordinate equal to 1.  This vector will be perpendicular to    *
 * the tangent, if the dot product of the two is zero.  Compute the    *
 * value of the remaining coordinate using this fact.                  *
 *                                                                     *
 * Assumes that the tangents for the SplinePoints has already been     *
 * calculated.                                                         *
 ***********************************************************************/

void sys_geom_SplineGetRotVectors(SplinePoints *input)
{
  int i, j;
  int dimensions;
  vtkFloatingPointType *tmpRotVector = new vtkFloatingPointType[MAX_DIM];
  vtkFloatingPointType *tmpNormRotVector = new vtkFloatingPointType[MAX_DIM];
  vtkFloatingPointType dotProduct;

  /*****************
   *     body      *
   *****************/

  dimensions = input->dim;

  for (i = 0; i < input->numPts; i++)
    {

      /* Reset */
      for (j = 0; j < MAX_DIM; j++)
	tmpRotVector[j] = 0;
     
      int replaceComp;

      if (fabs(input->tangents[dimensions * i + 2]) > 0.0001) {
	  tmpRotVector[1] = 1;
          replaceComp = 2;
      } else if (fabs(input->tangents[dimensions * i + 1]) > 0.0001)  {
	  tmpRotVector[0] = 1;
          replaceComp = 1;
      } else {
          tmpRotVector[2] = 1;
          replaceComp = 0;
      }

      dotProduct = 0;

      for (j = 0; j < (dimensions - 1); j++)
	dotProduct = dotProduct + (input->tangents[dimensions * i + j] * tmpRotVector[j]);
	
      tmpRotVector[replaceComp] = -dotProduct / input->tangents[dimensions * i + replaceComp];

      /* Now normalize vector */
      sys_geom_NormalizeVector(tmpRotVector, tmpNormRotVector, MAX_DIM);


      /* Copy vector to input */
      for (j = 0; j < dimensions; j++)
	input->rotVectors[dimensions * i + j] = tmpNormRotVector[j];
    }

  /* Clean up */
  delete[] tmpRotVector;
  delete[] tmpNormRotVector;
}


/***********************************************************************
 *                                                                     *
 * sys_geom_NormalizeVector                                            *
 * ------------------------                                            *
 * Returns the normalized vector.                                      *
 ***********************************************************************/

int sys_geom_NormalizeVector(vtkFloatingPointType *input, vtkFloatingPointType *output, int sizeVector)
{
  vtkFloatingPointType magnitude;
  int j;

  /*****************
   *     body      *
   *****************/

  magnitude = 0;

  for (j = 0; j < sizeVector; j++)
    magnitude = magnitude + pow(input[j], 2);
  
  magnitude = sqrt(magnitude);
  
  if (magnitude == 0)
    {
      fprintf(stderr, "Vector cannot be normalized.  Magnitude of 0.\n");
      return 0;
    }

  for (j = 0; j < sizeVector; j++)
    output[j] = input[j] / magnitude;

  return 1;
}


/***********************************************************************
 *                                                                     *
 * sys_geom_SplinePath                                                 *
 * -------------------                                                 *
 * Performs spline interpolation and calculates tangents and rotation  *
 * vectors for the interpolated points.                                *
 *                                                                     *   
 * Assumptions:  Open spline.  Results of interpolation not clamped    *
 *               to input data.                                        *
 ***********************************************************************/

int sys_geom_SplinePath (SplinePoints *input, int type, int numOutputPts, int matchEndPoints, SplinePoints *output)
{

  if (sys_geom_SplineGetTangents(input, type, numOutputPts, matchEndPoints, output) == CV_ERROR) {
        return CV_ERROR;
  }

  sys_geom_SplineGetRotVectors(output);
  return CV_OK;
}
