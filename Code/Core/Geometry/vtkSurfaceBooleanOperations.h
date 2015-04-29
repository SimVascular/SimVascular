//
// Author: Bryn Lloyd, blloyd at vision.ee.ethz.ch
// Date: August 2009
//


#ifndef vtkSurfaceBooleanOperations_h
#define vtkSurfaceBooleanOperations_h

#include "vtkPolyDataAlgorithm.h"




/**
 * This class performs set operations on two surfaces
 * The operation is implemented using the GTS library: http://gts.sourceforge.net/
 *
 * The code to copy a vtkPolyData to gts was written by Hiraki Hideaki:
 * http://www.vtk.org/pipermail/vtkusers/2009-July/101865.html
 *
 * By default, the Union operator will be applied.
 *
 * By setting e.g. Difference to On, you automatically set Union and Intersection to Off.
 *
 */
class vtkSurfaceBooleanOperations : public vtkPolyDataAlgorithm
{
public:
  vtkTypeRevisionMacro(vtkSurfaceBooleanOperations,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent);
  static vtkSurfaceBooleanOperations *New();

  vtkSetMacro(Loud, int);
  vtkBooleanMacro(Loud, int);

  //BTX
  enum Modes {BOOLEAN_UNION, BOOLEAN_INTERSECTION, BOOLEAN_DIFFERENCE};
  //ETX

  void SetMode(int mode);

  void SetModeToUnion();
  void SetModeToIntersection();
  void SetModeToDifference();

protected:
  vtkSurfaceBooleanOperations();
  ~vtkSurfaceBooleanOperations();

  /** implementation of algorithm */
  int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *);

  virtual int FillInputPortInformation(int port, vtkInformation* info);

  int Mode;

  int Loud;


private:
  vtkSurfaceBooleanOperations(const vtkSurfaceBooleanOperations&);  // Not implemented.
  void operator=(const vtkSurfaceBooleanOperations&);  // Not implemented.

};

#endif
