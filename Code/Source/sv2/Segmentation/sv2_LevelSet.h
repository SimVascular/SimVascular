/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
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
 */

// We want cvLevelSet objects to be managers of various pieces of a
// level set execution:
//   - grid
//       . numerics
//       . seed front (i.e. initial phi)
//   - velocity
//   - performance monitors

// As such, cvLevelSet objects should be able to manage high-level level
// set functionality:
//   - evolve one time step
//   - rebuild phi
//   - extract front
//   - extract velocity vectors


#ifndef __CVLEVELSET_H
#define __CVLEVELSET_H

#include "SimVascular.h"
#include "svLSetExports.h" // For exports
#include "sv2_LevelSetStructuredGrid.h"
#include "sv_SolidModel.h"
#include "sv_PolyData.h"
#include "sv_DataObject.h"
#include "sv_StrPts.h"
#include "sv2_Timer.h"
#include "sv_misc_utils.h"


#define LSETCORE_STRLEN  1000


class SV_EXPORT_LSET cvLevelSetVelocity;


typedef enum { EXTEND_V, PROJECT_V, INVALID_V } ExtensionT;

SV_EXPORT_LSET ExtensionT ExtensionT_StrToEnum( char *name );
SV_EXPORT_LSET char *ExtensionT_EnumToStr( ExtensionT val );


typedef struct {
  double pos[3];
  double r;
} Seed_T;


class SV_EXPORT_LSET cvLevelSet {

  friend class cvLevelSetVelocity;

public:
  cvLevelSet();
  ~cvLevelSet();

  // Time management:
  int SetDtFactor( double dtf );
  int GetDtFactor( double *dtf );
  inline double GetDtFactor() { return dtFactor_; }
  int SetSimTime( double simTime );
  int GetSimTime( double *simTime );
  int GetCurrTime( double *currTime );
  inline int GetTimeStep() { return tn_; }
  inline void ResetTimeStep() { tn_ = 0; }

  // Grid parameters:
  int SetGridType( GridT gt );
  inline GridT GetGridType() { return gridType_; }
  int SetGridSpacing( double hv[] );
  int GetGridSpacing( double hv[] );
  int SetGridSize( int gsize[] );
  int GetGridSize( int gsize[] );
  int SetGridOrigin( double origin[] );
  int GetGridOrigin( double origin[] );
  int SetGridBandParams( double bandExt[], double mineWd );
  int GetGridBandParams( double bandExt[], double *mineWd );

  // Seed-related methods:
  int SetSeed( Seed_T *s );
  int GetSeed( Seed_T *s );
  int SetSeed( cvPolyData *s );
  int GetSeed( cvPolyData **s );
  int SetSeed( cvSolidModel *s );
  int GetSeed( cvSolidModel **s );
  int SetSeed( cvStrPts *s, double thr );
  int GetSeed( cvStrPts **s, double *thr );

  // Velocity access methods:
  int LinkVelocity( cvLevelSetVelocity *v );
  int UnlinkVelocity();
  int GetVelocity( cvLevelSetVelocity **v );
  cvLevelSetStructuredGrid *GetGrid() { return grid_; };

  // These are the essential high-level level set methods:
  int Status() { return status_; };
  int Init();
  int EvolveOneTimeStep();
  cvPolyData *ExtractFront( int closed = 0 );
  int RebuildPhi();

  // Access level set quantities:
  cvDataObject *ExtractPhi();
  cvStrPts *ExtractCurvature();
  cvPolyData *ExtractVectors();
  double GetMaxPhiIncr() { return maxPhiIncr_; }
  double GetMaxV() { return maxV_; }
  int Vanished() { return zlsVanished_; }

  // Access grid information:
  cvPolyData *ExtractActiveNodes();
  char *GetGridStatString();

  // Control method of velocity extension:
  int SetVExtension( ExtensionT etype );
  int GetVExtension( ExtensionT *etype );

  // Timers:
  void SetTimers( int flag ) { timers_ = flag; };
  void GetTimers( int *flag ) { *flag = timers_; };
  double GetTimerGranularity() { return cpuTimer.granularity(); };

  // Memory usage:
  int GetMemoryUsage();

  // Debug:
  void SetSaveProjectionSets( int flag ) { saveProjectionSets_ = flag; };
  void GetSaveProjectionSets( int *flag ) { *flag = saveProjectionSets_; };

  // To facilitate use with Tcl hash tables:
  char tclName_[CV_STRLEN];

private:
  cvLevelSetStructuredGrid *grid_;
  GridT gridType_;
  cvLevelSetVelocity *velocity_;

  void DeallocateSeedObjs();
  int ValidSeedExists();
  Seed_T *seed_;
  cvPolyData *pdSeed_;
  cvSolidModel *smSeed_;
  cvStrPts *imgSeed_;
  double imgSeedThr_;

  double dt_, dtFactor_;
  double simTime_, currTime_;
  int tn_;
  int status_;
  ExtensionT etype_;
  int rebuildPhiValid_;
  double maxPhiIncr_;
  double maxV_;

  // Grid-related parameters... kept here as a convenience
  double hx_, hy_, hz_;
  int gridx_, gridy_, gridz_;
  double origin_[3];
  int originValid_;
  double bandExt_[2];
  double mineWd_;

  int StopCondition();
  int zlsVanished_;

  int timers_;
  cvCPUTimer cpuTimer;
  int saveProjectionSets_;

};


#endif // __LEVEL_SET_H
