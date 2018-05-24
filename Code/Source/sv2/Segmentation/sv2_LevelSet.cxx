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

#include "SimVascular.h"

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "sv2_LevelSet.h"
#include "sv2_LevelSetDenseGrid.h"
#include "sv2_LevelSetSparseGrid.h"
#include "sv2_LevelSetVelocity.h"
#include "sv_misc_utils.h"

#ifdef __NON_STD_TCL_INSTALL
  #include "tcl.h"
#else
  #include <tcl.h>
#endif


// --------
// cvLevelSet
// --------

cvLevelSet::cvLevelSet()
{
  dt_ = -1.0;
  dtFactor_ = 1.0;
  simTime_ = -1.0;
  currTime_ = 0.0;
  tn_ = 0;
  hx_ = -1.0;
  hy_ = -1.0;
  hz_ = -1.0;
  gridx_ = 0;
  gridy_ = 0;
  gridz_ = 0;
  originValid_ = 0;
  bandExt_[0] = 1.0;
  bandExt_[1] = -1.0;
  mineWd_ = -1.0;
  grid_ = NULL;
  gridType_ = Dense_GridT;
  velocity_ = NULL;

  seed_ = NULL;
  pdSeed_ = NULL;
  smSeed_ = NULL;
  imgSeed_ = NULL;

  status_ = 0;
  timers_ = 0;
  saveProjectionSets_ = 0;
  etype_ = PROJECT_V;
  rebuildPhiValid_ = 0;
  zlsVanished_ = 0;
  maxPhiIncr_ = 0.0;
  maxV_ = 0.0;
}


// ---------
// ~cvLevelSet
// ---------

cvLevelSet::~cvLevelSet()
{
  if ( grid_ ) delete grid_;
  if ( velocity_ ) {
    velocity_->UnlinkLevelSet();
  }
  DeallocateSeedObjs();
}


// -----------
// SetDtFactor
// -----------

int cvLevelSet::SetDtFactor( double dtf )
{
  if ( ( dtf <= 0.0 ) || ( dtf > ( 1.0 + fabs(1e6*FindMachineEpsilon()) ) ) ) {
    printf("ERR: cfl factor f must satisfy 0.0 < f <= 1.0\n");
    return SV_ERROR;
  } else {
    dtf = svminimum( dtf, 1.0 );
    dtFactor_ = dtf;
    return SV_OK;
  }
}


// -----------
// GetDtFactor
// -----------

int cvLevelSet::GetDtFactor( double *dtf )
{
  *dtf = dtFactor_;
  return SV_OK;
}


// ----------
// SetSimTime
// ----------

int cvLevelSet::SetSimTime( double simTime )
{
  if ( simTime <= 0.0 ) {
    simTime_ = -1.0;
  } else {
    simTime_ = simTime;
  }
  return SV_OK;
}


// ----------
// GetSimTime
// ----------

int cvLevelSet::GetSimTime( double *simTime )
{
  *simTime = simTime_;
  return SV_OK;
}


// -----------
// SetGridType
// -----------

int cvLevelSet::SetGridType( GridT gt )
{
  if ( gt != Invalid_GridT ) {
    gridType_ = gt;
    return SV_OK;
  } else {
    return SV_ERROR;
  }
}


// --------------
// SetGridSpacing
// --------------

int cvLevelSet::SetGridSpacing( double hv[] )
{
  if ( ( hv[0] < 0.0 ) || ( hv[1] < 0.0 ) || ( hv[2] < 0.0 ) ) {
    return SV_ERROR;
  } else {
    hx_ = hv[0];
    hy_ = hv[1];
    hz_ = hv[2];
    return SV_OK;
  }
}


// --------------
// GetGridSpacing
// --------------

int cvLevelSet::GetGridSpacing( double hv[] )
{
  if ( ( hx_ < 0.0 ) || ( hy_ < 0.0 ) || ( hz_ < 0.0 ) ) {
    return SV_ERROR;
  } else {
    hv[0] = hx_;
    hv[1] = hy_;
    hv[2] = hz_;
    return SV_OK;
  }
}


// -----------
// SetGridSize
// -----------

int cvLevelSet::SetGridSize( int gsize[] )
{
  if ( ( gsize[0] <= 0 ) || ( gsize[1] <= 0 ) || ( gsize[2] <= 0 ) ) {
    return SV_ERROR;
  } else {
    gridx_ = gsize[0];
    gridy_ = gsize[1];
    gridz_ = gsize[2];
    return SV_OK;
  }
}


// -----------
// GetGridSize
// -----------

int cvLevelSet::GetGridSize( int gsize[] )
{
  if ( ( gridx_ <= 0 ) || ( gridy_ <= 0 ) || ( gridz_ <= 0 ) ) {
    return SV_ERROR;
  } else {
    gsize[0] = gridx_;
    gsize[1] = gridy_;
    gsize[2] = gridz_;
    return SV_OK;
  }
}


// -------------
// SetGridOrigin
// -------------

int cvLevelSet::SetGridOrigin( double origin[] )
{
  origin_[0] = origin[0];
  origin_[1] = origin[1];
  origin_[2] = origin[2];
  originValid_ = 1;
  return SV_OK;
}


// -------------
// GetGridOrigin
// -------------

int cvLevelSet::GetGridOrigin( double origin[] )
{
  if ( ! originValid_ ) {
    return SV_ERROR;
  } else {
    origin[0] = origin_[0];
    origin[1] = origin_[1];
    origin[2] = origin_[2];
    return SV_OK;
  }
}


// -----------------
// SetGridBandParams
// -----------------

int cvLevelSet::SetGridBandParams( double bandExt[], double mineWd )
{
  if ( ( bandExt[0] >= 0.0 ) || ( bandExt[1] <= 0.0 ) ) {
    return SV_ERROR;
  } else {
    bandExt_[0] = bandExt[0];
    bandExt_[1] = bandExt[1];
    mineWd_ = mineWd;
    return SV_OK;
  }
}


// -----------------
// GetGridBandParams
// -----------------

int cvLevelSet::GetGridBandParams( double bandExt[], double *mineWd )
{
  if ( ( bandExt_[0] >= 0.0 ) || ( bandExt_[1] <= 0.0 ) ) {
    return SV_ERROR;
  } else {
    bandExt[0] = bandExt_[0];
    bandExt[1] = bandExt_[1];
    *mineWd = mineWd_;
    return SV_OK;
  }
}


// -----------
// GetCurrTime
// -----------

int cvLevelSet::GetCurrTime( double *currTime )
{
  *currTime = currTime_;
  return SV_OK;
}


// ------------------
// DeallocateSeedObjs
// ------------------

void cvLevelSet::DeallocateSeedObjs()
{
  if ( seed_ != NULL ) {
    delete seed_;
    seed_ = NULL;
  }
  if ( pdSeed_ != NULL ) {
    delete pdSeed_;
    pdSeed_ = NULL;
  }
  if ( smSeed_ != NULL ) {
    delete smSeed_;
    smSeed_ = NULL;
  }
  if ( imgSeed_ != NULL ) {
    delete imgSeed_;
    imgSeed_ = NULL;
  }
}


// ---------------
// ValidSeedExists
// ---------------

int cvLevelSet::ValidSeedExists()
{
  if ( imgSeed_ || smSeed_ || pdSeed_ || seed_ ) {
    return SV_OK;
  } else {
    return SV_ERROR;
  }
}


// -------
// SetSeed
// -------
// Successive calls to either SetSeed will override the previous calls.

int cvLevelSet::SetSeed( Seed_T *s )
{
  DeallocateSeedObjs();

  // Now save the given circular seed:
  if ( seed_ == NULL ) {
    seed_ = new Seed_T;
  }
  *seed_ = *s;
  return SV_OK;
}


// -------
// GetSeed
// -------
// The input parameter s must be pre-allocated by the caller.

int cvLevelSet::GetSeed( Seed_T *s )
{
  if ( seed_ == NULL ) {
    return SV_ERROR;
  } else {
    *s = *seed_;
    return SV_OK;
  }
}


// -------
// SetSeed
// -------
// Successive calls to either SetSeed will override the previous calls.

int cvLevelSet::SetSeed( cvPolyData *s )
{
  DeallocateSeedObjs();

  pdSeed_ = new cvPolyData( s );
  pdSeed_->SetName( s->GetName() );
  return SV_OK;
}


// -------
// GetSeed
// -------

int cvLevelSet::GetSeed( cvPolyData **s )
{
  if ( pdSeed_ == NULL ) {
    return SV_ERROR;
  } else {
    *s = pdSeed_;
    return SV_OK;
  }
}


// -------
// SetSeed
// -------
// Successive calls to any SetSeed will override the previous calls.

int cvLevelSet::SetSeed( cvSolidModel *s )
{
  DeallocateSeedObjs();

  smSeed_ = s->Copy();
  smSeed_->SetName( s->GetName() );
  return SV_OK;
}


// -------
// GetSeed
// -------

int cvLevelSet::GetSeed( cvSolidModel **s )
{
  if ( smSeed_ == NULL ) {
    return SV_ERROR;
  } else {
    *s = smSeed_;
    return SV_OK;
  }
}


// -------
// SetSeed
// -------

int cvLevelSet::SetSeed( cvStrPts *s, double thr )
{
  DeallocateSeedObjs();

  imgSeed_ = new cvStrPts( s->GetVtkStructuredPoints() );
  imgSeed_->SetName( s->GetName() );
  imgSeedThr_ = thr;
  return SV_OK;
}


// -------
// GetSeed
// -------

int cvLevelSet::GetSeed( cvStrPts **s, double *thr )
{
  if ( imgSeed_ == NULL ) {
    return SV_ERROR;
  } else {
    *s = imgSeed_;
    *thr = imgSeedThr_;
    return SV_OK;
  }
}


// ------------
// LinkVelocity
// ------------
// It makes more sense to link cvLevelSetVelocity objects directly to cvLevelSet
// objects, instead of to Grid's.  The reason for this is that Grid
// objects are constructed according to a lazy protocol... i.e. memory
// is not allocated until all the initialization has been completed
// and the first call to EvolveOneTimeStep is reached.

int cvLevelSet::LinkVelocity( cvLevelSetVelocity *v )
{
  cvLevelSet *dummy;

  if ( v == NULL ) {
    return SV_ERROR;
  }

  if ( velocity_ != NULL ) {
    UnlinkVelocity();
  }

  velocity_ = v;

  v->GetLevelSet( &dummy );
  if ( dummy != this ) {
    if ( v->LinkLevelSet( this ) != SV_OK ) {
      return SV_ERROR;
    }
  }

  return SV_OK;
}


// --------------
// UnlinkVelocity
// --------------

int cvLevelSet::UnlinkVelocity()
{
  if ( velocity_ == NULL ) {
    return SV_OK;
  }

  //  velocity_->UnlinkLevelSet();
  velocity_->ls_ = NULL;
  velocity_ = NULL;
  return SV_OK;
}


// -----------
// GetVelocity
// -----------

int cvLevelSet::GetVelocity( cvLevelSetVelocity **v )
{
  *v = velocity_;
  return SV_OK;
}


// ----
// Init
// ----
// Create cvLevelSetStructuredGrid.

int cvLevelSet::Init()
{
  double h[3];
  int dims[3];
  int initStatus;

  if ( (hx_ < 0.0) || (hy_ < 0.0) || (hz_ < 0.0) ) {
    return SV_ERROR;

  } else if ( (gridx_ <= 0) || (gridy_ <= 0) || (gridz_ <= 0) ) {
    return SV_ERROR;

  } else if ( ! originValid_ ) {
    return SV_ERROR;

  } else if ( ! ValidSeedExists() ) {
    return SV_ERROR;

  } else if ( ! velocity_ ) {
    return SV_ERROR;

  } else {
    h[0] = hx_;
    h[1] = hy_;
    h[2] = hz_;

    dims[0] = gridx_;
    dims[1] = gridy_;
    dims[2] = gridz_;

    if ( grid_ ) {
      delete grid_;
    }

    switch (gridType_) {
    case Dense_GridT:
      grid_ = new cvLevelSetDenseGrid( h, dims, origin_ );
      if ( grid_ == NULL ) {
	printf( "ERR: Couldn't allocate cvLevelSetDenseGrid.\n" );
    return SV_ERROR;
      }
      break;
    case Sparse_GridT:
      if ( ( bandExt_[0] >= 0.0 ) || ( bandExt_[1] <= 0.0 ) ) {
    return SV_ERROR;
      }
      grid_ = new cvLevelSetSparseGrid( h, dims, origin_ );
      if ( grid_ == NULL ) {
	printf( "ERR: Couldn't allocate cvLevelSetSparseGrid.\n" );
    return SV_ERROR;
      }
      if ( grid_->SetBandParams( bandExt_[0], bandExt_[1], mineWd_ )
	   != SV_OK ) {
	delete grid_;
    return SV_ERROR;
      }
      break;
    default:
      grid_ = NULL;
      return SV_ERROR;
    }

    if (timers_) {
      cpuTimer.reset();
    }
    if ( imgSeed_ ) {
      initStatus = grid_->InitPhi( imgSeed_, imgSeedThr_ );
    } else if ( smSeed_ ) {
      initStatus = grid_->InitPhi( smSeed_ );
    } else if ( pdSeed_ ) {
      initStatus = grid_->InitPhi( pdSeed_ );
    } else if ( seed_ ) {
      initStatus = grid_->InitPhi( seed_->pos, seed_->r );
    } else {
      assert(0);
    }

    if ( initStatus != SV_OK ) {
      delete grid_;
      grid_ = NULL;
      return SV_ERROR;
    }
    if (timers_) {
      printf("InitPhi:\t %lf sec\n", cpuTimer.seconds());
      cpuTimer.reset();
    }

    currTime_ = 0.0;
    tn_ = 0;

    if ( velocity_ != NULL ) {
      velocity_->UpdateTolerance();
    }
  }

  status_ = 1;
  zlsVanished_ = 0;
  return SV_OK;
}


// -----------------
// EvolveOneTimeStep
// -----------------

// KCW [4/23/99]
// ---
// The idea is something like this:
//   Grid::EvaluateV
//     foreach active edge
//       cvLevelSetVelocity::FindZLS
//       cvLevelSetVelocity::FindK
//       cvLevelSetVelocity::FindN
//       cvLevelSetVelocity::Evaluate
//   Grid::ProjectV
//   Grid::UpdatePhi

// Note that before we get to this point, we will have done:
//   cvLevelSetVelocity::LinkGrid  OR  Grid::LinkVelocity
//   cvLevelSetVelocityKGI::SetImage
//   cvLevelSetVelocityKGI::SetEK
//   cvLevelSetVelocityKGI::SetEI

int cvLevelSet::EvolveOneTimeStep()
{
  if ( zlsVanished_ ) {
    printf("ERR: Zero level set has no geometry.\n");
    return SV_ERROR;

  } else if ( velocity_ == NULL ) {
    printf("ERR: no velocity currently associated with %s\n", tclName_);
    return SV_ERROR;

  } else {

    // Evaluate velocity:
    // ------------------
    if (timers_) {
      cpuTimer.reset();
    }
    if ( grid_->EvaluateV( velocity_, dtFactor_ ) != SV_OK ) {
	printf("ERR: EvaluateV failure\n");
	cpuTimer.reset();
    return SV_ERROR;
    }
    if (timers_) {
      printf("EvaluateV:\t %lf sec\n", cpuTimer.seconds());
      cpuTimer.reset();
    }

    // Project / extend velocity:
    // --------------------------
    switch (etype_) {
    case PROJECT_V:
      if (timers_) {
	cpuTimer.reset();
      }
      if ( grid_->ProjectV( saveProjectionSets_ ) != SV_OK ) {
	printf("ERR: ProjectV failure\n");
	zlsVanished_ = 1;
    return SV_ERROR;
      }
      if (timers_) {
	printf("ProjectV:\t %lf sec\n", cpuTimer.seconds());
	cpuTimer.reset();
      }
      break;
    case EXTEND_V:
      grid_->ExtendV();
      if (timers_) {
	printf("ExtendV:\t %lf sec\n", cpuTimer.seconds());
	cpuTimer.reset();
      }
      break;
    default:
      printf("Invalid etype_ member.\n");
      return SV_ERROR;
      break;
    }

    // Compute phi increments, check stop condition and update:
    // --------------------------------------------------------
    if (timers_) {
      cpuTimer.reset();
    }
    dt_ = grid_->ComputeDeltaPhi( dtFactor_ );
    if ( StopCondition() ) {
      return SV_ERROR;
    }
    if ( grid_->UpdatePhi() != SV_OK ) {
      printf("ERR: update error\n");
      return SV_ERROR;
    }
    if (timers_) {
      printf("Level set update:\t %lf sec\n", cpuTimer.seconds());
      cpuTimer.reset();
    }

    tn_++;
    currTime_ += dt_;
    rebuildPhiValid_ = 0;
    return SV_OK;
  }
}


// ------------
// ExtractFront
// ------------

cvPolyData *cvLevelSet::ExtractFront( int closed )
{
  cvPolyData *result;

  if (timers_) {
    cpuTimer.reset();
  }
  result = grid_->GetFront( closed );
  if (timers_) {
    printf("ExtractFront:\t %lf sec\n", cpuTimer.seconds());
  }

  return result;
}


// ----------
// RebuildPhi
// ----------

int cvLevelSet::RebuildPhi()
{
  int status;

  if ( ! rebuildPhiValid_ ) {
    if (timers_) {
      cpuTimer.reset();
    }
    status = grid_->ReinitPhi();
    if (timers_) {
      printf("RebuildPhi:\t %lf sec\n", cpuTimer.seconds());
    }
    rebuildPhiValid_ = 1;
  } else {
    if (timers_) {
      printf("RebuildPhi:\t previous reinitialization still valid\n");
    }
  }

  if ( status != SV_OK ) {
    return SV_ERROR;
  } else {
    return SV_OK;
  }
}


// -------------
// StopCondition
// -------------
// Grid::GetMaxF is potentially time-consuming.

int cvLevelSet::StopCondition()
{
  double maxF;
  double stopV;

  if ( ( simTime_ > 0.0 ) && ( currTime_ > simTime_ ) ) {
    printf("STOP: currTime [%f] > simTime [%f]\n", currTime_, simTime_);
    return SV_OK;
  } else if ( dt_ < 0.0 ) {
    printf("STOP: max velocity [%g] --> 0\n", grid_->GetMaxF());
    return SV_OK;
  } else {

    //    if ( currTime_ > 0.0) {

      /*
      // The max fabs( F0_ + F1_ ) may not accurately reflect the
      // velocity which is applied during UpdatePhi.
      maxF = grid_->GetMaxF();
      */

    /*
    // It is probably more appropriate and meaningful to apply a
    // distance increment criterion, as opposed to a velocity
    // criterion, since velocity values do not capture the stage of
    // filtering which occurs in selection of appropriate gradient
    // approximations.  In this case, "maxF" is a misnomer.  However,
    // displacement is not in general going to diminish since dt is
    // expressly *chosen* to give displacements of a certain size.
    maxPhiIncr_ = grid_->GetMaxPhiIncr();
    maxF = maxPhiIncr_;
    */

    maxV_ = grid_->GetMaxV();
    maxF = maxV_;

    stopV = velocity_->GetStopV();
    if ( maxF < stopV ) {
      printf("STOP: curr max velocity [%f] < stopV [%f]\n", maxF, stopV);
      return SV_OK;
    }
    return( velocity_->StopCondition() );
  }
}


// ----------
// ExtractPhi
// ----------

cvDataObject *cvLevelSet::ExtractPhi()
{
  cvDataObject *result;

  if (timers_) {
    cpuTimer.reset();
  }
  result = grid_->GetPhi();
  if (timers_) {
    printf("ExtractPhi:\t %lf sec\n", cpuTimer.seconds());
  }

  return result;
}


// ----------------
// ExtractCurvature
// ----------------

cvStrPts *cvLevelSet::ExtractCurvature()
{
  cvStrPts *result;

  if (timers_) {
    cpuTimer.reset();
  }
  result = grid_->GetCurvature();
  if (timers_) {
    printf("ExtractCurvature:\t %lf sec\n", cpuTimer.seconds());
  }

  return result;
}


// --------------
// ExtractVectors
// --------------

cvPolyData *cvLevelSet::ExtractVectors()
{
  cvPolyData *result;

  if (timers_) {
    cpuTimer.reset();
  }
  result = grid_->GetVelocityVectors();
  if (timers_) {
    printf("ExtractVel:\t %lf sec\n", cpuTimer.seconds());
  }

  return result;
}


// ------------------
// ExtractActiveNodes
// ------------------

cvPolyData *cvLevelSet::ExtractActiveNodes()
{
  cvPolyData *result;

  if (timers_) {
    cpuTimer.reset();
  }
  result = grid_->GetActiveNodes();
  if (timers_) {
    printf("ExtractActiveNodes:\t %lf sec\n", cpuTimer.seconds());
  }

  return result;
}


// -------------
// SetVExtension
// -------------
// Note that the default (to PROJECT_V) is set in the cvLevelSet
// constructor.

int cvLevelSet::SetVExtension( ExtensionT etype )
{
  if ( etype == INVALID_V ) {
    return SV_ERROR;
  } else {
    etype_ = etype;
    return SV_OK;
  }
}


// -------------
// GetVExtension
// -------------

int cvLevelSet::GetVExtension( ExtensionT *etype )
{
  *etype = etype_;
  return SV_OK;
}


// -----------------
// GetGridStatString
// -----------------

char *cvLevelSet::GetGridStatString()
{
  char *result;

  result = new char [CV_STRLEN];

  if ( grid_ == NULL ) {
    sprintf( result, "No currently valid grid." );
    return result;
  }
  return grid_->StatString();
}


// --------------
// GetMemoryUsage
// --------------

int cvLevelSet::GetMemoryUsage()
{
  int tot = 0;

  if ( grid_ ) {
    tot += grid_->GetMemoryUsage();
  }
  if ( velocity_ ) {
    tot += velocity_->GetMemoryUsage();
  }
  if ( seed_ ) {
    tot += sizeof( seed_ );
  }
  if ( pdSeed_ ) {
    tot += pdSeed_->GetMemoryUsage();
  }
  if ( smSeed_ ) {
    tot += smSeed_->GetMemoryUsage();
  }
  if ( imgSeed_ ) {
    tot += imgSeed_->GetMemoryUsage();
  }
  tot += sizeof( this );

  return tot;
}


// --------------------
// ExtensionT_StrToEnum
// --------------------
// NOT part of the cvLevelSet class.

ExtensionT ExtensionT_StrToEnum( char *name )
{
  if ( !strcmp( name, "ProjectV" ) ) {
    return PROJECT_V;
  } else if ( !strcmp( name, "ExtendV" ) ) {
    return EXTEND_V;
  } else {
    return INVALID_V;
  }
}


// --------------------
// ExtensionT_EnumToStr
// --------------------
// NOT part of the cvLevelSet class.  Caller need not worry about result
// clean up.

char *ExtensionT_EnumToStr( ExtensionT val )
{
  static Tcl_DString ds;

  Tcl_DStringFree( &ds );

  switch (val) {
  case EXTEND_V:
    Tcl_DStringAppend( &ds, "ExtendV", -1 );
    break;
  case PROJECT_V:
    Tcl_DStringAppend( &ds, "ProjectV", -1 );
    break;
  default:
    Tcl_DStringAppend( &ds, "Invalid", -1 );
    break;
  }

  return Tcl_DStringValue( &ds );
}
