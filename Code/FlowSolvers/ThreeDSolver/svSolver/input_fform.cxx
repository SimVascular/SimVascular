/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University, 
 * Rensselaer Polytechnic Institute, Charles A. Taylor, 
 * Kenneth E. Jansen.
 * 
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:

 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer. 
 * Redistributions in binary form must reproduce the above copyright 
 * notice, this list of conditions and the following disclaimer in the 
 * documentation and/or other materials provided with the distribution. 
 * Neither the name of the Stanford University or Rensselaer Polytechnic
 * Institute nor the names of its contributors may be used to endorse or
 * promote products derived from this software without specific prior 
 * written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 *=========================================================================*/

#include "cvFlowsolverOptions.h"

#include <fstream>
#include <stdlib.h>
#include <vector>
#include <string>

#include <stdlib.h>
#include <string.h>

#include "Input.h"
#include "common_c.h"

using namespace std; //::cout;
void print_error_code(int ierr);
int input_fform(char inpfname[]);
int SONFATH = 0;
extern "C" char cvsolver_iotype[80];
extern int myrank;
extern "C" {

  int input_fform_(char inpfname[]) {
    return input_fform(inpfname);
  }

}

int input_fform(char inpfname[])
{

  int ierr = 0 ;
  int i,j;
  char* path_to_config = 0 ;
  char complete_filename[256];
  string strvalue;

  try {

    // Get the input file stream
//    path_to_config = getenv("FLOWSOLVER_CONFIG");
//    if(path_to_config) strcpy(complete_filename, path_to_config);
//    else strcpy(complete_filename,".");
//    strcat(complete_filename, "/input.config");
//    printf("\n Complete Filename: %s \n", complete_filename);
	if(myrank==0){
		printf("Solver Input Files listed as below:\n------------------------------------\n");
		printf(" Local Config: %s \n", inpfname);
	}
//    string def(complete_filename);
//    Input inp(inpfname,def);
    string inpfile(inpfname);
    Input inp(inpfile);

    if(myrank==0){
    printf("Parameter Values setup as below:\n----------------------------------------------\n");
    }
    // ========================
	// BCT CONTROL KEY WORDS
    // ========================
    strvalue=(string)inp.GetValue("Time Varying Boundary Conditions From File","True",false,true);
    (strvalue=="True")? nomodule.itvn = 1: nomodule.itvn = 0;
    if ( nomodule.itvn ==1){
        strvalue=(string)inp.GetValue("BCT File Type","DAT",false,true);
        if(strvalue=="DAT"){
        	inpdat.BCTFlag=0;
        }else if(strvalue=="VTP"){
        	inpdat.BCTFlag=1;
        }else{
        	if(myrank==0){
        	cerr << " ERROR: Invalid BCT File Type." << endl;
        	}
        	return 001;
        }

        inpdat.BCTFileNumber = inp.GetValue("Number of BCT Files","1",false,true);

        strvalue=(string)inp.GetValue("BCT Matching Type","Global Node ID",false,true);
        if(strvalue=="Coordinates"){
        	inpdat.BCTMatchingFlag=0;
        }else if(strvalue=="Global Node ID"){
        	inpdat.BCTMatchingFlag=1;
        }else{
        	if(myrank==0){
        	cerr << " ERROR: Invalid BCT Matching Type." << endl;
        	}
        	return 001;
        }

        nomodule.bcttimescale = inp.GetValue("BCT Time Scale Factor","1.0",false,true);
    }

    // Disabled Features.
    conpar.iALE=inp.GetValue("iALE","0",false,false);
    timpar.ntseq=inp.GetValue("ntseq","1",false,false);

    // ========================
    // SOLUTION CONTROL KEY WORDS
    // ========================
    strvalue=(string)inp.GetValue("Equation of State","Incompressible",false,true);
    (strvalue=="Incompressible")? matdat.matflg[0][0] =-1:matdat.matflg[0][0] =0 ;

    inpdat.Delt[0]=inp.GetValue("Time Step Size","",true,true);
    inpdat.nstep[0]=inp.GetValue("Number of Timesteps","",true,true);

    strvalue=(string)inp.GetValue("Viscous Control","Viscous",false,true);
    (strvalue=="Viscous")? conpar.navier=1:conpar.navier=0;

    int solflow, solheat , solscalr;
    strvalue=(string)inp.GetValue("Solve Flow","True",false,false);
    (strvalue == "True")? solflow=1:solflow=0;
    strvalue=(string)inp.GetValue("Solve Heat","False",false,false);
    (strvalue == "True")? solheat=1:solheat=0;
    //for compressible solheat= False so
    if(matdat.matflg[0][0] ==0) solheat=0;
    
    // Solve Scalars
    solscalr = inp.GetValue("Solve Scalars","0",false,false);
    if ( solscalr > 4 ) {
    	if(myrank==0){
    	 cout << " Only Four Scalars are supported \n";
         cout <<" Please reduce number of scalars \n";
    	}
      exit(1);
    }
    inpdat.impl[0] = 10*solflow+solscalr*100+solheat;

    vector<double> vec;
    vector<int> ivec;

    // ========================
    // OUTPUT CONTROL KEY WORDS
    // ========================
    conpar.necho = inp.GetValue("Verbosity Level","3",false,false);
    outpar.ntout = inp.GetValue("Number of Timesteps between Restarts","",true,true);

    strvalue=(string)inp.GetValue("Print Statistics","False",false,false);
    (strvalue == "True")? outpar.ioform = 2:outpar.ioform = 1;

    //no longer used. Use aerfrc.nsrfCM instead.
//    strvalue=(string)inp.GetValue("Print Wall Fluxes","True",false,false);
//    (strvalue == "True")? outpar.iowflux = 1:outpar.iowflux = 0;

//    //never used
//    strvalue=(string)inp.GetValue("Print FieldView","False",false,false);
//    (strvalue == "True")? outpar.iofieldv = 1:outpar.iofieldv = 0;

    strvalue=(string)inp.GetValue("Print Average Solution","True",false,true);
    (strvalue == "True")? outpar.ioybar = 1: outpar.ioybar = 0;

    strvalue=(string)inp.GetValue("Print Error Indicators","False",false,true);
    (strvalue == "True")? outpar.ioyerror = 1: outpar.ioyerror = 0;

    strvalue=(string)inp.GetValue("Data Block Format","binary",false,false);//normally use binary
    strcpy( outpar.iotype , strvalue.c_str());
    strcpy( cvsolver_iotype ,strvalue.c_str());
  
//    //never used
//    strvalue=(string)inp.GetValue("Print Residual at End of Step","False",false,false);
//    (strvalue == "True")? genpar.lstres = 1:genpar.lstres = 0;
  
    aerfrc.nsrfCM = inp.GetValue("Number of Force Surfaces","0",false,true);
    if (aerfrc.nsrfCM > 0) {
      ivec = inp.GetValue("Surface ID's for Force Calculation","",true,true);
      for(i=0;i<MAXSURF+1; i++) aerfrc.nsrflist[i] = 0;
      for(i=0; i< aerfrc.nsrfCM; i++){
          aerfrc.nsrflist[ivec[i]] = 1;
      }
      ivec.erase(ivec.begin(),ivec.end());


      strvalue=(string)inp.GetValue("Force Calculation Method","Velocity Based",false,true);
      if( strvalue =="Velocity Based" ){
          inpdat.tractionMethod=0;
      }else if ( strvalue =="Residual Based" ){
          inpdat.tractionMethod=1;
      }else if ( strvalue =="Both" ){
          inpdat.tractionMethod=2;
      }else{
          if(myrank==0){
              cerr << " ERROR: Invalid Force Calculation Method." << endl;
          }
          return 001;
      }

      if(inpdat.tractionMethod==0){
          strvalue=(string)inp.GetValue("Apply Wall Deformation","False",false,true);
          (strvalue=="True")? nomodule.applyWallDeformation = 1: nomodule.applyWallDeformation = 0;
      }

    }

    if(nomodule.numCalcSrfs=inp.GetValue("Number of Surfaces which Output Pressure and Flow","0",false,true)){
	    ivec = inp.GetValue("List of Output Surfaces","",true,true);
	    for(i=0; i<MAXSURF+1; i++) nomodule.nsrflistCalc[i] = 0;
        for(i=0; i<nomodule.numCalcSrfs; i++){
	        nomodule.nsrflistCalc[i+1]=ivec[i];
        }
    }
    ivec.erase(ivec.begin(),ivec.end());

//    //never used
//    aerfrc.isrfIM = inp.GetValue("Surface ID for Integrated Mass","1",false,false);

    // ============================
    // MATERIAL PROPERTIES KEYWORDS
    // ============================
    //matdat.nummat = 1;//never used
//    //never used
//    strvalue=(string)inp.GetValue("Shear Law","Constant Viscosity",false,false);
//    if(strvalue == "Constant Viscosity") matdat.matflg[0][1] = 0;

//    //never used
//    strvalue=(string)inp.GetValue("Bulk Viscosity Law","Constant Bulk Viscosity",false,false);
//    if(strvalue == "Constant Bulk Viscosity")  matdat.matflg[0][2] = 0;

//    //never used
//    mmatpar.pr = inp.GetValue("Prandtl Number","0.72",false,false);

//    //never used
//    strvalue=(string)inp.GetValue("Conductivity Law","Constant Conductivity",false,false);
//    if(strvalue == "Constant Conductivity") matdat.matflg[0][3] = 0;

    vec = inp.GetValue("Density","",true,true);
    for(i=0; i< 1 ; i++){
      matdat.datmat[i][0][0] = vec[i];
    }
    vec.erase(vec.begin(),vec.end());

    vec = inp.GetValue("Viscosity","",true,true);
    for(i=0; i< 1 ; i++){
      matdat.datmat[i][1][0] = vec[i];
    }
    vec.erase(vec.begin(),vec.end());

//    //never used
//    vec = inp.GetValue("Specific Heat");
//    for(i=0; i< 1 ; i++){
//      matdat.datmat[i][2][0] = 0;
//    }
//    vec.erase(vec.begin(),vec.end());

    vec = inp.GetValue("Thermal Conductivity","0.2",false,false);
    for(i=0; i< 1 ; i++){
      matdat.datmat[i][3][0] = vec[i];
    }
    vec.erase(vec.begin(),vec.end());
  
    vec = inp.GetValue("Scalar Diffusivity","0.2",false,false);
    for(i=0; i< solscalr ; i++){
      sclrs.scdiff[i] = vec[i];
    }
    vec.erase(vec.begin(),vec.end());

    strvalue=(string)inp.GetValue("Body Force Option","None",false,false);
    if ( strvalue == "None" ) {
      for( i=0; i< 1 ; i++)  matdat.matflg[i][4] = 0;
    }
    else if ( strvalue == "Vector" ) {
      for( i=0; i< 1 ; i++)  matdat.matflg[i][4] = 1;
    }
    else if ( strvalue == "User e3source.f" ) {
      for( i=0; i< 1 ; i++) matdat.matflg[i][4] = 3;
    }
    else if ( strvalue == "Boussinesq" ) {
      for(i=0; i< 1 ; i++) matdat.matflg[i][4] = 2;
    }
    else if ( strvalue == "Cooling Analytic" ) {
      for(i=0; i< 1 ; i++) matdat.matflg[i][4] = 4;
    }
    else if ( strvalue == "Cooling Initial Condition" ) {
      for(i=0; i< 1 ; i++) matdat.matflg[i][4] = 5;
    }

    vec = inp.GetValue("Body Force","0.0 0.0 0.0",false,false);
    for(i=0; i< 1 ; i++){
      matdat.datmat[i][4][0] = vec[0+i*3];
      matdat.datmat[i][4][1] = vec[1+i*3];
      matdat.datmat[i][4][2] = vec[2+i*3];
    }
    vec.erase(vec.begin(),vec.end());

//    //never used
//    vec = inp.GetValue("Body Force Pressure Gradient","0.0 0.0 0.0",false,false);
//    for(i=0; i< 1 ; i++){
//      matdat.datmat[i][6][0] = vec[0+i*3];
//      matdat.datmat[i][6][1] = vec[1+i*3];
//      matdat.datmat[i][6][2] = vec[2+i*3];
//    }
//    vec.erase(vec.begin(),vec.end());

    strvalue=(string)inp.GetValue("Surface Tension Option","No",false,false);
    if ( strvalue == "No" ){
        genpar.isurf = 0;
    }
    else if (strvalue == "Yes" ){
        genpar.isurf = 1;
    }
    else {
      if(myrank==0){
        cout << " Surface Tension: Only Legal Values (Yes, No) ";
        cout << endl;
      }
      exit(1);
    }
    if( genpar.isurf > 0) {
      genpar.Bo = inp.GetValue("Bond Number","",true,true);
    }

//    //never used
//    genpar.EntropyPressure = inp.GetValue("Entropy Form of Pressure Constraint on Weight Space","0",false,false);

    strvalue=(string)inp.GetValue("Rotating Frame of Reference","False",false,false);
    if ( strvalue== "True" ) {
      matdat.matflg[0][5] = 1;
      vec = inp.GetValue("Rotating Frame of Reference Rotation Rate","0.0 0.0 0.0",false,true);
      matdat.datmat[0][5][0] = vec[0];
      matdat.datmat[0][5][1] = vec[1];
      matdat.datmat[0][5][2] = vec[2];
      vec.erase(vec.begin(),vec.end());
    }
    else {
      matdat.matflg[0][5] = 0;
      matdat.datmat[0][5][0] = 0.;
      matdat.datmat[0][5][1] = 0.;
      matdat.datmat[0][5][2] = 0.;
    }

    // ========================
    // LINEAR SOLVER PARAMETERS
    // ========================
    inpdat.svLSFlag=0;
    strvalue=(string)inp.GetValue("Solver Type","svLS",false,true);
    if( strvalue =="ACUSIM with P Projection" ){
      incomp.iprjFlag = 0; incomp.ipresPrjFlag=1;}
    else if ( strvalue =="ACUSIM" ){
      incomp.iprjFlag = 0; incomp.ipresPrjFlag=0;}
    else if( strvalue =="ACUSIM with Velocity Projection" ){
      incomp.iprjFlag = 1; incomp.ipresPrjFlag=0;}
    else if( strvalue =="ACUSIM with Full Projection" ){
      incomp.iprjFlag = 1; incomp.ipresPrjFlag=1;}
    else if( strvalue =="GMRES Matrix Free"){
      inpdat.impl[0] += 10*solflow;}
    else if( strvalue =="GMRES EBE"){
      inpdat.impl[0] += 20*solflow;}
    else if( strvalue =="svLS"){
      inpdat.svLSFlag=1;
    }else{
      if(myrank==0){
        cerr << " ERROR: Invalid Linear Solver Type." << endl;
        return 001;
      }
    }
    //GMRES sparse is assumed default and has the value of 10, MFG 20,
    // EBE 30

    strvalue=(string)inp.GetValue("svLS Type","NS",false,true);
    //based on svLS_STRUCT.h
    if( strvalue =="CG" ){
      inpdat.svLSType=1;
    }else if ( strvalue =="GMRES" ){
    	inpdat.svLSType=2;
    }else if( strvalue =="NS"){
      inpdat.svLSType=3;
    }else{
      if(myrank==0){
        cerr << " ERROR: Invalid svLS Type." << endl;
      }
      return 001;
    }

    //    inpdat.niter[0] = inp.GetValue("Number of Solves per Time Step");
    solpar.nGMRES = inp.GetValue("Number of GMRES Sweeps per Solve","1",false,false);//not used,only print out
    solpar.Kspace = inp.GetValue("Number of Krylov Vectors per GMRES Sweep","100",false,true);
    inpdat.LHSupd[0] = inp.GetValue("Number of Solves per Left-hand-side Formation","1",false,true);

    inpdat.epstol[0] = inp.GetValue("Tolerance on Momentum Equations","0.05",false,true);
    inpdat.epstol[6] = inp.GetValue("Tolerance on Continuity Equations","0.4",false,true);
    inpdat.epstol[7] = inp.GetValue("Tolerance on svLS NS Solver","0.4",false,true);

    incomp.maxNSIters = inp.GetValue("Maximum Number of Iterations for svLS NS Solver","1",false,true);
    incomp.maxMomentumIters = inp.GetValue("Maximum Number of Iterations for svLS Momentum Loop","2",false,true);
    incomp.maxContinuityIters = inp.GetValue("Maximum Number of Iterations for svLS Continuity Loop","400",false,true);

    incomp.prestol = inp.GetValue("Tolerance on ACUSIM Pressure Projection","0.1",false,false);//for LesLib
    incomp.minIters = inp.GetValue("Minimum Number of Iterations per Nonlinear Iteration","1",false,false);//for LesLib
    incomp.maxIters = inp.GetValue("Maximum Number of Iterations per Nonlinear Iteration","500",false,false);//for LesLib
    incomp.nPrjs = inp.GetValue("Number of Velocity Projection Vectors","20",false,false);//for LesLib
    incomp.nPresPrjs = inp.GetValue("Number of Pressure Projection Vectors","20",false,false);//for LesLib
    incomp.iverbose = inp.GetValue("ACUSIM Verbosity Level","0",false,false);//for LesLib

    inpdat.deltol[0][0]=inp.GetValue("Velocity Delta Ratio","0",false,false);
    inpdat.deltol[1][0]=inp.GetValue("Pressure Delta Ratio","0",false,false);

    if(solheat==1){ 
      inpdat.epstol[1]=inp.GetValue("Temperature Solver Tolerance","0.001",false,true);
      inpdat.LHSupd[1]=inp.GetValue("Number of Solves of Temperature per Left-hand-side Formation","1",false,true);
    }

    // The following is where you should put any inputs that are able to 
    // input differently for each scalar.  It is a little tedious in the code 
    // but it should make the solver.inp easier to understand. Note this will 
    // require some care with regression tests.


    if(solscalr>0){
      inpdat.epstol[2]=inp.GetValue("Scalar 1 Solver Tolerance","0.001",false,false);
      inpdat.LHSupd[2]=inp.GetValue("Number of Solves of Scalar 1 per Left-hand-side Formation","1",false,false);
    } 

    if(solscalr>1){
      inpdat.epstol[3]=inp.GetValue("Scalar 2 Solver Tolerance","0.001",false,false);
      inpdat.LHSupd[3]=inp.GetValue("Number of Solves of Scalar 2 per Left-hand-side Formation","1",false,false);
    } 

    if(solscalr>2){
      inpdat.epstol[4]=inp.GetValue("Scalar 3 Solver Tolerance","0.001",false,false);
      inpdat.LHSupd[4]=inp.GetValue("Number of Solves of Scalar 3 per Left-hand-side Formation","1",false,false);
    } 

    if(solscalr>3){
      inpdat.epstol[5]=inp.GetValue("Scalar 4 Solver Tolerance","0.001",false,false);
      inpdat.LHSupd[5]=inp.GetValue("Number of Solves of Scalar 4 per Left-hand-side Formation","1",false,false);
    }
     
    // ======================
    // DISCRETIZATION CONTROL
    // ======================    
    genpar.ipord = inp.GetValue("Basis Function Order","1",false,false);

    strvalue=(string)inp.GetValue("Time Integration Rule","Second Order",false,true);
    if(strvalue == "First Order"){
      inpdat.rhoinf[0] = -1 ;
    }
    else {
    	inpdat.rhoinf[0] = (double)inp.GetValue("Time Integration Rho Infinity","0.5",false,true);
    }
    
    strvalue=(string)inp.GetValue("Predictor at Start of Step","Same Velocity",false,false);
    if(strvalue=="Same Velocity") genpar.ipred = 1;
    if(strvalue=="Zero Acceleration") genpar.ipred = 2;
    if(strvalue=="Same Acceleration") genpar.ipred = 3;
    if(strvalue=="Same Delta")       genpar.ipred = 4;

    strvalue=(string)inp.GetValue("Weak Form","SUPG",false,false);
    if(strvalue == "Galerkin") solpar.ivart = 1;
    if(strvalue == "SUPG")     solpar.ivart = 2;

    strvalue=(string)inp.GetValue("Flow Advection Form","Convective",false,true);
    if(strvalue == "Convective")
      solpar.iconvflow = 2;
    else if(strvalue == "Conservative")
      solpar.iconvflow = 1;

    strvalue=(string)inp.GetValue("Scalar Advection Form","Convective",false,false);
    if(strvalue == "Convective")
      solpar.iconvsclr = 2;
    else if(strvalue == "Conservative")
      solpar.iconvsclr = 1;

    strvalue=(string)inp.GetValue("Use Conservative Scalar Convection Velocity","False",false,false);
    if(strvalue == "True")
      sclrs.consrv_sclr_conv_vel = true;
    else
      sclrs.consrv_sclr_conv_vel = false;

    // =========
    // TAU INPUT
    // ========= 
    strvalue=(string)inp.GetValue("Tau Matrix","Diagonal-Shakib",false,false);
    if(strvalue == "Diagonal-Shakib")
      genpar.itau = 0;
    else if(strvalue == "Diagonal-Franca")
      genpar.itau =1;
    else if(strvalue == "Diagonal-Jansen(dev)")
      genpar.itau = 2;
    else if(strvalue == "Diagonal-Compressible")
      genpar.itau = 3;
    else if(strvalue == "Matrix-Mallet")
      genpar.itau = 10;
    else if(strvalue == "Matrix-Modal")
      genpar.itau = 11;

    genpar.dtsfct = inp.GetValue("Tau Time Constant","1",false,false);
    genpar.taucfct = inp.GetValue("Tau C Scale Factor","1",false,false);

    // ============================
    // FLOW DISCONTINUITY CAPTURING
    // ============================
    strvalue=(string)inp.GetValue("Discontinuity Capturing","Off",false,false);//not used, only print out
    if(strvalue == "Off") solpar.iDC = 0;
    else if(strvalue == "DC-mallet") solpar.iDC = 1;
    else if(strvalue == "DC-quadratic") solpar.iDC = 2;
    else if(strvalue == "DC-minimum") solpar.iDC = 3;
    else {
      if(myrank==0){
    	cout<< "Condition not defined for Discontinuity Capturing \n ";
      }
      exit(1);
    }

    // ==============================
    // SCALAR DISCONTINUITY CAPTURING
    // ==============================
    ivec = inp.GetValue("Scalar Discontinuity Capturing","0 0",false,false);
    for(i=0; i< 2; i++)  solpar.idcsclr[i] = ivec[i];
    ivec.erase(ivec.begin(),ivec.end());
 
    strvalue=(string)inp.GetValue("Include Viscous Correction in Stabilization","True",false,false);
    if(strvalue == "True"){
        if(genpar.ipord == 1 ) genpar.idiff = 1;
        else genpar.idiff = 2;
    }else{
    	genpar.idiff = 0;
    }

    timdat.flmpl = inp.GetValue("Lumped Mass Fraction on Left-hand-side","0",false,false);
    timdat.flmpr = inp.GetValue("Lumped Mass Fraction on Right-hand-side","0",false,false);

//    //never used
//    strvalue=(string)inp.GetValue("Dump CFL","False",false);
//    if(strvalue == "True") timdat.iCFLworst = 1;

    intdat.intg[0][0]=inp.GetValue("Quadrature Rule on Interior","2",false,true);
    intdat.intg[0][1]=inp.GetValue("Quadrature Rule on Boundary","3",false,true);
    genpar.ibksiz = inp.GetValue("Number of Elements Per Block","255",false,true);
   
    // ==================================
    // CARDIOVASCULAR MODELING PARAMETERS
    // ==================================

    nomodule.ipvsq=0;
    if(nomodule.icardio = inp.GetValue("Number of Coupled Surfaces","0",false,true)){
      if ( nomodule.icardio > MAXSURF ) {
    	 if(myrank==0){
    	  cout << "Number of Coupled Surfaces > MAXSURF \n";
    	 }
        exit(1);
      } 
      strvalue=(string)inp.GetValue("Pressure Coupling","Implicit",false,true);
      if ( strvalue == "None")
        nomodule.ipvsq=0;
      if ( strvalue== "Explicit")
        nomodule.ipvsq=1;
      if ( strvalue == "Implicit")
        nomodule.ipvsq=2;
      if ( strvalue == "P-Implicit")
        nomodule.ipvsq=3;

      //Resistance
      if(nomodule.numResistSrfs=inp.GetValue("Number of Resistance Surfaces","0",false,false)){
          ivec = inp.GetValue("List of Resistance Surfaces","",true,true);
          for(i=0;i<MAXSURF+1; i++) nomodule.nsrflistResist[i] = 0;
          for(i=0; i< nomodule.numResistSrfs; i++){
              nomodule.nsrflistResist[i+1] = ivec[i];
          }
          vec = inp.GetValue("Resistance Values","",true,true);
          for(i =0; i< MAXSURF+1 ; i++) nomodule.ValueListResist[i] = 0;
          for(i =0; i< nomodule.numResistSrfs ; i++) nomodule.ValueListResist[i+1] = vec[i];
          vec.erase(vec.begin(),vec.end());
      }

#if(VER_CLOSEDLOOP == 1)
      //CLOSEDLOOP
      nomodule.iGenInitialization = inp.GetValue("Number of Timesteps for GenBC Initialization","0",false,false);
      strvalue=(string)inp.GetValue("Find the GenBC Inside the Running Directory","True",false,false);
      if ( strvalue == "True")
         nomodule.iGenFromFile = 1;
      else
    	 nomodule.iGenFromFile = 0;

      if(nomodule.numDirichletSrfs=inp.GetValue("Number of Dirichlet Surfaces","0",false,false)){
          ivec = inp.GetValue("List of Dirichlet Surfaces","",true,true);
          for(i=0;i<MAXSURF+1; i++) nomodule.nsrflistDirichlet[i] = 0;
          for(i=0; i< nomodule.numDirichletSrfs; i++){
              nomodule.nsrflistDirichlet[i+1] = ivec[i];
          }
      }

      if(nomodule.numNeumannSrfs=inp.GetValue("Number of Neumann Surfaces","0",false,false)){
          ivec = inp.GetValue("List of Neumann Surfaces","",true,true);
          for(i=0;i<MAXSURF+1; i++) nomodule.nsrflistNeumann[i] = 0;
          for(i=0; i< nomodule.numNeumannSrfs; i++){
              nomodule.nsrflistNeumann[i+1] = ivec[i];
          }
      }

      if(nomodule.numNormalSrfs=inp.GetValue("Number of Normal Constrained Surfaces","0",false,false)){
	     ivec = inp.GetValue("List of Normal Constrained Surfaces","",true,true);
         for(i=0; i<MAXSURF+1; i++) nomodule.nsrflistNormal[i] = 0;
         for(i=0; i<nomodule.numNormalSrfs; i++){
	        nomodule.nsrflistNormal[i+1]=ivec[i];
         }
     }
//    =============================================
#endif

      //Impedance
      if(nomodule.numImpSrfs=inp.GetValue("Number of Impedance Surfaces","0",false,false)){
          ivec = inp.GetValue("List of Impedance Surfaces","",true,true);
          for(i=0;i<MAXSURF+1; i++) nomodule.nsrflistImp[i] = 0;
          for(i=0; i< nomodule.numImpSrfs; i++){
              nomodule.nsrflistImp[i+1] = ivec[i];
          }
          strvalue=(string)inp.GetValue("Impedance From File","False",false,true);
          if (strvalue == "True")
              nomodule.impfile = 1; else nomodule.impfile = 0;
      }

      //RCR
      if(nomodule.numRCRSrfs=inp.GetValue("Number of RCR Surfaces","0",false,false)){
          ivec = inp.GetValue("List of RCR Surfaces","",true,true);
          for(i=0;i<MAXSURF+1; i++) nomodule.nsrflistRCR[i] = 0;
          for(i=0; i< nomodule.numRCRSrfs; i++){
              nomodule.nsrflistRCR[i+1] = ivec[i];
          }
          strvalue=(string)inp.GetValue("RCR Values From File","False",false,true);
          if (strvalue == "True")
              nomodule.ircrfile = 1; else nomodule.ircrfile = 0;
      }

#if(VER_CORONARY == 1)
      //CORONARY
	  if(nomodule.numCORSrfs=inp.GetValue("Number of COR Surfaces","0",false,false)){
          ivec = inp.GetValue("List of COR Surfaces","",true,true);
          for(i=0;i<MAXSURF+1; i++) nomodule.nsrflistCOR[i] = 0;
          for(i=0; i< nomodule.numCORSrfs; i++){
              nomodule.nsrflistCOR[i+1] = ivec[i];
          }
          strvalue=(string)inp.GetValue("COR Values From File","False",false,true);
          if ( strvalue == "True")
              nomodule.icorfile = 1; else nomodule.icorfile = 0;
      }      
#endif
//      if(nomodule.numCalcSrfs=inp.GetValue("Number of Surfaces which Output Pressure and Flow","0",false)){
//	     ivec = inp.GetValue("List of Output Surfaces","",true);
//         for(i=0; i<MAXSURF+1; i++) nomodule.nsrflistCalc[i] = 0;
//          for(i=0; i<nomodule.numCalcSrfs; i++){
//	        nomodule.nsrflistCalc[i+1]=ivec[i];
//          }
//         }

	  //Backflow control
	  nomodule.backFlowStabCoef = (double)inp.GetValue("Backflow Stabilization Coefficient","0.2",false,true);

	  if(nomodule.numVisFluxSrfs=inp.GetValue("Number of Surfaces which zero out in-plane tractions","0",false,false)){
		 ivec = inp.GetValue("List of Surfaces which zero out in-plane tractions","",true,true);
		 for(i=0; i<MAXSURF+1; i++) nomodule.nsrflistVisFlux[i] = 0;
		 for(i=0; i<nomodule.numVisFluxSrfs; i++){
			nomodule.nsrflistVisFlux[i+1]=ivec[i];
		 }
	  }
      strvalue=(string)inp.GetValue("Lagrange Multipliers","False",false,false);
	  if ( strvalue == "True")
         nomodule.Lagrange = 1; else nomodule.Lagrange = 0;
	  if ( nomodule.Lagrange ==1) {
         nomodule.numLagrangeSrfs = inp.GetValue("Number of Constrained Surfaces","",true,true);
	     ivec = inp.GetValue("List of Constrained Surfaces","",true,true);
         for(i=0;i<MAXSURF+1; i++) nomodule.nsrflistLagrange[i] = 0;
		 for(i=0; i< nomodule.numLagrangeSrfs; i++) {
                    nomodule.nsrflistLagrange[i+1] = ivec[i];
		 }
		 strvalue=(string)inp.GetValue("Constrained Surface Information From File","False",false,true);
         if ( strvalue == "True")
            nomodule.iLagfile = 1; else nomodule.iLagfile = 0;
	  }

	}

   // =========================
   // DEEFORMABLE WALL SETTINGS
   // =========================
   nomodule.ideformwall = 0;
   nomodule.ivarwallprop = 0;
   strvalue=(string)inp.GetValue("Deformable Wall","False",false,true);
   if(strvalue=="True"){
     // Set Deformable Wall Flag
     nomodule.ideformwall = 1;

#if(VER_VARWALL == 1)
     // Check Variable wall properties
     strvalue=(string)inp.GetValue("Variable Wall Thickness and Young Mod","False",false,true);
     if(strvalue == "True"){
       nomodule.ivarwallprop = 1;
     }else{
       nomodule.ivarwallprop = 0;
     } 
#endif

     // Read isotropic material properties only if the thi
     if(nomodule.ivarwallprop == 0){
       nomodule.thicknessvw = inp.GetValue("Thickness of Vessel Wall","",true,true);
       nomodule.evw = inp.GetValue("Young Mod of Vessel Wall","",true,true);
     }else{
       // Read Isotropic values from File
       nomodule.thicknessvw = 0.0;
       nomodule.evw = 0.0;
     }
     // Density and shear constant of the walls need to be defined
     nomodule.rhovw = inp.GetValue("Density of Vessel Wall","",true,true);
     nomodule.rshearconstantvw = inp.GetValue("Shear Constant of Vessel Wall","",true,true);
     // Continue with other properties having default values
     nomodule.rnuvw = inp.GetValue("Poisson Ratio of Vessel Wall","0.5",false,true);
     nomodule.nProps = inp.GetValue("Number of Wall Properties per Node","10",false,true);

     strvalue=(string)inp.GetValue("Wall Mass Matrix for LHS","True",false,true);
     if(strvalue == "True"){
       nomodule.iwallmassfactor = 1;
     }else{
       nomodule.iwallmassfactor = 0;
     }
     strvalue=(string)inp.GetValue("Wall Stiffness Matrix for LHS","True",false,true);
     if(strvalue == "True"){
       nomodule.iwallstiffactor = 1;
     }else{
       nomodule.iwallstiffactor = 0;
     }
    }

    // ============================
    // Non-linear Iteration Control
    // ============================
    strvalue=(string)inp.GetValue("Residual Control","True",false,true);
    if (strvalue == "True") nomodule.rescontrol = 1;
    else nomodule.rescontrol = 0;
    if ( nomodule.rescontrol ==1) {
       nomodule.ResCriteria = inp.GetValue("Residual Criteria","0.01",false,true);
       nomodule.MinNumIter = inp.GetValue("Minimum Required Iterations","3",false,true);
    }
    // Step Sequencing
    ivec = inp.GetValue("Step Construction","0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1",false,true);
    sequence.seqsize = ivec.size();
    if( sequence.seqsize > 100 || sequence.seqsize < 2 ){
      if(myrank==0){
    	cerr<<"Sequence size must be between 2 and 100 "<<endl;
      }
    }
    for(i=0; i< sequence.seqsize; i++){
      sequence.stepseq[i] = ivec[i];
    }

    // ============================
    // Task Control
    // ============================
    strvalue=(string)inp.GetValue("Solver Task","Full Simulation",false,true);
    if( strvalue =="Full Simulation" ){
        inpdat.solverTask=0;
    }else if ( strvalue =="Only Partition" ){
        inpdat.solverTask=1;
    }else{
        if(myrank==0){
            cerr << " ERROR: Invalid Solver Task." << endl;
        }
        return 001;
    }

    // ==================
    // SCALING PARAMETERS
    // ==================
    outpar.ro = inp.GetValue("Density Scaling","1.0",false,false);
    outpar.vel = inp.GetValue("Velocity Scaling","1.0",false,false);
    outpar.press = inp.GetValue("Pressure Scaling","1.0",false,false);
    outpar.temper = inp.GetValue("Temperature Scaling","1.0",false,false);
    outpar.entrop = inp.GetValue("Entropy Scaling","1.0",false,false);

    cout << endl ;
  }
  catch ( exception &e ) {
	ierr = 001;
	if(myrank==0){
	 cout << endl << "Input exception: " << e.what() << endl << endl;
     print_error_code(ierr);
	}
    return ierr;
  }

  return ierr;
  
}

void print_error_code(int ierr) {
  /*
    Return Error codes:
    0xx         Input error
    1xx         Solution Control

    2xx         Material Properties

    3xx         Output Control

    4xx         Discretization Control

    5xx         Scaling Parameters

    6xx         Linear Solver Control
    601         linear solver type not supported
  */
  cout << endl << endl << "Input error detected: " << endl << endl;
  if ( ierr == 001 ) {
    cout << endl << "Input Directive not understood" << endl << endl;
  }
  if ( ierr == 601 ) {
    cout << endl << "Linear Solver Type Not Supported" << endl << endl;
  }

}
