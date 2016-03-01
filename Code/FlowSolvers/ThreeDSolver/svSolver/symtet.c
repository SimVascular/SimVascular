/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
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
 */

typedef double DARR4[4];

int tetIntPnt(int,DARR4**,double **);

#ifdef SV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  #define SYMTET symtet_
#endif

void  SYMTET(int *n1, double pt[][4], double wt[], int *err)
{
  double *lwt;
  DARR4 *lpt;
  int i,j;
  *err = tetIntPnt(*n1, &lpt, &lwt);
  for(i=0; i < *n1; i++) {
    wt[i] = lwt[i];
    for(j=0; j <4; j++) 
      pt[i][j]=lpt[i][j];
  }
}

/*$Id: symtet.c,v 1.3 2005/06/29 19:36:05 nwilson Exp $*/
#include <stdio.h>

/* constants for 4-point rule */
#define  a4  0.5854101966249685  
#define  b4  0.1381966011250150  

/* constants for 5-point rule */
#define  a5  0.5000000000000000  
#define  b5  0.1666666666666667  

/* constants for 16-point rule */
#define  a16  0.0503737941001228  
#define  b16  0.0665420686332923  
#define  c16  0.7716429020672371  
#define  d16  0.0761190326442543  
#define  e16  0.1197005277978019  
#define  f16  0.0718316452676693  
#define  g16  0.4042339134672644  

/* constants for 17-point rule */
#define  a17  0.1884185567365411  
#define  b17  0.0670385837260428  
#define  c17  0.0452855923632739  
#define  p17  0.7316369079576180  
#define  q17  0.0894543640141273  
#define  e17  0.1325810999384657  
#define  f17  0.0245400397290300  
#define  g17  0.4214394310662522  


/* constants for 29-point rule */
#define  a29  0.0904012904601475  
#define  b29  0.0191198342789912  
#define  c29  0.0436149384066657  
#define  d29  0.0258116759619916  
#define  p29  0.8277192480479295  
#define  q29  0.0574269173173568  
#define  e29  0.0513518841255634  
#define  f29  0.4860510285706072  
#define  g29  0.2312985436519147  
#define  h29  0.2967538129690260  
#define  i29  0.6081079894015281  
#define  j29  0.0475690988147229  

#define a25   0.2500000000000000
#define a26  -0.8000000000000000
#define a27   0.4500000000000000

/* typedef double DARR4[4] ; */

  static double rstw1[][4] = {{a25,a25,a25,a25}};
  static double twt1[] = {1.000000000000000000000};

  static double rstw4[][4] = {{a4,b4,b4,b4},{b4,a4,b4,b4},{b4,b4,a4,b4},
                              {b4,b4,b4,a4}};
  static double twt4[] = {a25,a25,a25,a25};

  static double rstw5[][4] = {{a25,a25,a25,a25},
                             {a5,b5,b5,b5},
                             {b5,a5,b5,b5},
                             {b5,b5,a5,b5},
                             {b5,b5,b5,a5}};
  static double twt5[] = {a26,a27,a27,a27,a27};

  static double rstw16[][4] = {{c16,d16,d16,d16},
                              {d16,c16,d16,d16},
                              {d16,d16,c16,d16},
                              {d16,d16,d16,c16},

                              {e16,f16,g16,g16},
                              {f16,e16,g16,g16},
                              {e16,g16,g16,f16},
                              {f16,g16,g16,e16},
                              {g16,g16,e16,f16},
                              {g16,g16,f16,e16},
                              {g16,e16,f16,g16},
                              {g16,f16,e16,g16},
                              {e16,g16,f16,g16},
                              {f16,g16,e16,g16},
                              {g16,e16,g16,f16},
                              {g16,f16,g16,e16} };
  static double twt16[] = {a16,a16,a16,a16,
                          b16,b16,b16,b16,
                          b16,b16,b16,b16,
                          b16,b16,b16,b16 };

  static double rstw17[][4] = {{a25,a25,a25,a25},
                              {p17,q17,q17,q17},
                              {q17,p17,q17,q17},
                              {q17,q17,p17,q17},
                              {q17,q17,q17,p17},

                              {e17,f17,g17,g17},
                              {f17,e17,g17,g17},
                              {e17,g17,g17,f17},
                              {f17,g17,g17,e17},
                              {g17,g17,e17,f17},
                              {g17,g17,f17,e17},
                              {g17,e17,f17,g17},
                              {g17,f17,e17,g17},
                              {e17,g17,f17,g17},
                              {f17,g17,e17,g17},
                              {g17,e17,g17,f17},
                              {g17,f17,g17,e17} };

  static double twt17[] = {a17,b17,b17,b17,b17,
                          c17,c17,c17,c17,c17,c17,
                          c17,c17,c17,c17,c17,c17};

  static double twt29[] = {a29,b29,b29,b29,b29,
                          c29,c29,c29,c29,c29,c29,
                          c29,c29,c29,c29,c29,c29,
                          d29,d29,d29,d29,d29,d29,
                          d29,d29,d29,d29,d29,d29};

  static double rstw29[][4] = {{a25,a25,a25,a25},

                              {p29,q29,q29,q29},
                              {q29,p29,q29,q29},
                              {q29,q29,p29,q29},
                              {q29,q29,q29,p29},

                              {e29,f29,g29,g29},
                              {f29,e29,g29,g29},
                              {e29,g29,g29,f29},
                              {f29,g29,g29,e29},
                              {g29,g29,e29,f29},
                              {g29,g29,f29,e29},
                              {g29,e29,f29,g29},
                              {g29,f29,e29,g29},
                              {e29,g29,f29,g29},
                              {f29,g29,e29,g29},
                              {g29,e29,g29,f29},
                              {g29,f29,g29,e29},

                              {h29,i29,j29,j29},
                              {i29,h29,j29,j29},
                              {h29,j29,j29,i29},
                              {i29,j29,j29,h29},
                              {j29,j29,h29,i29},
                              {j29,j29,i29,h29},
                              {j29,h29,i29,j29},
                              {j29,i29,h29,j29},
                              {h29,j29,i29,j29},
                              {i29,j29,h29,j29},
                              {j29,h29,j29,i29},
                              {j29,i29,j29,h29} };

#ifdef __cplusplus
extern "C" {
#endif

int tetIntPnt(int nint, DARR4 **bcord, double **wt)
{
  int retval = 1;

  if( nint == 1 ) {*bcord = rstw1 ; *wt = twt1; }
  else if( nint == 4 ){*bcord = rstw4 ; *wt = twt4; }
  else if( nint == 5 ){*bcord = rstw5 ; *wt = twt5; }
  else if( nint == 16 ){*bcord = rstw16 ; *wt = twt16; }
  else if( nint == 17 ){*bcord = rstw17 ; *wt = twt17; }
  else if( nint == 29 ){*bcord = rstw29 ; *wt = twt29; }
  else
  {
    fprintf(stderr,"\n%d integration points unsupported in symtet.c; give {1,4,5,16,17,29}\n",nint);
    retval = 0;
  }
  return retval ;
}

#ifdef __cplusplus
}
#endif
