#include "SimVascular.h"

SV_EXPORT_ADAPTOR int bakvec ( int n, double t[], double e[], int m, double z[] );
SV_EXPORT_ADAPTOR void cbabk2 ( int n, int low, int igh, double scale[], int m, double zr[], 
  double zi[] );
SV_EXPORT_ADAPTOR void bandr ( int n, int mb, double a[], double d[], double e[], double e2[], 
  int matz, double z[] );
SV_EXPORT_ADAPTOR void csroot ( double xr, double xi, double &yr, double &yi );
SV_EXPORT_ADAPTOR int i4_max ( int i1, int i2 );
SV_EXPORT_ADAPTOR int i4_min ( int i1, int i2 );
SV_EXPORT_ADAPTOR double pythag ( double a, double b );
SV_EXPORT_ADAPTOR double r8_abs ( double x );
SV_EXPORT_ADAPTOR double r8_epsilon ( );
SV_EXPORT_ADAPTOR double r8_max ( double x, double y );
SV_EXPORT_ADAPTOR double r8_min ( double x, double y );
SV_EXPORT_ADAPTOR double r8_sign ( double x );
SV_EXPORT_ADAPTOR void r8mat_identity  ( int n, double a[] );
SV_EXPORT_ADAPTOR double *r8mat_mm_new ( int n1, int n2, int n3, double a[], double b[] );
SV_EXPORT_ADAPTOR void r8mat_print ( int m, int n, double a[], std::string title );
SV_EXPORT_ADAPTOR void r8mat_print_some ( int m, int n, double a[], int ilo, int jlo, int ihi,
  int jhi, std::string title );
SV_EXPORT_ADAPTOR double *r8mat_uniform_01_new ( int m, int n, int &seed );
SV_EXPORT_ADAPTOR void r8vec_print ( int n, double a[], std::string title );
SV_EXPORT_ADAPTOR int rs ( int n, double a[], double w[], int matz, double z[] );
SV_EXPORT_ADAPTOR int rsb ( int n, int mb, double a[], double w[], int matz, double z[] );
SV_EXPORT_ADAPTOR void timestamp ( );
SV_EXPORT_ADAPTOR int tql2 ( int n, double d[], double e[], double z[] );
SV_EXPORT_ADAPTOR int tqlrat ( int n, double w[], double fv2[] );
SV_EXPORT_ADAPTOR void tred1 ( int n, double a[], double w[], double fv1[], double fv2[] );
SV_EXPORT_ADAPTOR void tred2 ( int n, double a[], double d[], double e[], double z[] );

