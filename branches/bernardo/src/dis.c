#include <R.h>
#include <Rmath.h>

double F77_SUB(dis)( double *x )
   { return pnorm( *x, 0, 1, 1, 0 ); }
