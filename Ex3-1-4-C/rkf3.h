#define RKF45_STAGE (6)

typedef void function(double t, double *x, double *f);
int bf_rkf45(int d, function f,
	     double t, double *x, double h,
	     double *nextx, double *error,
	     double k[][RKF45_STAGE], double *work);
