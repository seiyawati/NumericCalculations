#include <stdio.h>
#include <math.h>
#include "./rkf3.h"

void myf(double t, double *x, double *f)
{
  f[0] = x[1];
  f[1] = -x[0];
}

int main()
{
  int i, n;
  double a = 0, b = 1;
  double x[2], newx[2], localerror, error, diff, lastdiff, lasterror;
  double k[2][RKF45_STAGE], work[2];
  double h;
  printf("真の値=%20.15f\n", cos(b));
  printf("    n                  x   誤差 推定誤差\n");
  for (n = 1; n <= 10000; n *= 2) {
    x[0] = cos(a); x[1] = - sin(a);
    h = (b - a) / n;
    error = 0;
    for (i = 0; i < n; i++) {
      bf_rkf45(2, myf,
	       a + i * h, x, h, newx, &localerror,
	       k, work);
      x[0] = newx[0];
      x[1] = newx[1];
      error += fabs(localerror);
    }
    diff = fabs(x[0] - cos(b));
    printf("%5d %18.15f %7.1e %7.1e", n, x[0], diff, error);
    if (n != 1) {
      printf(" %4.1f分の1 %4.1f分の1", lastdiff / diff, lasterror / error);
    }
    printf("\n");
    lastdiff = diff; lasterror = error;
  }
  return 0;
}
