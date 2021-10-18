#include <stdio.h>
#include <math.h>   /* fabs() */
#include "rkf3.h"

static double rkf45_alpha[RKF45_STAGE] = {
  0.0, 1.0/4, 3.0/8, 12.0/13, 1.0, 1.0/2};

static double rkf45_beta[RKF45_STAGE][RKF45_STAGE-1] = {
  {   0.0,          0.0,                      0.0,        0.0},
  {   1.0/4,        0.0,                      0.0,        0.0},
  {   3.0/32,       9.0/32,                   0.0,        0.0},
  {1932.0/2197, -7200.0/2197, 7296.0/2197,    0.0,        0.0},
  { 439.0/216,     -8.0,      3680.0/513,  -845.0/4104,   0.0},
  {  -8.0/27,       2.0,     -3544.0/2565, 1859.0/4104, -11.0/40}
};

static double rkf45_mu5[RKF45_STAGE] = {
  16.0/135, 0.0, 6656.0/12825, 28561.0/56430, -9.0/50, 2.0/55
};

static double rkf45_mu4[RKF45_STAGE] = {
  25.0/216, 0.0, 1408.0/2565,  2197.0/4104,   -1.0/5,  0.0
};

static double rkf45_mu_diff[RKF45_STAGE] = {
  -1.0/360, 0.0, 128.0/4275, 2197.0/75240, -1.0/50, -2.0/55
};

static double sqr(double x) { return x * x; }

/*
 * d次元の ODE x'(t)=f(t,x) を解く。
 * 時刻 t で値が x だとして、h だけ時間を進めたときの値 nextx を計算する。
 *
 * 注意:
 *   x と next x は同じメモリー領域を指さないようにしておくこと。つまり
 *     bf_rkf45(d, f, t, x, h, x, err, k, w);
 *   のようにして呼び出すと結果は壊れてしまう。
 *   --- こういう仕様にした方が全体に無駄が少なくなると考えた。
 *
 * work は問題の次元 d だけの長さの double 型の作業用領域
 */

int bf_rkf45(int d, function f,
	     double t, double *x, double h,
	     double *nextx, double *error,
	     double k[][RKF45_STAGE], double *work)
{
  int i, j, s = RKF45_STAGE;

  /* x と nextx が重ならないことをチェック */
  if (x == nextx) {
    fprintf(stderr, "bf_rkf45(): x と nextx は同じメモリー領域ではいけない！");
    *error = 1e+10;
    return -1;
  }
  /* k1,k2,k3,k4,k5,k6 を計算する */
  for (i = 0; i < s; i++) {
    double *xx = nextx;  /* ちょっとの間使わせてもらう (お行儀が悪いけど) */
    double *kk = work;
    for (j = 0; j < d; j++)
      xx[j] = x[j] + dotprod(i, rkf45_beta[i], k[j]);
    f(t + rkf45_alpha[i] * h, xx, kk);
    for (j = 0; j < d; j++)
      k[j][i] = kk[j] * h;
  }
  /* 次のステップの値を5次公式で計算する＆4次公式の誤差を推測する */
  for (j = 0; j < s; j++) {
    nextx[j] = x[j] + dotprod(s, rkf45_mu5, k[j]);
    work[j]  = dotprod(s, rkf45_mu_diff, k[j]);
  }
  /* 推定誤差のノルム */
  *error = 0;
  for (j = 0; j < d; j++) *error += sqr(work[j]);
  *error = sqrt(*error);

  /* もし4次の値が欲しければ */
  for (j = 0; j < d; j++)
    work[j] -= nextx[j];
  return 0;
}
