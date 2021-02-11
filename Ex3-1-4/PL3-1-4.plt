res
set xr[-4:0]
  plot './gosa.txt' us 1:2 w p title 'Euler'
replot './gosa.txt' us 1:3 w p title 'Heun'
replot './gosa.txt' us 1:4 w p title 'RK4'

set key right bottom
set xlabel 'log_10(h)'
set ylabel 'log_10(|err|_L^inf)'

f(x) = a1 * x + b1
fit f(x) './gosa.txt' us 1:2 via a1, b1
rep a1 * x + b1 w l

f(x) = a2 * x + b2
fit f(x) './gosa.txt' us 1:3 via a2, b2
rep a2 * x + b2 w l

f(x) = a3 * x + b3
fit f(x) './gosa.txt' us 1:4 via a3, b3
rep a3 * x + b3 w l

print ' a1= ', a1
print ' a2= ', a2
print ' a3= ', a3

set terminal png
set output 'err.png'
rep
