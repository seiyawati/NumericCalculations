res
a=0.2
set samples 20
set isosamples 20
set size ratio -1
set xrange [0:2.0*pi]
set yrange [-1.0:2.0*pi+1.0]
unset key
plot "++" using 1:2:(a):(a*sin($2)) with vectors lt rgb "blue"
s1 = (1.0-cos(0.1)) / (1.0+cos(0.1)) # from 0.1
replot acos( (1.-s1*exp(2.*x)) / (1.+s1*exp(2.*x)) ) w l lw 3
s2 = (1.0-cos(0.3)) / (1.0+cos(0.3)) # from 0.3
replot acos( (1.-s2*exp(2.*x)) / (1.+s2*exp(2.*x)) ) w l lw 3
s3 = 1.0 / 3.0 # from pi / 3
replot acos( (1.-s3*exp(2.*x)) / (1.+s3*exp(2.*x)) ) w l lw 3
s4 = 3.0 # from 2 * pi / 3
replot acos( (1.-s4*exp(2.*x)) / (1.+s4*exp(2.*x)) ) w l lw 3
s5 = 3.0 # from 4 * pi / 3
replot 2.0*pi-acos( (1.-s5*exp(2.*x)) / (1.+s5*exp(2.*x)) ) w l lw 3
s6 = 1.0 / 3.0 # from 5 * pi / 3
replot 2.0*pi-acos( (1.-s6*exp(2.*x)) / (1.+s6*exp(2.*x)) ) w l lw 3
#set terminal png
#set output 'VectFld.png'
rep
