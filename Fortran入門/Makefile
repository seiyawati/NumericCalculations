# Fortran Compiler
FC = gfortran
FOPTION = -g # デバッグの時に使う

TARGET = ./a.out

$(TARGET): module.f90 main.f90
	$(FC) module.f90 main.f90

clean:
	rm *.mod *.out
