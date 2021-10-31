# FMKMF_F90 not set: using f90
# FMKMF_SFTAG not set: using f90
# FMKMF_SPATH not set: using . 
# FMKMF_LINKOPTS not set: using no link options 
# Using compile cmd openf95 from cmd line
# Main program is main.f90 
# process_fsource called with arg main.f90 
# main.f90 Uses Module utils
# Full list of modules in main.f90: utils 
# Uses utils which is in ./module.f90
# process_fsource called with arg ./module.f90 
# Full list of modules in ./module.f90:  

# ------------------Macro-Defs---------------------
F90=openf95

# -------------------End-macro-Defs---------------------------

# Here is the link step 
main:module.o main.o 
	 $(F90) -o main module.o main.o   -CB -CU -g -debug all -check all -implicitnone -warn unused -fp-stack-check -ftrapuv -check pointers -check bounds -traceback -static -L../fftw/fftw3-3.3/ -lfftw3f -lfftw3f_threads -i-static

# Here are the compile steps
 
module.o:./module.f90  
	 $(F90) -c ./module.f90 -CB -CU -g -debug all -check all -implicitnone -warn unused -fp-stack-check -ftrapuv -check pointers -check bounds -traceback -static -L../fftw/fftw3-3.3/ -lfftw3f -lfftw3f_threads -i-static

main.o:main.f90 module.o 
	 $(F90) -c main.f90 -CB -CU -g -debug all -check all -implicitnone -warn unused -fp-stack-check -ftrapuv -check pointers -check bounds -traceback -static -L../fftw/fftw3-3.3/ -lfftw3f -lfftw3f_threads -i-static
# This entry allows you to type " make clean " to get rid of
# all object and module files 
clean:
	rm -f -r f_{files,modd}* *~ *.g90 *.o *.mod *.M *.d V*.inc *.vo \
	V*.f *.dbg album F.err
  
