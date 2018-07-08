# make file to compile the program
#

run.exe: modules.o main.o
	gfortran -o run.exe modules.o main.o

modules.mod: modules.o modules.f90
	gfortran -c modules.f90

modules.o: modules.f90
	gfortran -c modules.f90

main.o: modules.mod main.f90
	gfortran -c main.f90



clean: 
	rm *.mod *.o run.exe


#end makefile
