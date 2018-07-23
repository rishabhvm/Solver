# make file to compile the program
#

run.exe: grid.o flow.o bc.o time.o io.o main.o
	gfortran -o run.exe grid.o flow.o bc.o time.o io.o main.o

grid.mod: grid.o grid.f90
	gfortran -c grid.f90

grid.o: grid.f90
	gfortran -c grid.f90

flow.mod: flow.o flow.f90
	gfortran -c flow.f90

flow.o: flow.f90
	gfortran -c flow.f90

bc.mod: bc.o bc.f90
	gfortran -c bc.f90

bc.o: bc.f90
	gfortran -c bc.f90

time.mod: time.o time.f90
	gfortran -c time.f90

time.o: time.f90
	gfortran -c time.f90

io.mod: io.o io.f90
	gfortran -c io.f90

io.o: io.f90
	gfortran -c io.f90

main.o: grid.mod flow.mod bc.mod time.mod io.mod main.f90
	gfortran -c main.f90



clean: 
	rm *.mod *.o run.exe


#end makefile
