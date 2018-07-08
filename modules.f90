module grid
! module containing variables used to define the grid
! Author: Rishabh More
! Date: 07-08-2018
implicit none

! Node coordinates x,y,z
real(8), allocatable :: x(:),y(:),z(:)

! grid size
real(8) :: dx, dy ,dz

! Node indices
integer :: i,j,k

! # grid points
integer :: Nx, Ny, Nz

! Domain size
real(8) :: Lx, Ly, Lz

end module grid


module flow
! module containing fluid flow variables 
! Author: Rishabh More
! Date: 07-08-2018

implicit none

! x,y,z velocities
real(8), allocatable :: u(:), v(:), w(:) 

! pressure
real(8), allocatable :: p(:,:,:)

! density
real(8), allocatable :: rho(:,:,:)

! dynamic viscosity
real(8) :: mu

! time related
real(8) :: dt     ! time step
real(8) :: t_final ! total time for running simulation
integer :: t_step ! time step counter


end module flow


module BC
! module for boundary conditions 
! Author: Rishabh More
! Date: 07-08-2018

implicit none

! character array containing BC in x,y,z directions respectively
! wall
character(len = 20), dimension(3) :: bdry_cond 

end module BC

module io
! input/output module 
! Author: Rishabh More
! Date: 07-08-2018
use grid
use flow
use BC

implicit none

! path for storing output files
character(len=20) :: out_path



contains

subroutine initialize 
! Initializes appropriate vatiables variables
! 
! 
implicit none

! initialize time variables
t_step = 0


end subroutine initialize


subroutine read_input
! Read input parameters from the input file
!
!
        
implicit none

! namelist containing a list of input variables to be
! read from the input file
! 
NAMELIST /parameters/ Nx, Ny, Nz, Lx, Ly, Lz, &
                      out_path, t_final, dt

open(9, FILE='input',STATUS='unknown')
read(9, NML=parameters)
close(9)



end subroutine read_input


end module io
