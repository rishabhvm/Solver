module grid
! module containing variables used to define the grid
! Author: Rishabh More
! Date: 07-08-2018
implicit none

! Node coordinates x,y,z
real(8), allocatable :: x(:),y(:)!,z(:)

! grid size
real(8) :: dx, dy !,dz

! Node indices
integer :: i,j!,k

! # grid points
integer :: Nx, Ny!, Nz

! # ghost points
integer :: Ng

! Domain size
real(8) :: Lx, Ly!, Lz

end module grid


module flow
! module containing fluid flow variables 
! Author: Rishabh More
! Date: 07-08-2018

implicit none

! x,y,z velocities
real(8), allocatable :: u(:,:), v(:,:)!, w(:,:,:) 

! pressure
real(8), allocatable :: p(:,:)

! density
real(8), allocatable :: rho(:,:)

! dynamic viscosity, density, 
! acceleration due to gravity g = (gx,gy,gz)
real(8) :: mu,rhof,gx, gy, gz

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
character(len = 20) :: bdry_cond(2) 

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

dx = Lx/(Nx-1); dy = Ly/(Ny-1) !; dz = Lz/(Nz-1)

! allocate unallocated variables
allocate(u(Nx,Ny+1),v(Nx+1,Ny), &  !w(),&
         p(Nx+1,Ny+1), rho(Nx+1,Ny+1))

u = 0d0; v = 0d0; p = 0d0; rho = rhof


end subroutine initialize


subroutine read_input
! Read input parameters from the input file
!
!
        
implicit none

! namelist containing a list of input variables to be
! read from the input file
! 
NAMELIST /parameters/ Nx, Ny, Ng, Lx, Ly, &
                      out_path, t_final, dt, &
                      bdry_cond, mu, rhof, gx, gy, gz 

open(9, FILE='input',STATUS='unknown')
read(9, NML=parameters)
close(9)



end subroutine read_input


end module io
