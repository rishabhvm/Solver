

!---------------------------------------------------------------------------
! Module for grid variables 
! Author: Rishabh More
! Date: 07-08-2018
!---------------------------------------------------------------------------

module grid

implicit none

! Cell faces
real(8), allocatable :: xu(:), yv(:) !,zw(:)

! Cell centers

real(8), allocatable :: xc(:), yc(:) !,zc(:)

! Grid sizes
real(8) :: dxu, dxc, dyv, dyc !dzw, dz

! Grid points
integer :: nx, ny, nxt, nyt!, nz, nzt

! # ghost points
integer :: ng

! Domain size
real(8) :: Lx, Ly!, Lz

end module grid

!---------------------------------------------------------------------------
! Module for fluid flow variables 
! Author: Rishabh More
! Date: 07-08-2018
!---------------------------------------------------------------------------

module flow

implicit none

! x,y,z velocities
real(8), allocatable :: u(:,:), v(:,:)      !, w(:,:,:) 

! x,y,z velocities: after adding advection
real(8), allocatable :: u_s(:,:), v_s(:,:)  !, w_s(:,:,:)

! x,y,z velocities: prev time step


! pressure
real(8), allocatable :: pc(:,:)

! density
real(8), allocatable :: rhoc(:,:)

! dynamic viscosity, density, 
! acceleration due to gravity g = (gx,gy,gz)
real(8) :: mu,rhof,gx, gy, gz

! time related
real(8) :: dt, time  ! time step, current time
real(8) :: t_final ! total time for running simulation
integer :: t_step ! time step counter

! other constants
real(8),allocatable :: c(:,:)

end module flow

!---------------------------------------------------------------------------
! module for boundary conditions 
! Author: Rishabh More
! Date: 07-08-2018
!---------------------------------------------------------------------------

module BC

implicit none

! character array containing BC in x,y,z directions respectively
! wall
character(len = 20) :: bdry_cond(2) 

! wall_vel contains velocities given to the walls
! 1st index: direction of the velocity : x,y
! 2nd index: which wall?
!            1 => -X
!            2 => +X
!            3 => -Y
!            4 => +Y
! so wall_vel(2,3) gives y-velocity of the bottom wall
!

real(8) :: wall_vel(2,4) 

!---------------------------------------------------------------------------
! subroutine for setting velocity BCs
! set_u_BC sets x-velocity BC
! set_v_BC sets y-velocity BC
!---------------------------------------------------------------------------

subroutine set_u_BC
implicit none





end subroutine Set_U_BC

subroutine set_v_BC
implicit none






end subroutine set_v_BC




end module BC

!---------------------------------------------------------------------------
! i/o module
! Author: Rishabh More
! Date: 07-08-2018
!---------------------------------------------------------------------------

module io
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
time = 0d0
dx = Lx/(Nx); dy = Ly/(Ny) !; dz = Lz/(Nz)

! allocate unallocated variables
allocate( u(Nx+Ng,Ny+2*Ng),   v(Nx+2*Ng,Ny+Ng),    &  !w(),&
          u_s(Nx+Ng,Ny+2*Ng), v_s(Nx+2*Ng,Ny+Ng),  &  !w(),&
          p(Nx+2*Ng,Ny+2*Ng), rho(Nx+2*Ng,Ny+2*Ng) )
          c(Nx+2,Ny+2),

allocate( x(Nx+1),     y(Nx+1),     &
          xu(Nx+Ng),   yu(Nx+2*Ng), &
          xv(Nx+2*Ng), yv(Nx+Ng),   &
          xp(Nx+2*Ng), yp(Nx+2*Ng)  )

u = 0d0; v = 0d0; p = 0d0; rho = rhof
c = 0.25d0
c(2,3:Ny) = 1d0/3d0
c(Nx+1,3:Ny) = 1d0/3d0
c(3:Nx,2) = 1d0/3d0
c(3:Nx,Ny+1) = 1d0/3d0
c(2,2) = 0.5d0
c(2,Ny+1) = 0.5d0
c(Nx+1,2) = 0.5d0
c(Nx+1,Ny+1) = 0.5d0


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
