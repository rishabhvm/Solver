module grid
! module containing variables used to define the grid
! Author: Rishabh More
! Date: 07-08-2018
implicit none

! Node coordinates x,y,z
real(8), allocatable :: x(:),y(:),z(:)

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

module output
! module for storing solution 
! Author: Rishabh More
! Date: 07-08-2018

implicit none
character(len=20) :: out_path

end module output




module input
use grid
use flow
use BC
use output




contains

subroutine initialize 
implicit none


end subroutine initialize


subroutine read_input
implicit none

NAMELIST /parameters/ Nx, Ny, Nz, Lx, Ly, Lz, &
                      out_path

open(9, FILE='input',STATUS='unknown')
read(9, NML=parameters)
close(9)



end subroutine read_input


end module input
