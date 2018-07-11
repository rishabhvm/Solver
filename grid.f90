!@t
! \textbf{Program Grid\_generation}
!@h
!   Description:
!     Used to generate the x1_grid.in x2_grid.in and x3_grid.in file
!@q
!   Current Code Owner:
!     Sheel Nidhan/Rishabh More 

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2018  Original code. [Sheel Nidhan] 

!   Language:
!     Fortran 90
!@h
!   Comments:
!     Grid settings are read from input file. This is a serial program. 
!     Support is provided for uniform grids, hyperbolic sine stretching and
!     percent stretching at this time. The edge grid is defined first and 
!     all other values are derived from it. 
!@q

subroutine grid(xu,yv,xc,yc,nx,ny)

    implicit none
    include 'common.h'
    
    integer :: i, j, ix1, ix2
    integer :: nx, ny
    real (kind=8) :: xu(nx), yv(ny), xc(nx), yc(ny)
    real (kind=8) :: dxu(nx-1), dxc(nx-1), dyc(ny-1), dyv(ny-1)
    real (kind=8) :: xlen, ylen
    character (len=160) :: str1, str2

        
    ix1 = 2

    open(unit=1,file=str1,status='old',form='formatted')
    read(1,*) ix2
    read(1,*) (j,xu(i),i=1,ix2)
    close(1)
    
    xlen = xu(ix2) - xu(1)

    xu(nx) = 2*xu(ix2) - xu(ix2-1)

 !!! Finding the cell centers for x grid

    do i = 2, nx
        xc(i) = xu(i-1) + 0.5*(xu(i) - xu(i-1))
    end do
 
    xc(1) = xu(1) - 0.5*(xu(2) - xu(1))

!!! Finding the distances between cell faces - x direction
    do i = 1, nx-1
        dxu(i) = xu(i+1) - xu(i)
        dxc(i) = xc(i+1) - xc(i)    
    end do
!!! 


!!!!!!!!!!!!!!
!
!
!   Write the denominator of different terms in x direction
!
!
!!!!!!!!!!!!

    write(*,*) "X direction grid setup done"
    
    iy1 = 2

    open(unit=1,file=str2,status='old',form='formatted')
    read(1,*) iy2
    read(1,*) (j,yv(i),i=1,iy2)
    close(1)
    
    ylen = yv(ix2) - xu(1)

    yv(ny) = 2*yv(iy2) - yv(iy2-1)

 !!! Finding the cell centers for x grid

    do i = 2, ny
        yc(i) = yv(i-1) + 0.5*(yv(i) - yv(i-1))
    end do
 
    yv(1) = yv(1) - 0.5*(yv(2) - yv(1))

!!! Finding the distances between cell faces - y direction
    do i = 1, ny-1
        dyv(i) = yv(i+1) - yv(i)
        dyc(i) = yc(i+1) - yc(i)    
    end do
!!! 

   
!!!!!!!!!!!!!!
!
!
!   Write the denominator of different terms in y direction
!
!
!!!!!!!!!!!!

    write(*,*) "Y direction grid setup done"



