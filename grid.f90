!---------------------------------------------------------------------------
! Module for grid variables 
! Author: Rishabh More
! Date: 07-08-2018
!---------------------------------------------------------------------------

module grid

    implicit none
!
!   Pressure cell
!   
!           !-------------------dxu(i-1)--------------------!
!
!                              v(i,j)
!   [xu(i-1),yv(j)]---------[xv(i),yv(j)]---------[xu(i-1),yv(j)]      ---
!           |                                             |             |
!           |                                             |             |
!      u(i-1,j)                p(i,j)                  u(i,j)           |
!   [xu(i-1),yu(j)]        [xc(i),yc(j)]            [xu(i),yv(j)]     dyv(j-1)  

!           |                                             |             |
!           |                                             |             |
!   [xu(i-1),yv(j-1)]------[xv(i),yv(j-1)]--------[xu(i),yv(j-1)]      ---
!                             v(i,j-1)
!
!
!   U-velocity cell 
!   
!           !-------------------dxc(i)--------------------!
!
!        v(i,j)                                      v(i+1,j)
!     [xv(i),yv(j)]---------[xu(i),yv(j)]---------[xv(i+1),yv(j)]      ---
!           |                                             |             |
!           |                                             |             |
!        p(i,j)                u(i,j)                 p(i+1,j)          |
!     [xc(i),yc(j)]         [xu(i),yu(j)]         [xc(i+1),yc(j)]     dyv(j-1)  
!           |                                             |             |
!           |                                             |             |
!   [xu(i-1),yv(j-1)]------[xu(i),yv(j-1)]--------[xv(i+1),yv(j-1)]    ---
!       v(i,j-1)                                     v(i+1,j-1)
!
!
!   V-velocity cell 
!   
!           !-------------------dxu(i-1)--------------------!
!
!      u(i-1,j+1)              p(i,j+1)                u(i,j+1)
!   [xu(i-1),yu(j+1)]-------[xc(i),yc(j+1)]---------[xu(i),yu(j+1)]    ---
!           |                                             |             |
!           |                                             |             |
!           |                  v(i,j)                     |             |
!   [xu(i-1),yv(j)]         [xv(i),yv(j)]           [xu(i),yv(j)]     dyc(j)  
!           |                                             |             |
!           |                                             |             |
!   [xu(i-1),yu(j)]---------[xc(i),yc(j)]-----------[xu(i),yu(j)]      ---
!       u(i-1,j)               p(i,j)                  u(i,j)
!
!
!



    ! Cell faces
    real(8), allocatable :: xu(:), yu(:), xv(:), yv(:) !,zw(:)

    ! Cell centers
    real(8), allocatable :: xc(:), yc(:) !,zc(:)

    ! Grid sizes
    real(8),allocatable :: dxu(:), dyu(:), dxc(:), dyc(:), dxv(:), dyv(:) !dzw, dz

    ! Grid points
    integer :: nx, ny, nxt, nyt!, nz, nzt

    ! # ghost points
    integer :: ng

    ! Domain size
    real(8) :: Lx, Ly!, Lz

    ! start-end indices excluding ghost nodes
    integer :: ics, jcs, ice, jce, &
               ius, iue, jus, jue, &
               ivs, ive, jvs, jve ! i-start, i-end, etc

contains

subroutine grid_generate

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
!@q


    implicit none
    
    integer :: i, j, ix1, ix2, iy1, iy2
    character (len=160) :: str1, str2


!!! Currently only utilizing 1 ghost cell => 2 extra nodes 
!!! nxt = nx + 2
!!! nyt = ny + 2
!!! nx and ny are total number of phyiscal cell centers
        
!!! Grid files contain nxt-1 and nyt-1 points
!!! Cell faces are read and cell centers are derived from cell faces

    ix1 = 2

    open(unit=1,file=str1,status='old',form='formatted')
    read(1,*) ix2
    read(1,*) (j,xu(i),i=1,ix2)
    close(1)

    Lx  = xu(ix2) - xu(1)

    xu(nxt) = 2*xu(ix2) - xu(ix2-1)

!!! Finding the cell centers for x grid

    do i = 2, nxt
        xc(i) = xu(i-1) + 0.5*(xu(i) - xu(i-1))
    end do
 
    xc(1) = xu(1) - 0.5*(xu(2) - xu(1))

    xv = xc
  
!!! Finding the distances between cell faces - x direction
    do i = 1, nxt-1
        dxu(i) = xu(i+1) - xu(i)
        dxc(i) = xc(i+1) - xc(i)    
    end do

    dxv = dxc    
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
    
    Ly = yv(ix2) - xu(1)

    yv(ny) = 2*yv(iy2) - yv(iy2-1)

!!! Finding the cell centers for x grid

    do i = 2, nyt
        yc(i) = yv(i-1) + 0.5*(yv(i) - yv(i-1))
    end do
 
    yc(1) = yv(1) - 0.5*(yv(2) - yv(1))

    yu = yc 

!!! Finding the distances between cell faces - y direction
    do i = 1, nyt-1
        dyv(i) = yv(i+1) - yv(i)
        dyc(i) = yc(i+1) - yc(i)    
    end do

    dyu = dyc

!!! 

   
!!!!!!!!!!!!!!
!
!
!   Write the denominator of different terms in y direction
!
!
!!!!!!!!!!!!

    write(*,*) "Y direction grid setup done"

end subroutine grid_generate    
       
       

       
       
       
       
       
       
       
end module grid


