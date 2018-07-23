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

    ! x,y,z velocity increments after advection 
    real(8), allocatable :: du(:,:), dv(:,:)

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

    ! advection scheme identifier
    character(len = 6) :: adv_sch

    ! Diffusion term for different directions
    real(kind = 8), allocatable :: diff_u(:,:), diff_v(:,:)

contains

!-------------------------------------------------------------------------------
! subroutine to calculate advection terms
! Author: Rishabh More
! Date: 07-14-2018
!-------------------------------------------------------------------------------

subroutine calc_advection

    use grid
    implicit none

    !real(8),intent(in) :: u(Nxt,Nyt),v(Nxt,Nyt)
    !real(8),intent(out):: du(Nxt,Nyt),dv(Nxt,Nyt)
    !character(len=6), intent(in) :: adv_sch
    integer :: i, j
    real(8) :: temp(Nxt,Nyt,2) ! temporary array containing interpolated velocities
                           ! temp(i,j,1) = interpolated u velo
                           ! temp(i,j,2) = interpolated v velo

    ! x-momentum

    call interpolate('x',adv_sch,temp)

    do i = ius, iue
        do j = jus, jue
            
            du(i,j) = du(i,j) + &
                      (temp(i+1,j,1)**2d0 - temp(i,j,1)**2d0)*dyv(j) + &
                      (temp(i,j+1,2)*((v(i+1,j) + v(i,j))/2d0)  - &
                       temp(i,j,2)*((v(i,j-1) + v(i+1,j-1))/2d0))*dxu(i) 
        enddo
    enddo

    ! y-momentum

    call interpolate('y',adv_sch,temp)

    do i = ivs, ive
        do j = jvs, jve

            dv(i,j) = dv(i,j) + & 
                      ( temp(i+1,j,1)**2d0 - temp(i,j,1)**2d0 ) * dyv(j) + &
                      ( temp(i,j+1,2) * ( ( v(i+1,j) + v(i,j) ) / 2d0 )  - &
                        temp(i,j,2) * ( ( v(i,j-1) + v(i+1,j-1) ) / 2d0 ) ) * dxu(i)
        enddo
    enddo

end subroutine calc_adv

!-------------------------------------------------------------------------------

subroutine interpolate(dir,sch1,temp1)
    use grid

    implicit none

    character, intent(in) :: dir
    character(len=6), intent(in) :: sch1
    real(8), intent(out) :: temp1(Nxt,Nyt,2)
    integer :: i,j
    character(len=6) :: sch
    sch = sch1
    ! Interpolation at the cell face (x)
    !     |       |   u>  |
    !     x2      x0  x   x1

    if(dir == 'x') then

        do i = ius,iue
            do j = jus,jue

                ! west face
                if(u(i,j)+u(i-1,j) .gt. 0d0) then
                    temp1(i,j,1) = inter(xu(i-1),xu(i),xu(i-2),xc(i),u(i-1,j),u(i,j),u(i-2,j),sch)
                else
                    temp1(i,j,1) = inter(xu(i),xu(i-1),xu(i+1),xc(i),u(i,j),u(i-1,j),u(i+1,j),sch)
                endif
        
                ! south face
                if(v(i,j-1)+v(i+1,j-1) .gt. 0d0) then 
                    temp1(i,j,2) = inter(yu(j-1),yu(j),yu(j-2),yv(j-1),u(i,j-1),u(i,j),u(i,j-2),sch)
                else
                    temp1(i,j,2) = inter(yu(j),yu(j-1),yu(j+1),yv(j-1),u(i,j),u(i,j-1),u(i,j+1),sch)
                endif

            enddo
        enddo

    elseif(dir == 'y') then
    
        do i = ivs,ive
            do j = jvs,jve
            
                ! west face
                if(u(i-1,j)+u(i-1,j+1) .gt. 0d0) then
                    temp1(i,j,1) = inter(xv(i-1),xv(i),xv(i-2),xu(i-1),v(i-1,j),v(i,j),v(i-2,j),sch)
                else
                    temp1(i,j,1) = inter(xv(i),xv(i-1),xv(i+1),xu(i-1),v(i,j),v(i-1,j),v(i+1,j),sch)
                endif

            ! south face
                if(v(i,j)+v(i,j-1) .gt. 0d0) then
                    temp1(i,j,2) = inter(yv(j-1),yv(j),yv(j-2),yc(j),v(i,j-1),v(i,j),v(i,j-2),sch)
                else
                    temp1(i,j,2) = inter(yv(j),yv(j-1),yv(j+1),yc(j),v(i,j),v(i,j-1),v(i,j+1),sch)
                endif
            enddo
        enddo

    endif

contains

!-------------------------------------------------------------------------------
real(8) function inter(x0,x1,x2,x,y0,y1,y2,scheme)
        
                character(len=6), intent(in) :: scheme 
                real(8), intent(in) ::  x0,x1,x2,x,y0,y1,y2
                real(8) :: k,xi,a,b

                ! First Order Upwind
                if(trim(scheme) == 'FOU') then

                    inter = y0

                    ! Second Order Upwind
                elseif(trim(scheme) == 'SOU') then
                
                    xi = (x-x0)/(x2-x0)
                    a = (y2-y0)/(x2-x0)
                    b = y0
                    inter = y0 + a*xi

                    ! Quadratic Upwind Interpolation for Convective Kinematics
                elseif(trim(scheme) == 'QUICK') then
                    ! Interpolation at the cell face (x)
                    !     |       |   u>  |
                    !     x2      x0  x   x1

                    xi = (x-x0)/(x1-x0)
                    k = (x2-x0)/(x1-x0)
                    a = (y2-k**2*y1+(k**2-1.0)*y0)/k/(1.0-k)
                    b = (y2-k   *y1+(k   -1.0)*y0)/k/(k-1.0)
                    inter = y0+a*xi+b*xi**2

                    ! central difference     
                elseif(trim(scheme) == 'CD') then
            
                    inter = 0.5d0*(y0+y1)
        
                    ! Second Order ENO
                elseif(trim(scheme) == 'S-ENO') then
            
                    inter = y0 + min(abs(y1-y0),abs(y0-y2))

                endif


        end function inter
!------------------------------------------------------------------------------
end subroutine interpolate

subroutine calc_diffusion(u,v,diff_u,diff_v)

!!! Calculates the momentum transfer due to diffusion in different directions
!!! Uses the FVM approach of integrated momentum transfer on the u,v cell volumes
!!! Author - Sheel Nidhan
!!! Last Updated - 21 July 2018
    
    use grid
    implicit none

    integer :: i,j        
    real(kind = 8) :: diff_u_x_out, diff_u_x_in, diff_u_y_out, diff_u_y_in 
    real(kind = 8) :: diff_v_x_out, diff_v_x_in, diff_v_y_out, diff_v_y_in
    
    real(kind = 8), intent(in) :: u(nxt,nyt), v(nxt,nyt)
    real(kind = 8), intent(out) :: diff_u(nxt,nyt), diff_v(nxt,nyt)


    diff_u = 0.d0
    dif_v  = 0.d0
    
    do j = 2, nyt-1
        do i = 2, nxt-1
    
            diff_u_x_out = (u(i+1,j) - u(i,j))/(dxu(i))
            diff_u_x_in =  (u(i,j) - u(i-1,j))/(dxu(i-1))
            diff_u_y_out = (u(i,j+1) - u(i,j))/(dyc(j))
            diff_u_y_in =  (u(i,j) - u(i,j-1))/(dyc(j-1))
    
            diff_u(i,j) = diff_u_x_out*dyv(j-1) - diff_u_x_in*dyv(j-1)
            diff_u(i,j) = diff_u(i,j) + diff_u_y_out*dxc(i) - diff_u_y_in*dxc(i)
   
         end do
    end do

    do j = 2, nyt-1
        do i = 2, nxt-1
    
            diff_v_x_out = (v(i+1,j) - v(i,j))/(dxc(i))
            diff_v_x_in =  (v(i,j) - v(i-1,j))/(dxc(i-1))
            diff_v_y_out = (v(i,j+1) - v(i,j))/(dyv(j))
            diff_v_y_in =  (v(i,j) - v(i,j-1))/(dyv(j-1))
    
            diff_v(i,j) = diff_v_y_out*dxu(i-1) - diff_v_y_in*dxu(i-1)
            diff_v(i,j) = diff_v(i,j) + diff_v_x_out*dyc(j) - diff_v_x_in*dyc(j)
 
         end do
    end do

end subroutine calc_diffusion

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

contains
!---------------------------------------------------------------------------
! subroutine for setting velocity BCs
! set_u_BC sets x-velocity BC
! set_v_BC sets y-velocity BC
!---------------------------------------------------------------------------

subroutine set_u_BC
implicit none





end subroutine set_u_BC

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

        ! allocate unallocated variables
        allocate( u(Nx+Ng,Ny+2*Ng),   v(Nx+2*Ng,Ny+Ng),    &  !w(),&
                  u_s(Nx+Ng,Ny+2*Ng), v_s(Nx+2*Ng,Ny+Ng),  &  !w(),&
                  pc(Nx+2*Ng,Ny+2*Ng), rhoc(Nx+2*Ng,Ny+2*Ng) )

        allocate( xc(Nx+1),    yc(Nx+1),     &
                  xu(Nx+Ng),   yu(Nx+2*Ng), &
                  xv(Nx+2*Ng), yv(Nx+Ng))

        u = 0d0; v = 0d0; pc = 0d0; rhoc = rhof

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

!---------------------------------------------------------------------------
! module concerning advection terms
! Author: Rishabh More
! Date: 07-08-2018
!---------------------------------------------------------------------------


!module advection



