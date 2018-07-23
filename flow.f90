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

end subroutine calc_advection

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
    diff_v = 0.d0
    
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

