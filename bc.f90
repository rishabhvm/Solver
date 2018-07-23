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


