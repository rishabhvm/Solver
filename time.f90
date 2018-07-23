!---------------------------------------------------------------------------
! Module for time stepping 
! Author: Rishabh More
! Date: 07-20-2018
!---------------------------------------------------------------------------
module time

    !  time related
    real(8) :: dt, t_current  ! time step, current time
    real(8) :: t_final ! total time for running simulation
    integer :: t_step ! time step counter

    integer :: icycle, nstep, itmax

end module time
