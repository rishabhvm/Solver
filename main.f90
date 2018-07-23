!@t
! \textbf{Program Main}
!@h
!   Description:
!     Two dimensional incompressible NS solver
!     Uses FVM approach 
!     Main controls the computational flow of the solver
!     Calls the reuqired subroutines, modules 
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
!     This is a serial program as of now
!     Parallel version will be generated in future
!@q


!!! Percentage of time taken by all main subroutines should be calculated in near future

!     COMPUTING TIME:     - in a typical run the single steps take
!                           the following comp. time (Adams Bashforth):
!
!                         - RHS                  1.707024       22.2 %
!                         - velocity update      0.7407840       9.8 %
!                         - b.c.                 4.2943999E-02   0.7 %
!                         - divergence           0.2957280       3.9 %
!                         - Poisson              0.5641280       7.5 %
!                         - velocity correct.    0.6744159       8.8 %
!                         - space average        0.1864160       2.6 %
!                         - twall                2.9279999E-03   0.1 %
!                         - b.c.                 4.1967999E-02   0.6 %
!                         - strain               0.7076000       3.7 %
!                         - turvis (Smagor.)     1.554768       20.2 %
!                         - space time average   0.4792160       6.4 %
!                         - screen info          0.7446880       9.8 %
!                         - total (sec)          7.7426078


program main
    
    !!! Set of modules which define the global variables to be used
    use grid
    use flow
    use BC
    use io
    use time
    implicit none


    !!! Generate the grid
    call grid_generate
    print*, 'Total number of grid points in x and y direction', nxt, nyt
    print*, 'Grid generated'

    !!! Initialize the flow
    call initialize  !!! To be written
    print*, 'Flow initialized at t=0'

    !!! Enforce the boundary conditions
    call boundary !!! To be written
    print*, 'Boundary conditions enforced'

    !!! Time stepping 
    do icycle = 1+nstep, itmax+nstep
        
        !!! Using a time-stepping scheme (explicit,semi-implicit,fully-implicit??)
        !!! Call advection subroutine
        !!! Call diffusion subroutine
        !!! Perform the predictor/corrector step
        !!! Write out the u,v,w,p full fields
        !!! Include subroutine for the i/o of derived variables
    end do

end program main
