!---------------------------------------------------------------------------
! i/o module
! Author: Rishabh More
! Date: 07-08-2018
!---------------------------------------------------------------------------

module io
    use grid
    use flow
    use BC
    use time
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
        t_current = 0d0

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


