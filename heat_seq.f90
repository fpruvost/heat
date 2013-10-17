!-------------------------------------------------------------------------------
!> @file heat_seq.f90
!> @author Inria SED Bordeaux
!> @brief     Heat computation main procedure code
!>
!> @details   This file declares the entry point (main() procedure) of the program.
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!> Main procedure
!>
!> Of course, this is the entry point :P
!>
!> @internal
!-------------------------------------------------------------------------------
program heat_seq
    implicit none
    integer :: nx, ny, size_x, size_y, iter_max, narg, save_flag
    double precision :: hx, hy, dt
    double precision, allocatable ::  u_in(:,:),  u_out(:,:)
    character(len=10)::param
    character(len=10)::name_save
  
    narg = command_argument_count()
    if (narg < 4) then
        call usage()
    end if
  
    call get_command_argument(1,param)
    read(param,*) nx
  
    call get_command_argument(2,param)
    read(param,*) ny

    call get_command_argument(3,param)
    read(param,*) iter_max
  
    call get_command_argument(4,param)
    read(param,*) save_flag
  
    hx = 1./ nx;
    hy = 1./ ny
    dt = min(hx*hx/4., hy*hy/4.)

    size_x = nx + 2
    size_y = ny + 2
  
    allocate(u_in(1:size_x,1:size_y))
    allocate(u_out(1:size_x,1:size_y))

    call set_bounds(size_x, size_y, u_in)
    call set_bounds(size_x, size_y, u_out)
  
  
    call compute_heat_propagation(hx, hy, dt, iter_max, save_flag, size_x, size_y, &
    u_in, u_out)

    deallocate(u_in)

contains

!-------------------------------------------------------------------------------
!> A usage function
!>
!> This function prints out the normal usage of the program and exit the program
!> with failure.
!>
!> @internal
!-------------------------------------------------------------------------------
subroutine usage()
    implicit none
    character(len=255)::name
    call get_command_argument(0,name)
    print *, 'Usage: ', TRIM(name), ' nx ny iter_max save'
    print *, '    nx       number of discretisation points in X'
    print *, '    ny       number of discretisation points in Y'
    print *, '    iter_max maximal number of iterations in temporal loop'
    print *, '    save     boolean flag (1 or 0) for recording states'
    stop
end subroutine usage

!-------------------------------------------------------------------------------
!> Set the boundaries of a 2-D map to 1.0
!> 
!> This function will set to 1.0 all the cells on the boundaries of a 2-D 
!> double maps @e u of size @e size_x in X and @e size_y in Y. @e u should
!> be an array of size (@e size_x + 2) * (@e size_y + 2).
!>
!> For example, if @e u = [0, 0, 0, 0, 0, 0, 0, 0, 0] then after a call to
!> set_bounds(1, 1, u), @e u will contains [1, 1, 1, 1, 0, 1, 1, 1, 1]. In
!> the same way, if @e u = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
!> then after a call to set_bounds(2, 2, u),  @e u will contains
!> [1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1]. That is in 2-D:
!> @code
!>   1 1 1 1
!>   1 0 0 1
!>   1 0 0 1
!>   1 1 1 1
!> @endcode
!>
!> @param size_x The size in X of the @e u map
!> @param size_y The size in Y of the @e u map
!> @param u The map to set boundaries to 1.0
!>
!> @internal
!-------------------------------------------------------------------------------
subroutine set_bounds(size_x, size_y, u)
  implicit none
  integer, intent(in) :: size_x, size_y 
  double precision, dimension(1:size_x, 1:size_y), intent(inout) :: u

        u(:,       1) = 1.d0
        u(:,  size_y) = 1.d0
end subroutine set_bounds

!-------------------------------------------------------------------------------
!> Compute the heat propagation equation using iterative approach
!> until convergence.
!> 
!> This function will compute the heat propagation equation on the map
!> @e u_in using the iterative approach outputing the result in @e u_out.
!> @e dt, @e hx and @e hy give the derivation approximation steps over time,
!> X and Y. @e iter_max limits the maximum number of iteration if the computation
!> does not converge.
!>
!> @param hx the derivation approximation step in X
!> @param hy the derivation approximation step in Y
!> @param dt the derivation approximation step in time
!> @param iter_max the maximum number of iteration to perform.
!> @param save a boolean indicating if the results is to be saved in a file after
!>        each step
!> @param size_x The size in X of the @e u_in and @e u_out maps
!> @param size_y The size in Y of the @e u_in and @e u_out maps
!> @param u_in the input map, it will be modified and invalidated by the
!>        call of this function
!> @param u_out the output map, it will contains the result of the computation
!>
!> @internal
!-------------------------------------------------------------------------------
subroutine compute_heat_propagation(hx, hy, dt, iter_max, save_flag, &
size_x, size_y, u_in, u_out)

    implicit none
    double precision :: hx, hy, dt, error, prec
    integer :: size_x, size_y, i, save_flag, iter_max
    double precision, dimension(1:size_x, 1:size_y), intent(inout) :: u_in, u_out

    prec = 1e-4
    error = 1e10
    do i=0, iter_max
        call heat(hx, hy, dt, size_x, size_y, u_in, u_out, error)
        error = sqrt(error)
        if (mod(i,10) == 0) then
            print * , 'it =', i, 't = ', i*dt, 'err = ', error
        endif
        if (save_flag /= 0) then
            write(name_save, '("sol_"i5.5)') i
            call save_mat(name_save, size_x, size_y, u_out)
        endif
        u_in = u_out
        if (error <= prec) exit
    end do

end subroutine compute_heat_propagation

end program heat_seq
