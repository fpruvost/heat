!-------------------------------------------------------------------------------
!> @file heat_par.f90
!> @author Inria SED Bordeaux
!> @brief     Heat computation main procedure code for parallel computation.
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
program heat_par
  implicit none 
  include 'mpif.h' 
  integer :: nx, ny, nc_x, nc_y, i, r, cell_x, cell_y, size_x, size_y, iter_max, ierr 
  integer, parameter :: ndims = 2 
  integer :: rank_w, size_w, rank_2D, comm2D, type_row , alloc_status, narg
  integer :: N=1, S = 2, E = 3, W = 4
  logical :: verbose, reorder = .true.
  integer, dimension(4) :: neighbour
  double precision hx, hy, dt, error, error_loc, prec
  double precision, allocatable ::  u_in(:,:),  u_out(:,:), solution(:,:), vec_temp(:), u_vec(:)
  character(len=10)::param

  integer, dimension(ndims) :: dims , coords, coo
  logical, dimension(ndims) :: periods = .false. 
  
 
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank_w, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, size_w, ierr)

  verbose = (rank_w == 0)

  narg = command_argument_count()
  if (narg < 5) then
      call usage();
  end if    
  
  call get_command_argument(1, param)
  read(param,*) nx 
  
  call get_command_argument(2, param)
  read(param,*) ny 
  
  call get_command_argument(3, param)
  read(param,*) iter_max 

  call get_command_argument(4, param)
  read(param,*) nc_x

  call get_command_argument(5, param)
  read(param,*) nc_y


  if (nc_x * nc_y /= size_w)   then
       if (verbose) print *, 'the total number of processus differs from the product px * py ', & 
       & nc_x, ' x ' , nc_y , '/= ', size_w 
       call MPI_finalize(ierr)
       stop
  endif
  
  cell_x = nx / nc_x
  cell_y = ny / nc_y
  
  size_x = cell_x + 2
  size_y = cell_y + 2 
  
  
  hx = 1.d0/ nx
  hy = 1.d0/ ny
  dt = min(hx*hx/4.d0, hy*hy/4.d0)

  ! construction of the cartesion topology
  dims(1) = nc_x
  dims(2) = nc_y
  
  call MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods, reorder, comm2D, ierr) 
  call MPI_Comm_rank(comm2D, rank_2D, ierr)


  ! Fetch the processus coordinates in the 2-D grid
  call MPI_Cart_coords(comm2D, rank_2D, ndims, coords, ierr)
  
  ! Creation of a non-contiguous in memory column type
  ! to address Fortran storage: no stride of size_x
  call MPI_Type_vector(cell_y, 1, size_x, MPI_DOUBLE_PRECISION, type_row, ierr)
  call MPI_Type_commit(type_row, ierr)
  
  ! fetching the neighbor ranks
  call MPI_Cart_shift(comm2D, 0, 1, neighbour(N), neighbour(S), ierr)
  call MPI_Cart_shift(comm2D, 1, 1, neighbour(W), neighbour(E), ierr)
  
  allocate(u_in(1:size_x,1:size_y), stat=alloc_status)
  if (alloc_status /=0) stop "Not enough memory"
  allocate(u_out(1:size_x,1:size_y), stat=alloc_status) 
  if (alloc_status /=0) stop "Not enough memory"

  call set_bounds(coords, nc_x, nc_y, size_x, size_y, u_in)
  call set_bounds(coords, nc_x, nc_y, size_x, size_y, u_out)
 
  
  prec = 1e-4
  error = 1e10 
  do i=0, iter_max 
      
      call heat(hx, hy, dt, size_x, size_y, u_in, u_out, error_loc)
      call MPI_Allreduce(error_loc, error, 1, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr) 
      error = sqrt(error)
     
      if (verbose .and. mod(i,10) == 0) print * , 'it =', i, 't = ', i*dt, 'err = ', error 
      
      u_in = u_out
      call ghosts_swap(comm2D, type_row, neighbour, size_x, size_y, u_in)
      if (error <= prec) exit 
  
  end do 

  ! We gather the solution on process 0
  
  if (verbose) then
    allocate(vec_temp(1:nx * ny))
  end if
  allocate(u_vec(1:cell_x * cell_y))
  u_vec = reshape(u_in(2:size_x - 1, 2:size_y - 1),(/cell_x * cell_y/)) 
  call MPI_Gather(u_vec , cell_x * cell_y , MPI_DOUBLE_PRECISION, &
                  vec_temp, cell_x * cell_y, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)

  if (verbose) then
    allocate(solution(1:nx,1:ny))
    solution = reshape(vec_temp, (/nx,ny/))
    do r=1,size_w 
        call MPI_Cart_coords(comm2D, r-1, ndims, coo, ierr)
        solution(coo(1) * cell_x + 1:(coo(1)+1) * cell_x, &
                 coo(2) * cell_y + 1:(coo(2)+1) * cell_y) = & 
        reshape(vec_temp(cell_x * cell_y * (r - 1) + 1: cell_x * cell_y * r),(/cell_x,cell_y/))
    end do 
    !do i=1,nx
    !    print *, (real(solution(i,j)), j=1,ny)
    !enddo
    deallocate(vec_temp)
    deallocate(solution)
  end if

  deallocate(u_vec)
  deallocate(u_in)
  deallocate(u_out)
  
  call MPI_Type_free(type_row, ierr)
  call MPI_Finalize(ierr)

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
  print *, 'Usage: mpirun -np (px*py) ', TRIM(name), ' nx ny iter_max px py'
  print *, '    nx       number of discretisation points in X'
  print *, '    ny       number of discretisation points in Y'
  print *, '    iter_max maximal number of iterations in temporal loop'
  print *, '    px       X process number'
  print *, '    py       Y process number'
  stop
end subroutine usage


subroutine ghosts_swap(comm, type_row, neighbour, size_x, size_y, u) 
  
  integer, intent(in) :: size_x, size_y, comm, type_row 
  integer, dimension(4), intent(in) :: neighbour
  double precision, dimension(1:size_x, 1:size_y), intent(inout) :: u
  integer, parameter ::  N  = 1, S = 2, E = 3, W = 4
  integer :: ierr, s_tag, r_tag
  integer, dimension(MPI_STATUS_SIZE) :: stat
  
  ! N --> S 
  !  N block last significant row goes to S block first ghost row  
  s_tag =0; r_tag = 0
  call MPI_Sendrecv(u(size_x - 1, 2),  1, type_row, neighbour(S), s_tag, &    
  &                 u(1, 2) , 1, type_row, neighbour(N), r_tag, comm, stat, ierr)
  
  ! S --> N 
  ! S block first significant row  goes to N block last ghost row 
  s_tag =1; r_tag = 1
  call MPI_Sendrecv(u(2, 2),  1, type_row, neighbour(N), s_tag, &
  &                 u(size_x , 2), 1, type_row, neighbour(S), r_tag, comm, stat, ierr)
  
  ! W --> E 
  ! W block last significant column goes to E block first ghost column
  s_tag =2; r_tag = 2
  call MPI_Sendrecv(u(1, size_y - 1), size_x, MPI_DOUBLE_PRECISION, neighbour(E), s_tag,&
  &                 u(1, 1) , size_x, MPI_DOUBLE_PRECISION, neighbour(W), r_tag, comm, stat, ierr)
  
  !  E --> W 
  !  E block first significant column goes to W block last ghost column
  s_tag =3; r_tag = 3
  call MPI_Sendrecv(u(1, 2), size_x , MPI_DOUBLE_PRECISION, neighbour(W), s_tag, &
  &                 u(1, size_y) , size_x, MPI_DOUBLE_PRECISION, neighbour(E), r_tag, comm, stat, ierr)

end subroutine ghosts_swap

subroutine set_bounds(coo, nc_x, nc_y, size_x, size_y, u) 

  implicit none
  integer, intent(in) :: size_x, size_y, nc_x, nc_y 
  integer, dimension(2), intent(in) :: coo 
  double precision, dimension(1:size_x, 1:size_y), intent(out) :: u
  u = 0.d0
  if (coo(1) == 0)          u(1,      :) = 1.d0
  if (coo(1) == (nc_x - 1)) u(size_x, :) = 1.d0

  if (coo(2) == 0)          u(:,      1) = 1.d0
  if (coo(2) == (nc_y - 1)) u(:, size_y) = 1.d0

end subroutine set_bounds

end program
