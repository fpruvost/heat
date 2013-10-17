subroutine print_mat(size_x, size_y, u)
   implicit none
   integer, intent(in) :: size_x, size_y
   integer :: i,j
   double precision, dimension(1:size_x, 1:size_y) :: u
   write(6, *), '[';
   do i=1, size_x
     do j=1, size_y 
        write(6, '(e15.5)',  advance='no') u(i,j)
     end do
     print *
   end do
   write(6, *), ']';
   print *
   close(12)
end subroutine print_mat   

!-------------------------------------------------------------------------------
!> A matrix saving function
!> 
!> It saves in a file the matrix given in the parameter @e u.
!> For instance if <code>u = [[1., 2.], [3., 4.]]</code>, then
!> save_mat("toto",2,2,u) will output in the file toto:
!> @code
!> 1.000000000000000e+00  2.000000000000000e+00
!> 3.000000000000000e+00  4.000000000000000e+00  
!> @endcode
!>
!> @param filename the file to output the result
!> @param size_x the size in X of the matrix u
!> @param size_y the size in Y of the matrix u
!> @param u the matrix to print
!-------------------------------------------------------------------------------
subroutine save_mat(filename, size_x, size_y, u)
   implicit none
   character(len=*), intent(in) :: filename
   integer, intent(in) :: size_x, size_y
   integer :: i,j
   double precision, dimension(1:size_x, 1:size_y) :: u
   
   open(unit=12,file=trim(filename),form='formatted')
   do i=1, size_y
     do j=1, size_y 
        write(12, '(e15.5)',  advance='no') u(i,j)
     end do
     write(12, '(/)') 
   end do
   close(12)
end subroutine save_mat   
