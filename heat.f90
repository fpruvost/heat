!-------------------------------------------------------------------------------
!> @file      heat.f90
!> @author    Inria SED Bordeaux
!> @brief     Heat computation iteration code
!>
!> @details   This file declares the heat() function.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> Do a single iteration of the heat equation iterative computation on
!> a 2-D cartesian map.
!>
!> For each cell in @e u_in that is in <code>[1..size_x]x[1..size_y]</code>,
!> it put in @e u_out the result of one iteration computation using a
!> cross-stencil.
!> The complete equation can be found in the @e README file of the project.
!>
!> @param hx precision of the derivation over x
!> @param hy precision of the derivation over y
!> @param dt precision of the derivation over time
!> @param size_x the size of the cartesian map in x
!> @param size_y the size of the cartesion map in y
!> @param u_in the input map
!> @param u_out the output map
!> @return the square of the quadratic differences between @e u_in and @e u_out after
!>         the execution of the heat function
!-------------------------------------------------------------------------------
subroutine heat(hx, hy, dt, size_x, size_y, u_in,  u_out, error)

    implicit none
    double precision hx, hy, dt
    integer size_x, size_y, i, j
    double precision, dimension(1:size_x, 1:size_y) :: u_in, u_out
    double precision w_x, w_y, error, d

    w_x =  dt / (hx * hx)
    w_y =  dt / (hy * hy)
    d = 1.d0 - 2.d0 * w_x - 2.d0 * w_y 

    error = 0.d0
    do i=2, size_x - 1
       do j = 2, size_y - 1
            u_out(i,j) = u_in(i,j) * d + &
                        (u_in(i - 1, j) + u_in(i + 1, j)) * w_x + & 
                        (u_in(i, j - 1) + u_in(i, j + 1)) * w_y
            error = error + (u_out(i,j) - u_in(i, j))**2
       end do 
    end do   

end subroutine heat

