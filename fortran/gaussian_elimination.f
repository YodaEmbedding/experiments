      program gaussian_elimination
        integer :: i
        integer :: row, pivot_row
        real :: val, max_val, scaling_factor
        real, dimension(3, 3) :: A
        real, dimension(4, 3) :: Ay
        real, dimension(3) :: y
        real, dimension(4) :: tmp_row

        ! A(:, 1) = (/ 1,  3,  1 /)
        ! A(:, 2) = (/ 1,  1, -1 /)
        ! A(:, 3) = (/ 3, 11,  5 /)

        ! A(:, 1) = (/ 3, 1, 1 /)
        ! A(:, 2) = (/ 1, 2, 5 /)
        ! A(:, 3) = (/ 2, 5, -1 /)
        ! y = (/ 6, -4, 27 /)

        A(:, 1) = (/ 0, 2, 1 /)
        A(:, 2) = (/ 1, -2, -3 /)
        A(:, 3) = (/ -1, 1, 2 /)
        y = (/ -8, 0, 3 /)

        print "(a)", "A:"
        print "(3f5.0)", A
        print *

        print "(a)", "y:"
        print "(3f5.0)", y
        print *

        ! Augment
        Ay(1:3, :) = A
        Ay(4, :) = y

        print "(a)", "Ay: "
        print "(4f6.1)", Ay
        print *

        ! todo MxN matrices
        ! todo put inside function
        ! todo underdetermined systems (might require row, col pivots)

        print "(a, /)", "Convert to Upper Triangular"

        ! Convert to upper triangular form
        do row = 1, 3  ! while?
          ! Locate pivot via argmax . abs
          max_val = 0
          do i = row, 3
            val = abs(Ay(row, i))
            if (val > max_val) then
              max_val = val
              pivot_row = i
            endif
          enddo

          ! Swap rows
          tmp_row = Ay(:, row)
          Ay(:, row) = Ay(:, pivot_row)
          Ay(:, pivot_row) = tmp_row

          ! TODO assert first element is non-zero... otherwise, just skip?
          ! TODO optimizations like (row:, row)

          ! Normalize row
          scaling_factor = Ay(row, row)
          Ay(:, row) = Ay(:, row) / scaling_factor

          do i = row + 1, 3
            scaling_factor = Ay(row, i)
            Ay(:, i) = Ay(:, i) - scaling_factor * Ay(:, row)
          enddo

          print "(4f6.1)", Ay
          print *
        enddo

        print "(a, /)", "Substitution"

        do i = 3, 1, -1
          do j = i + 1, 3
            ! TODO a lot of redundant computation
            scaling_factor = Ay(j, i)
            Ay(:, i) = Ay(:, i) - scaling_factor * Ay(:, j)
          enddo

          print "(4f6.1)", Ay
          print *
        enddo

        print "(a)", "Answer:"
        print "(3f6.1)", Ay(4, :)
        print *

        print "(a)", "Expected answer:"
        print "(3f6.1)", (/ -4., -5., 2. /)
        print *
      end program gaussian_elimination

! todo check that endif works in Fortran 77
! todo check that enddo works in Fortran 77
