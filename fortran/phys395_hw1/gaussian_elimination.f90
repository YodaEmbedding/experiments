program gaussian_elimination
  implicit none

  real, dimension(3, 3) :: A
  real, dimension(3) :: y, x, x_expected

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
  x_expected = (/ -4., -5., 2. /)

  print "(a)", "A:"
  print "(3f5.0)", A
  print *

  print "(a)", "y:"
  print "(3f5.0)", y
  print *

  x = solve(A, y)

  print "(a)", "Answer:"
  print "(3f6.1)", x
  print *

  print "(a)", "Expected answer:"
  print "(3f6.1)", x_expected
  print *

contains

  ! TODO singular, underdetermined systems (might require row, col pivots)
  ! TODO deal with arbitrary dimension matrix input
  ! TODO pivot_column
  !! Convert to upper triangular form
  pure function upper_triangular(mat) result(A)
    real, dimension(:, :), intent(in) :: mat
    real, dimension(size(mat, 1), size(mat, 2)) :: A
    real, dimension(size(mat, 1)) :: tmp_row
    real :: val, max_val, scaling_factor
    integer :: i, row, pivot_row

    A = mat

    do row = 1, size(mat, 2)
      ! TODO extract into separate function?
      ! Locate pivot via argmax . abs
      max_val = 0
      do i = row, size(mat, 2)
        val = abs(A(row, i))
        if (val > max_val) then
          max_val = val
          pivot_row = i
        endif
      enddo

      ! Swap rows
      tmp_row = A(:, row)
      A(:, row) = A(:, pivot_row)
      A(:, pivot_row) = tmp_row

      ! TODO assert first element is non-zero... otherwise, just skip?
      ! TODO optimizations like (row:, row)

      ! Normalize row
      scaling_factor = A(row, row)
      A(:, row) = A(:, row) / scaling_factor

      do i = row + 1, size(mat, 2)
        scaling_factor = A(row, i)
        A(:, i) = A(:, i) - scaling_factor * A(:, row)
      enddo
    enddo
  end function

  !! Convert upper-triangular form to reduced row-echelon form
  pure function substitution(mat) result(A)
    real, dimension(:, :), intent(in) :: mat
    real, dimension(size(mat, 1), size(mat, 2)) :: A
    real :: scaling_factor
    integer :: i, j

    A = mat

    ! TODO j seems to depend on size(mat, 2) rather than size(mat, 1)...?
    ! errr actually, no it doesn't. What is j anyways? Name it properly.
    do i = size(mat, 2), 1, -1
      do j = i + 1, size(mat, 2)
        ! TODO a lot of redundant computation
        scaling_factor = A(j, i)
        A(:, i) = A(:, i) - scaling_factor * A(:, j)
      enddo
    enddo
  end function

  ! TODO pure
  !! Reduced row-echelon form
  function rref(mat) result(A)
    real, dimension(:, :), intent(in) :: mat
    real, dimension(size(mat, 1), size(mat, 2)) :: A

    A = mat

    print "(a, /)", "Convert to Upper Triangular"
    A = upper_triangular(A)
    print "(4f6.1)", A
    print *

    print "(a, /)", "Substitution"
    A = substitution(A)
    print "(4f6.1)", A
    print *
  end function

  ! TODO pure
  !! Solve the matrix equation Ax = y using Gauss-Jordan elimination
  !! Arguments:
  !!   A must be a non-singular matrix of size NxN
  !!   y must be a vector of size N
  function solve(A, y) result(x)
    real, dimension(:, :), intent(in) :: A
    real, dimension(:), intent(in) :: y
    real, dimension(size(A, 1) + 1, size(A, 2)) :: Ay
    real, dimension(size(y)) :: x

    ! Augment
    Ay(1:size(A, 1), :) = A
    Ay(size(Ay, 1), :) = y

    print "(a)", "Augmented [A | y]: "
    print "(4f6.1)", Ay
    print *

    Ay = rref(Ay)
    x = Ay(size(Ay, 1), :)
  end function
end program gaussian_elimination

! TODO real vs double
! TODO makefile, "compilation instructions" on top
! TODO size N constant parameters; type/compile-time checking of sizes
! TODO function for printing variable size matrices...
