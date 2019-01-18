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

  !! Convert to upper triangular form
  pure function upper_triangular(mat) result(A)
    real, dimension(:, :), intent(in) :: mat
    real, dimension(size(mat, 1), size(mat, 2)) :: A
    real, dimension(size(mat, 1)) :: tmp
    real :: scaling_factor
    integer :: i, m, n, col, row, row_

    m = size(mat, 2)
    n = size(mat, 1)
    A = mat

    row = 1
    col = 1

    do while (row <= m .and. col <= n)
      ! Find row with highest magnitude in the current column
      row_ = maxloc(abs(A(col, row:)), dim=1) - 1 + row

      if (A(row_, col) == 0) then
        col = col + 1
        cycle
      endif

      ! TODO optimize by only swapping (col:, *)
      ! Swap rows
      tmp = A(:, row)
      A(:, row) = A(:, row_)
      A(:, row_) = tmp

      ! Normalize row
      scaling_factor = A(col, row)
      A(col:, row) = A(col:, row) / scaling_factor

      ! Zero the pivot column for remaining rows
      ! by subtracting off a scaled version of the current row
      forall (i=row+1:m) A(col:, i) = A(col:, i) - A(col, i) * A(col:, row)
      A(col, row+1:) = 0.0

      ! TODO optimize via column-major order indexing... but be wary of mutation
      ! row_ = row + 1
      ! forall (i=col:n) A(i, row_:) = A(i, row_:) - A(col, row_:) * A(i, row)
      ! A(col, row_:) = 0.0

      row = row + 1
      col = col + 1
    enddo
  end function

  !! Convert upper-triangular form to reduced row-echelon form
  pure function substitution(mat) result(A)
    real, dimension(:, :), intent(in) :: mat
    real, dimension(size(mat, 1), size(mat, 2)) :: A
    real :: scaling_factor
    integer :: i, j, m

    m = size(mat, 2)
    A = mat

    ! TODO can be optimized via column-major indexing?
    do i = m, 1, -1
      do j = i + 1, m
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

! TODO shouldn't the matrices be... transposed? (stored in column-major order)
! TODO also, notice that transposing a matrix before operating on it might result in speedups; transpose for cache-locality
! TODO real vs double
! TODO makefile, "compilation instructions" on top, -O, -real
! TODO size N constant parameters; type/compile-time checking of sizes
! TODO function for printing variable size matrices...
! TODO optimize by only swapping (col:, *)... but this makes code look ugly
