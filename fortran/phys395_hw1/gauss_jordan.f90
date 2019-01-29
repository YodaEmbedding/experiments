module gauss_jordan
  implicit none

contains

  !! Convert to upper triangular form
  pure function upper_triangular(mat) result(A_)
    real, dimension(:, :), intent(in) :: mat
    real, dimension(size(mat, 1), size(mat, 2)) :: A, A_
    integer, dimension(size(mat, 2)) :: r  ! Row permutation/index set
    real :: scaling_factor
    integer :: i, m, n, col, row, row_

    m = size(mat, 2)
    n = size(mat, 1)
    A = mat

    row = 1
    col = 1
    r = (/(i, i=1,m)/)

    do while (row <= m .and. col <= n)
      ! Find row with highest magnitude in the current column
      row_ = maxloc(abs(A(col, r(row:))), dim=1) - 1 + row

      if (A(col, r(row_)) == 0.0) then
        col = col + 1
        cycle
      endif

      ! Swap rows
      r([row, row_]) = r([row_, row])

      ! Normalize row
      scaling_factor = A(col, r(row))
      A(col:, r(row)) = A(col:, r(row)) / scaling_factor

      ! Zero the pivot column for remaining rows
      ! by subtracting off a scaled version of the current row
      forall (i=row+1:m) A(col+1:, r(i)) = A(col+1:, r(i)) &
          - A(col, r(i)) * A(col+1:, r(row))
      A(col, r(row+1:)) = 0.0

      row = row + 1
      col = col + 1
    enddo

    ! Perform reordering
    forall (i=1:m) A_(:, i) = A(:, r(i))
  end function

  !! Convert upper-triangular form to reduced row-echelon form
  pure function substitution(mat) result(A)
    real, dimension(:, :), intent(in) :: mat
    real, dimension(size(mat, 1), size(mat, 2)) :: A
    integer :: i, j, m

    m = size(mat, 2)
    A = mat

    do i = m - 1, 1, -1
      do j = i + 1, m
        A(:, i) = A(:, i) - A(j, i) * A(:, j)
      enddo
    enddo
  end function

  !! Reduced row-echelon form via Gauss-Jordan elimination
  pure function rref(mat) result(A)
    real, dimension(:, :), intent(in) :: mat
    real, dimension(size(mat, 1), size(mat, 2)) :: A

    A = mat
    A = upper_triangular(A)
    A = substitution(A)
  end function

  !! Solve the matrix equation Ax = y using Gauss-Jordan elimination
  !! Arguments:
  !!   A must be a non-singular matrix of size NxN
  !!   y must be a vector of size N
  pure function solve(A, y) result(x)
    real, dimension(:, :), intent(in) :: A
    real, dimension(:), intent(in) :: y
    real, dimension(size(A, 1) + 1, size(A, 2)) :: Ay
    real, dimension(size(y)) :: x

    ! Transpose (for row-memory locality) and augment
    Ay(1:size(A, 1), :) = transpose(A)
    Ay(size(Ay, 1), :) = y

    Ay = rref(Ay)
    x = Ay(size(Ay, 1), :)
  end function

end module gauss_jordan

! TODO size N constant parameters; type/compile-time checking of sizes
