module solver
  implicit none
contains

  function solve(A, B) result(x)
    !! Solve matrix equation Ax = B for x given A and B
    real, intent(in) :: A(:, :), B(:)
    real :: x(size(A, 2))

    call solve_lss(A, B, x)
  end function

  subroutine solve_lss(A, B, x)
    !! Solve matrix equation Ax = B for x given A and B
    real :: A(:, :), B(:)
    real :: A_(size(A, 1), size(A, 2))
    real :: B_(size(B, 1))
    real :: x(size(A, 2))
    real :: s(min(size(A, 1), size(A, 2)))
    real :: work(6 * max(size(A, 1), size(A, 2)))
    real :: rcond
    integer :: m, n, rank, stat

    m = size(A, 1)
    n = size(A, 2)
    A_ = A
    B_ = B
    call dgelss(m, n, 1, A_, m, B_, m, s, rcond, rank, work, size(work), stat)
    x = B_(1:size(x))
    if (stat /= 0) call abort
  end subroutine

end module solver
