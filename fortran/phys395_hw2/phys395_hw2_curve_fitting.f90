! Compile and run: make

program phys395_hw2_curve_fitting
  implicit none

  integer, parameter :: ifh = 1
  integer, parameter :: max_lines = 1048576
  integer :: i, stat
  real, dimension(:), allocatable :: x, y
  real, dimension(max_lines) :: x_, y_

  ! Read data
  open(unit=ifh, file="data.dat", action="read", status="old", &
    access="sequential", form="formatted")
  do i = 1, max_lines
    read(ifh, *, iostat=stat) x_(i), y_(i)
    if (stat < 0) exit
  end do
  close(ifh)

  ! Allocate properly-sized arrays
  i = i - 1
  allocate(x(i), y(i))
  x = x_(1:i)
  y = y_(1:i)

  print *
  call write_csv(x, y, "results_svd.csv", solver="svd")
  call write_csv(x, y, "results_lss.csv", solver="lss")

  print "(a)", "The condition number for the SVD matrix is larger than "
  print "(a)", "the condition number for the Linear Least Squares (LSS) matrix."
  print "(a)", "Otherwise, the best fit parameters appear to be the same."
  print *

  deallocate(x, y)

contains

  !! Write csv for specified solver (svd/lss)
  subroutine write_csv(x, y, filename, solver)
    real, dimension(:) :: x, y
    character(len=*) :: filename, solver
    integer, parameter :: ofh = 2
    integer :: i
    real, dimension(size(x)) :: y_fit3, y_fit7

    y_fit3 = fit(x, y, n=3, solver=solver)
    y_fit7 = fit(x, y, n=7, solver=solver)

    open(unit=ofh, file=filename, action="write", status="replace")
    write(ofh, *) "x, y, $f_3(x)$, $f_7(x)$"
    do i = 1, size(x)
      write(ofh, *) x(i), ", ", y(i), ", ", y_fit3(i), ", ", y_fit7(i)
    end do
    close(ofh)
  end subroutine

  !! Fit data to n+1-dimensional basis via specified solver
  function fit(x, y, n, solver) result(ys_fit)
    integer :: a, i, j, k, n
    real, dimension(:) :: x, y
    real, dimension(size(x)) :: ys_fit
    real, dimension(n + 1) :: coeffs, p, s
    real, dimension(size(x), n + 1) :: bxa
    real, dimension(n + 1, n + 1) :: B, U, Vh
    real :: cond_num, chi_actual, chi_expect
    character(len=64) :: fmt_str
    character(len=*) :: solver

    ! Construct bxa to minimize matrix equation | y - bxa x |^2
    ! Construct B and p to solve the matrix equation Bc = p
    forall (a=0:n, i=1:size(bxa, 1)) bxa(i, a + 1) = basis(a, x(i))
    forall (j=1:n+1, k=1:n+1) B(j, k) = sum(bxa(:, j) * bxa(:, k))
    p = matmul(transpose(bxa), y)

    call svd(n + 1, B, U, s, Vh)

    select case (solver)
    case ("svd"); coeffs = solve_svd(p, U, s, Vh, 1.0e-6)
    case ("lss"); call solve_lss(bxa, y, -1.0, coeffs, s)
    end select

    ys_fit = matmul(bxa, coeffs)
    chi_actual = sum((y - ys_fit)**2)
    chi_expect = size(x) - (n + 1)
    cond_num = s(1) / s(n + 1)

    write(fmt_str, "(a, i10, a)") "(", n + 1, "f7.2)"
    print "(a, a)", "solver = ", solver
    print "(a, i1)", "n = ", n
    print *
    write(*, "(a21)", advance="no") "Best fit params: "
    print fmt_str, coeffs
    print "(a21, f10.2)", "Condition number: ",    cond_num
    print "(a21, f10.2)", "Chi-square (actual): ", chi_actual
    print "(a21, f10.2)", "Chi-square (expect): ", chi_expect
    print "(/, a, /)", "----"
  end function

  !! Solve for x minimizing |Ax - B|^2
  !! Results are returned in x, s
  subroutine solve_lss(A, B, rcond, x, s)
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

  !! Solve the matrix equation (U s Vh) x = b
  function solve_svd(b, U, s, Vh, eps) result(x)
    real :: b(:), s(:), U(:, :), Vh(:, :), x(size(Vh, 2)), eps

    x = matmul(transpose(U), b)
    where (s > eps * s(1)); x = x / s
    elsewhere;              x = 0.0
    end where
    x = matmul(transpose(Vh), x)
  end function

  !! Decompose A into U, s, Vh
  subroutine svd(n, A, U, s, Vh)
    integer :: n, stat
    real :: A(n, n), A_(n, n), U(n, n), Vh(n, n), s(n), work(6 * n)

    A_ = A
    call dgesvd('A', 'A', n, n, A_, n, s, U, n, Vh, n, work, 6 * n, stat)
    if (stat /= 0) call abort
  end subroutine

  !! An even basis... for a more civilized age
  elemental function basis(a, x)
    integer, intent(in) :: a
    real, intent(in) :: x
    real :: basis
    real, parameter :: pi = 3.14159265358979323846264338327950288419716939937510

    basis = cos(2 * pi * a * x)
  end function

end program phys395_hw2_curve_fitting
