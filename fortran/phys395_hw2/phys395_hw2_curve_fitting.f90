! Compile and run: make

program phys395_hw2_curve_fitting
  use gauss_jordan
  implicit none

  integer, parameter :: ifh = 1, ofh = 2
  integer, parameter :: lines = 9130  ! TODO: allow arbitrary lengths
  character(len=*), parameter :: filename = "results_.csv"
  integer ::  i, stat
  real :: x, y
  real, dimension(lines) :: xs, ys, ys_fit3, ys_fit7

  open(unit=ifh, file="data.dat", action="read", status="old", &
    access="sequential", form="formatted")

  ! TODO another idea is to just add up/compute as we read through the file...
  do i = 1, lines
    read(ifh, *, iostat=stat) x, y
    xs(i) = x
    ys(i) = y
    if (stat < 0) exit
  end do

  print *
  ys_fit3 = fit(xs, ys, 3, "svd")
  ys_fit7 = fit(xs, ys, 7, "svd")

  open(unit=ofh, file=filename, action="write", status="replace")
  write(ofh, *) "x, y, $f_3(x)$, $f_7(x)$"
  do i=1, lines
    write(ofh, *) xs(i), ", ", ys(i), ", ", ys_fit3(i), ", ", ys_fit7(i)
  end do
  close(ofh)

contains

  function fit(xs, ys, n, solver) result(ys_fit)
    integer :: a, i, j, k, n
    real, dimension(:) :: xs, ys
    real, dimension(n + 1) :: ys_fit, coeffs, p, s
    real, dimension(n + 1, size(xs, 1)) :: bax
    real, dimension(n + 1, n + 1) :: B, U, Vh
    real :: cond_num, chi_actual, chi_expect
    character(len=64) :: fmt_str
    character(len=*) :: solver

    ! Construct B and p to solve the linear equation Bc = p
    forall (a=0:n, i=1:size(bax, 2)) bax(a + 1, i) = basis(a, xs(i))
    forall (j=1:n+1, k=1:n+1) B(j, k) = sum(bax(j, :) * bax(k, :))
    p = matmul(bax, ys)

    call svd(n + 1, B, U, s, Vh)

    select case (solver)
    case ("gj");  coeffs = solve(B, p)
    case ("svd"); coeffs = solve_svd(n + 1, p, U, s, Vh, 1.0e-6)
    case ("lss"); coeffs = solve_lss(n + 1, B, p, -1.0)
    end select

    ys_fit = matmul(transpose(bax), coeffs)
    chi_actual = sum((ys - ys_fit)**2)
    chi_expect = size(xs, 1) - (n + 1)
    cond_num = s(1) / s(n + 1)

    write(fmt_str, "(a, i10, a)") "(", n + 1, "f7.2)"
    print "(a, a)",  "solver = ", solver
    print "(a, i1)", "n      = ", n
    print *
    ! print "(a)", "B"
    ! print fmt_str, B
    ! print "(a)", "U"
    ! print fmt_str, U
    ! print "(a)", "s"
    ! print fmt_str, s
    ! print "(a)", "Vh"
    ! print fmt_str, Vh
    ! print "(a)", "p"
    ! print fmt_str, p
    print "(a)", "coeffs"
    print fmt_str, coeffs
    print *
    print "(a21, f10.2)", "Condition number: ",    cond_num
    print "(a21, f10.2)", "Chi-square (actual): ", chi_actual
    print "(a21, f10.2)", "Chi-square (expect): ", chi_expect
    print "(/, a, /)", "----"
  end function

  !! minimize |A.x - B|^2 using LAPACK canned SVD routine
  !! A gets destroyed, the answer is returned in B
  !! rcond determines the effective rank of A as described in LAPACK docs.
  function solve_lss(n, A, B, rcond) result(B_)
    integer :: n
    real :: A(n, n), A_(n, n), B(n), B_(n), s(n), work(6 * n), rcond
    integer :: rank, stat

    A_ = A
    B_ = B
    call dgelss(n, n, 1, A_, n, B_, n, s, rcond, rank, work, 6 * n, stat)
    if (stat /= 0) call abort
  end function

  !! TODO
  function solve_svd(n, b, U, s, Vh, eps) result(x)
    real :: b(n), s(n), U(n, n), Vh(n, n), x(n), eps
    integer :: n

    x = matmul(transpose(U), b)
    where (s > eps * s(1)); x = x / s
    elsewhere;              x = 0.0
    end where
    x = matmul(transpose(Vh), x)
  end function

  !! TODO
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

  !! Return n evenly spaced points within interval [a, b]
  pure function linspace(a, b, n)
    real, intent(in) :: a, b
    integer, intent(in) :: n
    real, dimension(n) :: linspace
    integer :: i

    linspace = (/(a + (b-a) * i / (n-1), i=0,n-1)/)
  end function

end program phys395_hw2_curve_fitting

! TODO rename xs, ys; x_val, y_val?
! TODO " (Hint: Your matrix A will have different dimensions.)"
