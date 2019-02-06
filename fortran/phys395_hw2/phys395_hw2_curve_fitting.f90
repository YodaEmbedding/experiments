! Compile and run: make

program phys395_hw2_curve_fitting
  use gauss_jordan
  implicit none

  integer, parameter :: ifh = 1, ofh = 2
  integer, parameter :: lines = 9130  ! TODO: allow arbitrary lengths
  character(len=*), parameter :: filename = "results_.csv"
  integer :: a, i, j, k, stat
  real :: x, y
  real, dimension(lines) :: xs, ys, ys_fit
  integer, parameter :: n = 3
  real, dimension(n + 1, lines) :: bax
  real, dimension(n + 1, n + 1) :: B, U, Vh
  real, dimension(n + 1) :: coeffs, p, s
  character(len=64) :: fmt_str

  open(unit=ifh, file="data.dat", action="read", status="old", &
    access="sequential", form="formatted")

  ! TODO another idea is to just add up/compute as we read through the file...
  do i = 1, lines
    read(ifh, *, iostat=stat) x, y
    xs(i) = x
    ys(i) = y
    if (stat < 0) exit
  end do

  forall (i=1:lines, a=0:n) bax(a + 1, i) = basis(a, xs(i))
  forall (j=1:n+1, k=1:n+1) B(j, k) = sum(bax(j, :) * bax(k, :))
  p = matmul(bax, ys)

  call svd(n + 1, B, U, s, Vh)

  ! coeffs = solve(B, p)
  ! coeffs = solve_svd(n + 1, p, U, s, Vh, 1.0e-6)
  coeffs = solve_lss(n + 1, B, p, -1.0)

  write(fmt_str, "(a, i10, a)") "(", n + 1, "f10.2)"
  print *, "B"
  print fmt_str, B
  print *, "p"
  print fmt_str, p
  print *, "coeffs"
  print fmt_str, coeffs
  print *, "U"
  print fmt_str, U
  print *, "s"
  print fmt_str, s
  print *, "Vh"
  print fmt_str, Vh

  open(unit=ofh, file=filename, action="write", status="replace")
  write(ofh, "(a)") "testing"
  close(ofh)

contains

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

! TODO ew n + 1 everywhere
! TODO Good fit: chi^2 = number of points - number of params fit
! (Chi-square should be on average 1, since sigma is 1)
