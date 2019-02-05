! Compile and run: make

program phys395_hw2_curve_fitting
  implicit none
  integer, parameter :: ifh = 1, ofh = 2
  integer, parameter :: lines = 9130  ! TODO: allow arbitrary lengths
  character(len=*), parameter :: filename = "results_.csv"
  integer :: a, i, j, k, stat
  real :: x, y
  real, dimension(lines) :: xs, ys, ys_fit
  integer, parameter :: n = 3
  real, dimension(n + 1, lines) :: bax
  real, dimension(n + 1, n + 1) :: B, B_
  real, dimension(n + 1) :: coeffs
  real, dimension(n + 1) :: p
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
  forall (j=1:n+1, k=1:n+1) B(j, k) = sum(bax(j, : ) * bax(k, : ))
  p = matmul(bax, ys)

  B_ = B
  coeffs = p
  call solve(n + 1, B_, coeffs, -1.0)

  write(fmt_str, "(a, i10, a)") "(", n+1, "f10.2)"
  print *, "B"
  print fmt_str, B
  print *, "p"
  print fmt_str, p
  print *, "coeffs"
  print fmt_str, coeffs

  open(unit=ofh, file=filename, action="write", status="replace")
  write(ofh, "(a)") "testing"
  close(ofh)

contains

  ! minimize |A.x - B|^2 using LAPACK canned SVD routine (A gets destroyed, the answer is returned in B)
  ! rcond determines the effective rank of A as described in LAPACK docs. Pass -1.0 for machine precision
  subroutine solve(n, A, B, rcond)
    integer :: n
    real :: A(n, n), B(n), S(n), W(6 * n), rcond
    integer :: rank, stat
    stat = 0 ! TODO removable?
    call dgelss(n, n, 1, A, n, B, n, S, rcond, rank, W, 6 * n, stat)
    if (stat /= 0) call abort
  end subroutine

  !! An even basis... from a more civilized age
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
