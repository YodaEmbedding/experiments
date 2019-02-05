! Compile and run: make

program phys395_hw2_curve_fitting
  implicit none
  integer, parameter :: ifh = 1, ofh = 2
  integer, parameter :: lines = 9130  ! TODO: allow arbitrary lengths
  character(len=*), parameter :: filename = "out.csv"
  integer :: a, i, j, k, stat
  real :: x, y
  real, dimension(lines) :: xs, ys, ys_fit
  integer, parameter :: n = 3
  real, dimension(n + 1, lines) :: bax
  real, dimension(n + 1, n + 1) :: B
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
  ! forall (j=1:n+1) p(j) = sum(bax(j, :) * ys(:))
  p = matmul(bax, ys)

  write(fmt_str, "(a, i10, a)") "(", n+1, "f10.2)"
  print fmt_str, B
  print fmt_str, p

  open(unit=ofh, file=filename, action="write", status="replace")
  write(ofh, "(a)") "testing"
  close(ofh)

contains

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
