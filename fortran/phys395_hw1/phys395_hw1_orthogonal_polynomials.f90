program orthogonal_polynomials
  use gauss_jordan
  implicit none

  integer, parameter :: fh = 1
  integer, parameter :: m = 100  ! Size of sampled point set
  integer, parameter :: n = 10  ! Size of basis set
  real, dimension(m) :: x       ! x_i
  real, dimension(m) :: f_x     ! Actual function evaluated at x_i
  real, dimension(m) :: y_x     ! Approximated function evaluated at x_i
  real, dimension(m, n) :: B_x  ! Basis functions evaluated at x_i
  real, dimension(n) :: c       ! Coefficients
  integer :: i, j
  character(len=32) :: fmt_str

  x = linspace(-1.0, 1.0, m)
  f_x = f(x)
  B_x = chebyshevT_matrix(n, x)
  ! c = solve(B_x, f_x)
  c = solve(matmul(transpose(B_x), B_x), matmul(transpose(B_x), f_x))
  ! TODO move least squares into gauss-jordan directly? nah


  write (fmt_str, "(a, i5, a)"), "(", n, "f8.2)"
  print fmt_str, transpose(B_x)
  print *
  print fmt_str, c

  ! forall (i=1:m) y_x(i) = chebyshevT_poly(c, x(i))
  print fmt_str, c
  forall (i=1:m) y_x(i) = chebyshevT_poly(c, x(i))

  ! character(len=32) :: fmt_str
  ! write (fmt_str, "(a,i3,a)") "(f8.4, ", m - 1, "(',', f8.4))"
  ! print *, fmt_str
  ! print "(f8.4)", x
  ! print "(f8.4)", f_x

  open(unit=fh, file="results.csv", action="write", status="replace")
  write(fh, "(a)") "x, f(x), f_approx(x)"

  ! TODO m should be 10, but we plot a higher resolution result by computing it on a different set of x_i values
  ! That is, we should loop through 1, samples or something like that
  do i = 1, m
    write(fh, "(f8.4, a, f8.4, a, f8.4)") x(i), ", ", f_x(i), ", ", y_x(i)
  end do

  close(fh)

contains

  !! TODO docstring
  pure function chebyshevT_matrix(n, x)
    real, dimension(:), intent(in) :: x
    integer, intent(in) :: n
    real, dimension(size(x), n) :: chebyshevT_matrix

    forall (i=1:n) chebyshevT_matrix(:, i) = chebyshevT(i - 1, x)
  end function

  !! TODO docstring
  pure function chebyshevT_poly(c, x)
    real, dimension(:), intent(in) :: c
    real, intent(in) :: x
    real :: chebyshevT_poly
    integer :: i

    ! chebyshevT_poly = sum((/(c(i) * chebyshevT(i - 1, x), i=1,size(c))/))
    chebyshevT_poly = sum(c * chebyshevT((/(i, i=0,size(c)-1)/), x))
  end function

  !! Chebyshev basis function of order n evaluated at x
  elemental function chebyshevT(n, x)
      real, intent(in) :: x
      integer, intent(in) :: n
      real :: chebyshevT

      chebyshevT = cos(n * acos(x))
  end function

  !! Function under test
  elemental function f(x)
    real, intent(in) :: x
    real :: f

    f = (1 + 10 * x**2)**(-1)
  end function

  !! Return n evenly spaced points within interval [a, b]
  pure function linspace(a, b, n)
    real, intent(in) :: a, b
    integer, intent(in) :: n
    real, dimension(n) :: linspace

    linspace = (/(a + (b-a) * i / (n-1), i=0,n-1)/)
  end function

end program orthogonal_polynomials

! TODO All questions; produce results for all of them
! TODO linear least squares for m != n
! TODO docstrings
