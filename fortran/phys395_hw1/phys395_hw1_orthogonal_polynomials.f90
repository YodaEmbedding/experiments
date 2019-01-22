program orthogonal_polynomials
  use gauss_jordan
  implicit none

  integer, parameter :: fh = 1    ! File handle
  integer, parameter :: m = 10    ! Size of sampled point set
  integer, parameter :: n = 10    ! Size of basis set
  real, dimension(m) :: x         ! x_i
  real, dimension(m) :: f_x       ! Actual function evaluated at x_i
  real, dimension(m) :: y_x       ! Approx function evaluated at x_i
  real, dimension(m, n) :: B_x    ! Basis functions evaluated at x_i
  real, dimension(m, n) :: B_x_T  ! Transpose of B_x
  real, dimension(n) :: c         ! Coefficients
  integer :: i
  character(len=32) :: fmt_str

  x = linspace(-1.0, 1.0, m)
  f_x = f(x)
  B_x = chebyshevT_matrix(n, x)
  B_x_T = transpose(B_x)

  ! Naively, one can perform c = solve(B_x, f_x).
  ! However, this does not work when m != n.
  ! Instead, one can use the linear least squares idea of multiplying by
  ! the transpose of B_x first in order to properly match the dimensions.
  c = solve(matmul(B_x_T, B_x), matmul(B_x_T, f_x))

  write (fmt_str, "(a, i5, a)") "(", n, "f8.2)"
  print fmt_str, transpose(B_x)
  print *
  print fmt_str, c

  open(unit=fh, file="results.csv", action="write", status="replace")
  write(fh, "(a)") "x, f(x), f_approx(x)"
  call write_csv_chebyshevT(fh, f, c, 100)
  close(fh)

contains

  !! Write a plottable csv of f(x) and chebyshevT using given coefficients
  subroutine write_csv_chebyshevT(fh, f, coeffs, num_samples)
    abstract interface
      function func (x)
        real :: func
        real, dimension(:), intent (in) :: x
      end function func
    end interface

    integer, intent(in) :: num_samples
    integer, intent(in) :: fh
    real, dimension(:), intent(in) :: coeffs
    real, dimension(num_samples) :: x     ! x_i
    real, dimension(num_samples) :: f_x   ! Actual function evaluated at x_i
    real, dimension(num_samples) :: y_x   ! Approx function evaluated at x_i
    integer :: i
    ! procedure (func), pointer, intent(in) :: f => null ()
    procedure (func), pointer, intent(in) :: f

    x = linspace(-1.0, 1.0, num_samples)
    f_x = f(x)
    forall (i=1:num_samples) y_x(i) = chebyshevT_poly(coeffs, x(i))

    do i = 1, num_samples
      write(fh, "(f8.4, a, f8.4, a, f8.4)") x(i), ", ", f_x(i), ", ", y_x(i)
    end do
  end subroutine

  !! Create the matrix Bij = chebyshevT(j, x_i)
  pure function chebyshevT_matrix(n, x)
    real, dimension(:), intent(in) :: x
    integer, intent(in) :: n
    real, dimension(size(x), n) :: chebyshevT_matrix

    forall (i=1:n) chebyshevT_matrix(:, i) = chebyshevT(i - 1, x)
  end function

  !! Chebyshev polynomial with coefficients c evaluated at x
  pure function chebyshevT_poly(c, x)
    real, dimension(:), intent(in) :: c
    real, intent(in) :: x
    real :: chebyshevT_poly
    integer :: i

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
