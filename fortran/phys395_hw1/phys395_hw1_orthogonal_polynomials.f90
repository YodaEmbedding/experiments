program orthogonal_polynomials
  use gauss_jordan
  implicit none

  integer, parameter :: samples = 201   ! Number of samples plotted
  integer, parameter :: fh = 1          ! File handle
  integer, parameter :: m = 10          ! Size of sampled point set
  integer, parameter :: n = 10          ! Size of basis set
  real, dimension(m) :: x               ! x_i
  real, dimension(n) :: c               ! Coefficients
  integer :: i

  open(unit=fh, file="results.csv", action="write", status="replace")
  write(fh, "(a)") "$x$, $f(x)$, $f_{10}(x)$, $f_{100}(x)$"
  call write_csv_chebyshevT(fh, samples)
  close(fh)

  ! x = linspace
  ! write_csv "x, f(x), f_10(x), f_100(x)"
  ! write_csv "x, g(x), g_10(x), g_100(x)"

  ! x = (\(cos(pi * (i - 0.5) / n), i=1,n)\)  ! TODO should this be m or n? I guess it doesn't matter if m = n
  ! write_csv "x, f(x), f_10(x), f_100(x)"
  ! write_csv "x, g(x), g_10(x), g_100(x)"

  ! TODO n = 10
  ! TODO n = 100
  ! TODO d/dx
  ! TODO errors (maybe do this inside python?)

contains

  ! TODO accept function pointer on vector function
  !! Write a plottable csv of f(x) and chebyshevT using given coefficients
  subroutine write_csv_chebyshevT(fh, num_samples)
    integer, intent(in) :: num_samples
    integer, intent(in) :: fh
    real, dimension(10) :: x10, c10
    real, dimension(100) :: x100, c100
    real, dimension(num_samples) :: x, f_x, f10_x, f100_x
    integer :: i

    ! Calculate coefficients for 10 and 100 terms
    x10  = linspace(-1.0, 1.0, size(x10))
    x100 = linspace(-1.0, 1.0, size(x100))
    c10  = chebyshevT_coeffs(x10,  f(x10),  size(x10))
    c100 = chebyshevT_coeffs(x100, f(x100), size(x100))

    ! Calculate table values
    x = linspace(-1.0, 1.0, num_samples)
    f_x = f(x)
    forall (i=1:num_samples) f10_x(i)  = chebyshevT_poly(c10,  x(i))
    forall (i=1:num_samples) f100_x(i) = chebyshevT_poly(c100, x(i))

    ! Write table
    do i = 1, num_samples
      write(fh, "(f8.4, a, f8.4, a, f8.4, a, f8.4)") &
        x(i), ", ", f_x(i), ", ", f10_x(i), ", ", f100_x(i)
    end do
  end subroutine

  ! TODO accept f instead of f_x?
  !! Compute best coefficients of chebyshevT polynomial of order n-1
  pure function chebyshevT_coeffs(x, f_x, n)
    real, dimension(:), intent(in) :: x
    real, dimension(:), intent(in) :: f_x
    integer, intent(in) :: n
    real, dimension(n) :: chebyshevT_coeffs
    real, dimension(size(x), n) :: B_x
    real, dimension(n, size(x)) :: B_x_T

    forall (i=1:n) B_x(:, i) = chebyshevT(i - 1, x)
    B_x_T = transpose(B_x)

    ! Naively, one can execute:
    ! chebyshevT_coeffs = solve(B_x, f_x)
    ! However, this does not work when m != n.
    ! Instead, one can use the linear least squares idea of multiplying by
    ! the transpose of B_x first in order to properly match the dimensions.
    chebyshevT_coeffs = solve(matmul(B_x_T, B_x), matmul(B_x_T, f_x))
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
