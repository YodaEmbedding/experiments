! To compile and run, simply: make
! Now look at the output plots *.png files
! And the terminal output giving a table of maximal error values

program orthogonal_polynomials
  use gauss_jordan
  implicit none

  integer, parameter :: samples = 2001

  call write_csv_chebyshevT("results_f_uniform.csv",  samples, .false., .false.)
  call write_csv_chebyshevT("results_df_uniform.csv", samples, .true.,  .false.)
  call write_csv_chebyshevT("results_f_zeros.csv",    samples, .false., .true.)
  call write_csv_chebyshevT("results_df_zeros.csv",   samples, .true.,  .true.)

contains

  !! Write a plottable csv file
  !! Args:
  !!  is_dv: f'(x) if true, f(x) if false
  !!  is_zeros: x on zeros of Tn(x) if true, x is uniformly spaced if false
  subroutine write_csv_chebyshevT(filename, num_samples, is_dv, is_zeros)
    character(len=*), intent(in) :: filename
    integer, intent(in) :: num_samples
    logical, intent(in) :: is_dv
    logical, intent(in) :: is_zeros
    integer, parameter :: fh = 1
    character(len=256), parameter :: f_header = &
      "$x$, $f(x)$, $f_{10}(x)$, $f_{100}(x)$"
    character(len=256), parameter :: df_header = &
      "$x$, $\frac{d}{dx}f(x)$, $\frac{d}{dx}f_{10}(x)$, $\frac{d}{dx}f_{100}(x)$"

    open(unit=fh, file=filename, action="write", status="replace")

    if (.not. is_dv) then
      write(fh, "(a)") f_header
    else
      write(fh, "(a)") df_header
    endif

    call write_table_chebyshevT(fh, num_samples, is_dv, is_zeros)
    close(fh)
  end subroutine

  !! Write csv table
  subroutine write_table_chebyshevT(fh, num_samples, is_dv, is_zeros)
    integer, intent(in) :: fh
    integer, intent(in) :: num_samples
    logical, intent(in) :: is_dv     ! spaghetti... I wish Fortran were dynamic
    logical, intent(in) :: is_zeros  ! spaghetti... I wish Fortran were dynamic
    real, dimension(10) :: x10, c10
    real, dimension(100) :: x100, c100
    real, dimension(num_samples) :: x, f_x, f10_x, f100_x
    real, parameter :: pi = 3.14159265358979323846264338327950288419716939937510
    integer :: i

    if (.not. is_zeros) then
      ! Use uniformly spaced
      x10  = linspace(-1.0, 1.0, size(x10))
      x100 = linspace(-1.0, 1.0, size(x100))
    else
      ! Use zeros
      x10  = (/(cos(pi * (i - 0.5) / size(x10)),  i=1,size(x10))/)
      x100 = (/(cos(pi * (i - 0.5) / size(x100)), i=1,size(x100))/)
    endif

    ! Calculate coefficients for 10 and 100 terms
    c10  = chebyshevT_coeffs(x10,  f(x10),  size(x10))
    c100 = chebyshevT_coeffs(x100, f(x100), size(x100))

    ! Calculate table values
    x = linspace(-1.0, 1.0, num_samples)

    if (.not. is_dv) then
      f_x = f(x)
      forall (i=1:num_samples) f10_x(i)  = chebyshevT_poly(c10,  x(i))
      forall (i=1:num_samples) f100_x(i) = chebyshevT_poly(c100, x(i))
    else
      f_x = fdv(x)
      forall (i=1:num_samples) f10_x(i)  = chebyshevTdv_poly(c10,  x(i))
      forall (i=1:num_samples) f100_x(i) = chebyshevTdv_poly(c100, x(i))
    endif

    ! Write table
    do i = 1, num_samples
      write(fh, *) &
        x(i), ", ", f_x(i), ", ", f10_x(i), ", ", f100_x(i)
    end do
  end subroutine

  !! Compute best coefficients of chebyshevT polynomial of order n-1
  pure function chebyshevT_coeffs(x, f_x, n)
    real, dimension(:), intent(in) :: x
    real, dimension(:), intent(in) :: f_x
    integer, intent(in) :: n
    real, dimension(n) :: chebyshevT_coeffs
    real, dimension(size(x), n) :: B_x
    real, dimension(n, size(x)) :: B_x_T
    integer :: i

    forall (i=1:n) B_x(:, i) = chebyshevT(i - 1, x)
    B_x_T = transpose(B_x)

    ! Naively, one can execute:
    ! chebyshevT_coeffs = solve(B_x, f_x)
    ! However, this does not work when m != n.
    ! Instead, one can use the linear least squares idea of multiplying by
    ! the transpose of B_x first in order to properly match the dimensions.
    chebyshevT_coeffs = solve(matmul(B_x_T, B_x), matmul(B_x_T, f_x))
  end function

  !! Compute best coefficients of chebyshevT polynomial of order n-1
  pure function chebyshevTdv_coeffs(x, f_x, n)
    real, dimension(:), intent(in) :: x
    real, dimension(:), intent(in) :: f_x
    integer, intent(in) :: n
    real, dimension(n) :: chebyshevTdv_coeffs
    real, dimension(size(x), n) :: B_x
    real, dimension(n, size(x)) :: B_x_T
    integer :: i

    forall (i=1:n) B_x(:, i) = chebyshevTdv(i - 1, x)
    B_x_T = transpose(B_x)

    ! Naively, one can execute:
    ! chebyshevTdv_coeffs = solve(B_x, f_x)
    ! However, this does not work when m != n.
    ! Instead, one can use the linear least squares idea of multiplying by
    ! the transpose of B_x first in order to properly match the dimensions.
    chebyshevTdv_coeffs = solve(matmul(B_x_T, B_x), matmul(B_x_T, f_x))
  end function

  !! Chebyshev polynomial with coefficients c evaluated at x
  pure function chebyshevT_poly(c, x)
    real, dimension(:), intent(in) :: c
    real, intent(in) :: x
    real :: chebyshevT_poly
    integer :: i

    chebyshevT_poly = sum(c * chebyshevT((/(i, i=0,size(c)-1)/), x))
  end function

  !! Chebyshev derivative polynomial with coefficients c evaluated at x
  pure function chebyshevTdv_poly(c, x)
    real, dimension(:), intent(in) :: c
    real, intent(in) :: x
    real :: chebyshevTdv_poly
    integer :: i

    chebyshevTdv_poly = sum(c * chebyshevTdv((/(i, i=0,size(c)-1)/), x))
  end function

  !! Chebyshev basis function of order n evaluated at x
  elemental function chebyshevT(n, x)
      real, intent(in) :: x
      integer, intent(in) :: n
      real :: chebyshevT

      chebyshevT = cos(n * acos(x))
  end function

  !! Chebyshev derivative of basis function of order n evaluated at x
  elemental function chebyshevTdv(n, x)
      real, intent(in) :: x
      integer, intent(in) :: n
      real :: chebyshevTdv

      chebyshevTdv = n * sin(n * acos(x)) / sqrt(1 - x**2)
  end function

  !! Function under test
  elemental function f(x)
    real, intent(in) :: x
    real :: f

    f = (1 + 10 * x**2)**(-1)
  end function

  !! Derivative of function under test
  elemental function fdv(x)
    real, intent(in) :: x
    real :: fdv

    fdv = -20 * x / (10 * x**2 + 1)**2
  end function

  !! Return n evenly spaced points within interval [a, b]
  pure function linspace(a, b, n)
    real, intent(in) :: a, b
    integer, intent(in) :: n
    real, dimension(n) :: linspace
    integer :: i

    linspace = (/(a + (b-a) * i / (n-1), i=0,n-1)/)
  end function

end program orthogonal_polynomials
