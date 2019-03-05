module q5_fit
  implicit none

  interface
    function RV2R(x)
      !! [Real] -> Real
      real, intent(in) :: x(:)
      real :: RV2R
    end function RV2R

    function RV2RV(x)
      !! [Real] -> [Real]
      real, intent(in) :: x(:)
      real :: RV2RV(size(x))
    end function RV2RV
  end interface

contains

  subroutine q5()
    !! Fit coeffs by finding minimum of chi-square via Levenberg-Marquardt
    integer, parameter :: n = 3
    real, dimension(:), allocatable :: x, y, y_fit
    real, dimension(n + 2) :: coeffs
    integer :: i

    call read_data("data.dat", x, y)
    allocate(y_fit(size(x)))

    coeffs = 0.1
    coeffs = gradient_descent(loss, dloss, x0=coeffs)
    forall (i=1:size(x)) y_fit(i) = model(coeffs, x(i))

    print "(a)", "5. Fit of data:"
    print "(5f9.3)", coeffs
    ! print *, iterations  ! TODO
    call write_csv("results_gradient_descent.csv", x, y, y_fit)
    print *

    deallocate(x, y, y_fit)

  contains

    function loss(c)
      real, intent(in) :: c(:)
      real :: loss

      forall (i=1:size(x)) y_fit(i) = model(c, x(i))
      loss = 0.5 * sum((y - y_fit)**2)
    end function

    function dloss(c)
      real, intent(in) :: c(:)
      real :: dloss(size(c))
      real :: df_dc(size(c), size(x))

      forall (i=1:size(x)) y_fit(i) = model(c, x(i))
      forall (i=1:size(x)) df_dc(:, i) = dmodel(c, x(i))
      forall (i=1:size(c)) dloss(i) = sum((y_fit - y) * df_dc(i, :))
    end function

  end subroutine

  subroutine read_data(filename, x, y)
    !! Read raw (x,y) data from .dat file
    character(len=*) :: filename
    real, dimension(:), allocatable :: x, y
    integer, parameter :: ifh = 1
    integer, parameter :: max_lines = 1048576
    integer :: i, stat
    real, dimension(max_lines) :: x_, y_

    ! Read data
    open(unit=ifh, file=filename, action="read", status="old", &
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
  end subroutine

  subroutine write_csv(filename, x, y, y_fit)
    !! Output a csv file with plottable data
    real, dimension(:) :: x, y, y_fit
    character(len=*) :: filename
    integer, parameter :: ofh = 2
    integer :: i

    open(unit=ofh, file=filename, action="write", status="replace")
    write(ofh, *) "x, y, f(x)"
    do i = 1, size(x)
      write(ofh, *) x(i), ", ", y(i), ", ", y_fit(i)
    end do
    close(ofh)
  end subroutine

  function gradient_descent(f, df, x0) result(x)
    real, intent(in) :: x0(:)
    procedure(RV2R) :: f
    procedure(RV2RV) :: df
    real :: x(size(x0))
    real :: step
    integer :: i

    x = x0
    step = 1e-6
    i = 0

    do while (step > 1e-8)
      ! print "('x    ', 5f9.3)", x
      ! print "('df(x)', 5f9.3)", df(x)
      x = x - step * df(x)
      step = step * 0.999
      i = i + 1
    end do

    print "('Fit iterations', i9)", i
  end function

  function levenberg_marquardt(f, x_min, x_max, tol)
    procedure(RV2R) :: f
    real, intent(in) :: x_min, x_max, tol
    real :: levenberg_marquardt

  end function

  ! pure function loss(coeffs)
  !   real :: loss
  !   forall (i=1:size(x)) y_fit(i) = model(coeffs, x(i))
  !   chi_square = sum((y - y_fit)**2)
  !
  !   ! TODO what are x, y, y_fit?
  !   ! TODO what does coeffs have to do with anything?
  ! end function

  elemental function basis(a, x)
    !! An even basis... for a more civilized age
    integer, intent(in) :: a
    real, intent(in) :: x
    real :: basis
    real, parameter :: pi = 3.14159265358979323846264338327950288419716939937510

    basis = cos(2 * pi * a * x)
  end function

  pure function model(coeffs, x)
    !! Non-linear model that we will fit our data to
    real, intent(in) :: coeffs(:), x
    real :: model, const, terms(size(coeffs) - 1)
    integer :: a

    forall (a=1:size(coeffs)-1) terms(a) = coeffs(a) * basis(a - 1, x)
    const = coeffs(size(coeffs))
    model = exp(sum(terms)) + coeffs(size(coeffs))
  end function

  pure function dmodel(coeffs, x)
    !! Derivative of non-linear model with respect to coefficients
    real, intent(in) :: coeffs(:), x
    real :: dmodel(size(coeffs)), factor, terms(size(coeffs) - 1)
    integer :: a

    forall (a=1:size(coeffs)-1) terms(a)  = coeffs(a) * basis(a - 1, x)
    factor = exp(sum(terms))
    forall (a=1:size(coeffs)-1) dmodel(a) = factor    * basis(a - 1, x)
    dmodel(size(coeffs)) = 1.0
  end function

end module q5_fit
