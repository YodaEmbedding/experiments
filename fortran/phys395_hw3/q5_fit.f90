module q5_fit
  use solver
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

    pure function pRVR2R(c, x)
      !! [Real], Real -> Real
      real, intent(in) :: c(:), x
      real :: pRVR2R
    end function pRVR2R

    pure function pRVR2RV(c, x)
      !! [Real], Real -> [Real]
      real, intent(in) :: c(:), x
      real :: pRVR2RV(size(c))
    end function pRVR2RV
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

    print "(a)", "5. Data fit:"
    print *

    print "(a)", "   Levenberg-Marquardt:"
    coeffs = 0.0
    coeffs = levenberg_marquardt(x, y, model, dmodel, &
        c0=coeffs, lambda0=0.7, tol=0.1, maxiter=100)
    print "('   Fit params (0..n): ', 4f9.3)", coeffs(1:n+1)
    print "('   Fit params (const):', f9.3)",  coeffs(n+2)
    print "('   Loss:              ', f9.0)", loss(coeffs)
    print *

    forall (i=1:size(x)) y_fit(i) = model(coeffs, x(i))
    call write_csv("results_levenberg_marquardt.csv", x, y, y_fit)

    print "(a)", "   Gradient Descent:"
    coeffs = 0.0
    coeffs = gradient_descent(loss, dloss, c0=coeffs, tol=0.1, maxiter=2000)
    print "('   Fit params (0..n): ', 4f9.3)", coeffs(1:n+1)
    print "('   Fit params (const):', f9.3)",  coeffs(n+2)
    print "('   Loss:              ', f9.0)", loss(coeffs)
    print *

    forall (i=1:size(x)) y_fit(i) = model(coeffs, x(i))
    call write_csv("results_gradient_descent.csv", x, y, y_fit)

    print "(a)", "Evidently, Levenberg-Marquardt requires far fewer iterations"
    print "(a)", "than Gradient Descent. It also results in a better fit,"
    print "(a)", "with less tweaking needed -- GD does not converge for poorly"
    print "(a)", "chosen step sizes and step decay rates."
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

  function gradient_descent(f, df, c0, tol, maxiter) result(c)
    !! Minimize f using df starting at c0
    procedure(RV2R) :: f
    procedure(RV2RV) :: df
    real, intent(in) :: c0(:), tol
    integer, intent(in) :: maxiter
    real :: c(size(c0)), step, tol_new, loss, loss_new
    integer :: iter

    step = 1e-5
    c = c0
    loss = f(c)

    do iter = 1, maxiter
      c = c - step * df(c)
      step = step * 0.995

      loss_new = f(c)
      tol_new = abs(loss_new - loss)
      loss = loss_new

      if (tol_new <= tol) exit
    end do

    print "('   Iterations:        ', i9)", iter - 1
  end function

  function levenberg_marquardt(x, y, f, df, c0, lambda0, tol, maxiter) result(c)
    !! Minimize f w.r.t. chi-square for given data x, y points starting at c0
    procedure(pRVR2R) :: f
    procedure(pRVR2RV) :: df
    real, intent(in) :: x(:), y(:), c0(:), lambda0, tol
    integer, intent(in) :: maxiter
    real, dimension(size(c0)) :: c, c_new, dloss
    real, dimension(size(x)) :: r, r_new
    real, dimension(size(c0), size(c0)) :: g, JTJ
    real :: J(size(x), size(c0))
    real :: JT(size(c0), size(x))
    real :: loss, loss_new, lambda, tol_new
    logical :: invalidated
    integer :: i, iter
    real, parameter :: lambda_up = 1.1, lambda_down = 1 / 1.1

    lambda = lambda0
    c = c0
    forall (i=1:size(x)) r(i) = y(i) - f(c, x(i))
    loss = 0.5 * sum(r**2)
    invalidated = .true.

    do iter = 1, maxiter
      ! Compute J, JT, JTJ, and loss-gradient
      ! but only if coefficients have changed
      if (invalidated) then
        forall (i=1:size(x)) JT(:, i) = df(c, x(i))
        J = transpose(JT)
        JTJ = matmul(JT, J)
        dloss = matmul(JT, r)
        invalidated = .false.
      end if

      ! Compute g
      g = JTJ
      forall (i=1:size(c0)) g(i, i) = (1.0 + lambda) * g(i, i)

      ! Compute new coefficients, new residuals, and new loss
      c_new = c + solve(g, dloss)
      forall (i=1:size(x)) r_new(i) = y(i) - f(c_new, x(i))
      loss_new = 0.5 * sum(r_new**2)
      tol_new = abs(loss_new - loss)

      ! If loss is smaller, accept new coefficients
      if (loss_new < loss) then
        c = c_new
        r = r_new
        loss = loss_new
        lambda = lambda * lambda_down
        invalidated = .true.
      else
        lambda = lambda * lambda_up
      end if

      if (tol_new <= tol) exit
    end do

    print "('   Iterations:        ', i9)", iter - 1
  end function

  elemental function basis(a, x)
    !! An even basis... for a more civilized age
    integer, intent(in) :: a
    real, intent(in) :: x
    real :: basis
    real, parameter :: pi = 3.14159265358979323846264338327950288419716939937510

    basis = cos(2 * pi * a * x)
  end function

  pure function model(c, x)
    !! Non-linear model that we will fit our data to
    !! f(x) = exp(sum(c_a * b_a(x))) + const
    real, intent(in) :: c(:), x
    real :: model, b(size(c) - 1)
    integer :: a, n
    n = size(c) - 1

    forall (a=1:n) b(a) = basis(a - 1, x)
    model = exp(sum(c(1:n) * b)) + c(n+1)
  end function

  pure function dmodel(c, x)
    !! Derivative of non-linear model with respect to coefficients
    !! df(x)/dc_k = exp(sum(c_a * b_a(x))) * b_k(x)
    real, intent(in) :: c(:), x
    real :: dmodel(size(c)), b(size(c) - 1), factor
    integer :: a, n
    n = size(c) - 1

    forall (a=1:n) b(a) = basis(a - 1, x)
    factor = exp(sum(c(1:n) * b))
    dmodel(1:n) = factor * b
    dmodel(n+1) = 1.0
  end function

end module q5_fit
