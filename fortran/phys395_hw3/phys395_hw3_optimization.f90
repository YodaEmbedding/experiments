! Compile and run: make

program phys395_hw3_optimization
  implicit none

  print *
  call q1_roots()
  call q3_minima()
  call q5_fit()

contains

  subroutine q1_roots()
    !! Find roots via Newton's method

    print "(a)", "1. Roots of x^3 - x + 0.25 = 0:"
    print "(f12.6)", newton(f1, df1, x0=-1.0, tol=1e-6)
    print "(f12.6)", newton(f1, df1, x0= 0.0, tol=1e-6)
    print "(f12.6)", newton(f1, df1, x0= 1.0, tol=1e-6)
    print *
  end subroutine

  subroutine q3_minima()
    !! Find minima via golden section search

    print "(a)", "3. Minima of (x^2 - 1)^2 + x:"
    print "(f12.6)", golden(f3, x_min=-2.0, x_max=0.0, tol=1e-6)
    print "(f12.6)", golden(f3, x_min= 0.0, x_max=1.0, tol=1e-6)
    print *
  end subroutine

  subroutine q5_fit()
    !! Fit coefficients by finding minimum of chi-square via Levenberg-Marquardt
    integer, parameter :: ifh = 1
    integer, parameter :: max_lines = 1048576
    integer :: i, stat
    real, dimension(:), allocatable :: x, y
    real, dimension(max_lines) :: x_, y_

    ! TODO move this into separate read data function
    ! Read data
    open(unit=ifh, file="data.dat", action="read", status="old", &
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

    print "(a)", "5. Fit of data:"
    call write_csv(x, y, "results.csv")  ! ummm is this really where things should happen?
    print *

    deallocate(x, y)
  end subroutine

  subroutine write_csv(x, y, filename)
    !! TODO
    real, dimension(:) :: x, y
    character(len=*) :: filename
    integer, parameter :: ofh = 2
    integer :: i
    real, dimension(size(x)) :: y_fit

    ! y_fit = fit(x, y, n=3)

    open(unit=ofh, file=filename, action="write", status="replace")
    write(ofh, *) "x, y, f(x)"
    do i = 1, size(x)
      write(ofh, *) x(i), ", ", y(i), ", ", y_fit(i)
    end do
    close(ofh)
  end subroutine

  ! function fit(x, y, n) result(ys_fit)
  !   !! TODO
  !   integer :: a, i, j, k, n
  !   real, dimension(:) :: x, y
  !   real, dimension(size(x)) :: ys_fit
  !   real, dimension(n + 1) :: coeffs, p, s
  !   real, dimension(size(x), n + 1) :: bxa
  !   real, dimension(n + 1, n + 1) :: B, U, Vh
  !   real :: chi_actual, chi_expect
  !   character(len=64) :: fmt_str

  !   ! ys_fit = matmul(bxa, coeffs)

  !   ! write(fmt_str, "(a, i10, a)") "(", n + 1, "f7.2)"
  !   ! print "(a, a)", "solver = ", solver
  !   ! print "(a, i1)", "n = ", n
  !   ! print *
  !   ! write(*, "(a21)", advance="no") "Best fit params: "
  !   ! print fmt_str, coeffs
  !   ! print "(a21, f10.2)", "Chi-square (actual): ", chi_actual
  !   ! print "(a21, f10.2)", "Chi-square (expect): ", chi_expect
  !   ! print "(/, a, /)", "----"
  ! end function

  function newton(f, df, x0, tol) result(x)
    !! Find root of f(x) via Newton's method
    real, external :: f, df
    real, intent(in) :: x0, tol
    real :: x, y

    x = x0
    y = f(x)

    do while (tol < abs(y))
      x = x - y / df(x)
      y = f(x)
    end do
  end function

  function golden(f, x_min, x_max, tol)
    !! Find a minimum of f(x) within interval via golden section search
    !! This implementation doubles the necessary calls to f; but it is simpler
    real, external :: f
    real, intent(in) :: x_min, x_max, tol
    real :: golden, a, b, c, d
    real, parameter :: phi = 1.6180339887498948482045868343656381177203091798057

    a = x_min
    b = x_max
    c = b - (b - a) / phi
    d = a + (b - a) / phi

    do while (tol < abs(c - d))
      if (f(c) < f(d)) then
        b = d
      else
        a = c
      end if

      c = b - (b - a) / phi
      d = a + (b - a) / phi
    end do

    golden = 0.5 * (b + a)
  end function

  pure function f1(x)
    !! Function for question 1
    real, intent(in) :: x
    real :: f1

    f1 = x**3 - x + 0.25
  end function

  pure function df1(x)
    !! Derivative of function for question 1
    real, intent(in) :: x
    real :: df1

    df1 = 3.0 * x**2 - 1.0
  end function

  pure function f3(x)
    !! Function for question 3
    real, intent(in) :: x
    real :: f3

    f3 = (x**2 - 1.0)**2 + x
  end function

  elemental function basis(a, x)
    !! An even basis... for a more civilized age
    integer, intent(in) :: a
    real, intent(in) :: x
    real :: basis
    real, parameter :: pi = 3.14159265358979323846264338327950288419716939937510

    basis = cos(2 * pi * a * x)
  end function

end program phys395_hw3_optimization
