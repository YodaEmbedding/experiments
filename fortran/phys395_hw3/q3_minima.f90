module q3_minima
  implicit none

contains

  subroutine q3()
    !! Find minima via golden section search
    print "(a)", "3. Minima of (x^2 - 1)^2 + x:"
    print "(f12.6)", golden(f3, x_min=-2.0, x_max=0.0, tol=1e-6)
    print "(f12.6)", golden(f3, x_min= 0.0, x_max=1.0, tol=1e-6)
    print *
  end subroutine

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

  pure function f3(x)
    !! Function for question 3
    real, intent(in) :: x
    real :: f3

    f3 = (x**2 - 1.0)**2 + x
  end function

end module q3_minima
