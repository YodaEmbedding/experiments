module q1_roots
  implicit none

  interface
    function pR2R(x)
      !! Real -> Real
      real, intent(in) :: x
      real :: pR2R
    end function pR2R
  end interface

contains

  subroutine q1()
    !! Find roots via Newton's method
    print "(a)", "1. Roots of x^3 - x + 0.25 = 0:"
    print "(f12.6)", newton(f1, df1, x0=-1.0, tol=1e-6)
    print "(f12.6)", newton(f1, df1, x0= 0.0, tol=1e-6)
    print "(f12.6)", newton(f1, df1, x0= 1.0, tol=1e-6)
    print *
  end subroutine

  function newton(f, df, x0, tol) result(x)
    !! Find root of f(x) via Newton's method
    procedure(pR2R) :: f, df
    real, intent(in) :: x0, tol
    real :: x, y

    x = x0
    y = f(x)

    do while (tol < abs(y))
      x = x - y / df(x)
      y = f(x)
    end do
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

end module q1_roots
