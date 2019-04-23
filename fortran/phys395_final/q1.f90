program q1
  use utils
  implicit none

  call main()

contains

  subroutine main()
    !! Find roots via Newton's method
    print *
    print "(a)", "1. Roots of cos x - x/5 = 0"
    print "(f18.12)", newton(f1, df1, x0=-4.0, tol=1e-13)
    print "(f18.12)", newton(f1, df1, x0=-2.0, tol=1e-13)
    print "(f18.12)", newton(f1, df1, x0= 1.0, tol=1e-13)
    print *
  end subroutine

  pure function f1(x)
    !! Function for question 1
    real, intent(in) :: x
    real :: f1

    f1 = cos(x) - x/5
  end function

  pure function df1(x)
    !! Derivative of function for question 1
    real, intent(in) :: x
    real :: df1

    df1 = -sin(x) - 1.0 / 5.0
  end function

end program q1
