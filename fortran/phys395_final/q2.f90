program q2
  use utils
  implicit none

  call main()

contains

  subroutine main()
    !! Find extrema
    print *
    print "(a)", "2. Extrema of x^4 + 3x^3 - 4x^2 - 3x + 4"
    print "(f18.12)", golden(f2,   x_min=-4.0, x_max=-2.0, tol=1e-12)
    print "(f18.12)", golden(f2_n, x_min=-2.0, x_max= 0.0, tol=1e-12)
    print "(f18.12)", golden(f2,   x_min= 0.0, x_max= 2.0, tol=1e-12)

    print *
  end subroutine

  pure function f2(x)
    !! Function for question 2
    real, intent(in) :: x
    real :: f2

    f2 = x**4 + 3*x**3 - 4*x**2 - 3*x + 4
  end function

  pure function f2_n(x)
    !! Function for question 2, negated
    real, intent(in) :: x
    real :: f2_n

    f2_n = -f2(x)
  end function

end program q2
