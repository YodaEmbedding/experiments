program q3
  use utils
  use integrator
  implicit none

  real, parameter :: a = 1.0, m = 1.0, V0 = 12.0 * m

  call main()

contains

  subroutine main()
    !! Numerically integrate equations of motion
    integer, parameter :: iters = 2000
    real, parameter :: dt = 1e-2, y0(2) = [-1.0, 0.0]
    real :: E0, ys(2, iters), results(4, iters)
    integer :: i

    print *
    print "(a)", "3. Numerically integrate equations of motion"
    print "(a)", "   for potential V(x) = -V0 / (cosh(x/a))^2"
    print *
    print "(a, f12.6)", "   E0 = ", E(y0)

    call integrate_ode(iters, dt, ys, y0)

    E0 = E(y0)
    results(1, :) = [(dt * i, i=0,iters-1)]
    results(2:3, :) = ys
    results(4, :) = [(E(ys(:, i)) / E0 - 1.0, i=1,iters)]

    call write_csv("results/q3.csv", results, &
      header="Time, Position, Velocity, Energy violation")

    call execute_command_line("python plot.py --time-series &
      &--title 'Q3: $\frac{-V_0}{\cosh^2 (x-a)}$' &
      &--nrows 2 &
      &results/" // "q3.csv &
      &plots/"   // "q3" // img_fileext)

    print *
  end subroutine

  function V(x)
    real, intent(in) :: x
    real :: V

    V = -V0 / (cosh(x / a))**2
  end function

  function E(y)
    !! Energy
    real, intent(in) :: y(2)
    real :: E

    E = 0.5 * m * y(2)**2 + V(y(1))
  end function

  function evalf(n, y) result(dydt)
    integer, intent(in) :: n  ! Mostly just needed to make Fortran compile...
    real, intent(in) :: y(n)
    real :: dydt(n)

    dydt(1) = y(2)
    dydt(2) = -2.0 * V0 / m / a * sinh(y(1) / a) / (cosh(y(1) / a))**3
  end function

  subroutine integrate_ode(n, dt, ys, y0)
    !! Integrate ODE for given initial condition y0
    !! Returns results in ys
    integer :: n, i
    integer, parameter :: nn = 2
    real :: ys(nn, n), y0(nn), y(nn), dt

    y = y0

    do i = 1, n
      ys(:, i) = y
      call gl10(evalf, nn, y, dt)
    end do
  end subroutine

end program q3
