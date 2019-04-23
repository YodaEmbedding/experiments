program q3
  use utils
  use integrator
  implicit none

  real, parameter :: a = 1.0, m = 1.0, V0 = 12.0 * m

  call q3_main()
  call q4_main()

contains

  subroutine q3_main()
    !! Numerically integrate equations of motion
    integer, parameter :: iters = 10000
    real, parameter :: dt = 1e-3, y0(2) = [0.1, 0.0]
    real :: E0, ys(2, iters), results(4, iters)
    integer :: i

    print *
    print "(a)", "3. Numerically integrate equations of motion"
    print "(a)", "   for potential V(x) = -V0 / (cosh(x/a))^2"
    print *
    print "(a, f18.12)", "   E0 = ", E(y0)

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

  subroutine q4_main()
    !! Periods of oscillation
    integer, parameter :: iters = 50
    real, parameter :: dt = 1e-3
    real :: x0, ys(2, iters), results(2, iters)
    integer :: i

    print *
    print "(a)", "4. Periods of oscillation"
    print "(a)", "   for potential V(x) = -V0 / (cosh(x/a))^2"
    print *

    do i = 1, iters
      x0 = 0.1 * i * a
      results(1, i) = x0
      results(2, i) = find_period(100000, dt, y0=[x0, 0.0])
      print "(a, f3.1, a, f4.1)", "   x0 = ", results(1, i), &
        ",    t = ", results(2, i)
    end do

    call write_csv("results/q4.csv", results, &
      header="Initial position, Period")

    call execute_command_line("python plot.py --time-series &
      &--title 'Q4: $\frac{-V_0}{\cosh^2 (x-a)}$ period' &
      &results/" // "q4.csv &
      &plots/"   // "q4" // img_fileext)

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

  function find_period(n, dt, y0) result(t)
    integer, parameter :: nn = 2
    integer, intent(in) :: n
    real, intent(in) :: dt, y0(nn)
    real :: t, y(nn), start_t
    integer :: i

    y = y0
    start_t = 1.0
    if (y0(1) > 3.0) start_t = 3.0
    if (y0(1) > 4.0) start_t = 8.0

    do i = 1, n
      call gl10(evalf, nn, y, dt)
      t = (i - 1) * dt
      if ((t >= start_t) .and. (abs(y(1) - y0(1)) / y0(1) <= 1e-2)) exit
    end do
  end function

end program q3
