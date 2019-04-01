! To compile and run: make

program q1
  use integrator
  implicit none

  integer, parameter :: nn = 3
  real, parameter :: pi = 3.1415926535897932384626433832795028841971693993751058

  call main()

contains

  subroutine main()
    integer, parameter :: steps_per_second = 2**8
    real, parameter :: time_period = 5.0
    real, parameter :: dt = 1.0 / steps_per_second
    integer, parameter :: steps = steps_per_second * time_period
    real :: ys(nn, steps)

    print *
    print "(a)", "Q1. Plot wavefunction"
    ! print "(a)", "    Initial conditions: theta_1 = pi / 3, theta_2 = -pi/3"
    print "(a, f9.7, a)", "    dt: ", dt, "s"
    print *
    call integrate_ode(steps, dt, ys, y0=[0.0, 1.0, 0.0])
    call write_csv("results.csv", steps, ys)
    call execute_command_line("python plot.py results.csv --plot-results")
    print *
  end subroutine

  subroutine write_csv(filename, n, y)
    !! Output a csv file with plottable data
    integer, parameter :: ofh = 2
    character(len=64), parameter :: fmt_str = &
      "(ES32.16, ',', ES32.16, ',', ES32.16)"
    character(len=*) :: filename
    integer :: n, i
    real :: y(nn, n)
    ! real :: E0

    ! E0 = energy(y(:, 1))

    open(unit=ofh, file=filename, action="write", status="replace")
    write(ofh, *) "$x$, $\psi(x)$, $\psi'(x)$"
    do i = 1, n
      write(ofh, fmt_str) y(1, i), y(2, i), y(3, i)
      ! energy(y(:, i)) / E0 - 1.0
    end do
    close(ofh)
  end subroutine

  subroutine integrate_ode(n, dt, ys, y0)
    !! Integrate ODE for given initial condition y0 at time step dt
    !! Returns results in ys
    integer :: n, i
    real :: ys(nn, n), y0(nn), y(nn), dt

    y = y0

    do i = 1, n
      ys(:, i) = y
      call gl10(y, dt)
    end do
  end subroutine

  pure function integrate_sum(dt, ys) result(res)
    !! Integrate via Reimann sum
    real, intent(in) :: dt, ys(:, :)
    real :: res

    res = dt * sum(ys(2, :))
  end function

end program q1

! TODO
! Plot odd/even solutions
! Accuracy 10^-12 (what is error?)
! Integration stop condition
