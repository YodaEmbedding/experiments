! To compile and run: make

program q2
  use integrator_r8
  implicit none

  real, parameter :: pi = 3.1415926535897932384626433832795028841971693993751058

  call main()

contains

  subroutine main()
    integer, parameter :: steps_per_second = 2**8 ! TODO change this
    real, parameter :: time_period = 100.0 * sqrt(1.0 / 9.806)
    real, parameter :: dt = 1.0 / steps_per_second
    integer, parameter :: steps = steps_per_second * time_period
    real :: ys(4, steps)
    real :: ts(steps)

    print *
    print "(a)", "Q2. Plot energy violation and create video animation"
    print "(a)", "    Initial conditions: theta_1 = pi / 3, theta_2 = -pi/3"
    print "(a, f9.7, a)", "    dt: ", dt, "s"
    print *
    call run(steps, dt, ts, ys, y0=[pi / 3, 0.0, -pi / 3, 0.0])
    call write_csv("results.csv", steps, ts, ys)
    call execute_command_line("python plot.py results.csv --plot-results")
    print *
  end subroutine

  subroutine run(n, dt, ts, ys, y0)
    !! Run simulation for given initial condition y0 at time step dt
    !! Returns simulation results in ts and ys
    integer :: n, i
    real :: ys(4, n), ts(n), y0(4), y(4), dt

    ts = [(i * dt, i=0,n-1)]
    y = y0

    do i = 1, n
      ys(:, i) = y
      call gl10(y, dt)
    end do
  end subroutine

  subroutine write_csv(filename, n, t, y)
    !! Output a csv file with plottable data
    integer, parameter :: ofh = 2
    character(len=64), parameter :: fmt_str = &
      "(ES24.16, ',', ES24.16, ',', ES24.16, ',', ES24.16)"
    character(len=*) :: filename
    integer :: n, i
    real :: t(n), y(4, n)
    real :: E0

    E0 = energy(y(:, 1))

    open(unit=ofh, file=filename, action="write", status="replace")
    write(ofh, *) "Time, $\theta_1$, $\theta_2$, Energy violation"
    do i = 1, n
      write(ofh, fmt_str) t(i), y(1, i), y(3, i), energy(y(:, i)) / E0 - 1.0
    end do
    close(ofh)
  end subroutine

end program q2

! TODO ensure animation works on VM
! TODO exploit symmetry?
! TODO fdefault-real-8 for part 2
