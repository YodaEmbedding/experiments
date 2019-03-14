! To compile and run: make

program phys395_hw4_ode
  use fitsio
  implicit none

  real, parameter :: pi = 3.1415926535897932384626433832795028841971693993751058
  real, parameter :: m1 = 1.0, m2 = 1.0
  real, parameter :: l1 = 1.0, l2 = 1.0
  real :: energy_flip_min

  energy_flip_min = min( &
    energy([pi, 0.0, 0.0, 0.0]), &
    energy([0.0, 0.0, pi, 0.0]))

  call q2()
  call q3()

contains

  ! TODO enforce real(8) for floating point computations for this question
  subroutine q2()
    integer, parameter :: steps_per_second = 2**8 ! TODO change this
    real, parameter :: time_period = 100.0 * sqrt(1.0 / 9.806)
    real, parameter :: dt = 1.0 / steps_per_second
    integer, parameter :: steps = steps_per_second * time_period
    real :: ys(4, steps)
    real :: ts(steps)

    print *
    print "(a)", "Q2. Plot energy violation and create video animation"
    print "(a)", "    Initial conditions: theta_1 = pi / 3, theta_2 = -pi/3"
    print "(a, f9.7)", "    dt: ", dt
    print *
    call run(steps, dt, ts, ys, y0=[pi / 3, 0.0, -pi / 3, 0.0])
    call write_csv("results.csv", steps, ts, ys)
    call execute_command_line("python plot.py results.csv --plot-results")
    print *
  end subroutine

  subroutine q3()
    integer, parameter :: steps_per_second = 7  ! TODO increase for stability?
    real, parameter :: time_period = 10000.0 * sqrt(1.0 / 9.806)
    real, parameter :: dt = 1.0 / steps_per_second
    integer, parameter :: steps = steps_per_second * time_period

    print "(a)", "Q3. Fractal output"
    print *
    call write_fractal("plot_fractal_16x16.fit",   dt, steps, [-pi, pi], [-pi, pi], 16, 16)
    call write_fractal("plot_fractal_64x64.fit",   dt, steps, [-pi, pi], [-pi, pi], 64, 64)
    call write_fractal("plot_fractal_128x128.fit", dt, steps, [-pi, pi], [-pi, pi], 128, 128)
    call write_fractal("plot_fractal_256x256.fit", dt, steps, [-pi, pi], [-pi, pi], 256, 256)

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

  function find_flip(n, dt, y0) result(i)
    !! Run simulation for given initial condition y0 at time step dt
    !! Returns steps taken until a flip occurs
    real, parameter :: g = 9.806
    real :: y0(4), y(4), dt
    integer :: n, i

    ! Exit if energy of system is too low to allow flip
    if (energy(y0) < energy_flip_min) then
      i = n + 1
      return
    end if

    y = y0
    do i = 1, n
      if ((abs(y(1)) > pi) .or. (abs(y(3)) > pi)) exit
      call gl10(y, dt)
    end do
  end function

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

  subroutine write_fractal(filename, dt, steps, th1, th2, width, height)
    !! Write phase plot within ranges given by th1 and th2
    character(len=*) :: filename
    character(len=256) :: cmd
    integer, parameter :: iters_since_msg_max = 512
    integer :: steps, width, height, i, j, iters_since_msg
    real :: dt, th1(2), th2(2), data_(1, width, height)

    print "(a, i3, a, i3, a, f5.2, a, f5.2, a, f5.2, a, f5.2, a)", &
      "Plotting fractal with dimensions ", &
      width, "x", height, " from [", &
      th1(1), ", ", th2(1), "] to [", th1(2), ", ", th2(2), "]"

    iters_since_msg = iters_since_msg_max

    do j = 1, height
      if (iters_since_msg >= iters_since_msg_max) then
        print "(i3, a)", 100 * (j - 1) / (height - 1), "% complete"
        iters_since_msg = 0
      end if

      !$omp parallel do
      do i = 1, width
        data_(1, i, j) = find_flip(steps, dt, y0=[ &
          th1(1) + (th1(2) - th1(1)) * (i - 1) / (width  - 1), 0.0, &
          th2(1) + (th2(2) - th2(1)) * (j - 1) / (height - 1), 0.0])
      end do

      iters_since_msg = iters_since_msg + width
    end do

    data_ = log(data_)

    call write2fits(filename, data_, th1, th2, &
      ['magnitude'], '($theta_1$,$theta_2$)')

    write (cmd, "(3a)") "python plot.py ", filename, " --plot-fractal"
    call execute_command_line(cmd)

    print "(2a)", "Written output to ", filename
    print *
  end subroutine

  function evalf(y) result(dydt)
    real, intent(in) :: y(4)
    real, parameter :: g = 9.806
    real :: dydt(4), m, c, s, s1, s2, scale1, scale2

    associate(th1 => y(1), w1 => y(2), th2 => y(3), w2 => y(4))
      m = m1 + m2
      c = cos(th2 - th1)
      s = sin(th2 - th1)
      s1 = sin(th1)
      s2 = sin(th2)
      scale1 = m * l1 - m2 * l1 * c * c
      scale2 = (l2 / l1) * scale1

      dydt(1) = w1

      dydt(2) = ( &
        (m2 * l1 * w1**2 * s * c) + &
        (m2 * l2 * w2**2 * s) +     &
        (m2 * g * s2 * c) +         &
        (-m * g * s1)) / scale1

      dydt(3) = w2

      dydt(4) = ( &
        (-m2 * l2 * w2**2 * s * c) + &
        (-m  * l1 * w1**2 * s) +     &
        ( m * g * s1 * c) +          &
        (-m * g * s2)) / scale2
    end associate
  end function

  pure function energy(y)
    real, intent(in) :: y(4)
    real, parameter :: g = 9.806
    real :: energy, T, V, y1, y2

    associate(th1 => y(1), w1 => y(2), th2 => y(3), w2 => y(4))
      y1 = -l1 * cos(th1) + 2.0
      y2 = -l2 * cos(th2) + y1

      T = 0.5 * ( &
        m1 * l1**2 * w1**2 + &
        m2 * ( &
          l1**2 * w1**2 + &
          l2**2 * w2**2 + &
          2 * l1 * l2 * w1 * w2 * cos(th2 - th1)))
      V = g * (m1 * y1 + m2 * y2)
    end associate

    energy = T + V
  end function

  ! 10th order implicit Gauss-Legendre integrator
  subroutine gl10(y, dt)
    integer, parameter :: s = 5, n = 4
    real y(n), g(n, s), dt
    integer i, k

    ! Butcher tableau for 10th order Gauss-Legendre method
    real, parameter :: a(s, s) = reshape([ &
      0.5923172126404727187856601017997934066Q-1,  &
      -1.9570364359076037492643214050884060018Q-2, &
      1.1254400818642955552716244215090748773Q-2,  &
      -0.5593793660812184876817721964475928216Q-2, &
      1.5881129678659985393652424705934162371Q-3,  &
      1.2815100567004528349616684832951382219Q-1,  &
      1.1965716762484161701032287870890954823Q-1,  &
      -2.4592114619642200389318251686004016630Q-2, &
      1.0318280670683357408953945056355839486Q-2,  &
      -2.7689943987696030442826307588795957613Q-3, &
      1.1377628800422460252874127381536557686Q-1,  &
      2.6000465168064151859240589518757397939Q-1,  &
      1.4222222222222222222222222222222222222Q-1,  &
      -2.0690316430958284571760137769754882933Q-2, &
      4.6871545238699412283907465445931044619Q-3,  &
      1.2123243692686414680141465111883827708Q-1,  &
      2.2899605457899987661169181236146325697Q-1,  &
      3.0903655906408664483376269613044846112Q-1,  &
      1.1965716762484161701032287870890954823Q-1,  &
      -0.9687563141950739739034827969555140871Q-2, &
      1.1687532956022854521776677788936526508Q-1,  &
      2.4490812891049541889746347938229502468Q-1,  &
      2.7319004362580148889172820022935369566Q-1,  &
      2.5888469960875927151328897146870315648Q-1,  &
      0.5923172126404727187856601017997934066Q-1], [s, s])

    real, parameter :: b(s) = [ &
      1.1846344252809454375713202035995868132Q-1, &
      2.3931433524968323402064575741781909646Q-1, &
      2.8444444444444444444444444444444444444Q-1, &
      2.3931433524968323402064575741781909646Q-1, &
      1.1846344252809454375713202035995868132Q-1]

    g = 0.0
    do k = 1, 16
      g = matmul(g, a)
      do i = 1, s
        g(:, i) = evalf(y + g(:, i) * dt)
      end do
    end do

    y = y + matmul(g, b) * dt
  end subroutine gl10

end program phys395_hw4_ode

! TODO ensure animation works on VM
! TODO exploit symmetry?
