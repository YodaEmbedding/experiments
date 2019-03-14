! To compile and run: make

program q3
  use fitsio
  use integrator
  implicit none

  real, parameter :: pi = 3.1415926535897932384626433832795028841971693993751058
  real :: energy_flip_min

  energy_flip_min = min( &
    energy([pi, 0.0, 0.0, 0.0]), &
    energy([0.0, 0.0, pi, 0.0]))

  call main()

contains

  subroutine main()
    integer, parameter :: steps_per_second = 7
    real, parameter :: time_period = 10000.0 * sqrt(1.0 / 9.806)
    real, parameter :: dt = 1.0 / steps_per_second
    integer, parameter :: steps = steps_per_second * time_period

    print "(a)", "Q3. Fractal generation"
    print *
    call write_fractal("plot_fractal_16x16.fit",     dt, steps, [-pi, pi], [-pi, pi], 16, 16)
    call write_fractal("plot_fractal_32x32.fit",     dt, steps, [-pi, pi], [-pi, pi], 32, 32)
    call write_fractal("plot_fractal_64x64.fit",     dt, steps, [-pi, pi], [-pi, pi], 64, 64)
    call write_fractal("plot_fractal_128x128.fit",   dt, steps, [-pi, pi], [-pi, pi], 128, 128)
    call write_fractal("plot_fractal_256x256.fit",   dt, steps, [-pi, pi], [-pi, pi], 256, 256)
    ! call write_fractal("plot_fractal_512x512.fit",   dt, steps, [-pi, pi], [-pi, pi], 512, 512)
    ! call write_fractal("plot_fractal_1024x1024.fit", dt, steps, [-pi, pi], [-pi, pi], 1024, 1024)
    ! call write_fractal("plot_fractal_2048x2048.fit", dt, steps, [-pi, pi], [-pi, pi], 2048, 2048)
  end subroutine

  subroutine write_fractal(filename, dt, steps, th1, th2, width, height)
    !! Write phase plot within ranges given by th1 and th2
    character(len=*) :: filename
    character(len=256) :: cmd
    integer, parameter :: iters_since_msg_max = 1024
    integer :: steps, width, height, i, j, iters_since_msg
    real :: dt, th1(2), th2(2), data_(1, width, height)

    print "(a, i4, a, i4, a, f5.2, a, f5.2, a, f5.2, a, f5.2, a)", &
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

    print "(i3, a)", 100, "% complete"
    data_ = log(data_)
    call write2fits(filename, data_, th1, th2, &
      ['magnitude'], '($theta_1$,$theta_2$)')

    write (cmd, "(3a)") "python plot.py ", filename, " --plot-fractal"
    call execute_command_line(cmd)

    print "(2a)", "Written output to ", filename
    print *
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

end program q3

! TODO increase steps for stability?
! TODO zoom into regions
! TODO write readme
