! To compile and run: make

program shoot
  use integrator
  use utils
  implicit none

  integer, parameter :: nn = 3
  integer, parameter :: step_rate = 2**8
  real, parameter :: dt = 1.0 / step_rate
  real, parameter :: pi = 3.1415926535897932384626433832795028841971693993751058
  logical :: is_even = .false.

  print *
  print "(a)", "Q1. Plot wavefunction"
  call q1()

  print *
  print "(a)", "Q2. Determine energy eigenvalues for harmonic oscillator"
  V => V_harmonic
  call find_eigenvalues("q2")

  print *
  print "(a)", "Q3. Determine energy eigenvalues for anharmonic oscillator"
  V => V_anharmonic
  call find_eigenvalues("q3")

  print *

contains

  subroutine q1()
    integer, parameter :: steps = 2.0 * step_rate, bi_steps = 2 * steps - 1
    real :: ys(nn, bi_steps)
    real :: y0(nn) = [0.0, 1.0, 1.0]
    real :: results(4, bi_steps)
    real, dimension(bi_steps) :: psis_even, psis_odd

    E = 4.20

    print "(a, f9.7)", "    dt:      ", dt
    print "(a, f9.7)", "    E:       ", E
    print "(a, f9.7)", "    psi(0):  ", y0(2)
    print "(a, f9.7)", "    dpsi(0): ", y0(3)

    call integrate_ode_bidirectional(steps, dt, ys, y0)
    call separate_even_odd(ys(2, :), psis_even, psis_odd)
    results(1, :) = ys(1, :)
    results(2, :) = psis_even
    results(3, :) = psis_odd
    results(4, :) = ys(3, :)

    call write_csv("results_q1.csv", results, &
      "$x$, &
      &$\psi_+(x)$, &
      &$\psi_-(x)$, &
      &$\frac{d}{dx}\psi(x)$")

    call execute_command_line("python plot.py --time-series --nrows 2 &
      &--title 'Even/odd wavefunctions at E = 4.2' &
      &results_q1.csv &
      &plot_q1.png")
  end subroutine

  subroutine find_eigenvalues(suffix)
    character(len=*) :: suffix
    integer, parameter :: steps = 8.0 * step_rate, bi_steps = 2 * steps - 1
    integer, parameter :: imax = 401
    real :: results(3, imax + 10), lambda_results(3, 10), lambdas(10), mode(3)
    integer :: i

    do i = 1, imax
      results(:, i) = psi_max_modes(20.0 * (i - 1) / (imax - 1))
    end do

    is_even = .true.
    lambdas(1:10:2) = k_zeros(5, psi_max, results(1, :imax), results(2, :imax))
    is_even = .false.
    lambdas(2:10:2) = k_zeros(5, psi_max, results(1, :imax), results(3, :imax))
    call partial_sort(lambdas, k=10)

    print "(a)", "    Energy eigenvalues:"
    print "(f19.12)", lambdas(1:10)

    do i = 1, 10
      mode = psi_max_modes(lambdas(i))
      mode(1 + minloc(abs(mode(2:3)), 1)) = 0.0  ! manually zero for aesthetics
      lambda_results(:, i) = mode
    end do

    call insert_results(results, lambda_results)

    call write_csv("results_" // suffix // "_eigenvalues.csv", &
      reshape(lambdas, [1, size(lambdas)]))

    call write_csv("results_" // suffix // ".csv", results, &
      "$E$, &
      &$\log (1 + |\psi_+(\infty)|)$, &
      &$\log (1 + |\psi_-(\infty)|)$")

    call execute_command_line("python plot.py --q2 &
      &--ticks results_" // suffix // "_eigenvalues.csv &
      &results_" // suffix // ".csv &
      &plot_" // suffix // "_eigenvalues.png")

    call plot_wavefunctions(suffix, lambdas(1:10))
  end subroutine

  function psi_max_modes(x)
    !! Calculate psi at "infinity" for even and odd modes at given energy
    integer, parameter :: step_rate = 2**4
    integer, parameter :: steps = 8.0 * step_rate
    integer, parameter :: bi_steps = 2 * steps - 1
    real,    parameter :: dt = 1.0 / step_rate
    real, intent(in) :: x
    real :: ys(nn, bi_steps), psi_max_modes(3)

    E = x
    call integrate_ode_bidirectional(steps, dt, ys, y0=[0.0, 1.0, 1.0])
    psi_max_modes(1) = x
    psi_max_modes(2) = 0.5 * (ys(2, bi_steps) + ys(2, 1))
    psi_max_modes(3) = 0.5 * (ys(2, bi_steps) - ys(2, 1))
  end function

  function psi_max(x)
    !! Calculate psi at "infinity" for even or odd mode at given energy
    !! Roots of this function correspond to energy eigenvalues,
    !! since the wavefunction vanishes (for a particular even or odd mode)
    real, intent(in) :: x
    real :: psi_max, modes(3)

    modes = psi_max_modes(x)
    psi_max = merge(modes(2), modes(3), is_even)
  end function

  subroutine insert_results(results, lambda_results)
    real :: results(:, :), lambda_results(:, :)
    integer :: i, j, k

    k = size(lambda_results, 2)
    j = size(results, 2) - k

    do i = size(results, 2), 1, -1
      if (k < 1) exit
      if ((results(1, j) > lambda_results(1, k))) then
        results(:, i) = results(:, j)
        j = j - 1
      else
        results(:, i) = lambda_results(:, k)
        k = k - 1
      end if
    end do
  end subroutine

end program shoot

! Q1 TODO
! Accuracy 1e-12 (what's error) similar to delta = matmul(H,psi) - lmbda*psi?
! Integration stop condition
