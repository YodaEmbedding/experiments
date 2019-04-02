! To compile and run: make

program shoot
  use integrator
  use utils
  implicit none

  integer, parameter :: nn = 3
  integer, parameter :: step_rate = 2**8
  real, parameter :: dt = 1.0 / step_rate
  real, parameter :: pi = 3.1415926535897932384626433832795028841971693993751058

  call q1()
  call q2_find_eigenvalues()

contains

  subroutine q1()
    integer, parameter :: steps = 2.0 * step_rate, bi_steps = 2 * steps - 1
    real :: ys(nn, bi_steps)
    real :: y0(nn) = [0.0, 1.0, 1.0]
    real :: results(4, bi_steps)
    real, dimension(bi_steps) :: psis_even, psis_odd

    E = 4.20
    print *
    print "(a)", "Q1. Plot wavefunction"
    print "(a, f9.7)", "    dt:      ", dt
    print "(a, f9.7)", "    E:       ", E
    print "(a, f9.7)", "    psi(0):  ", y0(2)
    print "(a, f9.7)", "    dpsi(0): ", y0(3)
    call integrate_ode_bidirectional(steps, dt, ys, y0)
    print *

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

  subroutine q2_find_eigenvalues()
    integer, parameter :: steps = 8.0 * step_rate, bi_steps = 2 * steps - 1
    integer, parameter :: iters = 201
    real :: ys(nn, bi_steps), results(3, iters), lambdas(12)
    real, dimension(bi_steps) :: psis_even, psis_odd
    integer :: i

    do i = 1, iters
      E = 0.5 + 10.0 * (i - 1) / (iters - 1)
      call integrate_ode_bidirectional(steps, dt, ys, y0=[0.0, 1.0, 1.0])
      call separate_even_odd(ys(2, :), psis_even, psis_odd)
      results(1, i) = E
      results(2, i) = psis_even(bi_steps)
      results(3, i) = psis_odd(bi_steps)
    end do

    print "(a)", "Q2. Energy eigenvalues"

    call write_csv("results_q2.csv", results, &
      "$E$, &
      &$\log (1 + |\psi_+(\infty)|)$, &
      &$\log (1 + |\psi_-(\infty)|)$")

    call execute_command_line("python plot.py --q2 results_q2.csv plot_q2.png")

    ! TODO plot the various psi, psi^2 graphs for eigenvalues

    ! lambdas = [(0.5 + i, i = 0, 9)]
    lambdas(1:12:2) = k_minimums(6, results(1, :), results(2, :), .true.)
    lambdas(2:12:2) = k_minimums(6, results(1, :), results(3, :), .false.)
    call partial_sort(lambdas)
    print "(f19.12)", lambdas(1:10)
    call plot_wavefunctions("q2", lambdas(1:10))
  end subroutine

end program shoot

! Q1 TODO
! Accuracy 1e-12 (what's error) similar to delta = matmul(H,psi) - lmbda*psi?
! Integration stop condition

! Q2 TODO
! Plot psi and psi^2
! Normalize

! Q4 TODO

! TODO Anharmonic potential... might need hand-done brackets and other parameters

! TODO Rename this program q1...
