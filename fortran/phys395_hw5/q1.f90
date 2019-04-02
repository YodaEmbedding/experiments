! To compile and run: make

program q1
  use integrator
  use utils
  implicit none

  integer, parameter :: nn = 3
  real, parameter :: pi = 3.1415926535897932384626433832795028841971693993751058

  call main()
  call find_eigenvalues()

contains

  subroutine main()
    integer, parameter :: steps = 2**8 * 4.0, bi_steps = 2 * steps - 1
    real, parameter :: dt = 1.0 / 2**8
    real :: ys(nn, bi_steps)
    real :: y0(nn) = [0.0, 0.0, 1.0]

    print *
    print "(a)", "Q1. Plot wavefunction"
    print "(a, f9.7)", "    dt:      ", dt
    print "(a, f9.7)", "    psi(0):  ", y0(2)
    print "(a, f9.7)", "    dpsi(0): ", y0(3)
    call integrate_ode_bidirectional(steps, dt, ys, y0)
    print "(a, f9.7)", "    N:       ", integrate_sum(dt, ys)
    print *

    call write_csv("results_q1.csv", ys, &
      "$x$, &
      &$\psi(x)$, &
      &$\frac{d}{dx}\psi(x)$")

    call execute_command_line("python plot.py --q1 results_q1.csv plot_q1.png")
  end subroutine

  subroutine find_eigenvalues()
    integer, parameter :: steps = 2**8 * 8.0, bi_steps = 2 * steps - 1
    integer, parameter :: iters = 101
    real, parameter :: dt = 1.0 / 2**8
    real :: ys(nn, bi_steps), results(3, iters), eigenvalues(10)
    integer :: i

    do i = 1, iters
      E = 0.5 + 10.0 * (i - 1) / (iters - 1)
      results(1, i) = E
      ! TODO function for separating into odd, even wavefunctions?
      call integrate_ode_bidirectional(steps, dt, ys, y0=[0.0, 1.0, 0.0])
      results(2, i) = ys(2, bi_steps)
      call integrate_ode_bidirectional(steps, dt, ys, y0=[0.0, 0.0, 1.0])
      results(3, i) = ys(2, bi_steps)
    end do

    print "(a)", "Q2. Energy eigenvalues"

    call write_csv("results_q2.csv", results, &
      "$E$, &
      &$\log (1 + |\psi_+(\infty)|)$, &
      &$\log (1 + |\psi_-(\infty)|)$")

    call execute_command_line("python plot.py --q2 results_q2.csv plot_q2.png")

    ! TODO determine eigenvalues by finding n minimums; check if enough 0 tol, and find bracketed root in nearby location, then mask
    ! out a certain region out of the minimum finding array

    ! TODO plot the various psi, psi^2 graphs for eigenvalues
    ! TODO bracketed roots version?
  end subroutine

  subroutine integrate_ode(n, dt, ys, y0)
    !! Integrate ODE for given initial condition y0
    !! Returns results in ys
    integer :: n, i
    real :: ys(nn, n), y0(nn), y(nn), dt

    y = y0

    do i = 1, n
      ys(:, i) = y
      call gl10(y, dt)
    end do
  end subroutine

  subroutine integrate_ode_bidirectional(n, dt, ys, y0)
    !! Integrate ODE for given initial condition y0 in both directions
    !! Returns results in ys
    integer :: n
    real :: ys(nn, 2*n-1), y0(nn), dt

    call integrate_ode(n, -dt, ys(:, n:1:-1), y0)
    call integrate_ode(n,  dt, ys(:, n:),     y0)
  end subroutine

  pure function integrate_sum(dt, ys) result(res)
    !! Integrate via Reimann sum
    real, intent(in) :: dt, ys(:, :)
    real :: res

    res = dt * sum(ys(2, :))
  end function

end program q1

! Q1 TODO
! Try E non-eigenvalue
! Plot odd/even solutions
! Accuracy 1e-12 (what's error) similar to delta = matmul(H,psi) - lmbda*psi?
! Integration stop condition

! Q2 TODO
! Violation of BC
! Find E values via bisection on "bracketed roots" (? what's this) for even/odd modes
! Plot psi and psi^2

! TODO uhh... the dx_dt = -1.0 doesn't seem to give correct odd functions
