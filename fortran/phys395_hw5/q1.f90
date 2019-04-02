! To compile and run: make

program q1
  use integrator
  implicit none

  integer, parameter :: nn = 3
  real, parameter :: pi = 3.1415926535897932384626433832795028841971693993751058

  call main()

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
    call write_csv("results.csv", ys, "$x$, $\psi(x)$, $\psi'(x)$")
    call execute_command_line("python plot.py results.csv --plot-results")
    print *

    call find_eigenvalues()
  end subroutine

  subroutine find_eigenvalues()
    integer, parameter :: steps = 2**8 * 4.0, bi_steps = 2 * steps - 1
    integer, parameter :: iters = 101
    real, parameter :: dt = 1.0 / 2**8
    real :: ys(nn, bi_steps)
    real :: y0(nn) = [0.0, 1.0, 0.0]
    real :: results(2, iters)
    integer :: i

    do i = 1, iters
      E = (i - 1) / 10.0
      call integrate_ode_bidirectional(steps, dt, ys, y0)
      results(1, i) = E
      results(2, i) = ys(2, 1)
    end do
    print "(2f24.16)", results
  end subroutine

  ! subroutine calc_step_params(steps, bi_steps, dt, step_rate, t)
  !   integer :: steps, bi_steps, step_rate
  !   real :: dt, t
  !
  !   dt = 1.0 / step_rate
  !   steps = step_rate * t
  !   bi_steps = 2 * steps - 1
  ! end function

  subroutine write_csv(filename, mat, header, num_fmt)
    !! Output a csv file with plottable data
    integer, parameter :: ofh = 2
    real :: mat(:, :)
    character(len=*) :: filename, header
    character(len=*), optional :: num_fmt
    character(:), allocatable :: num_fmt_, fmt_str
    integer :: i

    if (      present(num_fmt)) num_fmt_ = num_fmt
    if (.not. present(num_fmt)) num_fmt_ = "ES32.16"

    fmt_str = "(" // num_fmt_
    do i = 2, size(mat, 1)
      fmt_str = fmt_str // ", ',', " // num_fmt_
    end do
    fmt_str = fmt_str // ")"

    open(unit=ofh, file=filename, action="write", status="replace")
    write(ofh, *) header
    do i = 1, size(mat, 2)
      write(ofh, fmt_str) mat(:, i)
    end do
    close(ofh)
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
    real :: ys(nn, 2*n-1), y0(nn), y(nn), dt

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
! Accuracy 10^-12 (what is error?)
! Integration stop condition

! Q2 TODO
! Violation of BC
! Find E values via bisection on "bracketed roots" (? what's this) for even/odd modes
! Plot psi and psi^2

! TODO uhh... the dx_dt = -1.0 doesn't seem to give correct odd functions

