module utils
  use integrator
  implicit none

  integer, private, parameter :: nn = 3
contains

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

  subroutine plot_wavefunctions(suffix, lambdas)
    integer, parameter :: step_rate = 2**4
    real, parameter :: dt = 1.0 / step_rate
    integer, parameter :: steps = 4.0 * step_rate, bi_steps = 2 * steps - 1
    real :: ys(nn, bi_steps), y0(nn), lambdas(:), results(11, bi_steps)
    character(len=*) :: suffix
    integer :: i

    do i = 1, size(lambdas, 1)
      E = lambdas(i)
      if (mod(i - 1, 2) == 0) y0 = [0.0, 1.0, 0.0]
      if (mod(i - 1, 2) == 1) y0 = [0.0, 0.0, 1.0]
      call integrate_ode_bidirectional(steps, dt, ys, y0)  ! TODO odd, even?
      results(i + 1, :) = ys(2, :)
    end do
    results(1, :) = ys(1, :)

    ! TODO specific energy labels? (construct string)
    call write_csv("results_" // suffix // ".csv", results, &
      "$x$, &
      &$\psi(x; E_1)$, &
      &$\psi(x; E_2)$, &
      &$\psi(x; E_3)$, &
      &$\psi(x; E_4)$, &
      &$\psi(x; E_5)$, &
      &$\psi(x; E_6)$, &
      &$\psi(x; E_7)$, &
      &$\psi(x; E_8)$, &
      &$\psi(x; E_9)$, &
      &$\psi(x; E_{10})$")

    call execute_command_line("python plot.py --time-series &
      &--title 'Wavefunctions for various energy eigenvalues' &
      &results_" // suffix // ".csv &
      &plot_"    // suffix // ".png")
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
    real :: ys(nn, 2*n-1), ys_(nn, n), y0(nn), dt

    call integrate_ode(n, -dt, ys_, y0)
    ys(:, n:1:-1) = ys_
    call integrate_ode(n,  dt, ys_, y0)
    ys(:, n:) = ys_
  end subroutine

  pure function integrate_sum(dt, ys) result(res)
    !! Integrate via Reimann sum
    real, intent(in) :: dt, ys(:, :)
    real :: res

    res = dt * sum(ys(2, :))
  end function

  subroutine separate_even_odd(ys, ys_even, ys_odd)
    real, dimension(:) :: ys, ys_even, ys_odd
    ys_even = 0.5 * (ys + ys(size(ys, 1):1:-1))
    ys_odd  = 0.5 * (ys - ys(size(ys, 1):1:-1))
  end subroutine

  function k_minimums(k, xs, ys, is_even)
    real :: xs(:), ys(:), k_minimums(k)
    integer :: i, k, n, idx, idx_a, idx_b
    integer, parameter :: idx_pad = 1
    integer, parameter :: step_rate = 2**4
    integer, parameter :: steps = 8.0 * step_rate
    integer, parameter :: bi_steps = 2 * steps - 1
    real,    parameter :: dt = 1.0 / step_rate
    real,    parameter :: x_pad = 0.1
    real :: ys_(nn, bi_steps)
    real, dimension(bi_steps) :: psis_even, psis_odd
    logical :: is_even, mask(size(ys, 1))

    n = size(ys, 1)
    mask = .true.

    do i = 1, k
      idx = minloc(abs(ys), 1, mask)
      idx_a = max(1, idx - idx_pad)
      idx_b = min(n, idx + idx_pad)
      mask(idx_a:idx_b) = .false.
      k_minimums(i) = bisect(f, xs(idx_a) - x_pad, xs(idx_b) + x_pad, tol=1e-12)
    end do

  contains

    function f(x)
      real, intent(in) :: x
      real :: f

      E = x
      call integrate_ode_bidirectional(steps, dt, ys_, y0=[0.0, 1.0, 1.0])
      call separate_even_odd(ys_(2, :), psis_even, psis_odd)
      if (is_even)       f = psis_even(bi_steps)
      if (.not. is_even) f = psis_odd(bi_steps)
    end function
  end function

  function bisect(f, x_min, x_max, tol) result(c)
    !! Find a root of f(x) within interval via bisection search
    procedure(pR2R) :: f
    real, intent(in) :: x_min, x_max, tol
    real :: a, b, c, fa, fb, fc
    real, parameter :: phi = 1.6180339887498948482045868343656381177203091798057
    integer, parameter :: max_iter = 256
    integer :: i

    a = x_min
    b = x_max
    fa = f(a)
    fb = f(b)

    print *, a, b
    if (fa*fb > 0.0) stop "Root not bracketed"

    ! bisect the interval
    do i = 1, max_iter
      c = (a+b)/2.0; fc = f(c)
      if (abs(fc) <= tol) exit
      c = c + (c-a)*sign(1.0,fa-fb)*fc/sqrt(fc*fc-fa*fb); fc = f(c)
      if (fa*fc < 0.0) then; b = c; fb = fc; end if
      if (fc*fb < 0.0) then; a = c; fa = fc; end if
    end do
  end function

  subroutine partial_sort(x, k)
    ! Performs an insertion sort for k entries
    real, intent(inout) :: x(:)
    integer, intent(in), optional :: k
    real :: tmp
    integer :: i, k_, idx

    k_ = size(x)
    if (present(k)) k_ = k

    do i = 1, k_
      idx = minloc(x(i:), 1) + i - 1
      tmp = x(i)
      x(i) = x(idx)
      x(idx) = tmp
    end do
  end subroutine

end module
