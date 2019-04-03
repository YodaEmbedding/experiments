module utils
  use integrator
  implicit none

  integer, private, parameter :: nn = 3
  character(len=*), parameter :: img_fileext = ".png"
contains

  subroutine write_csv(filename, mat, header, num_fmt)
    !! Output a csv file with plottable data
    integer, parameter :: ofh = 2
    real :: mat(:, :)
    character(len=*) :: filename
    character(len=*), optional :: num_fmt, header
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
    if (present(header)) write(ofh, *) header
    do i = 1, size(mat, 2)
      write(ofh, fmt_str) mat(:, i)
    end do
    close(ofh)
  end subroutine

  subroutine plot_wavefunctions(suffix, lambdas)
    integer, parameter :: step_rate = 2**6
    real, parameter :: dt = 1.0 / step_rate
    integer, parameter :: steps = 4.0 * step_rate, bi_steps = 2 * steps - 1
    real :: ys(nn, bi_steps), y0(nn), lambdas(:), results(11, bi_steps)
    real :: N
    character(len=*) :: suffix
    integer :: i

    do i = 1, size(lambdas, 1)
      E = lambdas(i)
      if (mod(i - 1, 2) == 0) y0 = [0.0, 1.0, 0.0]
      if (mod(i - 1, 2) == 1) y0 = [0.0, 0.0, 1.0]
      call integrate_ode_bidirectional(steps, dt, ys, y0)
      N = integrate_normalization(dt, ys(2, :))
      results(i + 1, :) = ys(2, :) / sqrt(N)
    end do

    results(1, :) = ys(1, :)

    ! TODO specific energy labels? (construct string)
    call write_csv("results/" // suffix // "_wavefunctions.csv", results, &
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
      &results/" // suffix // "_wavefunctions.csv &
      &plots/"    // suffix // "_wavefunctions" // img_fileext)

    results(2:, :) = results(2:, :)**2

    call write_csv("results/" // suffix // "_probability.csv", results, &
      "$x$, &
      &$|\psi(x; E_1)|^2$, &
      &$|\psi(x; E_2)|^2$, &
      &$|\psi(x; E_3)|^2$, &
      &$|\psi(x; E_4)|^2$, &
      &$|\psi(x; E_5)|^2$, &
      &$|\psi(x; E_6)|^2$, &
      &$|\psi(x; E_7)|^2$, &
      &$|\psi(x; E_8)|^2$, &
      &$|\psi(x; E_9)|^2$, &
      &$|\psi(x; E_{10})|^2$")

    call execute_command_line("python plot.py --time-series &
      &--title 'Probability density functions for various energy eigenvalues' &
      &results/" // suffix // "_probability.csv &
      &plots/"    // suffix // "_probability" // img_fileext)
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

  pure function integrate_normalization(dt, psis) result(N)
    !! Integrate via Reimann sum
    real, intent(in) :: dt, psis(:)
    real :: N

    N = dt * sum(psis * psis)
  end function

  subroutine separate_even_odd(ys, ys_even, ys_odd)
    real, dimension(:) :: ys, ys_even, ys_odd
    ys_even = 0.5 * (ys + ys(size(ys, 1):1:-1))
    ys_odd  = 0.5 * (ys - ys(size(ys, 1):1:-1))
  end subroutine

  function zero_crossings(k, ys) result(zcs)
    !! Finds indexes of first k zero crossings
    real :: ys(:)
    logical :: sign_prev, sign_next
    integer :: i, j, k, zcs(k)

    zcs = -1
    sign_prev = ys(1) >= 0
    j = 1

    do i = 2, size(ys, 1)
      if (j > k) exit
      sign_next = ys(i) >= 0
      if (sign_prev .neqv. sign_next) then
        sign_prev = sign_next
        zcs(j) = i
        j = j + 1
      end if
    end do
  end function

  function k_zeros(k, f, xs, ys)
    procedure(pR2R) :: f
    real :: xs(:), ys(:), k_zeros(k)
    integer :: i, k, n, idx, idx_a, idx_b, zcs(k)
    integer, parameter :: idx_pad = 1
    real,    parameter :: x_pad = 0.001

    n = size(ys, 1)
    zcs = zero_crossings(k, ys)

    do i = 1, k
      idx = zcs(i)
      if (idx == -1) stop "Insufficient zero crossings"
      idx_a = max(1, idx - idx_pad - 1)
      idx_b = min(n, idx + idx_pad)
      k_zeros(i) = bisect(f, xs(idx_a) - x_pad, xs(idx_b) + x_pad, tol=1e-12)
    end do
  end function

  function bisect(f, x_min, x_max, tol) result(c)
    !! Find a root of f(x) within interval via bisection search
    procedure(pR2R) :: f
    real, intent(in) :: x_min, x_max, tol
    real :: a, b, c, fa, fb, fc
    real, parameter :: phi = 1.6180339887498948482045868343656381177203091798057
    integer, parameter :: max_iter = 32
    integer :: i

    a = x_min
    b = x_max
    fa = f(a)
    fb = f(b)

    if (fa*fb > 0.0) stop "Root not bracketed"

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

  function eigenvalues(A)
    real, intent(in) :: A(:, :)
    real, dimension(size(A, 1)) :: WR, WI
    real, dimension(size(A, 1), size(A, 1)) :: VL, VR
    real :: eigenvalues(2, size(A, 1))
    real :: work(6 * size(A, 1))
    integer :: n, stat

    if (size(A, 1) /= size(A, 2)) call abort
    n = size(A, 1)

    call dgeev('N', 'N', n, A, n, WR, WI, &
      VL, n, VR, n, &
      work, size(work, 1), stat)

    eigenvalues(1, :) = WR
    eigenvalues(2, :) = WI
  end function

  function real_eigenvalues(A)
    real, intent(in) :: A(:, :)
    real :: real_eigenvalues(size(A, 1))
    real :: e_(2, size(A, 1))

    e_ = eigenvalues(A)
    real_eigenvalues = e_(1, :)
  end function

end module
