module utils
  ! use integrator
  implicit none

  interface
    function pR2R(x)
      !! Real -> Real
      real, intent(in) :: x
      real :: pR2R
    end function pR2R
  end interface

  ! integer, private, parameter :: nn = 3
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

  function str(x, num_fmt)
    real, intent(in) :: x
    character(len=*) :: num_fmt
    character(len=32) :: str

    write (str, num_fmt) x
    str = adjustl(str)
  end function str

  function newton(f, df, x0, tol) result(x)
    !! Find root of f(x) via Newton's method
    procedure(pR2R) :: f, df
    real, intent(in) :: x0, tol
    real :: x, y

    x = x0
    y = f(x)

    do while (tol < abs(y))
      x = x - y / df(x)
      y = f(x)
    end do
  end function

  function golden(f, x_min, x_max, tol)
    !! Find a minimum of f(x) within interval via golden section search
    !! This implementation doubles the necessary calls to f; but it is simpler
    procedure(pR2R) :: f
    real, intent(in) :: x_min, x_max, tol
    real :: golden, a, b, c, d
    real, parameter :: phi = 1.6180339887498948482045868343656381177203091798057

    a = x_min
    b = x_max
    c = b - (b - a) / phi
    d = a + (b - a) / phi

    do while (tol < abs(c - d))
      if (f(c) < f(d)) then
        b = d
      else
        a = c
      end if

      c = b - (b - a) / phi
      d = a + (b - a) / phi
    end do

    golden = 0.5 * (b + a)
  end function

end module
