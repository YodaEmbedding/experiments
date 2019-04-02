module utils
  implicit none
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

end module
