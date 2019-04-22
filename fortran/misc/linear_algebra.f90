program linear_algebra
  implicit none

  real, dimension(3, 3) :: A
  real, dimension(3) :: x, y

  A(1, :) = (/  0,  2,  1 /)
  A(2, :) = (/  1, -2, -3 /)
  A(3, :) = (/ -1,  1,  2 /)

  x       = (/ -4, -5,  2 /)

  ! y     = (/ -8,  0,  3 /)
  y = matmul(A, x)

  print "(a)", "A:"
  print "(3f6.1)", transpose(A)
  print *

  print "(a)", "x:"
  print "(3f6.1)", x
  print *

  print "(a)", "y:"
  print "(3f6.1)", y
  print *
end program linear_algebra
