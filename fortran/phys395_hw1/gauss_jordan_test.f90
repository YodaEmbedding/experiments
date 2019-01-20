program gauss_jordan_test
  use gauss_jordan
  implicit none

  real, dimension(3, 3) :: A
  real, dimension(3) :: y, x, x_exp

  A(1, :) = (/  0,  2,  1 /)
  A(2, :) = (/  1, -2, -3 /)
  A(3, :) = (/ -1,  1,  2 /)
  y       = (/ -8,  0,  3 /)
  x_exp   = (/ -4, -5,  2 /)

  print "(a)", "A:"
  print "(3f6.1)", transpose(A)
  print *

  print "(a)", "y:"
  print "(3f6.1)", y
  print *

  x = solve(A, y)

  print "(a)", "Answer:"
  print "(3f6.1)", x
  print *

  print "(a)", "Expected answer:"
  print "(3f6.1)", x_exp
  print *
end program gauss_jordan_test
