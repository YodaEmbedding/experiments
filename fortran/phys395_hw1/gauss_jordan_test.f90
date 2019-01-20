! Compile and run:  make run_gauss_jordan_test

program gauss_jordan_test
  use gauss_jordan
  implicit none

  real, dimension(3, 3) :: A
  real, dimension(3) :: y, x, x_exp
  real, dimension(size(A, 1) + 1, size(A, 2)) :: Ay

  A(1, :) = (/  0,  2,  1 /)
  A(2, :) = (/  1, -2, -3 /)
  A(3, :) = (/ -1,  1,  2 /)
  y       = (/ -8,  0,  3 /)
  x_exp   = (/ -4, -5,  2 /)

  print *
  print "(a)", "Solving matrix equation Ax = y using Gauss-Jordan..."
  print *

  print "(a)", "A:"
  print "(3f6.1)", transpose(A)
  print *

  print "(a)", "y:"
  print "(3f6.1)", y
  print *

  Ay(1:size(A, 1), :) = transpose(A)
  Ay(size(Ay, 1), :) = y
  print "(a)", "Augmented [A | y]:"
  print "(4f6.1)", Ay
  print *

  Ay = upper_triangular(Ay)
  print "(a)", "Upper triangular form:"
  print "(4f6.1)", Ay
  print *

  Ay = substitution(Ay)
  print "(a)", "Substitution:"
  print "(4f6.1)", Ay
  print *

  x = solve(A, y)
  print "(a)", "Answer:"
  print "(3f6.1)", x
  print *

  print "(a)", "Expected answer:"
  print "(3f6.1)", x_exp
  print *
end program gauss_jordan_test
