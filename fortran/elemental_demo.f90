! gfortran elemental_demo.f90 && ./a.out

program elemental_demo
  real, dimension(3) :: x, y

  x = [1.0, 2.0, 3.0]
  y = f(x)
  print *, x
  print *, y

contains

  elemental function f(x)
    real, intent(in) :: x
    real :: f

    f = x**3 -  x + 0.25
  end function
end program elemental_demo
