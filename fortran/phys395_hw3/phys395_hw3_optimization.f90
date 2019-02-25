! Compile and run: make

program phys395_hw3_optimization
  use q1_roots
  use q3_minima
  use q5_fit
  implicit none

  print *
  call q1()
  call q3()
  call q5()
end program phys395_hw3_optimization
