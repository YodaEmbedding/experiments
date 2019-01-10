program fibonacci
  implicit none
  integer(kind=8) :: i, a = 0, b = 1, c

  do i = 1, 50
    c = a + b
    a = b
    b = c
    print *, a
  enddo
end program fibonacci
