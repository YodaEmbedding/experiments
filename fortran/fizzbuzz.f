      program fizzbuzz
        character(len=8) :: fizz_str
        do i = 1, 100
          fizz_str = ''
          if (mod(i, 15) == 0) then
            fizz_str = 'fizzbuzz'
          else if (mod(i, 3) == 0) then
            fizz_str = 'fizz'
          else if (mod(i, 5) == 0) then
            fizz_str = 'buzz'
          endif
          print '(i3, a9)', i, fizz_str
        enddo
      end program fizzbuzz
