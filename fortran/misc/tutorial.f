      program tutorial
        implicit none

        ! character (len = 20) :: f_name, l_name
        ! print *, 'What's your name? '
        ! read *, f_name, l_name
        ! print *, 'Hello ', trim(l_name), ', ', trim(f_name)

        real, parameter :: PI = 3.14159265
        character (len = 20) :: s
        integer, dimension(5) :: nums
        integer, dimension(5, 5) :: mat
        integer, dimension(:), allocatable :: vararray
        integer :: n
        logical :: y = .true.
        integer :: i, j

        nums = (/1, 1, 2, 3, 5/)
        write(s, '(i0)') 666
        print '(5i2, 2i4, "; best number: ", a)', nums, 8, 13, s
        print '(3i2)', (nums(n), n=3,5)
        print '(3i2)', nums(3:5)
        print '(4i2)', size(mat), rank(mat), shape(mat)

        allocate(vararray(1:5))
        vararray(1:3) = nums(3:5)
        print '(5i2)', vararray

        do i = 0, 4
          j = 5 * i
          mat(:, i + 1) = (/j, j+1, j+2, j+3, j+4/)
        enddo

        print '(/, 25i4)', mat(::2, ::2)
        print '(5i4)', transpose(mat)

        print 100,
     &    mat(:, 1) * mat(:, 1),
     &    mat(:, 1) + mat(:, 1)
100     format(/, 'x*x: ', 5i3, /, '2*x: ', 5i3)

      end program tutorial
