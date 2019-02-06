! Compile and run: make

program phys395_hw2_curve_fitting
  use gauss_jordan
  implicit none

  integer, parameter :: ifh = 1, ofh = 2
  integer, parameter :: lines = 9130  ! TODO: allow arbitrary lengths
  character(len=*), parameter :: filename = "results_.csv"
  integer :: a, i, j, k, stat
  real :: x, y
  real, dimension(lines) :: xs, ys, ys_fit
  integer, parameter :: n = 3
  real, parameter :: eps = 1.0e-6
  real, dimension(n + 1, lines) :: bax
  real, dimension(n + 1, n + 1) :: B, B_, B_inv, U, U_inv, Vh, Vh_inv
  real, dimension(n + 1) :: coeffs, p, s
  character(len=64) :: fmt_str

  open(unit=ifh, file="data.dat", action="read", status="old", &
    access="sequential", form="formatted")

  ! TODO another idea is to just add up/compute as we read through the file...
  do i = 1, lines
    read(ifh, *, iostat=stat) x, y
    xs(i) = x
    ys(i) = y
    if (stat < 0) exit
  end do

  forall (i=1:lines, a=0:n) bax(a + 1, i) = basis(a, xs(i))
  forall (j=1:n+1, k=1:n+1) B(j, k) = sum(bax(j, :) * bax(k, :))
  p = matmul(bax, ys)

  B_ = B
  call svd(n + 1, B_, U, s, Vh)

  ! call solve_svd(n + 1, B, p, U, s, Vh)  ! TODO
  ! coeffs = solve(B, p)  ! TODO probably use the lapack instead of our own Gauss-Jordan... actually it doesn't matter
  ! B_ = B
  ! coeffs = p
  ! call solve_lss(n + 1, B_, coeffs, -1.0)

  U_inv = transpose(U)
  Vh_inv = transpose(Vh)

  B_inv = U_inv
  where (S > eps * S(1)); B_inv = B_inv / S; elsewhere; B_inv = 0.0; end where  ! TODO column/row indexing?
  B_inv = matmul (((
  B_inv = matmul(Vh_inv, matmul(S_inv, U_inv))
  coeffs = matmul(Vh_inv, matmul(S_inv, matmul(U_inv, p)))
  ! coeffs = matmul(B_inv, s)

  write(fmt_str, "(a, i10, a)") "(", n + 1, "f10.2)"
  print *, "B"
  print fmt_str, B
  print *, "B_inv"
  print fmt_str, B_inv
  print *, "p"
  print fmt_str, p
  print *, "coeffs"
  print fmt_str, coeffs
  print *, "U"
  print fmt_str, U
  print *, "s"
  print fmt_str, s
  print *, "Vh"
  print fmt_str, Vh

  open(unit=ofh, file=filename, action="write", status="replace")
  write(ofh, "(a)") "testing"
  close(ofh)

contains

  !! minimize |A.x - B|^2 using LAPACK canned SVD routine
  !! A gets destroyed, the answer is returned in B
  !! rcond determines the effective rank of A as described in LAPACK docs.
  subroutine solve_lss(n, A, B, rcond)
    integer :: n
    real :: A(n, n), B(n), S(n), work(6 * n), rcond
    integer :: rank, stat

    stat = 0 ! TODO removable?
    call dgelss(n, n, 1, A, n, B, n, S, rcond, rank, work, 6 * n, stat)
    if (stat /= 0) call abort
  end subroutine

  !! TODO
  subroutine svd(n, A, U, S, Vh)
    integer :: n, stat
    real :: A(n, n), U(n, n), Vh(n, n), S(n), work(6 * n)

    stat = 0 ! TODO removable?
    call dgesvd('A', 'A', n, n, A, n, S, U, n, Vh, n, work, 6 * n, stat)
    if (stat /= 0) call abort
  end subroutine

  !! An even basis... from a more civilized age
  elemental function basis(a, x)
    integer, intent(in) :: a
    real, intent(in) :: x
    real :: basis
    real, parameter :: pi = 3.14159265358979323846264338327950288419716939937510

    basis = cos(2 * pi * a * x)
  end function

  !! Return n evenly spaced points within interval [a, b]
  pure function linspace(a, b, n)
    real, intent(in) :: a, b
    integer, intent(in) :: n
    real, dimension(n) :: linspace
    integer :: i

    linspace = (/(a + (b-a) * i / (n-1), i=0,n-1)/)
  end function

end program phys395_hw2_curve_fitting

! TODO ew n + 1 everywhere
! TODO Good fit: chi^2 = number of points - number of params fit
! (Chi-square should be on average 1, since sigma is 1)
