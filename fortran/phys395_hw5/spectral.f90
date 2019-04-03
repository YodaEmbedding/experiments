! spectral.f90  -  Rayleigh iteration solver for eigenvalue problem

program spectral
  use integrator
  use utils
  implicit none

  integer, parameter :: n = 150
  real, parameter :: ell = 1.0
  real, parameter :: pi = 3.1415926535897932384626433832795028842Q0
  real, dimension(n) :: x, theta, psi
  real, dimension (n,n) :: L, H
  real :: lambda
  real :: lambdas(n)

  ! Initialize spectral operators
  call init_grid()
  call init_l()

  print *
  print "(a)", "Q4. Spectral-based eigenvalues (harmonic potential)"
  call find_eigenvalues()
  print "(f19.12)", lambdas(1:10)
  call plot_wavefunctions("q4", lambdas(1:10))

  V => V_anharmonic
  print *
  print "(a)", "Q5. Spectral-based eigenvalues (anharmonic potential)"
  call find_eigenvalues()
  print "(f19.12)", lambdas(1:10)
  call plot_wavefunctions("q5", lambdas(1:10))

contains

  subroutine find_eigenvalues()
    ! TODO properly encapsulate local variables
    integer :: i

    ! Hamiltonian in static Schrödinger equation
    H = -L/2.0
    do i = 1, n
      H(i,i) = -L(i,i)/2.0 + V(x(i))
    end do

    psi = 1.0 / (1.0 + x*x)

    ! Relax using Rayleigh's iteration
    do i = 1,64
      lambda = dot_product(psi,matmul(H,psi))/dot_product(psi,psi)
      psi = lsolve(psi,lambda)
      psi = psi/sqrt(dot_product(psi,psi))
    end do

    lambdas = real_eigenvalues(H)
    call partial_sort(lambdas)
  end subroutine

  ! initialize the collocation grid
  subroutine init_grid()
    integer i

    forall (i=1:n) theta(i) = pi*(n-i+0.5)/n
    x = ell/tan(theta)
  end subroutine

  ! evaluate rational Chebyshev basis on a grid theta
  subroutine evalb(n, pts, theta, Tn, Tnx, Tnxx)
    integer n, pts
    real, dimension(pts), intent(in) :: theta
    real, dimension(pts), intent(out), optional :: Tn, Tnx, Tnxx

    ! Chebyshev basis and its derivatives
    if (present(Tn))   Tn = cos(n*theta)
    if (present(Tnx))  Tnx = n * sin(n*theta) * sin(theta)**2/ell
    if (present(Tnxx)) Tnxx = -n * sin(theta)**3 * &
      (n*cos(n*theta)*sin(theta) + 2.0*sin(n*theta)*cos(theta)) / ell**2
  end subroutine evalb

  ! initialize linear spectral derivative operator
  subroutine init_l()
    integer :: i, pivot(n), status
    real :: A(n,n), B(n,n), Tn(n), Tnxx(n)

    ! evaluate basis and differential operator values on collocation grid
    do i = 1,n
      call evalb(i-1, n, theta, Tn=Tn, Tnxx=Tnxx)
      A(i, :) = Tn
      B(i, :) = Tnxx
    end do

    ! find linear operator matrix
    status = 0; select case (kind(A))
    case(4); call sgesv(n, n, A, n, pivot, B, n, status)
    case(8); call dgesv(n, n, A, n, pivot, B, n, status)
    case default; call abort
    end select

    ! bail at first sign of trouble
    if (status /= 0) call abort

    L = transpose(B)
  end subroutine

  ! Rayleigh's iteration solving eigenvalue problem:
  ! [-1/2 d^2/dx^2 + V(x)] psi(x) = lambda psi(x)
  function lsolve(psi, lambda)
    real lambda, psi(n), lsolve(n), A(n,n), B(n)
    integer i, pivot(n), status

    ! linear improvement inflating eigenvector
    A = H
    forall (i=1:n) A(i,i) = H(i,i) - lambda
    B = psi

    ! find linear operator matrix
    status = 0; select case (kind(A))
    case(4); call sgesv(n, 1, A, n, pivot, B, n, status)
    case(8); call dgesv(n, 1, A, n, pivot, B, n, status)
    case default; call abort
    end select

    ! bail at first sign of trouble
    if (status /= 0) call abort

    lsolve = B
  end function

end program
