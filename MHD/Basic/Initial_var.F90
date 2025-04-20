Module Initial_var
  implicit none
  ! Public parameters
  integer, parameter :: Nx = 50, Ny = 50, Nt = 1000   ! Grid diminsions, plus number of time steps
  real(kind=8), parameter :: Lx = 1.0, Ly = 1.0, dt = 0.001   !Domain size and tme step size
  real(kind=8), parameter :: Re = 100.0, Rm = 100.0   !Reynold numbers and magnetic reynold numbers
  real(kind=8), parameter :: dx = Lx / Nx, dy = Ly / Ny   !Grid spacing
  real(kind=8), parameter :: T= 0.0                       ! Initial Tempeture
  real(kind=8), parameter :: eta = 0.001                  !Eta, or ...
  real(kind=8), parameter :: sigma = 1.0d-21              ! Sigma or...
  contains 
    subroutine velocity_fields(u, v, Bx, By, p)
        real(kind=8), intent(out) :: u(:,:), v(:,:), Bx(:,:), By(:,:), p(:,:)
        integer :: i, j

        do i = 1, size(u, 1)
            do j = 1, size(u, 2)
                u(i, j) = 0.0                    ! Initialize velocity components to zero
                v(i, j) = 0.0
                Bx(i, j) = 0.1 * sin(2.0 * 3.14159 * j / size(u, 2))  ! Magnetic field (x-component)
                By(i, j) = 0.1 * cos(2.0 * 3.14159 * i / size(u, 1))  ! Magnetic field (y-component)
                p(i, j) = 0.0                    ! Initialize pressure to zero
            end do
        end do
    end subroutine velocity_fields
    ! Initialize fields for tempeture 
    subroutine heat_fields(T, eta, sigma)
    real(kind=8), intent(out) :: T(:,:)
    integer :: i, j
      do i = 1, nx
        do j = 1, ny
          ! Initial temperature: Gaussian blob (actual temperature T)
          T(i,j) = exp(-((i*dx-0.5*Lx)**2 + (j*dy-0.5*Ly)**2)/(0.1**2))
          ! Magnetic field: Vary Bx for non-zero Jz
          Bx(i,j) = 0.1 * sin(2*3.14159*i*dx/Lx)
        end do
      end do
    end subroutine heat_fields
End Module
