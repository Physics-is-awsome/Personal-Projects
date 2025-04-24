Module Initial_var
  implicit none
  ! Public parameters
  integer, parameter :: Nx = 50, Ny = 50, Nt = 1000   ! Grid diminsions, plus number of time steps
  real(kind=8), parameter :: Lx = 1.0, Ly = 1.0, dt = 0.001   !Domain size and tme step size
  real(kind=8), parameter :: Re = 100.0, Rm = 100.0   !Reynold numbers and magnetic reynold numbers
  real(kind=8), parameter :: dx = Lx / Nx, dy = Ly / Ny   !Grid spacing
  real(kind=8), allocatable :: T(:,:)                       ! Initial Tempeture
  real(kind=8), parameter :: eta = 0.001                  !Eta, or ...
  real(kind=8), parameter :: sigma = 1.0d-21              ! Sigma or...
  real(kind=8), parameter :: Kappa = 1.0d+5                !Thermal conductivity 
  contains 
    subroutine initialize_variables
          implicit none
          integer :: stat
          if (allocated(T)) deallocate(T)
          allocate(T(Nx, Ny), stat=stat)
          if (stat /= 0) then
              print *, "Error: Failed to allocate T"
              stop
          end if
          T = 0.0d0  ! Initialize to zero before heat_fields
    end subroutine initialize_variables
    subroutine velocity_fields(u, v, Bx, By, p)
        real(kind=8), intent(out) :: u(:,:), v(:,:), By(:,:), Bx(:,:), p(:,:)
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
    subroutine heat_fields(Lx, Ly)
      real(kind=8), intent(in) :: Lx, Ly
      integer :: i, j
      ! Check T allocation
      if (.not. allocated(T)) then
          print *, "Error: T is not allocated in heat_fields"
          stop
      end if
      ! Check dimensions
      if (size(T,1) /= Nx .or. size(T,2) /= Ny) then
          print *, "Error: T has incorrect dimensions"
          stop
      end if
      ! Check parameters
      if (dx <= 0.0d0 .or. dy <= 0.0d0) then
          print *, "Error: dx or dy invalid"
          stop
      end if
      do i = 1, nx
        do j = 1, ny
          ! Initial temperature: Gaussian blob (actual temperature T)
          T(i,j) = exp(-((i*dx-0.5*Lx)**2 + (j*dy-0.5*Ly)**2)/(0.1**2))

        end do
      end do
    end subroutine heat_fields
End Module
