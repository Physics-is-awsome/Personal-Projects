Module Initial_var
  implicit none
  contains 
    subroutine initialize_fields(u, v, Bx, By, p)
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
    end subroutine initialize_fields
    subroutine Parameters(Nx, Ny, Nt, Lx, Ly, dt, Re, R,, dx, dy, u(Nx, Ny), v(Nx, Ny), Bx(Nx, Ny), By(Nx, Ny), p(Nx, Ny), Jz(Nx, Ny), Jy(Nx, Ny), u_new(Nx, Ny), v_new(Nx, Ny), Bx_new(Nx, Ny), By_new(Nx, Ny)
      ! Parameters
      integer, parameter :: Nx = 50, Ny = 50           ! Grid dimensions
      integer, parameter :: Nt = 1000                  ! Number of time steps
        real(kind=8), parameter :: Lx = 1.0, Ly = 1.0    ! Domain size
        real(kind=8), parameter :: dt = 0.001            ! Time step size
        (kind=8), parameter :: Re = 100.0            ! Reynolds number
        real(kind=8), parameter :: Rm = 100.0            ! Magnetic Reynolds number
        real(kind=8), parameter :: dx = Lx / Nx          ! Grid spacing in x
        real(kind=8), parameter :: dy = Ly / Ny          ! Grid spacing in y

        ! Field arrays
        real(kind=8) :: u(Nx, Ny), v(Nx, Ny)             ! Velocity components
        real(kind=8) :: Bx(Nx, Ny), By(Nx, Ny)           ! Magnetic field components
        real(kind=8) :: p(Nx, Ny)                        ! Pressure field
        real(kind=8) :: Jx(Nx, Ny), Jy(Nx, Ny)           ! Current density
        real(kind=8) :: u_new(Nx, Ny), v_new(Nx, Ny)     ! Updated velocity components
        real(kind=8) :: Bx_new(Nx, Ny), By_new(Nx, Ny)   ! Updated magnetic field components

        integer :: n                                      ! Time step counter
    end subroutine Parameters
