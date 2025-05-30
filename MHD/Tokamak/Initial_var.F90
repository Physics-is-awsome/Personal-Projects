module Initial_var
    implicit none
    integer, parameter :: Nx = 50, Ny = 50, Nt = 1000
    real(kind=8), parameter :: Rmin = 4.0d0, Rmax = 8.0d0, Zmin = -2.0d0, Zmax = 2.0d0  ! m
    real(kind=8), parameter :: Lx = Rmax - Rmin, Ly = Zmax - Zmin
    real(kind=8), parameter :: dx = Lx / (Nx-1), dy = Ly / (Ny-1)
    real(kind=8), parameter :: Re = 100.0, Rm = 100.0  ! Reynolds, magnetic Reynolds
    real(kind=8), parameter :: B0 = 5.0d0, R0 = 6.0d0  ! Toroidal field (T), major radius (m)
    real(kind=8), parameter :: eta = 0.001, sigma = 1.0d-21, Kappa = 1.0d5  ! Resistivity, radiation, conductivity
    real(kind=8), parameter :: mu0 = 4.0d0 * 3.14159d-7  ! Vacuum permeability
    real(kind=8), parameter :: dt_heat = 0.5 * min(dx**2, dy**2) / 1.0d5
    real(kind=8), parameter :: dt = min(0.001d0, dt_heat)  ! Placeholder, to be dynamic
    real(kind=8), allocatable :: T(:,:), rho(:,:)

    contains

    subroutine initialize_variables
        implicit none
        integer :: stat
        if (allocated(T)) deallocate(T)
        if (allocated(rho)) deallocate(rho)
        allocate(T(Nx,Ny), rho(Nx,Ny), stat=stat)
        if (stat /= 0) then
            print *, "Error: Failed to allocate T or rho"
            stop
        end if
        T = 1.0d6  ! Initial temperature ~1 keV
        rho = 1.0d-6  ! Initial density ~10^20 m^-3
    end subroutine initialize_variables

    subroutine velocity_fields(u, v, Bx, By, rho)
        implicit none
        real(kind=8), intent(out) :: u(Nx,Ny), v(Nx,Ny), Bx(Nx,Ny), By(Nx,Ny), rho(Nx,Ny)
        integer :: i, j
        real(kind=8) :: R, Z
        do i = 1, Nx
            do j = 1, Ny
                R = Rmin + (i-1) * dx
                Z = Zmin + (j-1) * dy
                u(i,j) = 0.0d0
                v(i,j) = 0.0d0
                Bx(i,j) = 0.01d0 * Z / sqrt((R-R0)**2 + Z**2 + 0.1)
                By(i,j) = -0.01d0 * (R-R0) / sqrt((R-R0)**2 + Z**2 + 0.1)
                rho(i,j) = 1.0d0 + 0.5d0 * exp(-((R-R0)**2 + Z**2)/0.5d0)
            end do
        end do
    end subroutine velocity_fields

    subroutine heat_fields
        integer :: i, j
        real(kind=8) :: R, Z, a = 2.0d0  ! Minor radius
        if (.not. allocated(T)) then
            print *, "Error: T is not allocated in heat_fields"
            stop
        end if
        do i = 1, Nx
            do j = 1, Ny
                R = Rmin + (i-1) * dx
                Z = Zmin + (j-1) * dy
                T(i,j) = 1.0d6 * exp(-((R-R0)**2 + Z**2)/(0.5*a)**2)  ! Gaussian profile
            end do
        end do
        ! Dirichlet BC: T = 0 at wall (approximate)
        T(1,:) = 0.0d0
        T(Nx,:) = 0.0d0
        T(:,1) = 0.0d0
        T(:,Ny) = 0.0d0
    end subroutine heat_fields
end module Initial_var
