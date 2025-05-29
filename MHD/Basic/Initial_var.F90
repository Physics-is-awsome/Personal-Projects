module Initial_var
    implicit none
    integer, parameter :: Nx = 50, Ny = 50, Nt = 1000
    real(kind=8), parameter :: Lx = 1.0, Ly = 1.0
    real(kind=8), parameter :: Re = 100.0, Rm = 100.0
    real(kind=8), parameter :: dx = Lx / Nx, dy = Ly / Ny
    real(kind=8), parameter :: dt_heat = 0.5 * min(dx**2, dy**2) / 1.0d5
    real(kind=8), parameter :: dt = min(0.001d0, dt_heat)
    real(kind=8), parameter :: eta = 0.001, sigma = 1.0d-21, Kappa = 1.0d5
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
        T = 0.0d0
        rho = 1.0d0
    end subroutine initialize_variables

    subroutine velocity_fields(u, v, Bx, By, p)
        real(kind=8), intent(out) :: u(:,:), v(:,:), Bx(:,:), By(:,:), p(:,:)
        integer :: i, j

        do i = 1, size(u, 1)
            do j = 1, size(u, 2)
                u(i,j) = 0.0
                v(i,j) = 0.0
                Bx(i,j) = 0.1 * sin(2.0 * 3.14159 * j * dy / Ly)
                By(i,j) = 0.1 * cos(2.0 * 3.14159 * i * dx / Lx)
                p(i,j) = 0.0
            end do
        end do
    end subroutine velocity_fields

    subroutine heat_fields(Lx, Ly)
        real(kind=8), intent(in) :: Lx, Ly
        integer :: i, j
        if (.not. allocated(T)) then
            print *, "Error: T is not allocated in heat_fields"
            stop
        end if
        if (size(T,1) /= Nx .or. size(T,2) /= Ny) then
            print *, "Error: T has incorrect dimensions"
            stop
        end if
        if (dx <= 0.0d0 .or. dy <= 0.0d0) then
            print *, "Error: dx or dy invalid"
            stop
        end if
        do i = 1, Nx
            do j = 1, Ny
                T(i,j) = exp(-((i*dx-0.5*Lx)**2 + (j*dy-0.5*Ly)**2)/(0.1**2))
            end do
        end do
        ! Apply Dirichlet boundary conditions for temperature (T = 0 at boundaries)
        T(1,:) = 0.0d0
        T(Nx,:) = 0.0d0
        T(:,1) = 0.0d0
        T(:,Ny) = 0.0d0
    end subroutine heat_fields
end module Initial_var
