module mhd_module
    use Initial_var
    implicit none
    contains

    !============================================================
    ! Subroutine: Compute the current density (J = curl(B))
    !============================================================
    subroutine compute_current(Bx, By, Jz)
        real(kind=8), intent(in) :: Bx(:,:), By(:,:)
        real(kind=8), intent(out) :: Jz(:,:)
        real(kind=8), parameter :: Mu_in = 7.9577D+5
        integer :: i, j

        do i = 2, size(Bx, 1) - 1
            do j = 2, size(Bx, 2) - 1
                Jz(i,j) = ((By(i+1,j) - By(i-1,j)) / (2.0 * dx) - &
                           (Bx(i,j+1) - Bx(i,j-1)) / (2.0 * dy)) / Mu_in
            end do
        end do
        ! Set boundary values for Jz (e.g., zero at boundaries)
        Jz(1,:) = 0.0d0
        Jz(Nx,:) = 0.0d0
        Jz(:,1) = 0.0d0
        Jz(:,Ny) = 0.0d0
    end subroutine compute_current

    !============================================================
    ! Subroutine: Update velocity field using the Navier-Stokes equation
    !============================================================
    subroutine update_velocity(u, v, Jz, Bx, By, p, u_new, v_new)
        real(kind=8), intent(in) :: u(:,:), v(:,:), Jz(:,:), Bx(:,:), By(:,:), p(:,:)
        real(kind=8), intent(out) :: u_new(:,:), v_new(:,:)
        real(kind=8) :: dpdx, dpdy
        integer :: i, j

        do i = 2, size(u, 1) - 1
            do j = 2, size(u, 2) - 1
                dpdx = (p(i+1,j) - p(i-1,j)) / (2.0 * dx)
                dpdy = (p(i,j+1) - p(i,j-1)) / (2.0 * dy)
                u_new(i,j) = u(i,j) + dt * ( &
                    -(u(i,j) * (u(i+1,j) - u(i-1,j)) / (2.0 * dx) + &
                      v(i,j) * (u(i,j+1) - u(i,j-1)) / (2.0 * dy)) + &
                    (1.0 / Re) * ((u(i+1,j) - 2.0 * u(i,j) + u(i-1,j)) / dx**2 + &
                                  (u(i,j+1) - 2.0 * u(i,j) + u(i,j-1)) / dy**2) - &
                    dpdx + Jz(i,j) * By(i,j))
                v_new(i,j) = v(i,j) + dt * ( &
                    -(u(i,j) * (v(i+1,j) - v(i-1,j)) / (2.0 * dx) + &
                      v(i,j) * (v(i,j+1) - v(i,j-1)) / (2.0 * dy)) + &
                    (1.0 / Re) * ((v(i+1,j) - 2.0 * v(i,j) + v(i-1,j)) / dx**2 + &
                                  (v(i,j+1) - 2.0 * v(i,j) + v(i,j-1)) / dy**2) - &
                    dpdy - Jz(i,j) * Bx(i,j))
            end do
        end do
        ! Apply no-slip boundary conditions for velocity
        u_new(1,:) = 0.0d0
        u_new(Nx,:) = 0.0d0
        u_new(:,1) = 0.0d0
        u_new(:,Ny) = 0.0d0
        v_new(1,:) = 0.0d0
        v_new(Nx,:) = 0.0d0
        v_new(:,1) = 0.0d0
        v_new(:,Ny) = 0.0d0
    end subroutine update_velocity

    !============================================================
    ! Subroutine: Solve heat transport equation
    !============================================================
    subroutine solve_heat_equation(Jz, Bx, By, T_new)
        use Initial_var
        implicit none
        real(kind=8), intent(in) :: Jz(Nx,Ny), Bx(Nx,Ny), By(Nx,Ny)
        real(kind=8), intent(out) :: T_new(Nx,Ny)
        real(kind=8) :: Bmag, bx, by, dTdx, dTdy, q_parallel, Q, rad
        real(kind=8) :: eta_local
        real(kind=8), parameter :: T_ref = 1.0d6
        integer :: i, j

        T_new = T

        do i = 2, Nx-1
            do j = 2, Ny-1
                Bmag = sqrt(Bx(i,j)**2 + By(i,j)**2)
                if (Bmag > 1.0d-10) then
                    bx = Bx(i,j) / Bmag
                    by = By(i,j) / Bmag
                else
                    bx = 0.0d0
                    by = 0.0d0
                endif

                dTdx = (T(i+1,j) - T(i-1,j)) / (2.0 * dx)
                dTdy = (T(i,j+1) - T(i,j-1)) / (2.0 * dy)

                q_parallel = Kappa * ( &
                    bx * bx * (T(i+1,j) - 2*T(i,j) + T(i-1,j)) / dx**2 + &
                    by * by * (T(i,j+1) - 2*T(i,j) + T(i,j-1)) / dy**2 + &
                    bx * by * ((T(i+1,j+1) - T(i+1,j-1) - T(i-1,j+1) + T(i-1,j-1)) / (4.0 * dx * dy)))

                eta_local = eta * max(T(i,j) / T_ref, 0.1d0)**(-1.5d0)
                Q = eta_local * Jz(i,j)**2
                rad = -sigma * T(i,j)**4

                T_new(i,j) = T(i,j) + dt * (q_parallel + Q + rad)
                if (T_new(i,j) < 0.0d0) T_new(i,j) = 0.0d0
            end do
        end do

        ! Apply Dirichlet boundary conditions (copy from initial T)
        T_new(1,:) = T(1,:)
        T_new(Nx,:) = T(Nx,:)
        T_new(:,1) = T(:,1)
        T_new(:,Ny) = T(:,Ny)
    end subroutine solve_heat_equation

    !============================================================
    ! Subroutine: Compute pressure from temperature and density
    !============================================================
    subroutine compute_pressure(rho_in, p)
        use Initial_var
        implicit none
        real(kind=8), intent(in) :: rho_in(Nx,Ny)
        real(kind=8), intent(out) :: p(Nx,Ny)
        real(kind=8), parameter :: gamma = 5.0d0 / 3.0d0
        real(kind=8), parameter :: cv = 1.0d0
        integer :: i, j

        do i = 1, Nx
            do j = 1, Ny
                p(i,j) = (gamma - 1.0d0) * rho_in(i,j) * cv * T(i,j)
                if (p(i,j) < 0.0d0) p(i,j) = 0.0d0
            end do
        end do
    end subroutine compute_pressure

    !============================================================
    ! Subroutine: Update density (continuity equation)
    !============================================================
    subroutine update_density(rho_in, u, v, rho_new)
        use Initial_var
        implicit none
        real(kind=8), intent(in) :: rho_in(Nx,Ny), u(Nx,Ny), v(Nx,Ny)
        real(kind=8), intent(out) :: rho_new(Nx,Ny)
        integer :: i, j

        rho_new = Initial_var%rho

        do i = 2, Nx-1
            do j = 2, Ny-1
                rho_new(i,j) = rho_in(i,j) - dt * ( &
                    u(i,j) * (rho_in(i+1,j) - rho_in(i-1,j)) / (2.0 * dx) + &
                    v(i,j) * (rho_in(i,j+1) - rho_in(i,j-1)) / (2.0 * dy) + &
                    rho_in(i,j) * ((u(i+1,j) - u(i-1,j)) / (2.0 * dx) + &
                                (v(i,j+1) - v(i,j-1)) / (2.0 * dy)))
                if (rho_new(i,j) < 0.0d0) rho_new(i,j) = 0.1d0
            end do
        end do

        ! Apply boundary conditions (copy from initial rho)
        rho_new(1,:) = Initial_var%rho(1,:)
        rho_new(Nx,:) = Initial_var%rho(Nx,:)
        rho_new(:,1) = Initial_var%rho(:,1)
        rho_new(:,Ny) = Initial_var%rho(:,Ny)
    end subroutine update_density

    !============================================================
    ! Subroutine: Update magnetic field using induction equation
    !============================================================
    subroutine update_magnetic_field(Bx, By, u, v, Bx_new, By_new)
        use Initial_var
        implicit none
        real(kind=8), intent(in) :: Bx(Nx,Ny), By(Nx,Ny), u(Nx,Ny), v(Nx,Ny)
        real(kind=8), intent(out) :: Bx_new(Nx,Ny), By_new(Nx,Ny)
        real(kind=8) :: eta_local
        real(kind=8), parameter :: T_ref = 1.0d6
        integer :: i, j

        do i = 2, Nx-1
            do j = 2, Ny-1
                eta_local = eta * max(T(i,j) / T_ref, 0.1d0)**(-1.5d0)
                Bx_new(i,j) = Bx(i,j) + dt * ( &
                    -(u(i,j) * (Bx(i+1,j) - Bx(i-1,j)) / (2.0 * dx) + &
                      v(i,j) * (Bx(i,j+1) - Bx(i,j-1)) / (2.0 * dy)) + &
                    eta_local * ((Bx(i+1,j) - 2.0 * Bx(i,j) + Bx(i-1,j)) / dx**2 + &
                                 (Bx(i,j+1) - 2.0 * Bx(i,j) + Bx(i,j-1)) / dy**2))
                By_new(i,j) = By(i,j) + dt * ( &
                    -(u(i,j) * (By(i+1,j) - By(i-1,j)) / (2.0 * dx) + &
                      v(i,j) * (By(i,j+1) - By(i,j-1)) / (2.0 * dy)) + &
                    eta_local * ((By(i+1,j) - 2.0 * By(i,j) + By(i-1,j)) / dx**2 + &
                                 (By(i,j+1) - 2.0 * By(i,j) + By(i,j-1)) / dy**2))
            end do
        end do
        ! Apply periodic boundary conditions for magnetic field
        Bx_new(1,:) = Bx_new(Nx-1,:)
        Bx_new(Nx,:) = Bx_new(2,:)
        Bx_new(:,1) = Bx_new(:,Ny-1)
        Bx_new(:,Ny) = Bx_new(:,2)
        By_new(1,:) = By_new(Nx-1,:)
        By_new(Nx,:) = By_new(2,:)
        By_new(:,1) = By_new(:,Ny-1)
        By_new(:,Ny) = By_new(:,2)
    end subroutine update_magnetic_field
end module mhd_module
