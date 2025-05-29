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
    end subroutine update_velocity

    !============================================================
    ! Subroutine: Solve heat transport equation
    !============================================================
    subroutine solve_heat_equation(Jz, Bx, By, T_new)
        use Initial_var
        implicit none
        real(kind=8), intent(in) :: Jz(nx,ny), Bx(nx,ny), By(nx,ny)
        real(kind=8), intent(out) :: T_new(nx,ny)
        real(kind=8) :: Bmag, bx, by, dTdx, dTdy, q_parallel, Q, rad
        real(kind=8) :: eta_local
        real(kind=8), parameter :: T_ref = 1.0d6
        integer :: i, j

        T_new = T

        do i = 2, nx-1
            do j = 2, ny-1
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

        T_new(1,:) = T(1,:)
        T_new(nx,:) = T(nx,:)
        T_new(:,1) = T(:,1)
        T_new(:,ny) = T(:,ny)
    end subroutine solve_heat_equation

    !============================================================
    ! Subroutine: Compute pressure from temperature and density
    !============================================================
    subroutine compute_pressure(rho, p)
        use Initial_var
        implicit none
        real(kind=8), intent(in) :: rho(nx,ny)
        real(kind=8), intent(out) :: p(nx,ny)
        real(kind=8), parameter :: gamma = 5.0d0 / 3.0d0
        real(kind=8), parameter :: cv = 1.0d0
        integer :: i, j

        do i = 1, nx
            do j = 1, ny
                p(i,j) = (gamma - 1.0d0) * rho(i,j) * cv * T(i,j)
            end do
        end do
    end subroutine compute_pressure

    !============================================================
    ! Subroutine: Update density (continuity equation)
    !============================================================
    subroutine update_density(rho, u, v, rho_new)
        use Initial_var
        implicit none
        real(kind=8), intent(in) :: rho(nx,ny), u(nx,ny), v(nx,ny)
        real(kind=8), intent(out) :: rho_new(nx,ny)
        integer :: i, j

        rho_new = rho

        do i = 2, nx-1
            do j = 2, ny-1
                rho_new(i,j) = rho(i,j) - dt * ( &
                    u(i,j) * (rho(i+1,j) - rho(i-1,j)) / (2.0 * dx) + &
                    v(i,j) * (rho(i,j+1) - rho(i,j-1)) / (2.0 * dy) + &
                    rho(i,j) * ((u(i+1,j) - u(i-1,j)) / (2.0 * dx) + &
                                (v(i,j+1) - v(i,j-1)) / (2.0 * dy)))
            end do
        end do

        rho_new(1,:) = rho(1,:)
        rho_new(nx,:) = rho(nx,:)
        rho_new(:,1) = rho(:,1)
        rho_new(:,ny) = rho(:,ny)
    end subroutine update_density

    !============================================================
    ! Subroutine: Update magnetic field using induction equation
    !============================================================
    subroutine update_magnetic_field(Bx, By, u, v, Bx_new, By_new)
        use Initial_var
        implicit none
        real(kind=8), intent(in) :: Bx(nx,ny), By(nx,ny), u(nx,ny), v(nx,ny)
        real(kind=8), intent(out) :: Bx_new(nx,ny), By_new(nx,ny)
        real(kind=8) :: eta_local
        real(kind=8), parameter :: T_ref = 1.0d6
        integer :: i, j

        do i = 2, nx-1
            do j = 2, ny-1
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
    end subroutine update_magnetic_field

    !============================================================
    ! Subroutine: Enforce incompressibility (pressure correction)
    !============================================================
    subroutine enforce_incompressibility(u, v, p)
        use Initial_var
        implicit none
        real(kind=8), intent(inout) :: u(:,:), v(:,:), p(:,:)
        real(kind=8), allocatable :: divergence(:,:), dpdx(:,:), dpdy(:,:)
        integer :: i, j, iter, max_iter
        real(kind=8), parameter :: tolerance = 1.0e-6

        allocate(divergence(size(u, 1), size(u, 2)))
        allocate(dpdx(size(u, 1), size(u, 2)))
        allocate(dpdy(size(u, 1), size(u, 2)))

        max_iter = 100
        do iter = 1, max_iter
            do i = 2, size(u, 1) - 1
                do j = 2, size(u, 2) - 1
                    divergence(i,j) = ((u(i+1,j) - u(i-1,j)) / (2.0 * dx)) + &
                                      ((v(i,j+1) - v(i,j-1)) / (2.0 * dy))
                end do
            end do

            if (maxval(abs(divergence)) < tolerance) exit

            do i = 2, size(p, 1) - 1
                do j = 2, size(p, 2) - 1
                    p(i,j) = (1.0 / 4.0) * (p(i+1,j) + p(i-1,j) + p(i,j+1) + p(i,j-1) - &
                             divergence(i,j) * dx * dy)
                end do
            end do

            do i = 2, size(p, 1) - 1
                do j = 2, size(p, 2) - 1
                    dpdx(i,j) = (p(i+1,j) - p(i-1,j)) / (2.0 * dx)
                    dpdy(i,j) = (p(i,j+1) - p(i,j-1)) / (2.0 * dy)
                end do
            end do

            do i = 2, size(u, 1) - 1
                do j = 2, size(u, 2) - 1
                    u(i,j) = u(i,j) - dpdx(i,j) * dt
                    v(i,j) = v(i,j) - dpdy(i,j) * dt
                end do
            end do
        end do

        deallocate(divergence, dpdx, dpdy)
    end subroutine enforce_incompressibility

    !============================================================
    ! Subroutine: Main time-stepping driver
    !============================================================
    subroutine mhd_step(u, v, Bx, By, p, rho, Jz)
        use Initial_var
        implicit none
        real(kind=8), intent(inout) :: u(nx,ny), v(nx,ny), Bx(nx,ny), By(nx,ny), p(nx,ny), rho(nx,ny)
        real(kind=8), intent(out) :: Jz(nx,ny)
        real(kind=8), allocatable :: u_new(:,:), v_new(:,:), Bx_new(:,:), By_new(:,:), T_new(:,:), rho_new(:,:)

        allocate(u_new(nx,ny), v_new(nx,ny), Bx_new(nx,ny), By_new(nx,ny), T_new(nx,ny), rho_new(nx,ny))

        call compute_current(Bx, By, Jz)
        call solve_heat_equation(Jz, Bx, By, T_new)
        T = T_new
        call compute_pressure(rho, p)
        call update_velocity(u, v, Jz, Bx, By, p, u_new, v_new)
        u = u_new
        v = v_new
        call update_density(rho, u, v, rho_new)
        rho = rho_new
        call update_magnetic_field(Bx, By, u, v, Bx_new, By_new)
        Bx = Bx_new
        By = By_new
        call enforce_incompressibility(u, v, p)

        deallocate(u_new, v_new, Bx_new, By_new, T_new, rho_new)
    end subroutine mhd_step

end module mhd_module
