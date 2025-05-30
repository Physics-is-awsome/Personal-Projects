module mhd_module
    use Initial_var
    implicit none
contains

    subroutine compute_dt(u, v, Bx, By, rho, dt_out)
        real(kind=8), intent(in) :: u(Nx,Ny), v(Nx,Ny), Bx(Nx,Ny), By(Nx,Ny), rho(Nx,Ny)
        real(kind=8), intent(out) :: dt_out
        real(kind=8) :: v_A, R, Bphi, Bmag
        integer :: i, j
        dt_out = 1.0d0
        do i = 2, Nx-1
            do j = 2, Ny-1
                R = Rmin + (i-1) * dx
                Bphi = B0 * R0 / R
                Bmag = sqrt(Bx(i,j)**2 + By(i,j)**2 + Bphi**2)
                v_A = Bmag / sqrt(mu0 * rho(i,j))
                dt_out = min(dt_out, 0.5 * min(dx / (abs(u(i,j)) + v_A), dy / (abs(v(i,j)) + v_A)))
            end do
        end do
    end subroutine compute_dt

    subroutine compute_Bmag(Bx, By, Bmag)
        real(kind=8), intent(in) :: Bx(Nx,Ny), By(Nx,Ny)
        real(kind=8), intent(out) :: Bmag(Nx,Ny)
        integer :: i, j
        real(kind=8) :: R, Bphi
        do i = 1, Nx
            do j = 1, Ny
                R = Rmin + (i-1) * dx
                Bphi = B0 * R0 / R
                Bmag(i,j) = sqrt(Bx(i,j)**2 + By(i,j)**2 + Bphi**2)
            end do
        end do
    end subroutine compute_Bmag

    subroutine compute_current(Bx, By, Jz)
        real(kind=8), intent(in) :: Bx(:,:), By(:,:)
        real(kind=8), intent(out) :: Jz(:,:)
        integer :: i, j
        real(kind=8) :: R

        do i = 2, Nx-1
            do j = 2, Ny-1
                R = Rmin + (i-1) * dx
                Jz(i,j) = ( (By(i+1,j) - By(i-1,j)) / (2.0 * dx) - &
                            (Bx(i,j+1) - Bx(i,j-1)) / (2.0 * dy) ) / mu0
            end do
        end do
        Jz(1,:) = 0.0d0
        Jz(Nx,:) = 0.0d0
        Jz(:,1) = 0.0d0
        Jz(:,Ny) = 0.0d0
    end subroutine compute_current

    subroutine update_velocity(u, v, Jz, Bx, By, p, u_new, v_new)
        real(kind=8), intent(in) :: u(:,:), v(:,:), Jz(:,:), Bx(:,:), By(:,:), p(:,:)
        real(kind=8), intent(out) :: u_new(:,:), v_new(:,:)
        real(kind=8) :: dpdx, dpdy, R, Bphi
        integer :: i, j

        do i = 2, Nx-1
            do j = 2, Ny-1
                R = Rmin + (i-1) * dx
                Bphi = B0 * R0 / R
                dpdx = (p(i+1,j) - p(i-1,j)) / (2.0 * dx)
                dpdy = (p(i,j+1) - p(i,j-1)) / (2.0 * dy)
                u_new(i,j) = u(i,j) + dt * ( &
                    -(u(i,j) * (u(i+1,j) - u(i-1,j)) / (2.0 * dx) + &
                      v(i,j) * (u(i,j+1) - u(i,j-1)) / (2.0 * dy)) + &
                    (1.0 / Re) * ((u(i+1,j) - 2.0 * u(i,j) + u(i-1,j)) / dx**2 + &
                                  (u(i,j+1) - 2.0 * u(i,j) + u(i,j-1)) / dy**2) - &
                    dpdx + Jz(i,j) * Bphi)  ! J x Bphi term
                v_new(i,j) = v(i,j) + dt * ( &
                    -(u(i,j) * (v(i+1,j) - v(i-1,j)) / (2.0 * dx) + &
                      v(i,j) * (v(i,j+1) - v(i,j-1)) / (2.0 * dy)) + &
                    (1.0 / Re) * ((v(i+1,j) - 2.0 * v(i,j) + v(i-1,j)) / dx**2 + &
                                  (v(i,j+1) - 2.0 * v(i,j) + v(i,j-1)) / dy**2) - &
                    dpdy)
            end do
        end do
        u_new(1,:) = 0.0d0
        u_new(Nx,:) = 0.0d0
        u_new(:,1) = 0.0d0
        u_new(:,Ny) = 0.0d0
        v_new(1,:) = 0.0d0
        v_new(Nx,:) = 0.0d0
        v_new(:,1) = 0.0d0
        v_new(:,Ny) = 0.0d0
    end subroutine update_velocity

    subroutine solve_heat_equation(Jz, Bx, By, T, T_new)
        implicit none
        real(kind=8), intent(in) :: Jz(Nx,Ny), Bx(Nx,Ny), By(Nx,Ny), T(Nx,Ny)
        real(kind=8), intent(out) :: T_new(Nx,Ny)
        real(kind=8) :: Bmag, bx_local, by_local, dTdx, dTdy, q_parallel, Q, rad, eta_local, R
        real(kind=8), parameter :: T_ref = 1.0d6
        integer :: i, j

        T_new = T

        do i = 2, Nx-1
            do j = 2, Ny-1
                R = Rmin + (i-1) * dx
                Bmag = sqrt(Bx(i,j)**2 + By(i,j)**2 + (B0 * R0 / R)**2)
                if (Bmag > 1.0d-10) then
                    bx_local = Bx(i,j) / Bmag
                    by_local = By(i,j) / Bmag
                else
                    bx_local = 0.0d0
                    by_local = 0.0d0
                endif

                dTdx = (T(i+1,j) - T(i-1,j)) / (2.0 * dx)
                dTdy = (T(i,j+1) - T(i,j-1)) / (2.0 * dy)

                q_parallel = Kappa * ( &
                    bx_local * bx_local * (T(i+1,j) - 2*T(i,j) + T(i-1,j)) / dx**2 + &
                    by_local * by_local * (T(i,j+1) - 2*T(i,j) + T(i,j-1)) / dy**2 + &
                    bx_local * by_local * ((T(i+1,j+1) - T(i+1,j-1) - T(i-1,j+1) + T(i-1,j-1)) / (4.0 * dx * dy)))

                eta_local = 5.2d-5 * (T(i,j) / T_ref)**(-1.5d0)
                Q = eta_local * Jz(i,j)**2
                rad = -sigma * T(i,j)**4

                T_new(i,j) = T(i,j) + dt * (q_parallel + Q + rad)
                if (T_new(i,j) < 0.0d0) T_new(i,j) = 0.0d0
            end do
        end do

        T_new(1,:) = 0.0d0
        T_new(Nx,:) = 0.0d0
        T_new(:,1) = 0.0d0
        T_new(:,Ny) = 0.0d0
    end subroutine solve_heat_equation

    subroutine compute_pressure(rho_in, T, p)
        implicit none
        real(kind=8), intent(in) :: rho_in(Nx,Ny), T(Nx,Ny)
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

    subroutine update_density(rho_in, u, v, rho_new)
        implicit none
        real(kind=8), intent(in) :: rho_in(Nx,Ny), u(Nx,Ny), v(Nx,Ny)
        real(kind=8), intent(out) :: rho_new(Nx,Ny)
        integer :: i, j
        real(kind=8) :: R

        do i = 1, Nx
            do j = 1, Ny
                rho_new(i,j) = rho_in(i,j)
            end do
        end do

        do i = 2, Nx-1
            do j = 2, Ny-1
                R = Rmin + (i-1) * dx
                rho_new(i,j) = rho_in(i,j) - dt * ( &
                    (u(i,j) * (rho_in(i+1,j) - rho_in(i-1,j)) / (2.0 * dx) + &
                     v(i,j) * (rho_in(i,j+1) - rho_in(i,j-1)) / (2.0 * dy)) / R + &
                    rho_in(i,j) * ((u(i+1,j) - u(i-1,j)) / (2.0 * dx) + &
                                   (v(i,j+1) - v(i,j-1)) / (2.0 * dy)))
                if (rho_new(i,j) < 0.0d0) rho_new(i,j) = 0.1d-6
            end do
        end do

        rho_new(1,:) = rho_in(1,:)
        rho_new(Nx,:) = rho_in(Nx,:)
        rho_new(:,1) = rho_in(:,1)
        rho_new(:,Ny) = rho_in(:,Ny)
    end subroutine update_density

    subroutine update_magnetic_field(Bx, By, u, v, T, Bx_new, By_new)
        implicit none
        real(kind=8), intent(in) :: Bx(Nx,Ny), By(Nx,Ny), u(Nx,Ny), v(Nx,Ny), T(Nx,Ny)
        real(kind=8), intent(out) :: Bx_new(Nx,Ny), By_new(Nx,Ny)
        real(kind=8) :: eta_local, R
        real(kind=8), parameter :: T_ref = 1.0d6
        integer :: i, j

        do i = 2, Nx-1
            do j = 2, Ny-1
                R = Rmin + (i-1) * dx
                eta_local = 5.2d-5 * (T(i,j) / T_ref)**(-1.5d0)
                Bx_new(i,j) = Bx(i,j) + dt * ( &
                    -(u(i,j) * (Bx(i+1,j) - Bx(i-1,j)) / (2.0 * dx) + &
                      v(i,j) * (Bx(i,j+1) - Bx(i,j-1)) / (2.0 * dy)) / R + &
                    eta_local * ((Bx(i+1,j) - 2.0 * Bx(i,j) + Bx(i-1,j)) / dx**2 + &
                                 (Bx(i,j+1) - 2.0 * Bx(i,j) + Bx(i,j-1)) / dy**2))
                By_new(i,j) = By(i,j) + dt * ( &
                    -(u(i,j) * (By(i+1,j) - By(i-1,j)) / (2.0 * dx) + &
                      v(i,j) * (By(i,j+1) - By(i,j-1)) / (2.0 * dy)) / R + &
                    eta_local * ((By(i+1,j) - 2.0 * By(i,j) + By(i-1,j)) / dx**2 + &
                                 (By(i,j+1) - 2.0 * By(i,j) + By(i,j-1)) / dy**2))
            end do
        end do
        Bx_new(1,:) = 0.0d0
        Bx_new(Nx,:) = 0.0d0
        By_new(:,1) = 0.0d0
        By_new(:,Ny) = 0.0d0
    end subroutine update_magnetic_field
end module mhd_module
