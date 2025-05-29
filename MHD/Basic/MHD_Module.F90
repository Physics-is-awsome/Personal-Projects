!============================================================
! Magnetohydrodynamics (MHD) Solver using Finite Difference Method
!============================================================

module mhd_module
    use Initial_var
    implicit none
    contains


    !============================================================
    ! Subroutine: Initialize the velocity, magnetic field, and pressure fields
    !============================================================



    !============================================================
    ! Subroutine: Compute the current density (J = curl(B))
    !============================================================
    subroutine compute_current(Bx, By, Jz, dx, dy)

        real(kind=8), intent(in) :: Bx(:,:), By(:,:), dx, dy
        real(kind=8), intent(out) :: Jz(:,:)
        REAL :: Mu_in
        integer :: i, j
        Mu_in= 7.9577D+5
        



        ! Calculate current density Jz using finite differences
        do i = 2, size(Bx, 1) - 1
             do j = 2, size(Bx, 2) - 1
                Jz(i, j) = ( (By(i+1, j) - By(i-1, j)) / (2.0 * dx) - &
                            (Bx(i, j+1) - Bx(i, j-1)) / (2.0 * dy) ) / Mu_in
            END DO
        END DO
    end subroutine compute_current



    !============================================================
    ! Subroutine: Update velocity field using the Navier-Stokes equation
    !============================================================
    subroutine update_velocity(u, v, Jz, Bx, By, p, u_new, v_new, dx, dy, dt, Re)
        real(kind=8), intent(in) :: u(:,:), v(:,:), Jz(:,:), Bx(:,:), By(:,:), p(:,:)
        real(kind=8), intent(out) :: u_new(:,:), v_new(:,:)
        real(kind=8), intent(in) :: dx, dy, dt, Re
\
        integer :: i, j

        do i = 2, size(u, 1) - 1
            do j = 2, size(u, 2) - 1
                u_new(i, j) = u(i, j) + dt * ( &
                    -(u(i, j) * (u(i+1, j) - u(i-1, j)) / (2.0 * dx) + &
                      v(i, j) * (u(i, j+1) - u(i, j-1)) / (2.0 * dy)) + &
                    (1.0 / Re) * ((u(i+1, j) - 2.0 * u(i, j) + u(i-1, j)) / dx**2 + &
                                  (u(i, j+1) - 2.0 * u(i, j) + u(i, j-1)) / dy**2) + &
                    (Jz(i,j) * Bx(i,j) ))
                v_new(i, j) = v(i, j) + dt * ( &
                    -(u(i, j) * (v(i+1, j) - v(i-1, j)) / (2.0 * dx) + &
                      v(i, j) * (v(i, j+1) - v(i, j-1)) / (2.0 * dy)) + &
                    (1.0 / Re) * ((v(i+1, j) - 2.0 * v(i, j) + v(i-1, j)) / dx**2 + &
                                  (v(i, j+1) - 2.0 * v(i, j) + v(i, j-1)) / dy**2) + &
                    (Jz(i,j) * Bx(i,j) ))
            end do
        end do

    end subroutine update_velocity

    !============================================================
    ! Update Heat transport 
    !===========================================================
    subroutine solve_heat_equation(Jz, T_new)
        use Initial_var
        implicit none
        real(kind=8), intent(in) :: Jz(nx,ny)  ! Current density
        real(kind=8), intent(out) :: T_new(nx,ny)  ! Updated temperature
        integer :: i, j
        real(kind=8) :: lap, Q, rad  ! Temporary variables for Laplacian, Ohmic heating, radiative loss

        ! Initialize output array
        T_new = T

        ! Update interior points
        do i = 2, nx-1
            do j = 2, ny-1
                ! Compute Laplacian using central finite differences
                lap = ((T(i+1,j) - 2*T(i,j) + T(i-1,j)) / dx**2 + &
                       (T(i,j+1) - 2*T(i,j) + T(i,j-1)) / dy**2)

                ! Compute Ohmic heating
                Q = eta * Jz(i,j)**2

                ! Compute radiative loss
                rad = -sigma * T(i,j)**4

                ! Update temperature using explicit Euler
                T_new(i,j) = T(i,j) + dt * (kappa * lap + Q + rad)

                ! Ensure non-negative temperature
                if (T_new(i,j) < 0.0d0) T_new(i,j) = 0.0d0
            end do
        end do

        ! Apply boundary conditions (Dirichlet: fixed temperature at boundaries)
        T_new(1,:) = T(1,:)
        T_new(nx,:) = T(nx,:)
        T_new(:,1) = T(:,1)
        T_new(:,ny) = T(:,ny)
    end subroutine solve_heat_equation

    !============================================================
    ! Subroutine: Update the magnetic field using the induction equation
    !============================================================
subroutine update_magnetic_field(Bx, By, u, v, T, Bx_new, By_new, dx, dy, dt)
    real(kind=8), intent(in) :: Bx(:,:), By(:,:), u(:,:), v(:,:), T(:,:), dx, dy, dt
    real(kind=8), intent(out) :: Bx_new(:,:), By_new(:,:)
    real(kind=8) :: Rm_local, eta_local
    integer :: i, j

    do i = 2, size(Bx, 1) - 1
        do j = 2, size(Bx, 2) - 1
            eta_local = eta * (T(i,j) / T_ref)**(-1.5d0)  ! Spitzer resistivity
            Rm_local = 1.0d0 / eta_local
            Bx_new(i, j) = Bx(i, j) + dt * ( &
                -(u(i, j) * (Bx(i+1, j) - Bx(i-1, j)) / (2.0 * dx)) + &
                (1.0 / Rm_local) * ((Bx(i+1, j) - 2.0 * Bx(i, j) + Bx(i-1, j)) / dx**2 + &
                                    (Bx(i, j+1) - 2.0 * Bx(i, j) + Bx(i, j-1)) / dy**2) )
            By_new(i, j) = By(i, j) + dt * ( &
                -(v(i, j) * (By(i, j+1) - By(i, j-1)) / (2.0 * dy)) + &
                (1.0 / Rm_local) * ((By(i+1, j) - 2.0 * By(i, j) + By(i-1, j)) / dx**2 + &
                                    (By(i, j+1) - 2.0 * By(i, j) + By(i, j-1)) / dy**2) )
        end do
    end do
end subroutine update_magnetic_field
!=====================================================
! Computing pressure
!=====================================================
   subroutine compute_pressure(T, rho, p)
    use Initial_var
    implicit none
    real(kind=8), intent(in) :: T(nx,ny), rho(nx,ny)
    real(kind=8), intent(out) :: p(nx,ny)
    real(kind=8) :: gamma, cv
    integer :: i, j

    gamma = 5.0d0 / 3.0d0  ! Adiabatic index for ideal gas
    cv = 1.0d0  ! Specific heat, adjust for units

    do i = 1, nx
        do j = 1, ny
            p(i,j) = (gamma - 1.0d0) * rho(i,j) * cv * T(i,j)
        end do
    end do
end subroutine compute_pressure
!====================================================
! Update density
!====================================================
subroutine update_density(rho, u, v, rho_new, dx, dy, dt)
    use Initial_var
    implicit none
    real(kind=8), intent(in) :: rho(nx,ny), u(nx,ny), v(nx,ny), dx, dy, dt
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

    ! Apply boundary conditions (e.g., fixed density)
    rho_new(1,:) = rho(1,:)
    rho_new(nx,:) = rho(nx,:)
    rho_new(:,1) = rho(:,1)
    rho_new(:,ny) = rho(:,ny)
end subroutine update_density

end module mhd_module
