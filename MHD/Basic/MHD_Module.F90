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
    ! Compute Laplacian using central finite differences
    subroutine compute_laplacian(T, lap, i, j, dx, dy)
        implicit none
        real, intent(in) :: T(:,:), dx, dy
        integer, intent(in) :: i, j
        real, intent(out) :: lap
        lap = (T(i+1,j) - 2*T(i,j) + T(i-1,j)) / dx**2 + &
          (T(i,j+1) - 2*T(i,j) + T(i,j-1)) / dy**2
    end subroutine compute_laplacian

    ! Compute Ohmic heating for the entire grid
    subroutine compute_ohmic_heating(Jz, eta, Q)
        implicit none
        real, intent(in) :: Jz(:,:), eta
        real, intent(out) :: Q(:,:)
        Q = eta * Jz**2
    end subroutine compute_ohmic_heating

    ! Compute radiative loss at a specific point
    subroutine compute_radiative_loss(T, i, j, sigma, loss)
        implicit none
        real, intent(in) :: T(:,:), sigma
        integer, intent(in) :: i, j
        real, intent(out) :: loss
        loss = -sigma * T(i,j)**4
    end subroutine compute_radiative_loss

    ! Solve the heat equation
    subroutine Heat_equation(Jz, T, T_new)
        use Initial_var
        implicit none
        real, intent(in) :: Jz(:,:)
        real, intent(in) :: T(:,:)
        real, intent(out) :: T_new(:,:)
        real, allocatable :: Q(:,:)
        integer :: i, j
        real :: lap_term, heat_term, rad_term

        ! Initialize output array
        T_new = T

        ! Compute Ohmic heating for the entire grid
        allocate(Q(nx, ny))
        call compute_ohmic_heating(Jz, eta, Q)

        ! Update interior points
        do i = 2, nx-1
            do j = 2, ny-1
                ! Compute each term separately
                call compute_laplacian(T, lap_term, i, j, dx, dy)
                heat_term = Q(i,j)
                call compute_radiative_loss(T, i, j, sigma, rad_term)

                ! Update temperature
                T_new(i,j) = T(i,j) + dt * (kappa * lap_term + heat_term + rad_term)

                ! Ensure non-negative temperature
                if (T_new(i,j) < 0.0) T_new(i,j) = 0.0
            end do
        end do

        ! Clean up
        deallocate(Q)
    end subroutine Heat_equation

    !============================================================
    ! Subroutine: Update the magnetic field using the induction equation
    !============================================================
    subroutine update_magnetic_field(Bx, By, u, v, Bx_new, By_new, dx, dy, dt, Rm)
        real(kind=8), intent(in) :: Bx(:,:), By(:,:), u(:,:), v(:,:), dx, dy, dt, Rm
        real(kind=8), intent(out) :: Bx_new(:,:), By_new(:,:)
        integer :: i, j

        do i = 2, size(Bx, 1) - 1
            do j = 2, size(Bx, 2) - 1
                Bx_new(i, j) = Bx(i, j) + dt * ( &
                    -(u(i, j) * (Bx(i+1, j) - Bx(i-1, j)) / (2.0 * dx)) + &
                    (1.0 / Rm) * ((Bx(i+1, j) - 2.0 * Bx(i, j) + Bx(i-1, j)) / dx**2 + &
                                  (Bx(i, j+1) - 2.0 * Bx(i, j) + Bx(i, j-1)) / dy**2) )
                By_new(i, j) = By(i, j) + dt * ( &
                    -(v(i, j) * (By(i, j+1) - By(i, j-1)) / (2.0 * dy)) + &
                    (1.0 / Rm) * ((By(i+1, j) - 2.0 * By(i, j) + By(i-1, j)) / dx**2 + &
                                  (By(i, j+1) - 2.0 * By(i, j) + By(i, j-1)) / dy**2) )
            end do
        end do
    end subroutine update_magnetic_field

    !============================================================
    ! Subroutine: Enforce incompressibility (pressure correction)
    !============================================================
    subroutine enforce_incompressibility(u, v, p, dx, dy, dt)
        real(kind=8), intent(inout) :: u(:,:), v(:,:), p(:,:)
        real(kind=8), intent(in) :: dx, dy, dt
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
                    divergence(i, j) = ((u(i+1, j) - u(i-1, j)) / (2.0 * dx)) + &
                                       ((v(i, j+1) - v(i, j-1)) / (2.0 * dy))
                end do
            end do

            if (maxval(abs(divergence)) < tolerance) exit

            do i = 2, size(p, 1) - 1
                do j = 2, size(p, 2) - 1
                    p(i, j) = (1.0 / 4.0) * (p(i+1, j) + p(i-1, j) + p(i, j+1) + p(i, j-1) - divergence(i, j) * dx * dy)
                end do
            end do

            do i = 2, size(p, 1) - 1
                do j = 2, size(p, 2) - 1
                    dpdx(i, j) = (p(i+1, j) - p(i-1, j)) / (2.0 * dx)
                    dpdy(i, j) = (p(i, j+1) - p(i, j-1)) /  (2.0 * dy)
                end do
            end do

            ! Correct velocity field
            do i = 2, size(u, 1) - 1
                do j = 2, size(u, 2) - 1
                    u(i, j) = u(i, j) - dpdx(i, j) * dt
                    v(i, j) = v(i, j) - dpdy(i, j) * dt
                end do
            end do
        end do

        ! Deallocate temporary arrays
        deallocate(divergence, dpdx, dpdy)
    end subroutine enforce_incompressibility

end module mhd_module
