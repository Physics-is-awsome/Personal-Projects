! mhd_2d_fourier.f90
module mhd_modules
    use, intrinsic :: iso_c_binding
    include 'fftw3.f03'  ! FFTW interface

    implicit none

    ! Parameters
    integer, parameter :: Nx = 128, Ny = 128
    real(C_DOUBLE), parameter :: Lx = 1.0, Ly = 1.0, dt = 0.001, t_max = 10.0
    real(C_DOUBLE), parameter :: cv = 1.0  ! Specific heat for internal energy

    ! Arrays
    complex(C_DOUBLE_COMPLEX), dimension(Nx, Ny) :: rho_k, u_k, v_k, Bx_k, By_k, s_k, H_k
    real(C_DOUBLE), dimension(Nx, Ny) :: rho_x, u_x, v_x, Bx_x, By_x, s_x
    complex(C_DOUBLE_COMPLEX), dimension(Nx*Ny, Nx*Ny) :: Q_matrix

    ! FFT plans
    type(C_PTR) :: plan_forward, plan_backward

    ! Wave numbers
    real(C_DOUBLE), dimension(Nx) :: kx
    real(C_DOUBLE), dimension(Ny) :: ky

contains

    subroutine initialize_simulation()
        integer :: i, j

        ! Initialize wave numbers
        do i = 1, Nx
            kx(i) = 2.0 * 3.14159265359 * (i - 1 - Nx/2) / Lx
        end do
        do j = 1, Ny
            ky(j) = 2.0 * 3.14159265359 * (j - 1 - Ny/2) / Ly
        end do

        ! Allocate and initialize arrays
        rho_x = 1.0 + 0.01 * sin(2.0 * 3.14159265359 * x / Lx) * cos(2.0 * 3.14159265359 * y / Ly)  ! Perturbed density
        u_x = 0.1 * sin(2.0 * 3.14159265359 * x / Lx)  ! Initial velocity
        v_x = 0.0
        Bx_x = 0.1  ! Uniform magnetic field
        By_x = 0.0
        s_x = 0.0   ! Initial entropy

        ! Create FFT plans
        plan_forward = fftw_plan_dft_2d(Nx, Ny, rho_x, rho_k, FFTW_FORWARD, FFTW_ESTIMATE)
        plan_backward = fftw_plan_dft_2d(Nx, Ny, rho_k, rho_x, FFTW_BACKWARD, FFTW_ESTIMATE)

        ! Transform initial conditions to Fourier space
        call fftw_execute_dft(plan_forward, rho_x, rho_k)
        call fftw_execute_dft(plan_forward, u_x, u_k)
        call fftw_execute_dft(plan_forward, v_x, v_k)
        call fftw_execute_dft(plan_forward, Bx_x, Bx_k)
        call fftw_execute_dft(plan_forward, By_x, By_k)
        call fftw_execute_dft(plan_forward, s_x, s_k)

        ! Normalize Fourier coefficients
        rho_k = rho_k / (Nx * Ny)
        u_k = u_k / (Nx * Ny)
        v_k = v_k / (Nx * Ny)
        Bx_k = Bx_k / (Nx * Ny)
        By_k = By_k / (Nx * Ny)
        s_k = s_k / (Nx * Ny)

        ! Build Poisson matrix (simplified)
        call build_poisson_matrix()

    end subroutine

    subroutine build_poisson_matrix()
        ! Simplified construction of Q_matrix (antisymmetric, placeholder)
        integer :: i, j, idx_i, idx_j
        Q_matrix = 0.0

        do i = 1, Nx*Ny
            do j = 1, Nx*Ny
                if (i /= j) then
                    Q_matrix(i,j) = -Q_matrix(j,i)  ! Antisymmetry
                    if (mod(i+j, 2) == 0) Q_matrix(i,j) = 0.1  ! Dummy values
                end if
            end do
        end do

        ! In practice, derive from paper Eqs. (6), (9), (13), and erratum

    end subroutine

    subroutine compute_hamiltonian()
        integer :: i, j
        complex(C_DOUBLE_COMPLEX) :: kinetic, internal, magnetic

        H_k = 0.0

        do i = 1, Nx
            do j = 1, Ny
                ! Kinetic energy: 1/2 rho (u^2 + v^2)
                kinetic = 0.5 * rho_k(i,j) * (conjg(u_k(i,j))*u_k(i,j) + conjg(v_k(i,j))*v_k(i,j))
                ! Internal energy: rho U(rho, s) ~ rho * s (simplified)
                internal = rho_k(i,j) * s_k(i,j) * cv
                ! Magnetic energy: 1/2 (Bx^2 + By^2)
                magnetic = 0.5 * (conjg(Bx_k(i,j))*Bx_k(i,j) + conjg(By_k(i,j))*By_k(i,j))
                H_k(i,j) = kinetic + internal + magnetic
            end do
        end do

    end subroutine

    subroutine poisson_bracket(z_k, H_k, dz_dt)
        complex(C_DOUBLE_COMPLEX), dimension(Nx, Ny) :: z_k, H_k, dz_dt
        integer :: i, j, k, l, idx_i, idx_j, idx_k, idx_l

        dz_dt = 0.0

        do i = 1, Nx
            do j = 1, Ny
                idx_i = (i-1)*Ny + j
                do k = 1, Nx
                    do l = 1, Ny
                        idx_k = (k-1)*Ny + l
                        dz_dt(i,j) = dz_dt(i,j) + Q_matrix(idx_i, idx_k) * H_k(k,l)
                    end do
                end do
            end do
        end do

    end subroutine

    subroutine rk4_step()
        complex(C_DOUBLE_COMPLEX), dimension(Nx, Ny) :: k1, k2, k3, k4, temp
        integer :: var

        ! Compute Hamiltonian
        call compute_hamiltonian()

        ! For each variable (rho, u, v, Bx, By, s)
        do var = 1, 6  ! Loop over variables (simplified)
            select case(var)
            case(1); temp = rho_k
            case(2); temp = u_k
            case(3); temp = v_k
            case(4); temp = Bx_k
            case(5); temp = By_k
            case(6); temp = s_k
            end select

            ! RK4 steps
            call poisson_bracket(temp, H_k, k1)
            call poisson_bracket(temp + 0.5*dt*k1, H_k, k2)
            call poisson_bracket(temp + 0.5*dt*k2, H_k, k3)
            call poisson_bracket(temp + dt*k3, H_k, k4)

            select case(var)
            case(1); rho_k = temp + (dt/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4)
            case(2); u_k = temp + (dt/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4)
            case(3); v_k = temp + (dt/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4)
            case(4); Bx_k = temp + (dt/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4)
            case(5); By_k = temp + (dt/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4)
            case(6); s_k = temp + (dt/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4)
            end select
        end do

        ! Dealias
        call dealias(rho_k)
        call dealias(u_k)
        call dealias(v_k)
        call dealias(Bx_k)
        call dealias(By_k)
        call dealias(s_k)

    end subroutine

    subroutine dealias(field_k)
        complex(C_DOUBLE_COMPLEX), dimension(Nx, Ny) :: field_k
        integer :: i, j

        do i = 2*Nx/3, Nx
            do j = 2*Ny/3, Ny
                field_k(i,j) = 0.0
            end do
        end do

    end subroutine

    subroutine save_fields(t)
        real(C_DOUBLE) :: t
        integer :: i, j

        open(unit=10, file='output.dat', status='unknown', position='append')
        write(10,*) t, sum(abs(rho_k)), sum(abs(H_k))  ! Example diagnostics
        close(10)

        ! Optional: Transform back to physical space for visualization
        call fftw_execute_dft(plan_backward, rho_k, rho_x)
        rho_x = rho_x / (Nx * Ny)
        ! Save rho_x, etc., to file or plot

    end subroutine

    subroutine cleanup()
        call fftw_destroy_plan(plan_forward)
        call fftw_destroy_plan(plan_backward)
        call fftw_cleanup()
        deallocate(rho_k, u_k, v_k, Bx_k, By_k, s_k, H_k, rho_x, u_x, v_x, Bx_x, By_x, s_x, Q_matrix)

    end subroutine

end module

program mhd_2d_fourier
    use mhd_modules

    real(C_DOUBLE) :: t

    call initialize_simulation()
    t = 0.0

    do while (t < t_max)
        call rk4_step()
        call save_fields(t)
        t = t + dt
        if (mod(int(t/dt), 100) == 0) write(*,*) 'Time:', t
    end do

    call cleanup()

end program
