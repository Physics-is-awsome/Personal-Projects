! mhd_2d_fourier.f90
module mhd_modules
    use, intrinsic :: iso_c_binding
    implicit none

    include 'fftw3.f03'  ! Try this first; if it fails, use manual definitions below

    ! Manual definitions as fallback
    integer(C_INT), parameter :: FFTW_FORWARD = -1
    integer(C_INT), parameter :: FFTW_BACKWARD = 1
    integer(C_INT), parameter :: FFTW_ESTIMATE = 64

    ! Parameters
    integer, parameter :: Nx = 64, Ny = 64
    real(C_DOUBLE), parameter :: Lx = 1.0, Ly = 1.0, dt = 0.001, t_max = 1.0
    real(C_DOUBLE), parameter :: cv = 1.0

    ! Arrays
    complex(C_DOUBLE_COMPLEX), dimension(Nx, Ny) :: rho_k, u_k, v_k, Bx_k, By_k, s_k, H_k
    real(C_DOUBLE), dimension(Nx, Ny) :: rho_x, u_x, v_x, Bx_x, By_x, s_x
    type(C_PTR) :: plan_forward, plan_backward

    ! Wave numbers
    real(C_DOUBLE), dimension(Nx) :: kx
    real(C_DOUBLE), dimension(Ny) :: ky

contains

    subroutine initialize_simulation()
        integer :: i, j

        do i = 1, Nx
            kx(i) = 2.0 * 3.14159265359 * (i - 1 - Nx/2) / Lx
        end do
        do j = 1, Ny
            ky(j) = 2.0 * 3.14159265359 * (j - 1 - Ny/2) / Ly
        end do

        rho_x = 1.0 + 0.01 * sin(2.0 * 3.14159265359 * (kx(1:Nx) / Lx)) * cos(2.0 * 3.14159265359 * (ky(1:Ny) / Ly))
        u_x = 0.1 * sin(2.0 * 3.14159265359 * (kx(1:Nx) / Lx))
        v_x = 0.0
        Bx_x = 0.1
        By_x = 0.0
        s_x = 0.0

        plan_forward = fftw_plan_dft_2d(Nx, Ny, rho_x, rho_k, FFTW_FORWARD, FFTW_ESTIMATE)
        plan_backward = fftw_plan_dft_2d(Nx, Ny, rho_k, rho_x, FFTW_BACKWARD, FFTW_ESTIMATE)

        call fftw_execute_dft(plan_forward, rho_x, rho_k)
        call fftw_execute_dft(plan_forward, u_x, u_k)
        call fftw_execute_dft(plan_forward, v_x, v_k)
        call fftw_execute_dft(plan_forward, Bx_x, Bx_k)
        call fftw_execute_dft(plan_forward, By_x, By_k)
        call fftw_execute_dft(plan_forward, s_x, s_k)

        rho_k = rho_k / (Nx * Ny)
        u_k = u_k / (Nx * Ny)
        v_k = v_k / (Nx * Ny)
        Bx_k = Bx_k / (Nx * Ny)
        By_k = By_k / (Nx * Ny)
        s_k = s_k / (Nx * Ny)

    end subroutine

    subroutine compute_hamiltonian()
        integer :: i, j
        complex(C_DOUBLE_COMPLEX) :: kinetic, internal, magnetic

        H_k = 0.0

        do i = 1, Nx
            do j = 1, Ny
                kinetic = 0.5 * rho_k(i,j) * (conjg(u_k(i,j))*u_k(i,j) + conjg(v_k(i,j))*v_k(i,j))
                internal = rho_k(i,j) * s_k(i,j) * cv
                magnetic = 0.5 * (conjg(Bx_k(i,j))*Bx_k(i,j) + conjg(By_k(i,j))*By_k(i,j))
                H_k(i,j) = kinetic + internal + magnetic
            end do
        end do

    end subroutine

    subroutine evolve_step(t)
        real(C_DOUBLE), intent(inout) :: t
        complex(C_DOUBLE_COMPLEX), dimension(Nx, Ny) :: k1_rho, k2_rho, k3_rho, k4_rho

        call compute_hamiltonian()

        k1_rho = -0.1 * rho_k
        k2_rho = -0.1 * (rho_k + 0.5*dt*k1_rho)
        k3_rho = -0.1 * (rho_k + 0.5*dt*k2_rho)
        k4_rho = -0.1 * (rho_k + dt*k3_rho)

        rho_k = rho_k + (dt/6.0) * (k1_rho + 2.0*k2_rho + 2.0*k3_rho + k4_rho)

        t = t + dt

    end subroutine

    subroutine save_fields(t)
        real(C_DOUBLE), intent(in) :: t
        open(unit=10, file='output.dat', status='unknown', position='append')
        write(10,*) t, sum(abs(rho_k)), sum(abs(H_k))
        close(10)
    end subroutine

    subroutine cleanup()
        call fftw_destroy_plan(plan_forward)
        call fftw_destroy_plan(plan_backward)
        call fftw_cleanup()
    end subroutine

end module

program mhd_2d_fourier
    use mhd_modules

    real(C_DOUBLE) :: t

    call initialize_simulation()
    t = 0.0

    do while (t < t_max)
        call evolve_step(t)
        call save_fields(t)
        if (mod(int(t/dt), 100) == 0) write(*,*) 'Time:', t
    end do

    call cleanup()

end program
