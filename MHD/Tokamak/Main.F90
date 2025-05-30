program mhd_solver
    use hdf5
    use Initial_var, only: Nx, Ny, Nt, velocity_fields
    use mhd_module
    implicit none

    ! Variables
    real(kind=8), dimension(Nx,Ny) :: u, v, Bx, By, rho, T, Jz, p, u_new, v_new, Bx_new, By_new, rho_new, T_new, Bmag
    real(kind=8) :: dt_dynamic, time
    integer(kind=8) :: file_id, dset_id, dspace_id
    integer :: n, error
    integer(kind=8), dimension(2) :: dims = (/Nx, Ny/)
    character(len=20) :: filename

    ! Initialize HDF5
    call h5open_f(error)
    if (error /= 0) stop 'HDF5 initialization failed'

    ! Initialize fields
    call velocity_fields(u, v, Bx, By, rho, T)
    call compute_Bmag(Bx, By, Bmag)

    ! Time loop
    time = 0.0d0
    do n = 1, Nt
        ! Compute dynamic time step
        call compute_dt(u, v, Bx, By, rho, dt_dynamic)

        ! Compute current and pressure
        call compute_current(Bx, By, Jz)
        call compute_pressure(rho, T, p)

        ! Update fields
        call update_density(rho, u, v, rho_new)
        rho = rho_new
        call solve_heat_equation(Jz, Bx, By, T, T_new)
        T = T_new
        call update_magnetic_field(Bx, By, u, v, T, Bx_new, By_new)
        Bx = Bx_new
        By = By_new
        call update_velocity(u, v, Jz, Bx, By, p, u_new, v_new)
        u = u_new
        v = v_new
        call compute_Bmag(Bx, By, Bmag)

        time = time + dt_dynamic

        ! Save output every 100 steps
        if (mod(n, 100) == 0) then
            write(filename, '(A,I0,A)') 'output_', n, '.h5'
            call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
            if (error /= 0) stop 'File creation failed'

            call h5screate_simple_f(2, dims, dspace_id, error)
            if (error /= 0) stop 'Dataspace creation failed'

            call h5dcreate_f(file_id, 'velocity_v', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, v, dims, error)
            call h5dclose_f(dset_id, error)

            call h5dcreate_f(file_id, 'temperature', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, T, dims, error)
            call h5dclose_f(dset_id, error)

            call h5dcreate_f(file_id, 'density', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rho, dims, error)
            call h5dclose_f(dset_id, error)

            call h5dcreate_f(file_id, 'B_magnitude', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, Bmag, dims, error)
            call h5dclose_f(dset_id, error)

            call h5sclose_f(dspace_id, error)
            call h5fclose_f(file_id, error)
        end if
    end do

    ! Final output
    call h5fcreate_f('output_final.h5', H5F_ACC_TRUNC_F, file_id, error)
    call h5screate_simple_f(2, dims, dspace_id, error)

    call h5dcreate_f(file_id, 'velocity_v', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, v, dims, error)
    call h5dclose_f(dset_id, error)

    call h5dcreate_f(file_id, 'temperature', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, T, dims, error)
    call h5dclose_f(dset_id, error)

    call h5dcreate_f(file_id, 'density', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rho, dims, error)
    call h5dclose_f(dset_id, error)

    call h5dcreate_f(file_id, 'B_magnitude', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, Bmag, dims, error)
    call h5dclose_f(dset_id, error)

    call h5sclose_f(dspace_id, error)
    call h5fclose_f(file_id, error)

    call h5close_f(error)

    print *, 'Simulation completed. Time reached: ', time
end program mhd_solver
