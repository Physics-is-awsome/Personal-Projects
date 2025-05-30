program mhd_solver
    use hdf5
    use Initial_var, only: Nx, Ny, Nt, Rmin, Rmax, Zmin, Zmax, R0, B0, mu0, dx, dy, dt, velocity_fields
    use mhd_module
    implicit none

    ! Variables (renamed to avoid conflicts)
    real(kind=8), dimension(Nx,Ny) :: vel_u, vel_v, mag_Bx, mag_By, density, temp, Jz, pressure, &
                                     vel_u_new, vel_v_new, mag_Bx_new, mag_By_new, density_new, temp_new, B_magnitude
    real(kind=8) :: dt_dynamic, time
    integer(kind=8) :: file_id, dset_id, dspace_id
    integer :: n, error
    integer(kind=8), dimension(2) :: dims = (/Nx, Ny/)
    character(len=20) :: filename

    ! Initialize HDF5
    call h5open_f(error)
    if (error /= 0) stop 'HDF5 initialization failed'

    ! Initialize fields
    call velocity_fields(vel_u, vel_v, mag_Bx, mag_By, density)
    call compute_Bmag(mag_Bx, mag_By, B_magnitude)
    call initialize_variables
    call heat_fields
    temp = T  ! Assuming T is the temperature array from Initial_var

    ! Time loop
    time = 0.0d0
    do n = 1, Nt
        ! Compute dynamic time step
        call compute_dt(vel_u, vel_v, mag_Bx, mag_By, density, dt_dynamic)

        ! Compute current and pressure
        call compute_current(mag_Bx, mag_By, Jz)
        call compute_pressure(density, temp, pressure)

        ! Update fields
        call update_density(density, vel_u, vel_v, density_new)
        density = density_new
        call solve_heat_equation(Jz, mag_Bx, mag_By, temp, temp_new)
        temp = temp_new
        call update_magnetic_field(mag_Bx, mag_By, vel_u, vel_v, temp, mag_Bx_new, mag_By_new)
        mag_Bx = mag_Bx_new
        mag_By = mag_By_new
        call update_velocity(vel_u, vel_v, Jz, mag_Bx, mag_By, pressure, vel_u_new, vel_v_new)
        vel_u = vel_u_new
        vel_v = vel_v_new
        call compute_Bmag(mag_Bx, mag_By, B_magnitude)

        time = time + dt_dynamic

        ! Save output every 100 steps
        if (mod(n, 100) == 0) then
            write(filename, '(A,I0,A)') 'output_', n, '.h5'
            call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
            if (error /= 0) stop 'File creation failed'

            call h5screate_simple_f(2, dims, dspace_id, error)
            if (error /= 0) stop 'Dataspace creation failed'

            call h5dcreate_f(file_id, 'velocity_v', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vel_v, dims, error)
            call h5dclose_f(dset_id, error)

            call h5dcreate_f(file_id, 'temperature', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, temp, dims, error)
            call h5dclose_f(dset_id, error)

            call h5dcreate_f(file_id, 'density', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, density, dims, error)
            call h5dclose_f(dset_id, error)

            call h5dcreate_f(file_id, 'B_magnitude', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, B_magnitude, dims, error)
            call h5dclose_f(dset_id, error)

            call h5sclose_f(dspace_id, error)
            call h5fclose_f(file_id, error)
        end if
    end do

    ! Final output
    call h5fcreate_f('output_final.h5', H5F_ACC_TRUNC_F, file_id, error)
    call h5screate_simple_f(2, dims, dspace_id, error)

    call h5dcreate_f(file_id, 'velocity_v', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vel_v, dims, error)
    call h5dclose_f(dset_id, error)

    call h5dcreate_f(file_id, 'temperature', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, temp, dims, error)
    call h5dclose_f(dset_id, error)

    call h5dcreate_f(file_id, 'density', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, density, dims, error)
    call h5dclose_f(dset_id, error)

    call h5dcreate_f(file_id, 'B_magnitude', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, B_magnitude, dims, error)
    call h5dclose_f(dset_id, error)

    call h5sclose_f(dspace_id, error)
    call h5fclose_f(file_id, error)

    call h5close_f(error)

    print *, 'Simulation completed. Time reached: ', time
end program mhd_solver
