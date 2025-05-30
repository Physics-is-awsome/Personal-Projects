program mhd_solver
    use hdf5
    use Initial_var, only: Nx, Ny, Rmin, R0, B0, mu0, dx, dy, Nt
    use mhd_module
    implicit none

    ! Variables (renamed to avoid conflicts with Initial_var)
    real(kind=8), dimension(Nx,Ny) :: vel_u, vel_v, mag_Bx, mag_By, density, temp, B_magnitude
    real(kind=8) :: dt_dynamic, time
    integer(kind=8) :: file_id, dset_id, dspace_id  ! HDF5 requires INTEGER(8)
    integer :: n, error
    integer(kind=8), dimension(2) :: dims = (/Nx, Ny/)  ! INTEGER(8) for HDF5
    character(len=20) :: filename

    ! Initialize HDF5
    call h5open_f(error)
    if (error /= 0) stop 'HDF5 initialization failed'

    ! Initialize fields
    call velocity_fields(vel_u, vel_v, mag_Bx, mag_By, density, temp)

    ! Compute initial B_magnitude
    call compute_Bmag(mag_Bx, mag_By, B_magnitude)

    ! Time loop
    time = 0.0d0
    do n = 1, Nt
        ! Compute dynamic time step
        call compute_dt(vel_u, vel_v, mag_Bx, mag_By, density, dt_dynamic)

        ! Update fields
        call update_density(density, vel_u, vel_v, dt_dynamic)
        call solve_heat_equation(temp, vel_u, vel_v, dt_dynamic)
        call update_magnetic_field(mag_Bx, mag_By, vel_u, vel_v, dt_dynamic)
        call update_velocity(vel_u, vel_v, mag_Bx, mag_By, density, temp, dt_dynamic)
        call compute_Bmag(mag_Bx, mag_By, B_magnitude)

        time = time + dt_dynamic

        ! Save output every 100 steps
        if (mod(n, 100) == 0) then
            write(filename, '(A,I0,A)') 'output_', n, '.h5'
            call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
            if (error /= 0) stop 'File creation failed'

            ! Create dataspace
            call h5screate_simple_f(2, dims, dspace_id, error)
            if (error /= 0) stop 'Dataspace creation failed'

            ! Write velocity_v
            call h5dcreate_f(file_id, 'velocity_v', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vel_v, dims, error)
            call h5dclose_f(dset_id, error)

            ! Write temperature
            call h5dcreate_f(file_id, 'temperature', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, temp, dims, error)
            call h5dclose_f(dset_id, error)

            ! Write density
            call h5dcreate_f(file_id, 'density', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, density, dims, error)
            call h5dclose_f(dset_id, error)

            ! Write B_magnitude
            call h5dcreate_f(file_id, 'B_magnitude', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, B_magnitude, dims, error)
            call h5dclose_f(dset_id, error)

            ! Close dataspace and file
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

    ! Close HDF5
    call h5close_f(error)

    print *, 'Simulation completed. Time reached: ', time
end program mhd_solver
