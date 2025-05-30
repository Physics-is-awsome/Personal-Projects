program mhd_solver
    use hdf5
    use Initial_var
    use mhd_module
    implicit none

    ! Variables
    real(kind=8), dimension(Nx,Ny) :: u, v, Bx, By, rho, T, Bmag
    real(kind=8) :: dt_dynamic, t
    integer :: n, error, file_id, dset_id, dspace_id
    integer, dimension(2) :: dims = (/Nx, Ny/)
    character(len=20) :: filename

    ! Initialize HDF5
    call h5open_f(error)
    if (error /= 0) stop 'HDF5 initialization failed'

    ! Initialize fields
    call velocity_fields(u, v, Bx, By, rho, T)

    ! Compute initial B_magnitude
    call compute_Bmag(Bx, By, Bmag)

    ! Time loop
    t = 0.0d0
    do n = 1, Nt
        ! Compute dynamic time step
        call compute_dt(u, v, Bx, By, rho, dt_dynamic)

        ! Update fields
        call update_density(rho, u, v, dt_dynamic)
        call solve_heat_equation(T, u, v, dt_dynamic)
        call update_magnetic_field(Bx, By, u, v, dt_dynamic)
        call update_velocity(u, v, Bx, By, rho, T, dt_dynamic)
        call compute_Bmag(Bx, By, Bmag)

        t = t + dt_dynamic

        ! Save output every 100 steps
        if (mod(n, 100) == 0) then
            write(filename, '(A,I0,A)') 'output_', n, '.h5'
            call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
            if (error /= 0) stop 'File creation failed'

            ! Create dataspace
            call h5screate_simple_f(2, dims, dspace_id, error)

            ! Write velocity_v
            call h5dcreate_f(file_id, 'velocity_v', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, v, dims, error)
            call h5dclose_f(dset_id, error)

            ! Write temperature
            call h5dcreate_f(file_id, 'temperature', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, T, dims, error)
            call h5dclose_f(dset_id, error)

            ! Write density
            call h5dcreate_f(file_id, 'density', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rho, dims, error)
            call h5dclose_f(dset_id, error)

            ! Write B_magnitude
            call h5dcreate_f(file_id, 'B_magnitude', H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
            call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, Bmag, dims, error)
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

    ! Close HDF5
    call h5close_f(error)

    print *, 'Simulation completed. Time reached: ', t
end program mhd_solver
