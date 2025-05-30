program mhd_solver
    use mhd_module
    use Initial_var
    use hdf5
    implicit none
    real(kind=8) :: u(Nx, Ny), v(Nx, Ny)
    real(kind=8) :: Bx(Nx, Ny), By(Nx, Ny)
    real(kind=8) :: p(Nx, Ny)
    real(kind=8) :: Jz(Nx, Ny)
    real(kind=8) :: u_new(Nx, Ny), v_new(Nx, Ny)
    real(kind=8) :: Bx_new(Nx, Ny), By_new(Nx, Ny)
    real(kind=8) :: T_new(Nx, Ny)
    real(kind=8) :: rho_new(Nx, Ny)
    real(kind=8) :: Bmag(Nx, Ny)
    integer :: n, i, j
    real(kind=8) :: R

    ! HDF5 variables
    integer(hid_t) :: file_id, dset_id, dataspace_id
    integer :: error
    integer(hsize_t), dimension(2) :: dims = (/Nx, Ny/)

    call initialize_variables
    call velocity_fields(u, v, Bx, By, p)
    call heat_fields

    call h5open_f(error)
    if (error /= 0) stop "Failed to initialize HDF5 library"

    do n = 1, Nt
        call compute_current(Bx, By, Jz)
        call update_velocity(u, v, Jz, Bx, By, p, u_new, v_new)
        call solve_heat_equation(Jz, Bx, By, T_new)
        T = T_new
        call update_density(rho, u, v, rho_new)
        rho = rho_new
        call compute_pressure(rho, p)
        call update_magnetic_field(Bx, By, u, v, Bx_new, By_new)
        u = u_new
        v = v_new
        Bx = Bx_new
        By = By_new
        if (mod(n, 100) == 0) then
            print *, "Time step:", n
        end if
    end do

    ! Compute B magnitude
    do i = 1, Nx
        do j = 1, Ny
            R = Rmin + (i-1) * dx
            Bmag(i,j) = sqrt(Bx(i,j)**2 + By(i,j)**2 + (B0 * R0 / R)**2)
        end do
    end do

    ! Write outputs to HDF5
    call h5fcreate_f("output.h5", H5F_ACC_TRUNC_F, file_id, error)
    call h5screate_simple_f(2, dims, dataspace_id, error)

    call h5dcreate_f(file_id, "velocity_v", H5T_NATIVE_DOUBLE, dataspace_id, dset_id, error)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, v, dims, error)
    call h5dclose_f(dset_id, error)

    call h5dcreate_f(file_id, "temperature", H5T_NATIVE_DOUBLE, dataspace_id, dset_id, error)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, T, dims, error)
    call h5dclose_f(dset_id, error)

    call h5dcreate_f(file_id, "density", H5T_NATIVE_DOUBLE, dataspace_id, dset_id, error)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rho, dims, error)
    call h5dclose_f(dset_id, error)

    call h5dcreate_f(file_id, "B_magnitude", H5T_NATIVE_DOUBLE, dataspace_id, dset_id, error)
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, Bmag, dims, error)
    call h5dclose_f(dset_id, error)

    call h5sclose_f(dataspace_id, error)
    call h5fclose_f(file_id, error)
    call h5close_f(error)

    print *, "Simulation complete!"
end program mhd_solver
