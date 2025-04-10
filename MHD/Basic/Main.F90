!============================================================
! Main Program: MHD Solver
!============================================================
program mhd_solver
    use MHD_Module
    use Initial_var
    use hdf5
    implicit none
    !==================
    ! Parameters
    !=======================
    call Parameters(Nx, Ny, Nt, Lx, Ly, dt, Re, R,, dx, dy, u(Nx, Ny), v(Nx, Ny), Bx(Nx, Ny), By(Nx, Ny), p(Nx, Ny), Jz(Nx, Ny), Jy(Nx, Ny), u_new(Nx, Ny), v_new(Nx, Ny), Bx_new(Nx, Ny), By_new(Nx, Ny)

    ! HDF5 variables
    integer(hid_t) :: file_id, dset_id, dataspace_id
    integer :: error
    integer(hsize_t), dimension(2) :: dims = (/Nx, Ny/)

    !============================================================
    ! Initialize fields
    !============================================================
    call initialize_fields(u, v, Bx, By, p)

    ! Initialize HDF5 library
    call h5open_f(error)
    if (error /= 0) stop "Failed to initialize HDF5 library"

    !============================================================
    ! Time-stepping loop
    !============================================================
    do n = 1, Nt
        ! Compute current density (J = curl(B))
        call compute_current(Bx, By, Jz, dx, dy)

        ! Update velocity field (momentum equation)
        call update_velocity(u, v, Jz, Bx, By, p, u_new, v_new, dx, dy, dt, Re)

        ! Update magnetic field (induction equation)
        call update_magnetic_field(Bx, By, u, v, Bx_new, By_new, dx, dy, dt, Rm)

        ! Enforce incompressibility (correct pressure)
        call enforce_incompressibility(u_new, v_new, p, dx, dy, dt)

        ! Update fields
        u = u_new
        v = v_new
        Bx = Bx_new
        By = By_new

        ! Output progress
        if (mod(n, 100) == 0) then
            print *, "Time step:", n
        end if
    end do

    ! Final output
    print *, "Simulation complete!"


    ! Write v to HDF5 file
    call h5fcreate_f("velocity_v.h5", H5F_ACC_TRUNC_F, file_id, error)
    if (error /= 0) stop "Failed to create HDF5 file"

    call h5screate_simple_f(2, dims, dataspace_id, error)
    if (error /= 0) stop "Failed to create dataspace"

    call h5dcreate_f(file_id, "velocity_v", H5T_NATIVE_DOUBLE, dataspace_id, dset_id, error)
    if (error /= 0) stop "Failed to create dataset"

    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, v, dims, error)
    if (error /= 0) stop "Failed to write to dataset"

    call h5dclose_f(dset_id, error)
    call h5sclose_f(dataspace_id, error)
    call h5fclose_f(file_id, error)
    call h5close_f(error)


    ! Final output
    print *, "Simulation complete!"

end program mhd_solver
