program mhd_solver
    use MHD_Module
    use Initial_var
    use hdf5
    implicit none
    !==================
    ! Parameters
    !=======================
    ! Field arrays
    real(kind=8) :: u(Nx, Ny), v(Nx, Ny)             ! Velocity components
    real(kind=8) :: Bx(Nx, Ny), By(Nx, Ny)           ! Magnetic field components
    real(kind=8) :: p(Nx, Ny)                        ! Pressure field
    real(kind=8) :: Jz(Nx, Ny)                       ! Current density
    real(kind=8) :: u_new(Nx, Ny), v_new(Nx, Ny)     ! Updated velocity components
    real(kind=8) :: Bx_new(Nx, Ny), By_new(Nx, Ny)   ! Updated magnetic field components
    real(kind=8) :: T_new(Nx, Ny)                    ! Updated temperature
    real(kind=8) :: rho_new(Nx, Ny)                  ! Updated density
    integer :: stat
    integer :: n                                      ! Time step counter

    ! HDF5 variables
    integer(hid_t) :: file_id, dset_id, dataspace_id
    integer :: error
    integer(hsize_t), dimension(2) :: dims = (/Nx, Ny/)

    !============================================================
    ! Initialize fields
    !============================================================
    ! Initialize module variables
    call initialize_variables

    call velocity_fields(u, v, Bx, By, p)

    call heat_fields(Lx, Ly)

    ! Initialize HDF5 library
    call h5open_f(error)
    if (error /= 0) stop "Failed to initialize HDF5 library"

    !============================================================
    ! Time-stepping loop
    !============================================================
    do n = 1, Nt
        ! Compute current density (J = curl(B))
        call compute_current(Bx, By, Jz)

        ! Update velocity field (momentum equation)
        call update_velocity(u, v, Jz, Bx, By, p, u_new, v_new)

        ! Update temperature
        call solve_heat_equation(Jz, T_new)
        T = T_new  ! Update global temperature

        ! Update density (continuity equation)
        call update_density(rho, u, v, rho_new)
        rho = rho_new

        ! Update pressure from temperature and density
        call compute_pressure(rho, p)

        ! Update magnetic field (induction equation)
        call update_magnetic_field(Bx, By, u, v, Bx_new, By_new)

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
