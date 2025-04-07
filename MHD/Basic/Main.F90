!============================================================
! Main Program: MHD Solver
!============================================================
program mhd_solver
    use MHD_Module
    use hdf5
    implicit none


    real(kind=8), intent(out) :: Nx, Ny, Nt, Lx, Ly, dt, Re, Rm
    character(len=256) :: line
    integer :: unit, io_stat

    open(unit=10, file='config.txt', status='old', action='read', iostat=io_stat )
    IF (io_stat /= 0) THEN
        PRINT *, "Error opening config file:", io_stat
        STOP
    END IF
    do
        read(unit, '(A)') line
        if (index(line, 'Nx_value') /= 0) read(line, '("Nx_value =", F8.2)') Nx
        if (index(line, 'Ny_value') /= 0) read(line, '("Ny_value =", F8.2)') Ny
        if (index(line, 'Nt_value') /= 0) read(line, '("Nt_value =", F8.2)') Nt
        if (index(line, 'Lx_value') /= 0) read(line, '("Nx_value =", F8.2)') Lx
        if (index(line, 'Ly_value') /= 0) read(line, '("Ly_value =", F8.2)') Ly
        if (index(line, 'dt_value') /= 0) read(line, '("dt_value =", F8.2)') dt
        if (index(line, 'Re_value') /= 0) read(line, '("Re_value =", F8.2)') Re
        if (index(line, 'Rm_value') /= 0) read(line, '("Rm_value =", F8.2)') Rm
    end do


    close(unit)
    ! Parameters

    real(kind=8), parameter :: dx = Lx / Nx          ! Grid spacing in x
    real(kind=8), parameter :: dy = Ly / Ny          ! Grid spacing in y

    ! Field arrays
    real(kind=8) :: u(Nx, Ny), v(Nx, Ny)             ! Velocity components
    real(kind=8) :: Bx(Nx, Ny), By(Nx, Ny)           ! Magnetic field components
    real(kind=8) :: p(Nx, Ny)                        ! Pressure field
    real(kind=8) :: Jz(Nx, Ny)                       ! Current density
    real(kind=8) :: u_new(Nx, Ny), v_new(Nx, Ny)     ! Updated velocity components
    real(kind=8) :: Bx_new(Nx, Ny), By_new(Nx, Ny)   ! Updated magnetic field components

    integer :: n                                      ! Time step counter

    ! HDF5 variables
    integer(hid_t) :: file_id, dset_id, dataspace_id
    integer :: error
    integer(hsize_t), dimension(2) :: dims = (/Nx, Ny/)

    !============================================================
    ! Initialize fields
    !============================================================
    call read_config(u(i, j), v(i, j), Bx(i, j), By(i, j), p(i, j))

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
