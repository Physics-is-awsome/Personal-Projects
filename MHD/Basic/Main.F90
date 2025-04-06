!============================================================
! Main Program: MHD Solver
!============================================================
program mhd_solver
    use MHD_Module
    use hdf5
    implicit none


    ! Parameters
    integer, parameter :: Nx = 50, Ny = 50           ! Grid dimensions
    integer, parameter :: Nt = 1000                  ! Number of time steps
    real(kind=8), parameter :: Lx = 1.0, Ly = 1.0    ! Domain size
    real(kind=8), parameter :: dt = 0.001            ! Time step size
    real(kind=8), parameter :: Re = 100.0            ! Reynolds number
    real(kind=8), parameter :: Rm = 100.0            ! Magnetic Reynolds number
    real(kind=8), parameter :: dx = Lx / Nx          ! Grid spacing in x
    real(kind=8), parameter :: dy = Ly / Ny          ! Grid spacing in y

    ! Field arrays
    real(kind=8) :: u(Nx, Ny), v(Nx, Ny)             ! Velocity components
    real(kind=8) :: Bx(Nx, Ny), By(Nx, Ny)           ! Magnetic field components
    real(kind=8) :: p(Nx, Ny)                        ! Pressure field
    real(kind=8) :: Jx(Nx, Ny), Jy(Nx, Ny)           ! Current density
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
    call initialize_fields(u, v, Bx, By, p)

    ! Initialize HDF5 library
    call h5open_f(error)
    if (error /= 0) stop "Failed to initialize HDF5 library"

    !============================================================
    ! Time-stepping loop
    !============================================================
    do n = 1, Nt
        ! Compute current density (J = curl(B))
        call compute_current(Bx, By, Jx, Jy, dx, dy)

        ! Update velocity field (momentum equation)
        call update_velocity(u, v, Jx, Jy, Bx, By, p, u_new, v_new, dx, dy, dt, Re)

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
