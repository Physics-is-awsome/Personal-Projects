program mhd_hamiltonian_3d
  implicit none
  integer, parameter :: nx = 32, ny = 32, nz = 32
  real(8), parameter :: Lx = 2.0*3.141592653589793, Ly = 2.0*3.141592653589793, Lz = 2.0*3.141592653589793
  real(8), parameter :: dx = Lx/nx, dy = Ly/ny, dz = Lz/nz
  real(8), parameter :: dt = 0.002, t_max = 2.0
  real(8), parameter :: rho = 1.0
  integer :: i, j, k, step, n_steps
  real(8), dimension(nx,ny,nz) :: vx, vy, vz, Bx, By, Bz
  real(8), dimension(nx,ny,nz) :: vx_half, vy_half, vz_half, Bx_new, By_new, Bz_new
  real(8) :: energy, t
  character(len=20) :: filename

  ! Initialize fields
  call initialize_fields(vx, vy, vz, Bx, By, Bz)

  ! Number of time steps
  n_steps = int(t_max / dt)

  ! Open file for energy diagnostics
  open(unit=10, file='energy.dat', status='replace')

  ! Main time loop
  t = 0.0
  do step = 1, n_steps
     ! Compute energy
     energy = compute_energy(vx, vy, vz, Bx, By, Bz)
     write(10, *) t, energy

     ! Write fields every 100 steps
     if (mod(step, 100) == 0) then
        write(filename, '("fields_", i5.5, ".dat")') step
        call write_fields(vx, vy, vz, Bx, By, Bz, filename)
     end if

     ! Leapfrog step
     call leapfrog_step(vx, vy, vz, Bx, By, Bz, vx_half, vy_half, vz_half, Bx_new, By_new, Bz_new)
     vx = vx_half
     vy = vy_half
     vz = vz_half
     Bx = Bx_new
     By = By_new
     Bz = Bz_new
     t = t + dt
  end do

  close(10)
  print *, 'Simulation complete. Energy data in energy.dat, fields in fields_*.dat'

contains

  subroutine initialize_fields(vx, vy, vz, Bx, By, Bz)
    real(8), dimension(nx,ny,nz), intent(out) :: vx, vy, vz, Bx, By, Bz
    integer :: i, j, k
    real(8) :: x, y, z
    do k = 1, nz
       z = (k-1)*dz
       do j = 1, ny
          y = (j-1)*dy
          do i = 1, nx
             x = (i-1)*dx
             vx(i,j,k) = 0.1 * sin(x) * cos(y) * cos(z)
             vy(i,j,k) = 0.1 * cos(x) * sin(y) * cos(z)
             vz(i,j,k) = -0.2 * cos(x) * cos(y) * sin(z) ! Ensures div v = 0
             Bx(i,j,k) = 0.1 * cos(x) * sin(y) * cos(z)
             By(i,j,k) = 0.1 * sin(x) * cos(y) * cos(z)
             Bz(i,j,k) = -0.2 * sin(x) * sin(y) * sin(z) ! Ensures div B = 0
          end do
       end do
    end do
  end subroutine initialize_fields

  function ddx(f) result(df)
    real(8), dimension(nx,ny,nz), intent(in) :: f
    real(8), dimension(nx,ny,nz) :: df
    integer :: i, j, k, ip, im
    do k = 1, nz
       do j = 1, ny
          do i = 1, nx
             ip = mod(i, nx) + 1
             im = mod(i-2+nx, nx) + 1
             df(i,j,k) = (f(ip,j,k) - f(im,j,k)) / (2.0*dx)
          end do
       end do
    end do
  end function ddx

  function ddy(f) result(df)
    real(8), dimension(nx,ny,nz), intent(in) :: f
    real(8), dimension(nx,ny,nz) :: df
    integer :: i, j, k, jp, jm
    do k = 1, nz
       do j = 1, ny
          jm = mod(j-2+ny, ny) + 1
          jp = mod(j, ny) + 1
          do i = 1, nx
             df(i,j,k) = (f(i,jp,k) - f(i,jm,k)) / (2.0*dy)
          end do
       end do
    end do
  end function ddy

  function ddz(f) result(df)
    real(8), dimension(nx,ny,nz), intent(in) :: f
    real(8), dimension(nx,ny,nz) :: df
    integer :: i, j, k, kp, km
    do k = 1, nz
       km = mod(k-2+nz, nz) + 1
       kp = mod(k, nz) + 1
       do j = 1, ny
          do i = 1, nx
             df(i,j,k) = (f(i,j,kp) - f(i,j,km)) / (2.0*dz)
          end do
       end do
    end do
  end function ddz

  subroutine compute_derivatives(vx, vy, vz, Bx, By, Bz, dvx_dt, dvy_dt, dvz_dt, dBx_dt, dBy_dt, dBz_dt)
    real(8), dimension(nx,ny,nz), intent(in) :: vx, vy, vz, Bx, By, Bz
    real(8), dimension(nx,ny,nz), intent(out) :: dvx_dt, dvy_dt, dvz_dt, dBx_dt, dBy_dt, dBz_dt
    real(8), dimension(nx,ny,nz) :: tmp_x, tmp_y, tmp_z
    ! Momentum equations
    dvx_dt = -vx * ddx(vx) - vy * ddy(vx) - vz * ddz(vx) + (Bx * ddx(Bx) + By * ddy(Bx) + Bz * ddz(Bx)) / rho
    dvy_dt = -vx * ddx(vy) - vy * ddy(vy) - vz * ddz(vy) + (Bx * ddx(By) + By * ddy(By) + Bz * ddz(By)) / rho
    dvz_dt = -vx * ddx(vz) - vy * ddy(vz) - vz * ddz(vz) + (Bx * ddx(Bz) + By * ddy(Bz) + Bz * ddz(Bz)) / rho
    ! Induction equations
    tmp_x = vz * By - vy * Bz
    tmp_y = vx * Bz - vz * Bx
    tmp_z = vy * Bx - vx * By
    dBx_dt = ddy(tmp_x) - ddz(tmp_y)
    dBy_dt = ddz(tmp_y) - ddx(tmp_z)
    dBz_dt = ddx(tmp_z) - ddy(tmp_x)
  end subroutine compute_derivatives

  subroutine leapfrog_step(vx, vy, vz, Bx, By, Bz, vx_new, vy_new, vz_new, Bx_new, By_new, Bz_new)
    real(8), dimension(nx,ny,nz), intent(in) :: vx, vy, vz, Bx, By, Bz
    real(8), dimension(nx,ny,nz), intent(out) :: vx_new, vy_new, vz_new, Bx_new, By_new, Bz_new
    real(8), dimension(nx,ny,nz) :: dvx_dt, dvy_dt, dvz_dt, dBx_dt, dBy_dt, dBz_dt
    ! Compute derivatives at current state
    call compute_derivatives(vx, vy, vz, Bx, By, Bz, dvx_dt, dvy_dt, dvz_dt, dBx_dt, dBy_dt, dBz_dt)
    ! Half-step for velocity
    vx_new = vx + 0.5 * dt * dvx_dt
    vy_new = vy + 0.5 * dt * dvy_dt
    vz_new = vz + 0.5 * dt * dvz_dt
    ! Full step for magnetic field
    Bx_new = Bx + dt * dBx_dt
    By_new = By + dt * dBy_dt
    Bz_new = Bz + dt * dBz_dt
    ! Compute derivatives at half-step
    call compute_derivatives(vx_new, vy_new, vz_new, Bx_new, By_new, Bz_new, dvx_dt, dvy_dt, dvz_dt, dBx_dt, dBy_dt, dBz_dt)
    ! Full step for velocity
    vx_new = vx + dt * dvx_dt
    vy_new = vy + dt * dvy_dt
    vz_new = vz + dt * dvz_dt
  end subroutine leapfrog_step

  function compute_energy(vx, vy, vz, Bx, By, Bz) result(energy)
    real(8), dimension(nx,ny,nz), intent(in) :: vx, vy, vz, Bx, By, Bz
    real(8) :: energy
    energy = sum(0.5 * rho * (vx**2 + vy**2 + vz**2) + 0.5 * (Bx**2 + By**2 + Bz**2)) * dx * dy * dz
  end function compute_energy

  subroutine write_fields(vx, vy, vz, Bx, By, Bz, filename)
    real(8), dimension(nx,ny,nz), intent(in) :: vx, vy, vz, Bx, By, Bz
    character(len=*), intent(in) :: filename
    integer :: i, j, k
    open(unit=11, file=filename, status='replace')
    do k = 1, nz
       do j = 1, ny
          do i = 1, nx
             write(11, *) (i-1)*dx, (j-1)*dy, (k-1)*dz, vx(i,j,k), vy(i,j,k), vz(i,j,k), Bx(i,j,k), By(i,j,k), Bz(i,j,k)
          end do
          write(11, *)
       end do
       write(11, *)
    end do
    close(11)
  end subroutine write_fields

end program mhd_hamiltonian_3d
