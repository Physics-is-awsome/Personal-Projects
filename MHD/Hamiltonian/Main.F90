program mhd_hamiltonian_2d
  implicit none
  integer, parameter :: nx = 64, ny = 64
  real(8), parameter :: Lx = 2.0*3.141592653589793, Ly = 2.0*3.141592653589793
  real(8), parameter :: dx = Lx/nx, dy = Ly/ny
  real(8), parameter :: dt = 0.005, t_max = 5.0
  real(8), parameter :: rho = 1.0
  integer :: i, j, step, n_steps
  real(8), dimension(nx,ny) :: vx, vy, Bx, By
  real(8), dimension(nx,ny) :: vx_half, vy_half, Bx_new, By_new
  real(8) :: energy, t
  character(len=20) :: filename

  ! Initialize fields
  call initialize_fields(vx, vy, Bx, By)

  ! Number of time steps
  n_steps = int(t_max / dt)

  ! Open file for energy diagnostics
  open(unit=10, file='energy.dat', status='replace')

  ! Main time loop
  t = 0.0
  do step = 1, n_steps
     ! Compute energy
     energy = compute_energy(vx, vy, Bx, By)
     write(10, *) t, energy

     ! Write fields every 50 steps
     if (mod(step, 50) == 0) then
        write(filename, '("fields_", i5.5, ".dat")') step
        call write_fields(vx, vy, Bx, By, filename)
     end if

     ! Leapfrog step
     call leapfrog_step(vx, vy, Bx, By, vx_half, vy_half, Bx_new, By_new)
     vx = vx_half
     vy = vy_half
     Bx = Bx_new
     By = By_new
     t = t + dt
  end do

  close(10)
  print *, 'Simulation complete. Energy data in energy.dat, fields in fields_*.dat'

contains

  subroutine initialize_fields(vx, vy, Bx, By)
    real(8), dimension(nx,ny), intent(out) :: vx, vy, Bx, By
    integer :: i, j
    real(8) :: x, y
    do j = 1, ny
       y = (j-1)*dy
       do i = 1, nx
          x = (i-1)*dx
          vx(i,j) = 0.1 * sin(x) * cos(y)
          vy(i,j) = 0.1 * cos(x) * sin(y)
          Bx(i,j) = 0.1 * cos(x) * sin(y)
          By(i,j) = -0.1 * sin(x) * cos(y)  ! Ensures div B = 0
       end do
    end do
  end subroutine initialize_fields

  function ddx(f) result(df)
    real(8), dimension(nx,ny), intent(in) :: f
    real(8), dimension(nx,ny) :: df
    integer :: i, j, ip, im
    do j = 1, ny
       do i = 1, nx
          ip = mod(i, nx) + 1
          im = mod(i-2+nx, nx) + 1
          df(i,j) = (f(ip,j) - f(im,j)) / (2.0*dx)
       end do
    end do
  end function ddx

  function ddy(f) result(df)
    real(8), dimension(nx,ny), intent(in) :: f
    real(8), dimension(nx,ny) :: df
    integer :: i, j, jp, jm
    do j = 1, ny
       jm = mod(j-2+ny, ny) + 1
       jp = mod(j, ny) + 1
       do i = 1, nx
          df(i,j) = (f(i,jp) - f(i,jm)) / (2.0*dy)
       end do
    end do
  end function ddy

  subroutine compute_derivatives(vx, vy, Bx, By, dvx_dt, dvy_dt, dBx_dt, dBy_dt)
    real(8), dimension(nx,ny), intent(in) :: vx, vy, Bx, By
    real(8), dimension(nx,ny), intent(out) :: dvx_dt, dvy_dt, dBx_dt, dBy_dt
    real(8), dimension(nx,ny) :: tmp
    ! dvx/dt = -(vx * d(vx)/dx + vy * d(vx)/dy) + (Bx * d(Bx)/dx + By * d(Bx)/dy)
    dvx_dt = -vx * ddx(vx) - vy * ddy(vx) + (Bx * ddx(Bx) + By * ddy(Bx)) / rho
    ! dvy/dt = -(vx * d(vy)/dx + vy * d(vy)/dy) + (Bx * d(By)/dx + By * d(By)/dy)
    dvy_dt = -vx * ddx(vy) - vy * ddy(vy) + (Bx * ddx(By) + By * ddy(By)) / rho
    ! dBx/dt = d(vy * Bx - vx * By)/dy
    tmp = vy * Bx - vx * By
    dBx_dt = ddy(tmp)
    ! dBy/dt = -d(vy * Bx - vx * By)/dx
    dBy_dt = -ddx(tmp)
  end subroutine compute_derivatives

  subroutine leapfrog_step(vx, vy, Bx, By, vx_new, vy_new, Bx_new, By_new)
    real(8), dimension(nx,ny), intent(in) :: vx, vy, Bx, By
    real(8), dimension(nx,ny), intent(out) :: vx_new, vy_new, Bx_new, By_new
    real(8), dimension(nx,ny) :: dvx_dt, dvy_dt, dBx_dt, dBy_dt
    ! Compute derivatives at current state
    call compute_derivatives(vx, vy, Bx, By, dvx_dt, dvy_dt, dBx_dt, dBy_dt)
    ! Half-step for velocity
    vx_new = vx + 0.5 * dt * dvx_dt
    vy_new = vy + 0.5 * dt * dvy_dt
    ! Full step for magnetic field
    Bx_new = Bx + dt * dBx_dt
    By_new = By + dt * dBy_dt
    ! Compute derivatives at half-step
    call compute_derivatives(vx_new, vy_new, Bx_new, By_new, dvx_dt, dvy_dt, dBx_dt, dBy_dt)
    ! Full step for velocity
    vx_new = vx + dt * dvx_dt
    vy_new = vy + dt * dvy_dt
  end subroutine leapfrog_step

  function compute_energy(vx, vy, Bx, By) result(energy)
    real(8), dimension(nx,ny), intent(in) :: vx, vy, Bx, By
    real(8) :: energy
    energy = sum(0.5 * rho * (vx**2 + vy**2) + 0.5 * (Bx**2 + By**2)) * dx * dy
  end function compute_energy

  subroutine write_fields(vx, vy, Bx, By, filename)
    real(8), dimension(nx,ny), intent(in) :: vx, vy, Bx, By
    character(len=*), intent(in) :: filename
    integer :: i, j
    open(unit=11, file=filename, status='replace')
    do j = 1, ny
       do i = 1, nx
          write(11, *) (i-1)*dx, (j-1)*dy, vx(i,j), vy(i,j), Bx(i,j), By(i,j)
       end do
       write(11, *)
    end do
    close(11)
  end subroutine write_fields

end program mhd_hamiltonian_2d
