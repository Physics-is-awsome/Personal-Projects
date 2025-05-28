program planet_destruction
  implicit none
  integer, parameter :: nr = 200, nt = 100  ! Increased resolution
  real, parameter :: r_planet = 6.371e6     ! Planet radius (m)
  real, parameter :: t_max = 10.0           ! Simulation time (s)
  real, parameter :: rho_0 = 3000.0         ! Planet density (kg/m^3)
  real, parameter :: beam_energy = 1.0e20   ! Beam energy (J)
  real, parameter :: sigma = 1.0e5          ! Beam width (m)
  real, parameter :: G = 6.67430e-11       ! Gravitational constant (m^3 kg^-1 s^-2)
  real, parameter :: M_planet = 5.972e24    ! Earth-like planet mass (kg)
  integer, parameter :: output_freq = 10     ! Output every 10 steps
  real, parameter :: cfl = 0.5              ! CFL number for stability

  real :: r(nr), theta(nt), dr, dtheta
  real :: rho(nr, nt), u(nr, nt), v(nr, nt), e(nr, nt)  ! Density, velocities, energy
  real :: p(nr, nt), cs(nr, nt)                        ! Pressure, sound speed
  real :: grav_acc(nr)                                 ! Gravitational acceleration
  integer :: i, j, step_count, ierr
  real :: t, r2, q, cs_max, dt_cfl
  character(len=20) :: filename
  real :: dt = 1.0e-3
  ! Initialize grid
  dr = r_planet / real(nr - 1)
  dtheta = 3.14159 / real(nt - 1)
  do i = 1, nr
    r(i) = (i - 1) * dr
    grav_acc(i) = -G * M_planet / max(r(i)**2, 1.0e3**2)  ! Gravity, avoid r=0
    do j = 1, nt
      theta(j) = (j - 1) * dtheta
      rho(i, j) = rho_0
      u(i, j) = 0.0
      v(i, j) = 0.0
      e(i, j) = 1.0e6  ! Small initial energy to set sound speed
      p(i, j) = 0.0
      cs(i, j) = 0.0
    end do
  end do

  ! Initialize time and step counter
  t = 0.0
  step_count = 0

  ! Time loop
  do while (t < t_max)
    step_count = step_count + 1

    ! Compute sound speed and CFL time step
    cs_max = 0.0
    do i = 1, nr
      do j = 1, nt
        ! Simplified Tillotson-like EOS: cs = sqrt(gamma * p / rho)
        cs(i, j) = sqrt(1.4 * max(p(i, j), 1.0e-10) / max(rho(i, j), 1.0e-10))
        cs_max = max(cs_max, cs(i, j))
      end do
    end do
    dt_cfl = cfl * min(dr, r_planet * dtheta) / cs_max
    dt = min(dt, dt_cfl)  ! Use smallest of initial or CFL time step

    ! Apply plasma beam energy
    do i = 1, nr
      do j = 1, nt
        r2 = (r(i) - r_planet)**2 + (theta(j) * r_planet)**2
        q = (beam_energy / (2.0 * 3.14159 * sigma**2)) * exp(-r2 / (2.0 * sigma**2))
        e(i, j) = e(i, j) + q * dt / max(rho(i, j), 1.0e-10)
      end do
    end do

    ! Update hydrodynamics (with gravity)
    do i = 2, nr - 1
      do j = 2, nt - 1
        ! Simplified Tillotson-like EOS (approximate)
        p(i, j) = (1.4 - 1.0) * rho(i, j) * e(i, j)  ! Replace with better EOS later
        ! Momentum equations with gravity
        u(i, j) = u(i, j) - dt * (p(i+1, j) - p(i-1, j)) / (2.0 * dr * rho(i, j)) &
                  + dt * grav_acc(i)
        v(i, j) = v(i, j) - dt * (p(i, j+1) - p(i, j-1)) / (2.0 * dtheta * rho(i, j))
        ! Continuity equation
        rho(i, j) = rho(i, j) - dt * rho(i, j) * &
                    ((u(i+1, j) - u(i-1, j)) / (2.0 * dr) + &
                     (v(i, j+1) - v(i, j-1)) / (2.0 * dtheta))
      end do
    end do

    ! Apply reflective boundary conditions (simple)
    do j = 1, nt
      rho(1, j) = rho(2, j)
      u(1, j) = -u(2, j)  ! Reflect velocity
      v(1, j) = v(2, j)
      e(1, j) = e(2, j)
      p(1, j) = p(2, j)
      rho(nr, j) = rho(nr-1, j)
      u(nr, j) = -u(nr-1, j)
      v(nr, j) = v(nr-1, j)
      e(nr, j) = e(nr-1, j)
      p(nr, j) = p(nr-1, j)
    end do
    do i = 1, nr
      rho(i, 1) = rho(i, 2)
      u(i, 1) = u(i, 2)
      v(i, 1) = -v(i, 2)
      e(i, 1) = e(i, 2)
      p(i, 1) = p(i, 2)
      rho(i, nt) = rho(i, nt-1)
      u(i, nt) = u(i, nt-1)
      v(i, nt) = -v(i, nt-1)
      e(i, nt) = e(i, nt-1)
      p(i, nt) = p(i, nt-1)
    end do

    ! Output data
    if (mod(step_count, output_freq) == 0) then
      write(filename, '(A, I4.4, A)') 'output_t', step_count / output_freq, '.dat'
      open(unit=10, file=filename, status='replace', iostat=ierr)
      if (ierr /= 0) then
        print *, 'Error opening file: ', trim(filename), ' Error code: ', ierr
        stop
      end if
      do i = 1, nr
        do j = 1, nt
          write(10, *) r(i), theta(j), rho(i, j), e(i, j)
        end do
      end do
      close(10)
      write(*, '(A,A)') 'Wrote file: ', trim(filename)
    end if

    ! Update time
    t = t + dt
    write(*, '(A,F10.3,A,I6,A,F10.6)') 'Time = ', t, ' Step = ', step_count, ' dt = ', dt
  end do

end program planet_destruction
