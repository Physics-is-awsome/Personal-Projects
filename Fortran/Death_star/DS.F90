program planet_destruction
  implicit none
  integer, parameter :: nr = 200, nt = 100
  real, parameter :: r_planet = 6.371e6
  real, parameter :: t_max = 10.0
  real, parameter :: rho_0 = 3000.0
  real, parameter :: beam_energy = 1.0e20
  real, parameter :: sigma = 1.0e5
  real, parameter :: G = 6.67430e-11
  real, parameter :: M_planet = 5.972e24
  integer, parameter :: output_freq = 10
  real, parameter :: cfl = 0.5
  real, parameter :: cs_max_limit = 1.0e5  ! Max sound speed (m/s, ~100 km/s)
  real, parameter :: rho_min = 1.0e-3      ! Min density (kg/m^3)
  real, parameter :: dt_min = 1.0e-6       ! Min time step (s)

  real :: r(nr), theta(nt), dr, dtheta
  real :: rho(nr, nt), u(nr, nt), v(nr, nt), e(nr, nt)
  real :: p(nr, nt), cs(nr, nt)
  real :: grav_acc(nr)
  integer :: i, j, step_count, ierr
  real :: t, dt, r2, q, cs_max, dt_cfl
  character(len=20) :: filename

  ! Initialize grid
  dr = r_planet / real(nr - 1)
  dtheta = 3.14159 / real(nt - 1)
  do i = 1, nr
    r(i) = (i - 1) * dr
    grav_acc(i) = -G * M_planet / max(r(i)**2, 1.0e3**2)
    do j = 1, nt
      theta(j) = (j - 1) * dtheta
      rho(i, j) = rho_0
      u(i, j) = 0.0
      v(i, j) = 0.0
      e(i, j) = 1.0e6
      p(i, j) = 0.0
      cs(i, j) = 0.0
    end do
  end do

  ! Initialize time and step counter
  t = 0.0
  step_count = 0
  dt = 1.0e-3

  ! Time loop
  do while (t < t_max)
    step_count = step_count + 1

    ! Compute sound speed and CFL time step
    cs_max = 0.0
    do i = 1, nr
      do j = 1, nt
        p(i, j) = (1.4 - 1.0) * rho(i, j) * e(i, j)  ! Update pressure first
        cs(i, j) = sqrt(1.4 * max(p(i, j), 1.0e-10) / max(rho(i, j), rho_min))
        cs(i, j) = min(cs(i, j), cs_max_limit)  ! Cap sound speed
        cs_max = max(cs_max, cs(i, j))
      end do
    end do
    dt_cfl = cfl * min(dr, r_planet * dtheta) / max(cs_max, 1.0e-10)
    dt = max(min(dt, dt_cfl), dt_min)  ! Enforce min time step

    ! Apply plasma beam energy
    do i = 1, nr
      do j = 1, nt
        r2 = (r(i) - r_planet)**2 + (theta(j) * r_planet)**2
        q = (beam_energy / (2.0 * 3.14159 * sigma**2)) * exp(-r2 / (2.0 * sigma**2))
        e(i, j) = e(i, j) + q * dt / max(rho(i, j), rho_min)
      end do
    end do

    ! Update hydrodynamics
    do i = 2, nr - 1
      do j = 2, nt - 1
        p(i, j) = (1.4 - 1.0) * rho(i, j) * e(i, j)
        u(i, j) = u(i, j) - dt * (p(i+1, j) - p(i-1, j)) / (2.0 * dr * max(rho(i, j), rho_min)) &
                  + dt * grav_acc(i)
        v(i, j) = v(i, j) - dt * (p(i, j+1) - p(i, j-1)) / (2.0 * dtheta * max(rho(i, j), rho_min))
        rho(i, j) = rho(i, j) - dt * rho(i, j) * &
                    ((u(i+1, j) - u(i-1, j)) / (2.0 * dr) + &
                     (v(i, j+1) - v(i, j-1)) / (2.0 * dtheta))
        rho(i, j) = max(rho(i, j), rho_min)  ! Prevent negative density
      end do
    end do

    ! Apply reflective boundary conditions
    do j = 1, nt
      rho(1, j) = max(rho(2, j), rho_min)
      u(1, j) = -u(2, j)
      v(1, j) = v(2, j)
      e(1, j) = e(2, j)
      p(1, j) = p(2, j)
      rho(nr, j) = max(rho(nr-1, j), rho_min)
      u(nr, j) = -u(nr-1, j)
      v(nr, j) = v(nr-1, j)
      e(nr, j) = e(nr-1, j)
      p(nr, j) = p(nr-1, j)
    end do
    do i = 1, nr
      rho(i, 1) = max(rho(i, 2), rho_min)
      u(i, 1) = u(i, 2)
      v(i, 1) = -v(i, 2)
      e(i, 1) = e(i, 2)
      p(i, 1) = p(i, 2)
      rho(i, nt) = max(rho(i, nt-1), rho_min)
      u(i, nt) = u(i, nt-1)
      v(i, nt) = -v(i, nt-1)
      e(i, nt) = e(i, nt-1)
      p(i, nt) = p(i, nt-1)
    end do

    ! Output data
    if (mod(step_count, output_freq) == 0) then
      write(filename, '(A10, I4.4, A4)') 'output_t', step_count / output_freq, '.dat'
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
      write(*, '(A,A,A,F10.6,A,F10.2)') 'Wrote file: ', trim(filename), ' dt = ', dt, ' cs_max = ', cs_max
    end if

    ! Update time
    t = t + dt
    write(*, '(A,F10.3,A,I6,A,F10.6,A,F10.2)') 'Time = ', t, ' Step = ', step_count, &
                                               ' dt = ', dt, ' cs_max = ', cs_max
  end do

end program planet_destruction
