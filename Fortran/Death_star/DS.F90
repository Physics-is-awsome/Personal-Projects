program planet_destruction
  implicit none
  integer, parameter :: nr = 100, nt = 50  ! Grid size (radial, theta)
  real, parameter :: r_planet = 6.371e6    ! Planet radius (m)
  real, parameter :: dt = 1.0e-3           ! Time step (s)
  real, parameter :: t_max = 10.0           ! Simulation time (s)
  real, parameter :: rho_0 = 3000.0        ! Planet density (kg/m^3)
  real, parameter :: beam_energy = 1.0e20  ! Beam energy (J)
  real, parameter :: sigma = 1.0e5         ! Beam width (m)
  integer, parameter :: output_freq = 10    ! Output every 10 time steps

  real :: r(nr), theta(nt), dr, dtheta
  real :: rho(nr, nt), u(nr, nt), v(nr, nt), e(nr, nt)  ! Density, velocities, energy
  real :: p(nr, nt)                                     ! Pressure
  integer :: i, j, n, step_count                ! Added step_count for output
  real :: t, r2, q
  character(len=20) :: filename                 ! For dynamic file names

  ! Initialize grid
  dr = r_planet / real(nr - 1)
  dtheta = 3.14159 / real(nt - 1)
  do i = 1, nr
    r(i) = (i - 1) * dr
    do j = 1, nt
      theta(j) = (j - 1) * dtheta
      rho(i, j) = rho_0
      u(i, j) = 0.0
      v(i, j) = 0.0
      e(i, j) = 0.0
      p(i, j) = 0.0
    end do
  end do

  ! Initialize time and step counter
  t = 0.0
  step_count = 0

  ! Time loop
  do while (t < t_max)
    step_count = step_count + 1  ! Increment step counter

    ! Apply plasma beam energy at surface (r = r_planet, theta = 0)
    do i = 1, nr
      do j = 1, nt
        r2 = (r(i) - r_planet)**2 + (theta(j) * r_planet)**2
        q = (beam_energy / (2.0 * 3.14159 * sigma**2)) * exp(-r2 / (2.0 * sigma**2))
        e(i, j) = e(i, j) + q * dt / rho(i, j)  ! Update energy
      end do
    end do

    ! Update hydrodynamics (simplified Euler equations)
    do i = 2, nr - 1
      do j = 2, nt - 1
        ! Pressure from ideal gas EOS (example: P = (gamma-1) * rho * e)
        p(i, j) = (1.4 - 1.0) * rho(i, j) * e(i, j)
        ! Update velocities (momentum conservation, simplified)
        u(i, j) = u(i, j) - dt * (p(i+1, j) - p(i-1, j)) / (2.0 * dr * rho(i, j))
        v(i, j) = v(i, j) - dt * (p(i, j+1) - p(i, j-1)) / (2.0 * dtheta * rho(i, j))
        ! Update density (continuity equation, simplified)
        rho(i, j) = rho(i, j) - dt * rho(i, j) * &
                    ((u(i+1, j) - u(i-1, j)) / (2.0 * dr) + &
                     (v(i, j+1) - v(i, j-1)) / (2.0 * dtheta))
      end do
    end do

    ! Output data every output_freq steps
    if (mod(step_count, output_freq) == 0) then
      write(filename, '(A10, I4.4, A4)') 'output_t', step_count / output_freq, '.dat'
      open(unit=10, file=filename, status='replace')
      do i = 1, nr
        do j = 1, nt
          write(10, *) r(i), theta(j), rho(i, j), e(i, j)
        end do
      end do
      close(10)
      print *, 'Wrote file: ', filename
    end if

    ! Update time
    t = t + dt
    print *, 'Time = ', t
  end do

end program planet_destruction
