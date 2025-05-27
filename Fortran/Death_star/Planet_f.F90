program sph_planet_formation
  implicit none
  integer, parameter :: n = 500  ! Number of particles
  real, parameter :: G = 1.0     ! Gravitational constant
  real, parameter :: dt = 0.005  ! Time step
  real, parameter :: t_max = 10.0 ! Total simulation time
  real, parameter :: h = 0.5     ! Smoothing length
  real, parameter :: eps = 0.2  ! Gravitational softening length
  real, parameter :: m_central = 100.0 ! Central body mass
  real, parameter :: coll_dist = 0.1 ! Distance for repulsive force
  real, parameter :: k_coll = 50.0  ! Repulsive force constant
  real, parameter :: viscosity = 3.0 ! Artificial viscosity coefficient
  real, parameter :: k_press = 1.0  ! Pressure constant (equation of state)
  real, parameter :: gamma = 5.0/3.0 ! Adiabatic index
  integer, parameter :: n_steps = int(t_max / dt)
  real :: x(n), y(n), vx(n), vy(n), mass(n), ax(n), ay(n), rho(n), P(n)
  real :: r, w, dwdr, force, fx, fy, r_disk, v_circ, m
  real :: mu, v_dot_r, pi_term, visc_term
  integer :: i, j, step
  real :: t

  ! Random number seed
  call random_seed()

  ! Initialize particles in a disk with circular velocities
  do i = 1, n
    call random_number(r_disk)
    call random_number(t)
    call random_number(m)
    r_disk = r_disk * 2.0  ! Random radius in [0, 5]
    t = t * 2.0 * 3.141592653589793  ! Random angle
    x(i) = r_disk * cos(t)
    y(i) = r_disk * sin(t)
    v_circ = sqrt(G * m_central / r_disk) * 0.4
    vx(i) = -v_circ * sin(t) - 0.01 * x(i) / r_disk
    vy(i) = v_circ * cos(t) - 0.01 * y(i) / r_disk
    mass(i) = m * 10
  end do

  ! Open file for output
  open(unit=10, file='particle_positions.dat', status='replace')

  ! Main simulation loop
  do step = 1, n_steps
    t = step * dt

    ! Compute density and pressure
    do i = 1, n
      rho(i) = 0.0
      do j = 1, n
        r = sqrt((x(i) - x(j))**2 + (y(i) - y(j))**2)
        w = kernel(r, h)
        rho(i) = rho(i) + mass(j) * w
      end do
      ! Equation of state: P = k * rho^gamma
      P(i) = k_press * rho(i)**gamma
    end do

    ! Write particle positions to file (same format as N-body)
    write(10, *) t, (x(i), y(i), i=1, n)

    ! Calculate accelerations
    ax = 0.0
    ay = 0.0
    do i = 1, n
      ! Particle-particle interactions
      do j = 1, n
        if (i /= j) then
          r = sqrt((x(i) - x(j))**2 + (y(i) - y(j))**2)
          ! Gravitational force
          force = -G * mass(i) * mass(j) / (r**2 + eps**2)**1.5
          fx = force * (x(i) - x(j))
          fy = force * (y(i) - y(j))
          ! SPH pressure and viscosity forces
          if (r < 2.0 * h) then
            dwdr = kernel_deriv(r, h)
            ! Pressure term
            force = -(mass(j) * (P(i)/rho(i)**2 + P(j)/rho(j)**2)) * dwdr
            fx = fx + force * (x(i) - x(j))
            fy = fy + force * (y(i) - y(j))
            ! Artificial viscosity
            v_dot_r = (vx(i) - vx(j)) * (x(i) - x(j)) + (vy(i) - vy(j)) * (y(i) - y(j))
            if (v_dot_r < 0.0) then
              mu = h * v_dot_r / (r**2 + 0.01 * h**2)
              pi_term = (-viscosity * mu) / (rho(i) * rho(j))
              visc_term = mass(j) * pi_term * dwdr
              fx = fx + visc_term * (x(i) - x(j))
              fy = fy + visc_term * (y(i) - y(j))
            end if
          end if
          ! Repulsive force to keep particles distinct
          if (r < coll_dist) then
            force = k_coll * (coll_dist - r)
            fx = fx + force * (x(i) - x(j)) / r
            fy = fy + force * (y(i) - y(j)) / r
          end if
          ax(i) = ax(i) + fx / mass(i)
          ay(i) = ay(i) + fy / mass(i)
        end if
      end do
      ! Central body force
      r = sqrt(x(i)**2 + y(i)**2)
      force = -G * mass(i) * m_central / (r**2 + eps**2)**1.5
      ax(i) = ax(i) + force * x(i) / mass(i)
      ay(i) = ay(i) + force * y(i) / mass(i)
    end do

    ! Update velocities and positions (Euler method)
    do i = 1, n
      vx(i) = vx(i) + ax(i) * dt
      vy(i) = vy(i) + ay(i) * dt
      x(i) = x(i) + vx(i) * dt
      y(i) = y(i) + vy(i) * dt
    end do
  end do

  ! Close output file
  close(10)

  print *, 'Simulation complete. Positions written to particle_positions.dat'

contains
  ! Cubic spline kernel
  real function kernel(r, h)
    real, intent(in) :: r, h
    real :: q, sigma
    sigma = 10.0 / (7.0 * 3.141592653589793 * h**2)  ! 2D normalization
    q = r / h
    if (q <= 1.0) then
      kernel = sigma * (1.0 - 1.5 * q**2 + 0.75 * q**3)
    else if (q <= 2.0) then
      kernel = sigma * 0.25 * (2.0 - q)**3
    else
      kernel = 0.0
    end if
  end function kernel

  ! Derivative of cubic spline kernel
  real function kernel_deriv(r, h)
    real, intent(in) :: r, h
    real :: q, sigma
    sigma = 10.0 / (7.0 * 3.141592653589793 * h**2)  ! 2D normalization
    q = r / h
    if (q <= 1.0) then
      kernel_deriv = sigma * (-3.0 * q + 2.25 * q**2) / h
    else if (q <= 2.0) then
      kernel_deriv = sigma * (-0.75 * (2.0 - q)**2) / h
    else
      kernel_deriv = 0.0
    end if
  end function kernel_deriv
end program sph_planet_formation
