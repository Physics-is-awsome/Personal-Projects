program planet_formation_no_merge_repulse
  implicit none
  integer, parameter :: n = 100  ! Number of particles
  real, parameter :: G = 1.0     ! Gravitational constant
  real, parameter :: dt = 0.01   ! Time step
  real, parameter :: t_max = 10.0 ! Total simulation time
  real, parameter :: eps = 0.02  ! Softening length for gravity
  real, parameter :: damp = 0.98 ! Velocity damping factor
  real, parameter :: coll_dist = 0.1 ! Distance for repulsive force
  real, parameter :: k_coll = 80.0 ! Repulsive force constant
  integer, parameter :: n_steps = int(t_max / dt)
  real :: x(n), y(n), vx(n), vy(n), mass(n), ax(n), ay(n)
  real :: r, force, fx, fy, r_disk, v_circ
  integer :: i, j, step
  real :: t

  ! Random number seed
  call random_seed()

  ! Initialize particles in a disk with circular velocities
  do i = 1, n
    call random_number(r_disk)
    call random_number(t)
    r_disk = r_disk * 2.0  ! Random radius in [0, 5]
    t = t * 5.0 * 3.141592653589793  ! Random angle
    x(i) = r_disk * cos(t)
    y(i) = r_disk * sin(t)
    ! Circular velocity with slight inward component
    v_circ = sqrt(G * n * 1.0 / r_disk) * 0.8  ! Assume central mass ~ n*1.0
    vx(i) = -v_circ * sin(t) - 0.1 * x(i) / r_disk
    vy(i) = v_circ * cos(t) - 0.1 * y(i) / r_disk
    mass(i) = 1.0  ! Equal mass initially
  end do

  ! Open file for output
  open(unit=10, file='particle_positions.dat', status='replace')

  ! Main simulation loop
  do step = 1, n_steps
    t = step * dt
    ! Write particle positions to file
    write(10, *) t, (x(i), y(i), i=1, n)

    ! Calculate accelerations
    ax = 0.0
    ay = 0.0
    do i = 1, n
      do j = 1, n
        if (i /= j) then
          r = sqrt((x(i) - x(j))**2 + (y(i) - y(j))**2)
          ! Gravitational force
          force = -G * mass(i) * mass(j) / (r**2 + eps**2)**1.5
          fx = force * (x(i) - x(j))
          fy = force * (y(i) - y(j))
          ! Add repulsive force if particles are too close
          if (r < coll_dist) then
            force = k_coll * (coll_dist - r)  ! Linear repulsive force
            fx = fx + force * (x(i) - x(j)) / r
            fy = fy + force * (y(i) - y(j)) / r
          end if
          ax(i) = ax(i) + fx / mass(i)
          ay(i) = ay(i) + fy / mass(i)
        end if
      end do
    end do

    ! Update velocities and positions (Euler method with damping)
    do i = 1, n
      vx(i) = vx(i) * damp + ax(i) * dt
      vy(i) = vy(i) * damp + ay(i) * dt
      x(i) = x(i) + vx(i) * dt
      y(i) = y(i) + vy(i) * dt
    end do
  end do

  ! Close output file
  close(10)

  print *, 'Simulation complete. Positions written to particle_positions.dat'
end program planet_formation_no_merge_repulse
