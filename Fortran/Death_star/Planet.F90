program planet_formation_accretion
  implicit none
  integer, parameter :: n = 1000  ! Initial number of particles
  real, parameter :: G = 1.0     ! Gravitational constant
  real, parameter :: dt = 0.01   ! Time step
  real, parameter :: t_max = 10.0 ! Total simulation time
  real, parameter :: eps = 0.05  ! Softening length
  real, parameter :: merge_dist = 0.1 ! Distance for merging particles
  real, parameter :: damp = 0.995 ! Velocity damping factor
  integer, parameter :: n_steps = int(t_max / dt)
  real :: x(n), y(n), vx(n), vy(n), mass(n), ax(n), ay(n)
  logical :: active(n)           ! Tracks active particles
  integer :: n_active           ! Number of active particles
  real :: r, force, fx, fy, r_disk, v_circ
  integer :: i, j, step
  real :: t, px, py, total_mass

  ! Random number seed
  call random_seed()

  ! Initialize particles in a disk with circular velocities
  n_active = n
  do i = 1, n
    call random_number(r_disk)
    call random_number(t)
    r_disk = r_disk * 5.0  ! Random radius in [0, 5]
    t = t * 2.0 * 3.141592653589793  ! Random angle
    x(i) = r_disk * cos(t)
    y(i) = r_disk * sin(t)
    ! (trying 0 at the moment) Circular velocity with slight inward component
    v_circ = 0  !sqrt(G * n * 1.0 / r_disk) * 0.8  ! Assume central mass ~ n*1.0
    vx(i) = 0    !-v_circ * sin(t) - 0.05 * x(i) / r_disk
    vy(i) = 0    !v_circ * cos(t) - 0.05 * y(i) / r_disk
    mass(i) = 30.0  ! Equal mass initially
    active(i) = .true.
  end do

  ! Open file for output
  open(unit=10, file='particle_positions.dat', status='replace')

  ! Main simulation loop
  do step = 1, n_steps
    t = step * dt
    ! Write active particle positions to file
    write(10, *) t, n_active, (x(i), y(i), i=1, n), (active(i), i=1, n)

    ! Calculate accelerations
    ax = 0.0
    ay = 0.0
    do i = 1, n
      if (active(i)) then
        do j = 1, n
          if (active(j) .and. i /= j) then
            r = sqrt((x(i) - x(j))**2 + (y(i) - y(j))**2)
            if (r > merge_dist) then
              force = -G * mass(i) * mass(j) / (r**2 + eps**2)**1.5
              fx = force * (x(i) - x(j))
              fy = force * (y(i) - y(j))
              ax(i) = ax(i) + fx / mass(i)
              ay(i) = ay(i) + fy / mass(i)
            end if
          end if
        end do
      end if
    end do

    ! Update velocities and positions (Euler method with damping)
    do i = 1, n
      if (active(i)) then
        vx(i) = vx(i) * damp + ax(i) * dt
        vy(i) = vy(i) * damp + ay(i) * dt
        x(i) = x(i) + vx(i) * dt
        y(i) = y(i) + vy(i) * dt
      end if
    end do

    ! Check for collisions and merge particles
    do i = 1, n
      if (active(i)) then
        do j = i+1, n
          if (active(j)) then
            r = sqrt((x(i) - x(j))**2 + (y(i) - y(j))**2)
            if (r < merge_dist) then
              ! Merge particle j into i
              total_mass = mass(i) + mass(j)
              px = mass(i) * vx(i) + mass(j) * vx(j)
              py = mass(i) * vy(i) + mass(j) * vy(j)
              x(i) = (mass(i) * x(i) + mass(j) * x(j)) / total_mass
              y(i) = (mass(i) * y(i) + mass(j) * y(j)) / total_mass
              vx(i) = px / total_mass
              vy(i) = py / total_mass
              mass(i) = total_mass
              active(j) = .false.
              n_active = n_active - 1
            end if
          end if
        end do
      end if
    end do
  end do

  ! Close output file
  close(10)

  print *, 'Simulation complete. Positions written to particle_positions.dat'
  print *, 'Final number of active particles:', n_active
end program planet_formation_accretion
