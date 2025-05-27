program planet_formation
  implicit none
  integer, parameter :: n = 100  ! Number of particles
  real, parameter :: G = 1.0     ! Gravitational constant
  real, parameter :: dt = 0.01   ! Time step
  real, parameter :: t_max = 10.0 ! Total simulation time
  integer, parameter :: n_steps = int(t_max / dt)
  real :: x(n), y(n), vx(n), vy(n), mass(n), ax(n), ay(n)
  real :: r, force, fx, fy
  integer :: i, j, step
  real :: t

  ! Random number seed
  call random_seed()

  ! Initialize particles with random positions and velocities
  do i = 1, n
    call random_number(x(i))
    call random_number(y(i))
    call random_number(vx(i))
    call random_number(vy(i))
    x(i) = x(i) * 10.0 - 5.0  ! Random x in [-5, 5]
    y(i) = y(i) * 10.0 - 5.0  ! Random y in [-5, 5]
    vx(i) = (vx(i) - 0.5) * 0.1 ! Small random velocities
    vy(i) = (vy(i) - 0.5) * 0.1
    mass(i) = 1.0  ! Equal mass for simplicity
  end do

  ! Open file for output
  open(unit=10, file='particle_positions.dat', status='replace')

  ! Main simulation loop
  do step = 1, n_steps
    t = step * dt
    ! Write positions to file
    write(10, *) t, (x(i), y(i), i=1, n)

    ! Calculate accelerations
    ax = 0.0
    ay = 0.0
    do i = 1, n
      do j = 1, n
        if (i /= j) then
          r = sqrt((x(i) - x(j))**2 + (y(i) - y(j))**2)
          if (r > 0.01) then  ! Avoid division by zero
            force = -G * mass(i) * mass(j) / r**3
            fx = force * (x(i) - x(j))
            fy = force * (y(i) - y(j))
            ax(i) = ax(i) + fx / mass(i)
            ay(i) = ay(i) + fy / mass(i)
          end if
        end if
      end do
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
end program planet_formation
