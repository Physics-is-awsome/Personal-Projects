!!****if* source/Simulation/SimulationMain/radflaHD/RadBlastWave/Simulation_init
!!
!! NAME
!!
!!  Simulation_init
!!
!!
!! SYNOPSIS
!!
!!  Simulation_init()
!!
!! DESCRIPTION
!!
!!  Initializes all the data specified in Simulation_data.
!!  It calls RuntimeParameters_get rotuine for initialization.
!!  Initializes initial conditions for Sedov Spherical Explosion 
!!  problem.
!!
!! ARGUMENTS
!!
!!   
!!
!! PARAMETERS
!!
!!  p_ambient       Initial ambient pressure
!!  rho_Ambient     Initial ambient density
!!  exp_energy      Explosion energy (distributed over 2^dimen central zones)
!!  t_init          Initial time since explosion
!!  sim_nsubzones      Number of `sub-zones' in cells for applying 1d profile
!!
!!***
subroutine Simulation_init()
  use Simulation_data
  use Simulation_interface, ONLY : Simulation_mapIntToStr
  use Driver_interface, ONLY : Driver_abortFlash, Driver_getMype
  use RuntimeParameters_interface, ONLY : RuntimeParameters_get
  use Logfile_interface, ONLY : Logfile_stampMessage
  use Grid_interface, ONLY : Grid_getGeometry
    use Simulation_data
  use Grid_interface, ONLY : Grid_getGeometry
  use Driver_interface, ONLY : Driver_getMype, Driver_abortFlash
  use RuntimeParameters_interface, ONLY : RuntimeParameters_get, &
       RuntimeParameters_set
  use PhysicalConstants_interface, ONLY : PhysicalConstants_get
  use Logfile_interface, ONLY : Logfile_stamp

  implicit none

#include "constants.h"
#include "Flash.h"

  integer :: i
  integer :: geom

  integer, parameter :: max_stored_vars = 30
  real var_temp(max_stored_vars)

  logical :: dr_restart

  character (len=256) :: current_line

  integer :: j, ipos, NUNK_VARS_stored
!  integer :: var_key(max_stored_vars)
  integer :: var_key (NUNK_VARS)
  character (len=4) :: var_labels(max_stored_vars)

  call Driver_getMype(MESH_COMM, sim_meshMe)

  call Grid_getGeometry(geom)

  call RuntimeParameters_get( 'model_file',   model_file)
  call RuntimeParameters_get( 'smlrho',   sim_smlrho)
  call RuntimeParameters_get( 'smallt',   sim_smallt)
  call RuntimeParameters_get( 'smallx',   sim_smallx)
  call RuntimeParameters_get( 'vel_wind', sim_windVel)
  call RuntimeParameters_get( 'mass_loss', sim_massLoss)
  call RuntimeParameters_get( 'vel_mult', sim_velMult)
  call RuntimeParameters_get( 'nsub', nsub)
  call RuntimeParameters_get( 'restart', sim_restart)
  call RuntimeParameters_get( 'ener_exp', sim_expEner)

  call RuntimeParameters_get( 'point_mass', sim_pointMass)

  call RuntimeParameters_get( 'hole_radius', sim_holeRadius)

  call RuntimeParameters_get( 'hole_radius', sim_holeRad)

  call RuntimeParameters_get("use_PnotT", sim_usePnotT)
  call RuntimeParameters_get("shellcond",sim_shellcond)
  call RuntimeParameters_get("paircond",sim_paircond)

  call RuntimeParameters_get("staticGpot", sim_staticGpot)
  call RuntimeParameters_get("shelldens", sim_shelldens)
  call RuntimeParameters_get("shelltempfac",sim_shelltempfac)
  call RuntimeParameters_get("rinner",sim_rinner)
  call RuntimeParameters_get("router",sim_router)
  call RuntimeParameters_get("bombRad",sim_bombRad)
  call RuntimeParameters_get("bombRadIn",sim_bombRadIn)
  call RuntimeParameters_get("ExpEner",sim_ExpEner)
  call RuntimeParameters_get("coremass",sim_coremass)
  call RuntimeParameters_get("sim_plotScaledPressures",sim_plotScaledPressures)
  call RuntimeParameters_get("sim_tele",sim_tele)
  call RuntimeParameters_get("sim_tion",sim_tion)
  call RuntimeParameters_get("sim_trad",sim_trad)

  call RuntimeParameters_get("t_s",sim_t_s)
  call RuntimeParameters_get("t_vac",sim_t_vac)
  call RuntimeParameters_get("rho_s",sim_rho_s)
  call RuntimeParameters_get("rho_vac",sim_rho_vac)
  call RuntimeParameters_get("steep",sim_steep)
  call RuntimeParameters_get("r_s",sim_r_s)


  sim_massLoss = sim_massLoss * 2.0e33 / 31556926.

  do i = UNK_VARS_BEGIN, UNK_VARS_END
     call Simulation_mapIntToStr(i, unklabels(i), MAPBLOCK_UNK)
     call makeLowercase(unklabels(i))
  enddo

  ! open the file and read in the header 
!!$  open(unit=2,file=model_file,status='old')
!!$  read (2,'(a80)') current_line
!!$
!!$  if (sim_meshMe == MASTER_PE) print *, 'file opened'

  ! read in the number of variables line
!!$  read (2,'(a80)') current_line
!!$  ipos = index(current_line,'=') + 1
!!$  read (current_line(ipos:),*) nvar_stored
!!$  if (sim_meshMe == MASTER_PE) print *,"read nvar_stored", nvar_stored
!!$
!!$  if (NUNK_VARS .NE. nvar_stored .AND. sim_meshMe == MASTER_PE) then
!!$     print *, ' '
!!$     print *, 'Warning: the number of variables stored in the'
!!$     print *, 'input file is different than the number of'
!!$     print *, 'variables in the current version of FLASH.'
!!$     print *, ' '
!!$     print *, 'The variables in the file that are also defined'
!!$     print *, 'in FLASH will be read in.  Any missing variables'
!!$     print *, 'will be initialized to zero'
!!$     print *, ' '
!!$  endif
!!$
!!$  if (sim_meshMe == MASTER_PE) then
!!$     print *, "Vaiables in file:"
!!$  endif
!!$  do i = 1, nvar_stored
!!$     read (2,'(a4)') var_labels(i)
!!$     if (sim_meshMe == MASTER_PE) &
!!$          print *, var_labels(i)
!!$     call makeLowercase(var_labels(i))
!!$  enddo
!!$
!!$  do j = 1, NUNK_VARS
!!$     var_key(j) = NONEXISTENT
!!$
!!$     do i = 1, nvar_stored
!!$
!!$        if (unklabels(j) == var_labels(i)) then
!!$           var_key(j) = i
!!$        endif
!!$
!!$     enddo
!!$
!!$     if (var_key(j) == NONEXISTENT) then
!!$        if(sim_meshMe == MASTER_PE) then
!!$           print *, 'Warning, variable: ', unklabels(j), & 
!!$                ' not found in the input file.'
!!$           print *, 'initializing ', unklabels(j), ' to 0'
!!$           print *, ' '
!!$        endif
!!$     endif
!!$
!!$  enddo

!!$  do i = 1, n1d_max
!!$     read(2,*,end=11) xzn(i), (var_temp(j),j=1,nvar_stored)
!!$
!!$     ! put these in order, so model1d_var always contains the same variables 
!!$     ! in the same spots
!!$     do j = 1, NUNK_VARS
!!$        if (var_key(j) /= NONEXISTENT) then
!!$           model_1d(i,j) = var_temp(var_key(j))
!!$        else
!!$           model_1d(i,j) = 0.0
!!$        endif
!!$
!!$     enddo
!!$
!!$  enddo
!!$
!!$11 close(unit=2)
!!$
!!$  n1d_total = 0
!!$
!!$  do while (xzn(n1d_total+1) .NE. 0.00)
!!$     n1d_total = n1d_total + 1
!!$  enddo
!!$
!!$  if (sim_meshMe .EQ. MASTER_PE) then
!!$     print *, 'file read completed'
!!$     print *, n1d_total, 'points read in'
!!$  endif

!!$  do i = 1, n1d_total
!!$     print *, xzn(i), sum(model_1d(i,SPECIES_BEGIN:SPECIES_END))
!!$  enddo
  integer :: geometry
  real    :: amean, p0, mass, AA3


  call Driver_getMype(MESH_COMM, sim_meshMe)

  call PhysicalConstants_get("pi", sim_pi)
  call PhysicalConstants_get("Newton", sim_Newton)

  call RuntimeParameters_get("eccentricity", sim_eccentricity)
  call RuntimeParameters_get("equatorial_semimajor_axis", sim_a1)
  call RuntimeParameters_get("angular_velocity", sim_Omega1)
  call RuntimeParameters_get("density", sim_density)
  call RuntimeParameters_get("gamma", sim_gamma)
  call RuntimeParameters_get("smallx", sim_smallX)
  call RuntimeParameters_get("smlrho", sim_smallRho)
  call RuntimeParameters_get("smallp", sim_smallP)
  call RuntimeParameters_get("smalle", sim_smallE)
  call RuntimeParameters_get("xctr", sim_xctr)
  call RuntimeParameters_get("yctr", sim_yctr)
  call RuntimeParameters_get("zctr", sim_zctr)
  call RuntimeParameters_get("nsubzones", sim_nsubzones)

  ! Compute derived quantities.

  sim_nsubinv = 1./sim_nsubzones

  sim_a3     = sim_a1 * sqrt(1.0-sim_eccentricity**2)

  ! if density is -1, calculate a density that will produce a mass of 1.0 in the spheriod
  if (abs(sim_density + 1.0) < tiny(0.0)) then
     sim_density = 0.75 / ( sim_pi * sim_a1**2 * sim_a3)
     call RuntimeParameters_set("density",sim_density)
     print *, 'sim_density set to ',sim_density,' to generate a mass of 1.0'
     call Logfile_stamp(sim_density,'[Simulation_init] Reset density to ')
  end if

  if (sim_eccentricity > 1.E-10) then
     AA3 = (2.0*sqrt(1.0-sim_eccentricity**2)/sim_eccentricity**2) * &
          (1.0/sqrt(1.0-sim_eccentricity**2) - asin(sim_eccentricity)/sim_eccentricity)
  else
     AA3 = 1.0
  endif
  amean  = sim_a1 * (1.0-sim_eccentricity**2)**(1.0/6.0)
  sim_Omega2 = sqrt(sim_pi*sim_Newton*sim_density) * sim_Omega1
  p0     = (2.0/3.0)*sim_pi*sim_Newton*sim_density**2*amean**2
  sim_Pconst = p0 * AA3 * (1.0-sim_eccentricity**2)**(2.0/3.0)
  mass   = (4.0/3.0)*sim_pi*sim_density*sim_a1**3*sqrt(1.0-sim_eccentricity**2)
  sim_a1inv  = 1.0/sim_a1
  sim_a3inv  = 1.0/sim_a3



  ! Determine location of spheroid center depending on grid geometry.
  ! Allowed geometries:  2D:  axisymmetric; 3D:  Cartesian.
  ! Eventually should also support 3D cylindrical and spherical.

  call Grid_getGeometry(geometry)
  if ((NDIM == 2) .and. (geometry == POLAR)) then                      
     ! 2D axisymmetric

     sim_initGeometry = sim_geom2DAxisymmetric
     sim_xctr = 0.0
     sim_yctr = 0.0
     sim_zctr = 0.0
     call RuntimeParameters_set("xctr", sim_xctr)
     call RuntimeParameters_set("yctr", sim_yctr)
     call RuntimeParameters_set("zctr", sim_zctr)

  else if ((NDIM == 3) .and. (geometry == CARTESIAN)) then
     ! 3D Cartesian

     sim_initGeometry = sim_geom3DCartesian

  else            ! unsupported geometry

     !    call Driver_abortFlash('Simulation_init:  unsupported geometry')
     sim_initGeometry = sim_geom2DAxisymmetric 
     sim_xctr = 0.0
     sim_yctr = 0.0
     sim_zctr = 0.0
     call RuntimeParameters_set("xctr", sim_xctr)
     call RuntimeParameters_set("yctr", sim_yctr)
     call RuntimeParameters_set("zctr", sim_zctr)

  endif

  ! Write a message to stdout describing the problem setup.

  if (sim_meshMe == MASTER_PE) then

     write (*,*)
     call Logfile_stamp( "initializing for maclaurin problem", & 
          "[SIMULATION Simulation_init]")
     write (*,*) 'Simulation_init:  initializing for maclaurin problem.'
     write (*,*)
     write (*,*) "density      = ", sim_density
     write (*,*) "a1           = ", sim_a1
     write (*,*) "eccentricity = ", sim_eccentricity
     write (*,*) "Omega        = ", sim_Omega1
     write (*,*)
     write (*,*) "a3           = ", sim_a3
     write (*,*) "amean        = ", amean
     write (*,*) "p0           = ", p0
     write (*,*) "mass         = ", mass
     write (*,*) "omega        = ", sim_omega2
     write (*,*)
     write (*,*) "xctr         = ", sim_xctr
     write (*,*) "yctr         = ", sim_yctr
     write (*,*) "zctr         = ", sim_zctr
     write (*,*)
     write (*,*) "nsubzones    = ", sim_nsubzones
     write (*,*)

  endif

  return
end subroutine Simulation_init
