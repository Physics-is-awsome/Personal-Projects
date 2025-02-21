! simulation Data for PBP simulation !
module Simulation_data

  implicit none
#include "constants.h"
#include "Flash.h"
  
  integer, save :: nvar_stored
  integer, parameter :: n1d_max = 10000 ! Max number of lines a file can have
  integer, save :: n1d_total ! Actual number of lines, calculated after input
  real, save :: sim_smlrho, sim_smallt,sim_smallx
  character(len=80),save :: model_file
  real,save :: xzn(n1d_max)
  real,save :: model_1d(n1d_max,NUNK_VARS)
  real, save    :: sim_xMin, sim_xMax, sim_yMin, sim_yMax, sim_zMin, sim_zMax
  real, save  :: sim_windVel, sim_massLoss, sim_velMult
  integer, save :: nsub
  character (len=4), save :: unklabels(UNK_VARS_BEGIN:UNK_VARS_END)

  integer, save :: sim_meshMe

  logical, save :: sim_restart, sim_burnUpdateEint
  real, save :: sim_pointMass, sim_holeRadius
  real, save :: sim_shelldens, sim_rinner, sim_router, sim_bombRad
  real, save :: sim_ExpEner,sim_coremass,sim_bombRadIn
  real, save :: sim_tele, sim_trad, sim_tion
  real, save :: sim_t_vac,sim_t_s,sim_rho_vac,sim_rho_s,sim_r_s,sim_steep
 
  logical, save :: sim_usePnotT
  logical, save :: sim_plotScaledPressures

  real, save :: sim_holeRad,sim_shelltempfac
  ! for planet !

  logical, save :: sim_staticGpot, sim_shellcond,sim_paircond
  real, save    :: sim_eccentricity
  real, save    :: sim_gamma, sim_density, sim_Omega1, sim_a1
  real, save    :: sim_xctr, sim_yctr, sim_zctr
  real, save    :: sim_smallX, sim_smallE, sim_smallRho, sim_smallP
  real, save    :: sim_Newton, sim_pi

  integer, save :: sim_nsubzones, sim_initGeometry
  integer, parameter :: sim_geom2DAxisymmetric=1, sim_geom3DCartesian=2

  !! *** Auxiliary Variables - Introduced in Simulation_init *** !!

  real, save    :: sim_nsubinv, sim_a3, sim_a1inv, sim_a3inv
  real, save    :: sim_Omega2, sim_Pconst


end module Simulation_data
