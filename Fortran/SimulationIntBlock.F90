!!****if* source/Simulation/SimulationMain/MacLaurin/Simulation_initBlock
!!
!! NAME
!!
!!  Simulation_initBlock
!!
!! SYNOPSIS
!!
!!  Simulation_initBlock(integer(IN) :: blockID) 
!!                       
!!
!! DESCRIPTION
!!
!!  Initializes fluid data (density, pressure, velocity, etc.) for
!!  a specified block.  This version sets up the Maclaurin spheroid problem.
!!
!!  References:  Maclaurin, C. 1742, 
!!               Chandrasekhar, S. 1987, Ellipsoidal Figures of Equilibrium
!!
!! ARGUMENTS
!!
!!  blockID     --   The number of the block to initialize
!!
!!***

subroutine Simulation_initBlock(blockID)

  use Simulation_data, ONLY: sim_density, sim_gamma, sim_Omega2, sim_Pconst, &
       sim_nsubinv, sim_nsubzones, sim_xctr, sim_yctr, sim_zctr, &
       sim_initGeometry, sim_geom2DAxisymmetric, sim_geom3DCartesian, &
       sim_a3inv, sim_a1inv, &
       sim_smallRho, sim_smallP, sim_smallE
  use Grid_interface, ONLY : Grid_getBlkIndexLimits, &
       Grid_getCellCoords, Grid_putRowData

  implicit none

#include "constants.h"
#include "Flash.h"

  integer, intent(in)  :: blockID

  integer, dimension(2,MDIM) :: blkLimits, blkLimitsGC
  integer, dimension(3) :: startingPos
  integer  :: sizeX, sizeY, sizeZ
  logical  :: gcell=.true.
  integer  :: i, j, k, ii, jj, kk
  real     :: xdist, ydist, zdist, dist2, rxy, rxyz2, rinv
  real     :: xx, yy, zz, dxx, dyy, dzz, vxfac, vyfac, vzfac
  real     :: sum_rho, sum_p, sum_vx, sum_vy, sum_vz, pres, vel

  real, dimension(:), allocatable :: x, y, z, xl, yl, zl, xr, yr, zr, dx, dy, dz
  real, dimension(:), allocatable :: vx, vy, vz, p, rho, e, ek, ei, gam


  ! Get the coordinate information for the current block

  call Grid_getBlkIndexLimits(blockID, blkLimits, blkLimitsGC)
  sizeX = blkLimitsGC(HIGH,IAXIS)-blkLimitsGC(LOW,IAXIS) + 1
  allocate(x(sizeX), xl(sizex), xr(sizeX), dx(sizeX))
  sizeY = blkLimitsGC(HIGH,JAXIS)-blkLimitsGC(LOW,JAXIS) + 1
  allocate(y(sizeY), yl(sizeY), yr(sizeY), dy(sizeY))
  sizeZ = blkLimitsGC(HIGH,KAXIS)-blkLimitsGC(LOW,KAXIS) + 1
  allocate(z(sizeZ), zl(sizeZ), zr(sizeZ), dz(sizeZ))
  if (NDIM == 3) then 
     call Grid_getCellCoords(KAXIS, blockId, CENTER, gcell, z, sizeZ)
     call Grid_getCellCoords(KAXIS, blockId, LEFT_EDGE, gcell, zl, sizeZ)
     call Grid_getCellCoords(KAXIS, blockId, RIGHT_EDGE, gcell, zr, sizeZ)
  endif
  if (NDIM >= 2) then 
     call Grid_getCellCoords(JAXIS, blockId, CENTER, gcell, y, sizeY)
     call Grid_getCellCoords(JAXIS, blockId, LEFT_EDGE, gcell, yl, sizeY)
     call Grid_getCellCoords(JAXIS, blockId, RIGHT_EDGE, gcell, yr, sizeY)
  endif
  call Grid_getCellCoords(IAXIS, blockId, CENTER, gcell, x, sizeX)
  call Grid_getCellCoords(IAXIS, blockId, LEFT_EDGE, gcell, xl, sizeX)
  call Grid_getCellCoords(IAXIS, blockId, RIGHT_EDGE, gcell, xr, sizeX)

  dx(:) = xr(:) - xl(:)
  dy(:) = yr(:) - yl(:)
  dz(:) = zr(:) - zl(:)

  ! Set initial conditions in each zone

  allocate(vx(sizeX), vy(sizeX), vz(sizeX), p(sizeX), rho(sizeX), e(sizeX), & 
       ek(sizeX), ei(sizeX), gam(sizeX))
  do k = blkLimitsGC(LOW,KAXIS), blkLimitsGC(HIGH,KAXIS)
     dzz = dz(k) * sim_nsubinv
     do j = blkLimitsGC(LOW,JAXIS), blkLimitsGC(HIGH,JAXIS)
        dyy = dy(j) * sim_nsubinv
        do i = blkLimitsGC(LOW,IAXIS), blkLimitsGC(HIGH,IAXIS)
           dxx = dx(i) * sim_nsubinv

           sum_rho = 0.0
           sum_p   = 0.0
           sum_vx  = 0.0
           sum_vy  = 0.0
           sum_vz  = 0.0

           ! Break the zone into nsubzones^ndim sub-zones and average the
           ! results to get the values for the zone.

           do kk = 0, sim_nsubzones-1
              zz    = zl(k) + (kk + 0.5)*dzz 
              zdist = (zz - sim_zctr) * K3D

              do jj = 0, sim_nsubzones-1
                 yy    = yl(j) + (jj + 0.5)*dyy
                 ydist = (yy - sim_yctr) * K2D

                 do ii = 0, sim_nsubzones-1
                    xx    = xl(i) + (ii + 0.5)*dxx
                    xdist = xx - sim_xctr

                    select case (sim_initGeometry)

                    case (sim_geom2DAxisymmetric)

                       dist2 = (xdist*sim_a1inv)**2 + (ydist*sim_a3inv)**2
                       rxy   = xdist
                       rxyz2 = dist2
                       rinv  = 1./sqrt(xdist**2 + ydist**2)
                       vxfac = 0.0
                       vyfac = 0.0
                       vzfac = 0.0

                    case (sim_geom3DCartesian)

                       dist2 = (xdist*sim_a1inv)**2 + (ydist*sim_a1inv)**2 + (zdist*sim_a3inv)**2
                       rxy   = sqrt(xdist**2 + ydist**2)
                       rxyz2 = (rxy*sim_a1inv)**2 + (zdist*sim_a3inv)**2
                       rinv  = 1./sqrt(xdist**2 + ydist**2 + zdist**2)
                       vxfac = -ydist*rinv
                       vyfac = xdist*rinv
                       vzfac = 0.0

                    end select

                    if (dist2 <= 1.) then    ! inside the spheroid
                       pres    = sim_Pconst * (1.0 - rxyz2)
                       vel     = rxy * sim_Omega2
                       sum_rho = sum_rho + sim_density
                       sum_p   = sum_p   + pres
                       sum_vx  = sum_vx  + vel*vxfac
                       sum_vy  = sum_vy  + vel*vyfac
                       sum_vz  = sum_vz  + vel*vzfac
                    else                     ! outside the spheroid
                       sum_rho = sum_rho + sim_smallRho*10
                       sum_p   = sum_p   + sim_smallP*10
                    endif

                 enddo
              enddo
           enddo

           rho(i) = max(sum_rho * sim_nsubinv**3, sim_smallRho)
           p(i)   = max(sum_p * sim_nsubinv**3, sim_smallP)
           vx(i)  = sum_vx*sim_nsubinv**3
           vy(i)  = sum_vy*sim_nsubinv**3
           vz(i)  = sum_vz*sim_nsubinv**3
           ek(i)  = 0.5*(vx(i)**2+vy(i)**2+vz(i)**2)
           gam(i) = sim_gamma
           ei(i)  = max(p(i)/(rho(i)*(gam(i)-1.0)), sim_smallE)
           e(i)   = ei(i) + ek(i)

        enddo

        startingPos(1) = 1
        startingPos(2) = j
        startingPos(3) = k
        call Grid_putRowData(blockID, CENTER, DENS_VAR, EXTERIOR, IAXIS, startingPos, rho, sizeX)
        call Grid_putRowData(blockID, CENTER, PRES_VAR, EXTERIOR, IAXIS, startingPos, p, sizeX)
        call Grid_putRowData(blockID, CENTER, ENER_VAR, EXTERIOR, IAXIS, startingPos, e, sizeX)
        call Grid_putRowData(blockID, CENTER, GAME_VAR, EXTERIOR, IAXIS, startingPos, gam, sizeX)
        call Grid_putRowData(blockID, CENTER, GAMC_VAR, EXTERIOR, IAXIS, startingPos, gam, sizeX)
        call Grid_putRowData(blockID, CENTER, VELX_VAR, EXTERIOR, IAXIS, startingPos, vx, sizeX)
        call Grid_putRowData(blockID, CENTER, VELY_VAR, EXTERIOR, IAXIS, startingPos, vy, sizeX)
        call Grid_putRowData(blockID, CENTER, VELZ_VAR, EXTERIOR, IAXIS, startingPos, vz, sizeX)
        call Grid_putRowData(blockID, CENTER, EINT_VAR, EXTERIOR, IAXIS, startingPos, ei, sizeX)

     enddo
  enddo
  deallocate(rho, p, vx, vy, vz, e, ei, ek, gam)
  deallocate(x, xl, xr, dx)
  deallocate(y, yl, yr, dy)
  deallocate(z, zl, zr, dz)

  return
   use Simulation_data
  use Driver_interface, ONLY : Driver_abortFlash
  use Grid_interface, ONLY : Grid_getBlkIndexLimits, &
       Grid_getCellCoords, Grid_getDeltas, Grid_getBlkPtr, &
       Grid_releaseBlkPtr, Grid_getGeometry, Grid_renormAbundance
  use Eos_interface, ONLY : Eos_wrapped, Eos_getAbarZbar, Eos!, Eos_nucOneZone
  !  use Multispecies_interface, ONLY : Multispecies_getSumFrac,  Multispecies_getSumInv
  use RadTrans_interface, ONLY: RadTrans_mgdEfromT

  implicit none

#include "constants.h"
#include "Flash.h"
#include "Eos.h"
#include "Multispecies.h"

  integer, intent(IN) :: blockID


  real, pointer, dimension(:,:,:,:) :: solnData
  real, allocatable, dimension(:) :: xCenter, xLeft, xRight
  real, allocatable, dimension(:) :: yCenter, yLeft, yRight
  real, allocatable, dimension(:) :: zCenter, zLeft, zRight
  real, dimension(MDIM) :: delta
  real :: dx, dy, dz

  real, dimension(EOS_NUM) :: eosData
  real, dimension(NSPECIES) :: massFrac

  integer,dimension(LOW:HIGH,MDIM)::blkLimits,blkLimitsGC
  integer :: iSize, jSize, kSize
  integer :: iSizeGC, jSizeGC, kSizeGC
  integer :: ilo, ihi

  integer :: ivar, meshGeom
  integer  ::  jlo, jhi
  integer  ::  n

  real     ::  dens, temp, pres, He4, C12, N14, O16,velx,vely
  real     ::  Ne20,pi,arad,eint
  real     ::  Fe56, Neut, H1,tot

  real :: angle, rho_wind
  real :: dxx_sub, dyy_sub, dzz_sub
  integer :: i, j, k, ii, jj, kk
  integer :: istat
  integer, dimension(MDIM) :: axis

  real :: vol, sum, suminv
  real :: rcc, r_xy, rcc_sub, v_xy
  real :: var_interp, var_sum, vtot
  real :: xcc_sub, ycc_sub, zcc_sub
  real :: abar,zbar,sumY,Ye,Ye0

  real :: temp0,xEner,pres0,xEntr,xdedt,xdpderho,xMuNu,xXp,xXn,xXa,xXh
  real :: dTdp, temp1
  integer :: iter
  real :: sign
  real :: bombRad,rinner,router,shelldens,ExpEner
  logical :: shellcond,coremass,bombRadIn,shelltempfac
  real :: tion,tele,trad,tradActual
  real :: t_vac,t_s,rho_vac,rho_s,steep,r_s


  call Grid_getBlkIndexLimits(blockID,blkLimits,blkLimitsGC)
  iSizeGC = blkLimitsGC(HIGH,IAXIS)-blkLimitsGC(LOW,IAXIS)+1
  jSizeGC = blkLimitsGC(HIGH,JAXIS)-blkLimitsGC(LOW,JAXIS)+1
  kSizeGC = blkLimitsGC(HIGH,KAXIS)-blkLimitsGC(LOW,KAXIS)+1

  iSize = blkLimits(HIGH,IAXIS)-blkLimits(LOW,IAXIS)+1
  jSize = blkLimits(HIGH,JAXIS)-blkLimits(LOW,JAXIS)+1
  kSize = blkLimits(HIGH,KAXIS)-blkLimits(LOW,KAXIS)+1

  ilo = blkLimits(LOW,IAXIS)
  ihi = blkLimits(HIGH,IAXIS)

  !! allocate all needed space
  allocate(xCenter(iSizeGC),STAT=istat)
  allocate(xLeft(iSizeGC),STAT=istat)
  allocate(xRight(iSizeGC),STAT=istat)
  allocate(yCenter(jSizeGC),STAT=istat)
  allocate(yLeft(jSizeGC),STAT=istat)
  allocate(yRight(jSizeGC),STAT=istat)
  allocate(zCenter(kSizeGC),STAT=istat)
  allocate(zLeft(kSizeGC),STAT=istat)
  allocate(zRight(kSizeGC),STAT=istat)

  xCenter(:) = 0.e0
  yCenter(:) = 0.e0
  zCenter(:) = 0.e0

  call Grid_getDeltas(blockId, delta)
  dx = delta(IAXIS)
  dy = delta(JAXIS)
  dz = delta(KAXIS)

  call Grid_getBlkPtr(blockID,solnData,CENTER)
  call Grid_getGeometry(meshGeom)

  if (.TRUE. .OR. meshGeom == CARTESIAN) then

     call Grid_getCellCoords(IAXIS,blockID, CENTER, .true.,xCenter,iSizeGC)
     call Grid_getCellCoords(IAXIS,blockID,LEFT_EDGE, .true.,xLeft, iSizeGC)
     call Grid_getCellCoords(IAXIS,blockID,RIGHT_EDGE,.true.,xRight,iSizeGC)

     do k = blkLimits(LOW,KAXIS),blkLimits(HIGH,KAXIS)
        do j = blkLimits(LOW,JAXIS),blkLimits(HIGH,JAXIS)
           do i = blkLimits(LOW,IAXIS),blkLimits(HIGH,IAXIS)

              axis(IAXIS) = i
              axis(JAXIS) = j
              axis(KAXIS) = k

              !! Setup for Radiative Blast Wave in 1D Spherical

              solnData(H1_SPEC,i,j,k) = 1.0
              solnData(VELX_VAR,i,j,k) = 0.0
              solnData(DENS_VAR,i,j,k) = sim_rho_s

              if (xCenter(i) <= sim_r_s) then
                 solnData(TEMP_VAR,i,j,k) = sim_t_s

                 tele = solnData(TEMP_VAR,i,j,k)
                 solnData(TELE_VAR,i,j,k) = tele

                 trad = solnData(TEMP_VAR,i,j,k)
                 solnData(TRAD_VAR,i,j,k) = trad


              else
                 solnData(TEMP_VAR,i,j,k) = sim_t_vac

                 tele = solnData(TEMP_VAR,i,j,k)
                 solnData(TELE_VAR,i,j,k) = tele
 
                 trad = solnData(TEMP_VAR,i,j,k)
                 solnData(TRAD_VAR,i,j,k) = trad

              endif


              tion = 0.0 !solnData(TEMP_VAR,i,j,k)
              solnData(TION_VAR,i,j,k) = tion

!!$              tele = solnData(TEMP_VAR,i,j,k)
!!$              solnData(TELE_VAR,i,j,k) = tele
!!$
!!$              trad = solnData(TEMP_VAR,i,j,k)
!!$              solnData(TRAD_VAR,i,j,k) = trad

              if (trad > 0.0) then

                 call RadTrans_mgdEfromT(blockId, axis, trad, tradActual)
                 solnData(TRAD_VAR,i,j,k) = tradActual  

              endif

#if NDIM >= 2
              solnData(VELY_VAR,i,j,k) = 0.
#if NDIM == 3
              solnData(VELZ_VAR,i,j,k) = 0.
#endif
#endif
#ifdef GPOT_VAR
              !  THis is necessary IF the gpot in the 1D model is positive...erroneously.
              solnData(GPOT_VAR,i,j,k) = min(-solnData(GPOT_VAR,i,j,k),solnData(GPOT_VAR,i,j,k))
#ifdef GPOL_VAR
              solnData(GPOL_VAR,i,j,k) = solnData(GPOT_VAR,i,j,k)
#endif
#endif

              !! NOW RE-NORMALIZE ABUNDANCES

#ifdef FLASH_MULTISPECIES
              sum = 0.e0
              do n = SPECIES_BEGIN,SPECIES_END
                 solnData(n,i,j,k) = & 
                      max(sim_smallx, &
                      min(1.e0,solnData(n,i,j,k)))
                 sum = sum + solnData(n,i,j,k)
              enddo
              suminv = 1.e0 / sum
              do n = SPECIES_BEGIN, SPECIES_END
                 solnData(n,i,j,k) =  & 
                      max(sim_smallx, min(1.e0,suminv*&
                      solnData(n,i,j,k)))
              enddo
#endif

#ifdef SUMY_MSCALAR
              call Eos_getAbarZbar(solnData(:,i,j,k),abar,zbar,sumY,Ye)
              solnData(SUMY_MSCALAR,i,j,k) = sumY
              solnData(YE_MSCALAR,i,j,k) = Ye
#endif
           enddo
        enddo
     enddo

  endif

  if (NDIM == 2) then
     !--------------------------------------------------------------------------
     ! create a circular mapping front 
     !--------------------------------------------------------------------------
     if (meshGeom == CARTESIAN .OR. meshGeom == CYLINDRICAL) then 

        call Grid_getCellCoords(IAXIS,blockID, CENTER, .true.,xCenter,iSizeGC)
        call Grid_getCellCoords(IAXIS,blockID,LEFT_EDGE, .true.,xLeft, iSizeGC)
        call Grid_getCellCoords(IAXIS,blockID,RIGHT_EDGE,.true.,xRight,iSizeGC)

        call Grid_getCellCoords(JAXIS,blockID, CENTER, .true.,yCenter,jSizeGC)
        call Grid_getCellCoords(JAXIS,blockID,LEFT_EDGE, .true.,yLeft, jSizeGC)
        call Grid_getCellCoords(JAXIS,blockID,RIGHT_EDGE,.true.,yRight,jSizeGC)

        ! now fill the master arrays
        do k = blkLimits(LOW,KAXIS),blkLimits(HIGH,KAXIS)
           do j = blkLimits(LOW,JAXIS),blkLimits(HIGH,JAXIS)
              do i = blkLimits(LOW,IAXIS),blkLimits(HIGH,IAXIS)
                 ! compute the distance of the current zone from the origin 
                 rcc = sqrt(xCenter(i)**2 + yCenter(j)**2)
                 ! the interpolation will be done using the parabolic interpolation routine
                 if (rcc <= xzn(n1d_total)) then
                    do ivar = 1, NUNK_VARS
                       ! subsample in each zone to get a more accurate zone average -- note,
                       ! this is geometry dependent, so be careful
                       var_sum = 0.0
                       dxx_sub = dx/float(nsub)
                       dyy_sub = dy/float(nsub)
                       do jj = 1, nsub
                          do ii = 1, nsub
                             xcc_sub = xLeft(i) + (ii - 0.5)*dxx_sub
                             ycc_sub = yLeft(j) + (jj - 0.5)*dyy_sub
                             rcc_sub = sqrt(xcc_sub**2 + ycc_sub**2)
                             ! since it is difficult to do the parabolic interpolation, send
                             ! rcc as the left, center, and right positions -- this will do
                             ! a linear interpolation
                             call parabolic_interp(xzn, model_1d(:,ivar), & 
                                  n1d_total, rcc_sub, rcc_sub, rcc_sub, var_interp)
                             ! add the subzone's contribution to entire zone's total -- taking into
                             ! account the geometrical weighting
                             if (meshGeom == CARTESIAN) then
                                var_sum = var_sum + var_interp
                             elseif (meshGeom == CYLINDRICAL) then
                                ! the 'x' coordinate is the cylindrical radial coord, the 'y' coordinate
                                ! is the cylindrical z coord
                                vol = 2.0*PI*2.0*xcc_sub*dxx_sub*dyy_sub
                                var_sum = var_sum + var_interp*vol
                             endif

                          enddo
                       enddo
                       ! divide by the volume of the entire zone to get the average
                       if (meshGeom == CARTESIAN) then
                          var_sum = var_sum / float(nsub*nsub)
                       else if (meshGeom == CYLINDRICAL) then
                          vol = 2.0*PI*2.0*xCenter(i)*dx*dy
                          var_sum = var_sum / vol
                       endif

                       ! fake the velocities -- assume that v_x in the table is v_tot -- it 
                       ! nearly is.  Then compute the angle from xCenter and yCenter and find the 
                       ! x and y compontents of the velocity -- do all of this when velx is 
                       ! read from the table
                       if (ivar .NE. VELY_VAR .AND. ivar .NE. VELX_VAR) then
                          solnData(ivar,i,j,k) = var_sum
                       else
                          ! do both velocities when ivar eq ivelx
                          if (ivar == VELX_VAR) then
                             vtot = var_sum * sim_velMult
                             if (xCenter(i) .NE. 0) then
                                angle = atan(yCenter(j)/xCenter(i))
                             else
                                angle = PI/2.
                             endif
                             solnData(VELX_VAR,i,j,k) = vtot*cos(angle)
                             solnData(VELY_VAR,i,j,k) = vtot*sin(angle)
                          endif

                       endif
                    enddo

                 else ! in the wind, outside the star
                    solnData(:,i,j,k) = model_1d(n1d_total,:)
                    vtot = sim_windVel
                    rho_wind = sim_massLoss / (4.0 * PI * rcc**2 * vtot)
                    solnData(DENS_VAR,i,j,k) = max(rho_wind, sim_smlrho)

                    eosData(EOS_DENS) = solnData(DENS_VAR,i,j,k)
                    eosData(EOS_EINT) = 1.0e13
                    eosData(EOS_TEMP) = 1.0e4
                    massFrac(1:NSPECIES) = solnData(SPECIES_BEGIN:SPECIES_END,i,j,k)
                    call Eos(MODE_DENS_EI,1,eosData,massFrac)
                    solnData(TEMP_VAR,i,j,k) = eosData(EOS_TEMP)
!!$! Make the wind 'pure' helium                      
!!$                       solnData(SPECIES_BEGIN:SPECIES_END,i,j,k) = 0.0
!!$                       solnData(HE4_SPEC,i,j,k) = 1.0
                    if (xCenter(i) .NE. 0) then
                       angle = atan(yCenter(j)/xCenter(i))
                    else
                       angle = PI/2
                    endif
                    solnData(VELX_VAR,i,j,k) = vtot*cos(angle)*sim_velMult
                    solnData(VELY_VAR,i,j,k) = vtot*sin(angle)*sim_velMult
                 end if

#if NSPECIES > 0
                 sum = 0.e0
                 do n = SPECIES_BEGIN,SPECIES_END
                    solnData(n,i,j,k) = & 
                         max(sim_smallx, &
                         min(1.e0,solnData(n,i,j,k)))
                    sum = sum + solnData(n,i,j,k)
                 enddo
                 suminv = 1.e0 / sum
                 do n = SPECIES_BEGIN, SPECIES_END
                    solnData(n,i,j,k) =  & 
                         max(sim_smallx, min(1.e0,suminv*&
                         solnData(n,i,j,k)))
                 enddo
!!$                 if (solnData(FE54_SPEC,i,j,k) >= 0.99) then
!!$                    solnData(SPECIES_BEGIN:SPECIES_END,i,j,k) = 0.0
!!$                    solnData(FE54_SPEC,i,j,k) = 1.00
!!$                 end if
#endif
#ifdef SUMY_MSCALAR
                 call Eos_getAbarZbar(solnData(:,i,j,k),abar,zbar,sumY,Ye)
                 solnData(SUMY_MSCALAR,i,j,k) = sumY
                 solnData(YE_MSCALAR,i,j,k) = Ye
#endif
              enddo
           enddo
        enddo

!!$     else ! Here we may add 2D spherical geometry
!!$        call Driver_abortFlash("incorrect geometry in Simulation_initBlock")
     end if

  else if (NDIM == 3 .and. meshGeom == CARTESIAN) then
     !------------------------------------------------------------------------------
     ! create a spherical mapPIng
     !------------------------------------------------------------------------------

     call Grid_getCellCoords(IAXIS,blockID, CENTER, .true.,xCenter,iSizeGC)
     call Grid_getCellCoords(IAXIS,blockID,LEFT_EDGE, .true.,xLeft, iSizeGC)
     call Grid_getCellCoords(IAXIS,blockID,RIGHT_EDGE,.true.,xRight,iSizeGC)

     call Grid_getCellCoords(JAXIS,blockID, CENTER, .true.,yCenter,jSizeGC)
     call Grid_getCellCoords(JAXIS,blockID,LEFT_EDGE, .true.,yLeft, jSizeGC)
     call Grid_getCellCoords(JAXIS,blockID,RIGHT_EDGE,.true.,yRight,jSizeGC)

     call Grid_getCellCoords(KAXIS,blockID, CENTER, .true.,zCenter,kSizeGC)
     call Grid_getCellCoords(KAXIS,blockID,LEFT_EDGE, .true.,zLeft, kSizeGC)
     call Grid_getCellCoords(KAXIS,blockID,RIGHT_EDGE,.true.,zRight,kSizeGC)

     ! now fill the master arrays
     do k = blkLimits(LOW,KAXIS),blkLimits(HIGH,KAXIS)
        do j = blkLimits(LOW,JAXIS),blkLimits(HIGH,JAXIS)
           do i = blkLimits(LOW,IAXIS),blkLimits(HIGH,IAXIS)
              ! compute the distance of the current zone from the origin 
              rcc = sqrt(xCenter(i)**2 + yCenter(j)**2 + zCenter(k)**2)
              ! the interpolation will be done using the parabolic interpolation 
              ! routine
              do ivar = 1, NUNK_VARS
                 ! subsample in each zone to get a more accurate zone average -- note,
                 ! this is geometry dependent, so be careful
                 var_sum = 0.0
                 dxx_sub = dx/float(nsub)
                 dyy_sub = dy/float(nsub)
                 dzz_sub = dz/float(nsub)

                 do kk = 1, nsub
                    do jj = 1, nsub
                       do ii = 1, nsub
                          xcc_sub = xLeft(i) + (ii - 0.5)*dxx_sub
                          ycc_sub = yLeft(j) + (jj - 0.5)*dyy_sub
                          zcc_sub = zLeft(k) + (kk - 0.5)*dzz_sub

                          rcc_sub = sqrt(xcc_sub**2 + &
                               ycc_sub**2 + &
                               zcc_sub**2)
                          ! since it is difficult to do the parabolic interpolation, send
                          ! rcc as the left, center, and right positions -- this will do
                          ! a linear interpolation
                          call parabolic_interp(xzn, model_1d(:,ivar), & 
                               n1d_total, rcc_sub, rcc_sub, rcc_sub, &
                               var_interp)
                          ! add the subzone's contribution to entire zone's total -- taking into
                          ! account the geometrical weighting
                          var_sum = var_sum + var_interp
                       enddo
                    enddo
                 enddo
                 ! divide by the volume of the entire zone to get the average
                 var_sum = var_sum / float(nsub*nsub*nsub)
                 ! fake the velocities -- assume that v_x in the table is v_tot -- it 
                 ! nearly is.  Then compute the angle from xCenter, yCenter, and zCenter and find the 
                 ! x, y, and z compontents of the velocity -- do all of this one velx is 
                 ! read from the table
                 if (ivar /= VELX_VAR .AND. &
                      ivar /= VELY_VAR .AND. &
                      ivar /= VELZ_VAR) then
                    solnData(ivar,i,j,k) = var_sum
                 else
                    ! do both velocities when ivar eq VELX_VAR
                    if (ivar == VELX_VAR) then
                       vtot = var_sum
                       ! first decompose the velocity into a z component and an 'xy' component
                       r_xy = sqrt(xCenter(i)**2 + yCenter(j)**2)
                       if (r_xy /= 0.0) then
                          angle = atan(zCenter(k)/r_xy)
                       else
                          angle = PI/2.
                       endif

                       solnData(VELZ_VAR,i,j,k) = vtot*sin(angle)*sim_velMult

                       v_xy = vtot*cos(angle)
                       if (xCenter(i) /= 0.0) then
                          angle = atan(yCenter(j)/xCenter(i))
                       else
                          angle = PI/2.0
                       endif
                       sign = xCenter(i)/abs(xCenter(i))
                       solnData(VELX_VAR,i,j,k) = sign*v_xy*cos(angle)*sim_velMult
                       solnData(VELY_VAR,i,j,k) = sign*v_xy*sin(angle)*sim_velMult
                    endif

                 endif
              enddo
#ifdef FLASH_MULTISPECIES
              sum = 0.e0
              do n = SPECIES_BEGIN,SPECIES_END
                 solnData(n,i,j,k) = & 
                      max(sim_smallx, &
                      min(1.e0,solnData(n,i,j,k)))
                 sum = sum + solnData(n,i,j,k)
              enddo
              suminv = 1.e0 / sum
              do n = SPECIES_BEGIN, SPECIES_END
                 solnData(n,i,j,k) =  & 
                      max(sim_smallx, min(1.e0,suminv*&
                      solnData(n,i,j,k)))
              enddo
#endif

#ifdef SUMY_MSCALAR
              !              call Eos_getAbarZbar(solnData(:,i,j,k),abar,zbar,sumY)
              sumY = 0.0
              solnData(SUMY_MSCALAR,i,j,k) = max(sumY,0.01)
#endif

           enddo
        enddo
     enddo

  end if

  ! renormalize the abundances -- interpolation can do wacky things to them
  !  call Grid_renormAbundance(blockID,blkLimits,solnData)  
  ! now make the thermodynamics of this zone consistent !! Don't need this here; done after init_block
  !call Eos_wrapped(MODE_DENS_TEMP,blkLimits,blockID)

  call Grid_releaseBlkPtr(blockID,solnData,CENTER)

  deallocate(xLeft)
  deallocate(xRight)
  deallocate(xCenter)
  deallocate(yLeft)
  deallocate(yRight)
  deallocate(yCenter)
  deallocate(zLeft)
  deallocate(zRight)
  deallocate(zCenter)

  return
end subroutine Simulation_initBlock


subroutine parabolic_interp(x,var,n,y_l,y_c,y_r,var_interp)
!
! Given a vector of coordinates, x, the size, n, and associated function
! values, var, take the zone edges and center (y_l, y_c, y_r), and return
! the parabolic interpolation to this.
!
! x(n)        coordinate values 
! var(n)      function values at x(n)
! n           size of x and var
!
! y_l         coordinate of left edge of the zone
! y_c         coordinate of center of the zone
! y_r         coordinate of right edge of the zone
! 
! var_interp  zone average value of the function in that zone, using 
!             a parabolic structure
!
  implicit none
      
  integer :: n
      
  real :: x(n), var(n)
  real :: y_l, y_c, y_r
      
  real :: var_interp

  real :: var_l, var_c, var_r, err_int

  integer, PARAMETER :: op = 2

  real, PARAMETER :: sixth = 1.e0/6.e0

  integer kat, tmp

! get the function value at the left edge of the zone
real :: entropy, dst, dsd

kat = 0

  if (y_l < x(1)) then  

! the x array is monotonic -- if we are below the minimum in x, then
! just set the index to the first value
     kat = 1
  else
     call ut_hunt(x,n,y_l,kat)
  endif

  kat = max(1, min(kat - op/2 + 1, n - op + 1))
  call ut_polint(x(kat),var(kat),op,y_l,var_l,err_int)

! get the function value at the center of the zone
  call ut_hunt(x,n,y_c,kat)
  kat = max(1, min(kat - op/2 + 1, n - op + 1))
  call ut_polint(x(kat),var(kat),op,y_c,var_c,err_int)

! get the function value at the right edge of the zone
  call ut_hunt(x,n,y_r,kat)
  kat = max(1, min(kat - op/2 + 1, n - op + 1))
  call ut_polint(x(kat),var(kat),op,y_r,var_r,err_int)

! construct the zone averaged value
  var_interp = (var_l + 4.e0*var_c + var_r)*sixth

  return
end subroutine parabolic_interp
