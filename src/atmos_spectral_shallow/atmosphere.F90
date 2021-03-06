Module atmosphere_mod

!=========================================================================

use     fms_mod, only: open_namelist_file,   &
                       open_restart_file,    &
                       file_exist,           &
                       check_nml_error,      &
                       error_mesg,           &
                       FATAL, WARNING,       &
                       write_version_number, &
                       mpp_pe,               &
                       mpp_root_pe,          &
                       close_file,           &
                       stdlog

use     mpp_mod, only: mpp_max

use     constants_mod, only: radius_earth => radius, &
                             omega_earth  => omega

use      transforms_mod,  only : get_deg_lon,               &
                                 get_deg_lat,               &
                                 get_grid_boundaries,       &
                                 get_grid_domain,           &
                                 get_spec_domain,           &
                                 area_weighted_global_mean, &
                                 atmosphere_domain

use    time_manager_mod,  only : time_type,      &
                                 set_time,       &
                                 get_time,       &
                                 interval_alarm, &
                                 operator(+),    &
                                 operator(<),    &
                                 operator(==)

use shallow_dynamics_mod,  only : shallow_dynamics_init,    &
                                  shallow_dynamics,         &
                                  shallow_dynamics_end,     &
                                  dynamics_type
    
use shallow_physics_mod, only : shallow_physics_init, &
                                shallow_physics,      &
                                shallow_physics_end,  &
                                phys_type

use shallow_diagnostics_mod,  only : shallow_diagnostics_init,   &
                                     shallow_diagnostics

use mass_pulse_mod, only : mass_pulse_init, mass_pulse
use wave_break_mod, only : init_wave_break, relax_wind2wave_break


!========================================================================
implicit none
private
!========================================================================

! version information 
!========================================================================
character(len=128) :: version = '$Id: atmosphere.F90,v 14.0 2007/03/15 22:13:18 fms Exp $'
character(len=128) :: tagname = '$Name: siena_201303 $'
!========================================================================

public :: atmosphere_init, &
          atmosphere,      &
          atmosphere_end,  &
          atmosphere_domain

!========================================================================

integer, parameter :: num_time_levels = 2

integer         :: unit, seconds, days
integer         :: pe, npes
integer         :: previous, current, future
logical         :: root

integer         :: dt_integer
real            :: dt_real
type(time_type) :: dt_time_type, Time_init, Time_step

real :: delta_t   ! = 2*dt_real for leapfrog step

integer, dimension(2) :: axes  

type(phys_type),     save :: Phys
type(dynamics_type), save :: Dyn

integer :: is, ie, js, je, ms, me, ns, ne
integer :: num_lon, num_lat
integer, dimension(4) :: axis_id         ! axes identifiers 

logical :: module_is_initialized =.false.


integer :: print_interval
! namelist 
!========================================================================
namelist /atmosphere_nml/ print_interval
!========================================================================

contains
!=======================================================================

subroutine atmosphere_init(Time_init_in, Time, Time_step_in)

type (time_type), intent(in) :: Time_init_in, Time, Time_step_in

integer :: i, j, n, nn, ierr, io, unit
integer :: nlon, nlat

pe   = mpp_pe()
root = (pe == mpp_root_pe())

Time_step = Time_step_in
call get_time(Time_step, seconds, days)
dt_integer   = 86400*days + seconds
dt_real      = float(dt_integer)
dt_time_type = Time_step
Time_init    = Time_init_in

! read the namelist

if (file_exist('input.nml')) then
  unit = open_namelist_file ()
  ierr=1
  do while (ierr /= 0)
    read  (unit, nml=atmosphere_nml, iostat=io, end=10)
    ierr = check_nml_error (io, 'atmosphere_nml')
  enddo
  10 call close_file (unit)
endif
call write_version_number(version, tagname)
if (root) write (stdlog(), nml=atmosphere_nml)

call shallow_dynamics_init (Dyn, Time, Time_init)    
call mass_pulse_init(dt_real)
call init_wave_break

call get_grid_domain(is,ie,js,je)
call get_spec_domain(ms,me,ns,ne)
 
num_lon = Dyn%num_lon
num_lat = Dyn%num_lat

nlon = ie+1-is  ! size of grid on each processor
nlat = je+1-js

call shallow_physics_init(Phys)
call shallow_diagnostics_init(Time, num_lon, num_lat)

if(Time == Time_init) then
  previous = 1
  current  = 1
else
  previous = 1  
  current  = 2
endif

module_is_initialized = .true.

return
end subroutine atmosphere_init

!=====================================================================

subroutine atmosphere(Time)

type (time_type), intent(in) :: Time
integer :: day, second, dt


if(.not.module_is_initialized) then
  call error_mesg('atmosphere', &
                  'atmosphere_init has not been called', FATAL)
end if

call get_time(Time_step, second, day)
dt = second + 86400*day

Dyn%Tend%u   = 0.0
Dyn%Tend%v   = 0.0
Dyn%Tend%h   = 0.0
if(Dyn%grid_tracer) Dyn%Tend%tr  = 0.0
if(Dyn%spec_tracer) Dyn%Tend%trs = 0.0

if(Time == Time_init) then
  delta_t = dt_real
  future = 2
else
  delta_t = 2.0*dt_real
  future = previous
endif

call shallow_physics(Time,                                 &
                     Dyn%Tend%u, Dyn%Tend%v, Dyn%Tend%h,   &
                     Dyn%Grid%u, Dyn%Grid%v, Dyn%Grid%h,   &
                     delta_t, previous, current,           &
                     Phys)
call mass_pulse(Dyn%Tend%h, dt_real)
call relax_wind2wave_break(Dyn%Tend%u, Dyn%Tend%v, Dyn%Grid%u, Dyn%Grid%v, &
                           delta_t, previous)

call shallow_dynamics(Time, Time_init, &
                      Dyn, previous, current, future, delta_t) 
     
previous   = current
current    = future

call shallow_diagnostics (Time+Time_step, Dyn%Grid, Phys, current) 

call get_time(Time+Time_step, second, day)
if(mod(second+86400*day, print_interval) < dt)  &
   call global_diag(second, day, current)

return
end subroutine atmosphere

!=======================================================================================

subroutine global_diag(second, day, current)

integer, intent(in) :: second, day, current

real    :: enstrophy, div_squared, max_Froude
real, dimension(size(Dyn%Grid%u,1), size(Dyn%Grid%u,2)) :: speed

enstrophy = &
area_weighted_global_mean(Dyn%grid%vor(:,:,current)*Dyn%grid%vor(:,:,current))

div_squared = &
area_weighted_global_mean(Dyn%grid%div(:,:,current)*Dyn%grid%div(:,:,current))

speed = Dyn%Grid%u(:,:,current)*Dyn%Grid%u(:,:,current) +&
        Dyn%Grid%v(:,:,current)*Dyn%Grid%v(:,:,current)
max_Froude = maxval(speed/Dyn%Grid%h(:,:,current))
call mpp_max(max_Froude)

if(root) then
  write(*,1000) day, second, enstrophy, div_squared, max_Froude
end if
1000 format(1x, 'day =',i6,2x,'second =', i6, &
    2x,'enstrophy = ',e13.6,3x,'div_squared = ',e13.6, 3x, &
    'max_Froude = ', e10.3)

return
end subroutine global_diag

!===============================================================================
subroutine atmosphere_end

if(.not.module_is_initialized) then
  call error_mesg('atmosphere_end', &
                  'atmosphere_init has not been called.', FATAL)
end if

call shallow_physics_end (Phys)
call shallow_dynamics_end (Dyn, previous, current)

module_is_initialized = .false.

return
end subroutine atmosphere_end

!=======================================================================================
end module atmosphere_mod
