module mass_pulse_mod
use rand_generator_mod, only : RandPoisson, RandPointOnSphere, LonLat, &
                               construct_RandPoisson,                  &
                               gen_rand

implicit none
private

public :: mass_pulse_init, &
          mass_pulse

type(RandPoisson)       :: num_storms_randgen 
type(RandPointOnSphere) :: storm_pos_randgen

integer :: is, ie, js, je
real, allocatable, dimension(:,:) :: total_forcing_field
integer :: num_forcing_fields

real, allocatable, dimension(:) :: rad_lat, rad_lon, sin_lat, cos_lat

type tForcingField
  real, allocatable, dimension(:,:) :: forcing_field_
  real :: lived_time_
end type tForcingField

type tForcingFieldPtr
  type(tForcingField), pointer :: ptr_
end type tForcingFieldPtr 

type(tForcingFieldPtr), allocatable, dimension(:) :: forcing_field_ptrs

! namelist 
!========================================================================
! outside storm_effect_time_max or storm_effect_radius_max, the storm has no
! effect
real :: storm_effect_time_max     = 10.0 ! Negative means days. Positive means second
real :: storm_effect_radius_max   = 0.2 ! distance on a unit sphere
real :: storm_lifetime_halfwidth  = 2.0
real :: storm_radius_halfwidth    = 0.05 
real :: mass_injection_rate       = 4.e-3 ! m/s
real :: num_storms_per_timestep   = 20.0 

namelist /mass_pulse_nml/ storm_effect_time_max, storm_effect_radius_max, &
                          storm_lifetime_halfwidth, storm_radius_halfwidth, &
                          mass_injection_rate, num_storms_per_timestep

contains

subroutine mass_pulse_init(delta_t)
#ifndef TEST
  use transforms_mod, only : get_grid_domain, &
                             get_deg_lon, get_deg_lat
  use fms_mod, only : file_exist, open_namelist_file, check_nml_error, &
                      close_file 
#endif
  real, intent(in) :: delta_t
  integer :: i, j, unit, ierr, io
#ifndef TEST
  if (file_exist('input.nml')) then
    unit = open_namelist_file ()
    ierr=1
    do while (ierr /= 0)
      read  (unit, nml=mass_pulse_nml, iostat=io, end=10)
      ierr = check_nml_error (io, 'mass_pulse_nml')
    enddo
    10 call close_file (unit)
  endif
#endif
  if(storm_effect_time_max<0) storm_effect_time_max = -storm_effect_time_max*86400
  if(storm_lifetime_halfwidth<0) storm_lifetime_halfwidth = -storm_lifetime_halfwidth*86400

  num_storms_randgen = construct_RandPoisson(1, num_storms_per_timestep)
  storm_pos_randgen%current_seed_ = 7788521 

#ifndef TEST
  call get_grid_domain(is,ie,js,je)
#else
  is = 1
  ie = 128 
  js = 1
  je = 64 
#endif

  allocate(rad_lat(js:je))
  allocate(rad_lon(is:ie))
  allocate(sin_lat(js:je))
  allocate(cos_lat(js:je))
#ifndef TEST
  call get_deg_lat(rad_lat)
  call get_deg_lon(rad_lon)
#else
  do i = 1, je
    rad_lat(i) = i*180./je - 90.
  enddo
  do i = 1, ie 
    rad_lon(i) = i*360./ie
  enddo
#endif
  rad_lat = rad_lat*atan(1.)/45.
  rad_lon = rad_lon*atan(1.)/45. 
  sin_lat = sin(rad_lat)
  cos_lat = cos(rad_lat)

  num_forcing_fields = ceiling(storm_effect_time_max/delta_t) 
  allocate(forcing_field_ptrs(num_forcing_fields))
  do i = 1, num_forcing_fields
    allocate(forcing_field_ptrs(i)%ptr_)
    forcing_field_ptrs(i)%ptr_%lived_time_ = (num_forcing_fields-i)*delta_t 
    allocate(forcing_field_ptrs(i)%ptr_%forcing_field_(is:ie, js:je))
    forcing_field_ptrs(i)%ptr_%forcing_field_(is:ie, js:je) = 0.0
  enddo

  allocate(total_forcing_field(is:ie, js:je))

end subroutine mass_pulse_init


subroutine mass_pulse(dt_hg, delta_t)
  real, intent(inout), dimension(is:ie, js:je) :: dt_hg
  real, intent(in) :: delta_t

  call generate_storms(delta_t)
  dt_hg(:,:) = dt_hg(:,:) + mass_injection_rate * total_forcing_field(:,:)
end subroutine mass_pulse


subroutine generate_storms(delta_t)
  real, intent(in) :: delta_t
  integer :: i, j, num_storms, t
  type(LonLat) :: storm_pos
  real :: damping_factor

  do t = 1, num_forcing_fields
    if(forcing_field_ptrs(t)%ptr_%lived_time_<=storm_effect_time_max) then
      forcing_field_ptrs(t)%ptr_%lived_time_ = forcing_field_ptrs(t)%ptr_%lived_time_ + delta_t
    else
      forcing_field_ptrs(t)%ptr_%lived_time_ = 0.
      forcing_field_ptrs(t)%ptr_%forcing_field_(:,:) = 0.
      num_storms = gen_rand(num_storms_randgen)
      do j = 1, num_storms
        storm_pos = gen_rand(storm_pos_randgen)
        call add_storm(storm_pos, forcing_field_ptrs(t)%ptr_%forcing_field_) 
      enddo
    endif
  enddo

  total_forcing_field(:,:) = 0.
  do t = 1, num_forcing_fields
    damping_factor = exp(-(forcing_field_ptrs(t)%ptr_%lived_time_ - &
                           storm_effect_time_max*0.5)**2 /        &
                          storm_lifetime_halfwidth**2) 
    total_forcing_field(:, :) = total_forcing_field(:, :) + &
                                    damping_factor * forcing_field_ptrs(t)%ptr_%forcing_field_(:, :)
  enddo
end subroutine generate_storms


subroutine add_storm(storm_pos, forcing_field)
  type(LonLat), intent(in) :: storm_pos
  real, dimension(is:ie, js:je) :: forcing_field
  real :: distance ! distance on a unit sphere
  integer :: i, j

  do j = js, je
    do i = is, ie
      distance = acos(sin_lat(j)*sin(storm_pos%lat_) + &
                      cos_lat(j)*cos(storm_pos%lat_)* cos(abs(rad_lon(i)-storm_pos%lon_)))
      if(distance < storm_effect_radius_max) then
        forcing_field(i, j) = forcing_field(i, j) + &
                              exp(-distance**2 / storm_radius_halfwidth**2)
      endif
    enddo
  enddo
end subroutine add_storm

end module mass_pulse_mod
