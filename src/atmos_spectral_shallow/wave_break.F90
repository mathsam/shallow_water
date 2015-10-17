! Relax field to a zonal-wind profile with wave disturbance to study wave
! breaking
! Refs:
! 1. Held and Phillips 1987
! 2. Farid Ait-Chaala, Tapio Schneider, Bettina Meyer, and J.B. Marston
!    Cumulant expansions for atomospheric flows
module wave_break_mod

use transforms_mod, only: get_sin_lat, get_cos_lat,  &
                          get_deg_lon, get_deg_lat,  &
                          get_grid_domain, get_spec_domain, &
                          uv_grid_from_vor_div, &
                          trans_grid_to_spherical
use fms_mod, only : file_exist, open_namelist_file, check_nml_error, &
                    close_file

implicit none
private

public :: init_wave_break, &
          relax_wind2wave_break

real, parameter :: PI = 3.1415926535897931d0
integer :: is, ie, js, je
integer :: ms, me, ns, ne
real :: relax_rate = 0.0
real :: relax_time_lapsed = 0.0
real, allocatable, dimension(:,:) :: U, V
real, allocatable, dimension(:) :: rad_lat, rad_lon, sin_lat, cos_lat


logical :: use_wave_break = .false.
real :: relax_timescale = -0.1 ! negative means unit is day
real :: max_relax_time = 24.*3600. ! unit sec
real :: wave_amplitude = 6.87e-5 ! unit 1/s
real :: A = 25.
real :: B = 30.
real :: C = 240.
real :: D = 120./7.
real :: half_width = 2*PI/18.
integer :: wave_num = 6

namelist /wave_break_nml/ use_wave_break, relax_timescale, max_relax_time, &
                          wave_amplitude, A, B, C, D, half_width, wave_num

contains

subroutine init_wave_break
integer :: i, j, unit, ierr, io
real, allocatable, dimension(:,:)    :: vor,  div
complex, allocatable, dimension(:,:) :: vors, divs


! read the namelist

if (file_exist('input.nml')) then
  unit = open_namelist_file ()
  ierr=1
  do while (ierr /= 0)
    read  (unit, nml=wave_break_nml, iostat=io, end=10)
    ierr = check_nml_error (io, 'wave_break_nml')
  enddo
  10 call close_file (unit)
endif

if (.not. use_wave_break) return

if (relax_timescale < 0.0) relax_timescale = - relax_timescale*86400.
if (relax_timescale .ne. 0.0) relax_rate = 1./relax_timescale

call get_grid_domain(is,ie,js,je)
call get_spec_domain(ms,me,ns,ne)

allocate ( rad_lat      (js:je) )
allocate ( rad_lon      (is:ie) )
allocate ( sin_lat      (js:je) )
allocate ( cos_lat      (js:je) )
allocate ( U      (is:ie,js:je) )
allocate ( V      (is:ie,js:je) )

allocate (vor     (is:ie,js:je) )
allocate (div     (is:ie,js:je) )
allocate (vors    (ms:me, ns:ne))
allocate (divs    (ms:me, ns:ne))

call get_deg_lat(rad_lat)
call get_deg_lon(rad_lon)
rad_lat = rad_lat*atan(1.)/45.
rad_lon = rad_lon*atan(1.)/45.
call get_sin_lat(sin_lat)
call get_cos_lat(cos_lat)

U = 0.0
V = 0.0
div = 0.0

do j = js, je
  do i = is, ie
    vor(i,j) = wave_amplitude*cos_lat(j) &
             * exp(-( (rad_lat(j)-PI/4)/half_width )**2) &
             * cos(wave_num*rad_lon(i))
  enddo
enddo

call trans_grid_to_spherical(vor, vors)
call trans_grid_to_spherical(div, divs)
call uv_grid_from_vor_div(vors, divs, U, V)
deallocate(vor)
deallocate(div)
deallocate(vors)
deallocate(divs)

do j = js, je
  do i = is, ie
    U(i,j) = U(i,j) + A*cos_lat(j) - B*cos_lat(j)**2   &
           + C*sin_lat(j)**2 * cos_lat(j)**4 - D*cos_lat(j)**6
  enddo
enddo

end subroutine init_wave_break


subroutine relax_wind2wave_break(dt_ug, dt_vg, ug, vg, &
                                 delta_t, previous)
real, intent(inout), dimension(is:ie, js:je)     :: dt_ug, dt_vg
real, intent(in)   ,  dimension(is:ie, js:je, 2) :: ug, vg
real,    intent(in)                              :: delta_t
integer, intent(in)                              :: previous

if (.not. use_wave_break) return

relax_time_lapsed = relax_time_lapsed + delta_t
if (relax_time_lapsed >= max_relax_time) return

dt_ug = dt_ug + relax_rate*(U-ug(:,:,previous))
dt_vg = dt_vg + relax_rate*(V-vg(:,:,previous))

end subroutine relax_wind2wave_break 

end module wave_break_mod
