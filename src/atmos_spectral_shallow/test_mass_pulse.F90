program test_mass_pulse
  use mass_pulse_mod
  implicit none

  real, dimension(1:128, 1:64) :: dt_hg
  real :: delta_t = 1.0
  integer :: t, i, j

  call mass_pulse_init(delta_t)
  do t = 1, 100
    dt_hg(:,:) = 0
    call mass_pulse(dt_hg, delta_t)
    do j = 1, 64
      do i = 1, 128
        write(*, *), dt_hg(i, j)
      enddo
    enddo
  enddo

end program test_mass_pulse
