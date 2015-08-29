program test_rand_generator
  use rand_generator_mod
  implicit none

  type(RandPointOnSphere) :: gen_point
  integer :: i
  type(LonLat) :: pos

  gen_point%current_seed_ = 1
  do i = 1, 10000
    pos = gen_rand(gen_point)
    print *, pos%lon_, ",", pos%lat_
  enddo

end program
  
