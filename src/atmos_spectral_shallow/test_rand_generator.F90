program test_rand_generator
  use rand_generator_mod
  implicit none

  type(RandPoisson) :: gen_poisson
  integer :: i

  gen_poisson = construct_RandPoisson(1, 10.0)
  do i = 1, 10000
    print *, gen_rand(gen_poisson)
  enddo

end program
  
