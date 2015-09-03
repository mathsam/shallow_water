module rand_generator_mod

implicit none
private

real :: PI = 3.1415926535897931d0 ! Class-wide private constant

interface gen_rand
  module procedure gen_rand_int,     &
                   gen_rand_uniform, &
                   gen_rand_poisson, &
                   gen_rand_onsphere
end interface

public :: RandInt,               &
          RandUniform,           &
          construct_RandPoisson, &
          RandPoisson,           &
          RandPointOnSphere,     &
          LonLat
public :: gen_rand

type RandInt
    integer(8) :: current_seed_
end type RandInt

type RandUniform
    integer(8) :: current_seed_
end type RandUniform

type RandPoisson
    integer(8) :: current_seed_
    real :: exp_neg_lambda_ !exp(-lambda)
end type RandPoisson

type RandPointOnSphere
    integer(8) :: current_seed_
end type RandPointOnSphere

! unit is Rad
type LonLat
    real :: lon_, lat_
end type LonLat

contains

!=============================================================================

!@brief Park-Miller random integer generator
!       Generate a random integer within [1, 2**31-1]
function gen_rand_int(this) result (random_int)
  type(RandInt), intent(inout) :: this
  integer(8) :: random_int

  random_int = mod(48271_8*this%current_seed_, 2147482647_8)
  this%current_seed_ = random_int
end function gen_rand_int

!@brief generate uniform random number within (0, 1)
function gen_rand_uniform(this) result (random_real)
  type(RandUniform), intent(inout) :: this
  real :: random_real

  this%current_seed_ = mod(48271_8*this%current_seed_, 2147482647_8)
  random_real = (1.0/2147482648.0)*this%current_seed_
end function gen_rand_uniform

!@brief constructor for generating a Poisson random number with distribution
!       lambda**k * exp(-lambda) / k!
function construct_RandPoisson(seed, lambda) result (this)
  integer, intent(in) :: seed
  real :: lambda
  type(RandPoisson) :: this

  this%current_seed_ = seed
  this%exp_neg_lambda_ = exp(-lambda)
end function construct_RandPoisson

!@brief see algorithm by D.P. Kroese, T. Taimre, Z.I. Botev:
!       Handbook of Monte Carlo Methods. John Wiley & Sons, 2011.
function gen_rand_poisson(this) result(random_poisson)
  type(RandPoisson), intent(inout) :: this
  integer :: random_poisson
  real :: random_real, produ

  random_poisson = 0
  random_real = (1.0/2147482648.0)*this%current_seed_
  this%current_seed_ = mod(48271_8*this%current_seed_, 2147482647_8)
  produ = random_real

  do while (produ >= (this%exp_neg_lambda_))
    random_real = (1.0/2147482648.0)*this%current_seed_
    this%current_seed_ = mod(48271_8*this%current_seed_, 2147482647_8)
    produ = produ * random_real
    random_poisson = random_poisson + 1
  enddo
end function gen_rand_poisson

!@brief generate uniform random points on a sphere
!       ref: http://mathworld.wolfram.com/SpherePointPicking.html
function gen_rand_onsphere(this) result (pos)
  type(RandPointOnSphere), intent(inout) :: this
  real :: random_real
  type(LonLat) :: pos

  random_real = (1.0/2147482648.0)*this%current_seed_
  this%current_seed_ = mod(48271_8*this%current_seed_, 2147482647_8)
  pos%lon_ = 2.0*PI*random_real

  random_real = (1.0/2147482648.0)*this%current_seed_
  this%current_seed_ = mod(48271_8*this%current_seed_, 2147482647_8)
  pos%lat_ = acos(2.0*random_real - 1.0)
end function gen_rand_onsphere

end module rand_generator_mod
