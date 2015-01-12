!~~~~~~ Sample functions ~~~~~~~~

! exponential function
! f(x) = exp(a*x)
module exponential_mod
  use sisofun_objects
  implicit none
  private

  type, extends(sisofun_object) :: exponential_def
   contains
     procedure, nopass :: eval => exp_eval
     procedure, nopass :: set => exp_set
  end type exponential_def

  double precision :: a
  type(exponential_def), public :: exponential
contains

  pure elemental function exp_eval(input) result(output)
    implicit none
    double precision, intent(in) :: input
    double precision :: output

    output = exp(a*input)
  end function exp_eval

  subroutine exp_set(var_name, var_val)
    character(*), intent(in) :: var_name
    double precision, intent(in) :: var_val

    if (var_name == 'a') a = var_val
  end subroutine exp_set
end module exponential_mod

! Logistic Function
! f(x) = b / (1 + exp(-a * x)) + c
module logistic_mod
  use sisofun_objects
  implicit none
  private

  type, extends(sisofun_object) :: logistic_def
   contains
     procedure, nopass :: eval => log_eval
     procedure, nopass :: set => log_set
  end type logistic_def
  
  type(logistic_def), public:: logistic
  double precision :: a, b, c

contains
  pure elemental function log_eval(input) result(output)
    implicit none
    double precision, intent(in) :: input
    double precision :: output

    output = b / (1.0D0 + exp(-a*input)) + c
  end function log_eval

  subroutine log_set(var_name, var_val)
    implicit none
    character(*), intent(in) :: var_name
    double precision, intent(in) :: var_val
    select case(var_name)
    case ('a') 
       a = var_val
    case ('b') 
       b = var_val
    case ('c') 
       c = var_val
    end select
  end subroutine log_set

end module logistic_mod

program demo
  use quadrature_mod
  use exponential_mod
  use logistic_mod
  use intervals
  implicit none
  type(interval) :: exponential_bounds, logistic_bounds
  double precision :: integral
  double precision, dimension(:), pointer :: nodes
  double precision, dimension(:), pointer :: cumul
  integer :: i

  call exponential_bounds%define(-5.0D0, 10.0D0)
  call exponential%set('a', 1.0D0)
  integral = quadrature%GaussLegendre2(exponential, exponential_bounds)
  write(*,*) "Renders exponential solution as ", integral
  write(*,*) "Intervals: ", exponential_bounds%count()
  write(*,*) "Mathematica solution : 22,026.5 (to six digits)"
  write(*,*)

  call logistic_bounds%define(-15.0D0, 2.5D0)
  call logistic%set('a', 2.0D0)
  call logistic%set('b', 3.0D0)
  call logistic%set('c', 10.0D0)
  integral = quadrature%GaussLegendre2(logistic, logistic_bounds)
  write(*,*) "Renders logistic solution as ", integral
  write(*,*) "Intervals: ", logistic_bounds%count()
  write(*,*) "Mathematica solution : 182.51 (to five digits)"
  write(*,*)
  call exponential_bounds%reset()
  integral = quadrature%GaussLegendre2(logistic, exponential_bounds)
  write(*,*) "Renders logistic solution on exponential bound as ", integral
  write(*,*) "Intervals: ", exponential_bounds%count()
  write(*,*) "Mathematica solution : 180.00  (to five digits)"
end program demo
