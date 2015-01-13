module GL2_1D_mod
  use sisofun_objects
  use intervals
!  use prefix_sum_mod
  implicit none
  private
  public :: GL2_1D!, GL2_1D_cumulative

  double precision, parameter :: root = 1.0D0 / sqrt(3.0D0)

contains

  recursive function GL2_1D(function_obj, interval_obj, tol) result(integral)
    implicit none
    class(sisofun_object), intent(in) :: function_obj
    class(interval), intent(inout) :: interval_obj
    double precision, intent(in), optional :: tol

    double precision :: current_integral, next_integral, tolerance, integral
    double precision :: integral_left, integral_right
    double precision :: error
    
    if(present(tol)) then
       tolerance = tol
    else
       tolerance = 1.0D-5
    endif

    current_integral = GL2(function_obj, interval_obj)

    call interval_obj%split()
    integral_left = GL2(function_obj, interval_obj%subinterval(0))
    integral_right = GL2(function_obj, interval_obj%subinterval(1))
    next_integral = integral_left + integral_right

    error = abs((next_integral - current_integral) / next_integral)

    if (error > tolerance) then
       integral_left = GL2_1D(function_obj, interval_obj%subinterval(0), tol = tolerance)
       integral_right = GL2_1D(function_obj, interval_obj%subinterval(1), tol = tolerance)
       next_integral = integral_left + integral_right
    endif

    integral = next_integral
  end function GL2_1D

  pure function GL2(function_obj, interval_obj) result(integral)
    class(sisofun_object), intent(in) :: function_obj
    class(interval), intent(in) :: interval_obj
    double precision :: integral

    double precision :: half_span, midpoint
    double precision, dimension(2) :: eval_points

    half_span = (interval_obj%right() - interval_obj%left())/2.0D0
    midpoint = (interval_obj%right() + interval_obj%left())/2.0D0
    eval_points = midpoint + half_span * [-root, root]
    integral = half_span * sum(function_obj%eval(eval_points))
  end function GL2

!!$  function GL2_1D_cumulative(function_obj, interval_obj) result(cumulative_array)
!!$    implicit none
!!$    class(sisofun_object), intent(in) :: function_obj
!!$    class(interval), intent(in) :: interval_obj
!!$    double precision, dimension(:), pointer :: differential_array, cumulative_array    
!!$    integer :: num_subintervals, left_count, right_count
!!$
!!$    num_subintervals = interval_obj%count()
!!$    allocate(differential_array(0:num_subintervals))
!!$    differential_array(0) = 0.0D0
!!$
!!$    if (.not. associated(interval_obj%subinterval)) then
!!$       differential_array(1) = GL2(function_obj, interval_obj)
!!$       cumulative_array => differential_array
!!$       differential_array => null()
!!$    else
!!$       left_count = interval_obj%subinterval(0)%count()
!!$       right_count = interval_obj%subinterval(1)%count()
!!$       call fill(function_obj, interval_obj%subinterval(0), differential_array(1:left_count))
!!$       call fill(function_obj, interval_obj%subinterval(1), differential_array(left_count+1 : num_subintervals))
!!$       cumulative_array => prefix_sum(differential_array)
!!$       deallocate(differential_array)
!!$       differential_array => null()
!!$    endif
!!$  end function GL2_1D_cumulative
!!$
!!$  recursive subroutine fill(function_obj, interval_obj, slice)
!!$    implicit none
!!$    class(sisofun_object), intent(in) :: function_obj
!!$    class(interval), intent(in) :: interval_obj
!!$    double precision, dimension(:), intent(inout) :: slice
!!$    integer :: num_subintervals, left_count, right_count
!!$
!!$    if (.not. associated(interval_obj%subinterval)) then
!!$       slice(1) = GL2(function_obj, interval_obj)
!!$    else
!!$       num_subintervals = interval_obj%count()
!!$       left_count = interval_obj%subinterval(0)%count()
!!$       right_count = interval_obj%subinterval(1)%count()
!!$       call fill(function_obj, interval_obj%subinterval(0), slice(1:left_count))
!!$       call fill(function_obj, interval_obj%subinterval(1), slice(left_count+1 : num_subintervals))
!!$    endif
!!$  end subroutine fill

end module GL2_1D_mod
