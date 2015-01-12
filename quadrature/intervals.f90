module intervals
  implicit none
  private

  type, public :: interval
     private
     double precision :: left_bound, right_bound
     type(interval), dimension(:), pointer, public :: subinterval
   contains
     procedure, public :: define
     procedure, public :: split
     procedure, public :: left
     procedure, public :: right
     procedure, public :: count
     procedure, public :: boundaries
     procedure, public :: reset
     procedure :: fill
     final :: interval_fin
  end type interval

contains

!~~~~~~~~ Modification Subroutines ~~~~~~~~

  subroutine define(self, left, right)                                  !pure elemental subroutine define(self, left, right)
    implicit none
    class(interval), intent(inout) :: self
    double precision, intent(in) :: left, right
    if (associated(self%subinterval)) write(*,*) "There's a compiler bug at work"
    self%left_bound = left
    self%right_bound = right
  end subroutine define

  subroutine split(self)                                                !pure elemental subroutine split(self)
    implicit none
    class(interval), intent(inout) :: self
    double precision :: midpoint
    integer :: error
    if (.not. associated(self%subinterval)) then
       midpoint = (self%left_bound + self%right_bound)/2
       allocate(self%subinterval(0:1), stat = error)
       if (.not. (error == 0)) write(*,*) "Allocation error during split"
       call self%subinterval(0)%define(self%left(), midpoint)
       call self%subinterval(1)%define(midpoint, self%right())
    endif
  end subroutine split

  recursive subroutine reset(self)
    implicit none
    class(interval), intent(inout) :: self
    if (associated(self%subinterval)) then
       call self%subinterval(0)%reset()
       call self%subinterval(1)%reset()
       deallocate(self%subinterval)
       self%subinterval => null()
    endif
  end subroutine reset

!~~~~~~~~ Getter Functions ~~~~~~~~~~~~~~~~

  pure elemental function left(self) result(left_bound)
    implicit none
    class(interval), intent(in) :: self
    double precision :: left_bound
    left_bound = self%left_bound
  end function left

  pure elemental function right(self) result(right_bound)
    implicit none
    class(interval), intent(in) :: self
    double precision :: right_bound
    right_bound = self%right_bound
  end function right

  recursive function count(self) result(counts)
    implicit none
    class(interval), intent(in) :: self
    integer :: counts
    if (associated(self%subinterval)) then
       counts = self%subinterval(0)%count() +  self%subinterval(1)%count()
    else
       counts = 1
    endif
  end function count

  function boundaries(self) result(boundary_array)
    implicit none
    class(interval), intent(in) :: self
    double precision, dimension(:), pointer :: boundary_array
    integer :: left_count, right_count
    if (.not. associated(self%subinterval)) then
       allocate(boundary_array(2))
       boundary_array(1) = self%left()
       boundary_array(2) = self%right()
    else
       left_count = self%subinterval(0)%count()
       right_count = self%subinterval(1)%count()
       allocate(boundary_array(left_count + right_count + 1))
       boundary_array(1) = self%left()
       boundary_array(left_count + 1) = (self%left() + self%right()) / 2
       boundary_array(left_count + right_count + 1) = self%right()
       if (left_count > 1) call self%subinterval(0)%fill(boundary_array(1:left_count + 1))
       if (right_count > 1)  call self%subinterval(1)%fill(boundary_array(left_count + 1: left_count + right_count + 1))          
    endif
  end function boundaries

  recursive subroutine fill(self, slice)
    implicit none
    class(interval), intent(in) :: self
    double precision, dimension(:), intent(inout) :: slice       
    integer :: left_count, right_count
    left_count = self%subinterval(0)%count()
    right_count = self%subinterval(1)%count()
    slice(left_count + 1) = (self%left() + self%right()) / 2
    if (left_count > 1) call self%subinterval(0)%fill(slice(1:left_count + 1))
    if (right_count > 1)  call self%subinterval(1)%fill(slice(left_count + 1: left_count + right_count + 1))
  end subroutine fill

!~~~~~~~~ Finalizer ~~~~~~~~~
  pure elemental subroutine interval_fin(self)
    implicit none
    type(interval), intent(inout) :: self
    if (associated(self%subinterval)) deallocate(self%subinterval)
    self%subinterval => null()
  end subroutine interval_fin
end module intervals



!~~~~~~~~ Debug ~~~~~~~~~~~~~
!!$program debug
!!$  use intervals
!!$  implicit none
!!$
!!$  type(interval) :: head
!!$  type(interval), pointer :: finger
!!$  double precision, dimension(:), pointer :: nodes
!!$  integer :: i
!!$
!!$  call head%define(0.0D0, 1024.0D0)
!!$  write(*,*) 'head interval:', head%left(), head%right()
!!$  call head%split()
!!$  finger => head%subinterval(0)
!!$  do i=1, 1000, 1
!!$     write(*,*) 'finger interval:', finger%left(), finger%right()
!!$     call finger%split()
!!$     finger => finger%subinterval(1)
!!$  enddo
!!$  write(*,*) 'finger interval:', finger%left(), finger%right()
!!$  write(*,*) 'head count:', head%count()
!!$     
!!$  nodes => head%boundaries()
!!$  write(*,*) 'boundaries:', nodes(:)
!!$  call head%reset()
!!$  write(*,*) 'head count:', head%count()
!!$
!!$end program debug
