 module command_line_singleton
  implicit none
  private

  type :: definition
   contains
     ! Initialization
     procedure, nopass, public :: init
     ! General Inquiry
     procedure, nopass, public :: has_argument
     procedure, nopass, public :: argument_index
     ! argument counts
     procedure, nopass, public :: argument_count
     procedure, nopass, public :: flag_count
     procedure, nopass, public :: nonflag_count
     ! retrieve specific arguments
     procedure, nopass, public :: argument
     procedure, nopass, public :: flag
     procedure, nopass, public :: nonflag
  end type definition
  
  type(definition), public :: command_line
  
  character(len = 256), dimension(:), allocatable, save :: argument_array
  integer, save :: argument_cnt = 0, flag_cnt = 0
  logical, dimension(:), allocatable, save :: is_flag
!  integer, dimension(:), allocatable, save :: index_array
  
contains

  subroutine init()
    implicit none
    integer :: i

    if (allocated(argument_array)) return

    argument_cnt = command_argument_count()
    flag_cnt = 0
    
    allocate(character(len = 256) :: argument_array(argument_cnt))
    allocate(is_flag(argument_cnt))
!    allocate(index_array(argument_cnt))
    
    do i = 1, argument_cnt
       call get_command_argument(i, argument_array(i))
       if (argument_array(i)(1:1) == '-') then
          flag_cnt = flag_cnt + 1
          is_flag(i) = .true.
       else
          is_flag(i) = .false.
       endif
       !index_array(i) = i
    enddo
  end subroutine init

  pure elemental function has_argument(argument, starting_index) result(presence)
    implicit none
    character(*), intent(in) :: argument
    integer, intent(in), optional :: starting_index
    logical :: presence
    
    integer :: si
    
    if (present(starting_index)) then
       si = starting_index
    else
       si = 1
    endif
    presence = any( argument_array(si:) == argument )
  end function has_argument

  pure elemental function argument_index(argument, starting_index) result(index)
    implicit none
    character(*), intent(in) :: argument
    integer, intent(in), optional :: starting_index
    integer :: index
    integer :: si
    logical, dimension(size(argument_array,1)) :: presence_array     

    if (present(starting_index)) then
       si = starting_index
    else
       si = 1
    endif

!    This feature is included in the fortran 2008 standard but is not yet supported by the ifort compiler
!    index = findloc(argument_array(si:), argument)

!    A work around with moderate annoyance
    presence_array(si:) = argument_array(si:) == argument

    if (any(presence_array)) then
       index = maxloc(merge(1.,0.,presence_array),dim=1)
       ! index = minval(index_array(si:), mask = presence_array)
    else
       index = -1
    endif
  end function argument_index

  function argument_count() result(count)
    implicit none
    integer :: count
    count = argument_cnt
  end function argument_count

  function flag_count() result( count )
    implicit none
    integer :: count
    count = flag_cnt
  end function flag_count

  function nonflag_count() result( count )
    implicit none
    integer :: count
    count = argument_cnt - flag_cnt
  end function nonflag_count

  pure elemental function argument(index) result(argument_value)
    implicit none
    integer, intent(in) :: index
    character(len = 256) :: argument_value
    if (index > argument_cnt) then
       argument_value = '\null'
       return 
    endif
    argument_value = argument_array(index)
  end function argument

  pure elemental function flag(index) result(flag_value)
    implicit none
    integer, intent(in) :: index
    character(len = 256) :: flag_value

    character(len = 256), dimension(max(count(is_flag), 1)) :: flag_array

    if (index > flag_cnt) then
       flag_value = '\null'
       return
    endif
    
    flag_array = pack(argument_array, mask = is_flag)
    flag_value = flag_array(index)
  end function flag

  pure elemental function nonflag(index) result(nonflag_value)
    implicit none
    integer, intent(in) :: index 
    character(len = 256) :: nonflag_value

    character(len = 256), dimension(max(count(.not. is_flag), 1)) :: nonflag_array

    if (index > (argument_cnt - flag_cnt)) then
       nonflag_value = '\null'
       return
    endif

    nonflag_array = pack( argument_array, mask = (.not. is_flag))
    nonflag_value = nonflag_array(index)
! This is included in the fortran 2008 standard, but isn't supported by ifort
!    nonflag_value = (pack( argument_array, mask = (.not. is_flag)))(index)
  end function nonflag

end module command_line_singleton
