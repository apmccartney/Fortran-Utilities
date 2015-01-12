! A data structure for managing file io: Opening/closing files, which files are open, associated units

module io_singleton
  use dictionaries_char256_int
  implicit none
  private

  type :: definition
   contains
     ! mutation methods 
     procedure, nopass, public :: open => open_wo_options ! to be replaced in future
     procedure, nopass, public :: close => close_wo_options  ! to be replaced in future
     ! inquiry methods
     procedure, nopass, public :: is_open
     procedure, nopass, public :: unit
     procedure, nopass, public :: number_open_files
     procedure, nopass, public :: open_files
     procedure, nopass, public :: associated_units
  end type definition

  integer, parameter :: max = 256

  integer, save :: free_unit = 103
  type(dictionary_char256_int), save :: file_units

  type(definition), public :: io

contains

  subroutine open_wo_options( filename, iostat )
    implicit none
    character(len = *), intent(in) :: filename
    integer, intent(out), optional :: iostat

    integer :: file_unit
    integer :: stat

    call get_free_unit(file_unit)
    open(unit = file_unit, file = filename, iostat = stat)
    if (stat == 0) call file_units%associate(key = truncate(filename), val = file_unit)

    if (present(iostat)) iostat = stat
  end subroutine open_wo_options

  subroutine close_wo_options( filename, iostat )
    implicit none
    character( len = *), intent(in) :: filename
    integer, intent(out), optional :: iostat
    
    integer :: file_unit
    integer :: stat

    if (is_open(filename)) then
       close(unit(filename), iostat = stat)
       if (present(iostat)) iostat = stat

       if (stat == 0) call file_units%delete(key = truncate(filename))
    else
       if (present(iostat)) iostat = 11
    endif
  end subroutine close_wo_options

  function is_open( filename ) result(is_open_value)
    implicit none
    character(len = *), intent(in) :: filename
    logical :: is_open_value

    is_open_value = file_units%has_key(truncate(filename))    
  end function is_open

  function unit( filename ) result(unit_value)
    implicit none
    character(len = *), intent(in) :: filename
    integer :: unit_value

    unit_value = file_units%value(truncate(filename))
  end function unit

  pure function open_files() result( filename_array )
    implicit none
    character( len = max ), dimension(file_units%length()) :: filename_array

    filename_array = file_units%keys()
  end function open_files

  pure function  associated_units() result( unit_array )
    implicit none
    integer, dimension(file_units%length()) :: unit_array

    unit_array = file_units%values()
  end function associated_units
  
  pure function number_open_files() result( no ) 
    implicit none
    integer :: no

    no = file_units%length()
  end function number_open_files

  subroutine get_free_unit(unit)
    implicit none
    integer, intent(out) :: unit

    free_unit = free_unit + 1
    unit = free_unit
  end subroutine get_free_unit
    
  function truncate(filename) result(truncated_filename)
    implicit none
    character(len = *), intent(in) :: filename
    character(len = 256) :: truncated_filename

    if (len(filename) > 256) then
       truncated_filename = filename(len(filename) - 255 : len(filename))
    else
       truncated_filename = filename
    endif
  end function truncate

end module io_singleton

!!$program debug
!!$  use io_singleton
!!$  implicit none
!!$  integer :: unit, i
!!$  character(len = 256), dimension(3) :: filenames
!!$  integer, dimension(3) :: units
!!$
!!$  call io%open('test.out')
!!$  write(*,*) "test is open : ", io%is_open('test.out')
!!$  write(*,*) " unit = ", io%unit('test.out')
!!$  call io%open('test1.out')
!!$  write(*,*) " unit = ", io%unit('test1.out')
!!$  call io%open('test2.out')
!!$  write(*,*) " unit = ", io%unit('test2.out')
!!$  write(*,*)
!!$  write(*,*) io%number_open_files(), " files open"
!!$  write(*,*)
!!$
!!$  filenames = io%open_files()
!!$  units = io%associated_units()
!!$
!!$  do i = 1, io%number_open_files()
!!$     write(*,*) trim(filenames(i)), units(i)
!!$  enddo
!!$
!!$  unit = io%unit('test.out')
!!$
!!$  write(unit,*) "This is a test"
!!$  call io%close('test.out')
!!$  call io%close('test1.out')
!!$  call io%close('test2.out')
!!$  write(*,*)
!!$  write(*,*) io%number_open_files(), " files open"
!!$  write(*,*)
!!$  write(*,*) "test is open : ", io%is_open('test.out')
!!$end program debug
