module sisofun_objects
  implicit none

  type, abstract :: sisofun_object
   contains
     procedure(set_interface), nopass, deferred :: set
     procedure(eval_interface), nopass, deferred :: eval
  end type sisofun_object

  abstract interface
     subroutine set_interface(var_name, var_val)
       character(*), intent(in) :: var_name
       double precision, intent(in) :: var_val
     end subroutine set_interface
  end interface

  abstract interface
     pure elemental function eval_interface(input) result(output)
       double precision, intent(in) :: input
       double precision :: output
     end function eval_interface
  end interface

end module sisofun_objects
