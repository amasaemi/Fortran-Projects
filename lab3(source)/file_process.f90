module File_Process
   use Environment
   use File_IO

   implicit none
   
contains
   pure recursive function RemoteStrings(F1, F2) result(F3)
      type(SourceLine), pointer     :: F3
      type(SourceLine), intent(in)  :: F1, F2

      allocate(F3)

      if(EQ(F1, F2)) then
         if(Associated(F1%Next)) then
            F3 = RemoteStrings(F1%Next, F2)
         end if
      else
         F3%String = F1%String

         if(.not.(Associated(F1%Next))) then
            F3%Next => Null()
         else
            F3%Next => RemoteStrings(F1%Next, F2)
         end if
      end if
         
   end function

   pure recursive function EQ(F1, F2) result(b)
      type(SourceLine), intent(in)  :: F1, F2
      logical                       :: b

      if(F1%String == F2%String) then
         b = .true.
      else if(Associated(F2%Next)) then
         b = EQ(F1, F2%Next)
      else
         b = .false.
      end if

   end function

end module File_Process
