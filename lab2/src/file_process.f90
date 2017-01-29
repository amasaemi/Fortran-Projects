module File_Process
   use Environment
   use File_IO

   implicit none
   
contains
   pure recursive function RemoteStrings(initialStrings, first, last, current) result(resultStrings)
      type(SourceLine), pointer     :: resultStrings
      type(SourceLine), intent(in)  :: initialStrings
      integer, intent(in)           :: first, last, current
      integer                       :: curSet

      ! current - текущая строка
            
      curSet = current + 1
      if((curSet < first) .or. (curSet > last)) then
         allocate(resultStrings)
         resultStrings%String = initialStrings%String
 
         if(Associated(initialStrings%Next)) then
            resultStrings%Next => RemoteStrings(initialStrings%Next, first, last, curSet)
         else
            resultStrings%Next => Null()      
         end if

      end if
   end function

end module File_Process
