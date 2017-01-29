
module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none
   
contains
    pure recursive subroutine sort(head, lastSorted)
      type(people), pointer :: tmp, head
      type(people), intent(inout)          :: lastsorted
      
      if (ASSOCIATED(lastsorted%next)) then
         if (lastsorted%next%tel > lastsorted%tel) then
             call sort(head, lastsorted%next)
         else
             tmp => lastsorted%next  !1
             lastsorted%next => lastsorted%next%next ! 2
             call Paste(head, tmp)
             call sort(head, lastsorted)
         end if          
      end if
    end subroutine sort
    
    pure recursive subroutine paste(head, tmp)    
      type(people), pointer :: head, tmp
      if (ASSOCIATED(head%next)) then
        if (head%next%tel>tmp%tel) then
          tmp%next => head%next
          head%next => tmp
        else
          call paste(head%next, tmp)
        endif
      else       
        head%next=> tmp
        tmp%next => null()  
      endif      
    end subroutine paste 

end module group_process
