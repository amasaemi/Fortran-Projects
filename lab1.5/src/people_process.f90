module People_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use People_IO

   implicit none
   
contains

    recursive subroutine Sort(Pers)
      type(people), pointer, intent(inout) :: Pers
      type(people), pointer                :: current
      current => Pers
      call Swap(Pers, Pers, current)
      if (Associated(Pers%next%next)) then
         call Sort(Pers%next)
      endif
   end subroutine Sort

    recursive subroutine Swap(list, curr, tempMax)
      type(people), pointer, intent(inout) :: curr, tempMax
      type(people), intent(inout)          :: list
      type(people)                         :: temp1, temp2

      if (Associated(curr%next)) then
         if (curr%next%surname < tempMax%next%surname) then
write(*,*) "*"
            call Swap(list, curr%next, curr)
         else if ((curr%next%surname == tempMax%next%surname) &
            .and. (curr%next%initials < tempMax%next%initials)) then
write(*,*) "**"
            call Swap(list, curr%next, curr)
         else
write(*,*) "***"
            call Swap(list, curr%next, tempMax)
         endif
      else if( .Not. Associated(list%next, curr)) then
         temp2 = list
         temp1 = tempMax%next
         temp1%next => list%next
         temp2%next => tempMax%next%next
         tempMax%next = temp2
         list = temp1
write(*,*) "****"
      end if
   end subroutine Swap

end module people_process
