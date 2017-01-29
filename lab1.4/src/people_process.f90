module People_Process
   ! модуль с ЧИСТЫМИ процедурами обработки данных
   use Environment
   use People_IO

   implicit none
   
contains
   pure subroutine Sort_people_list(pers)
      type(people), intent(inout) :: pers(:)
      
      call Drop_down(pers, 1)

      if(Size(pers) >= 3) call Drop_down(pers, Size(pers) - 1)
   end subroutine

   pure recursive subroutine Drop_down(pers, j)
      type(people), intent(inout)  :: pers(:)
      integer, intent(in)          :: j
      integer                      :: i, k, N
      
      type(people)  :: tempPers

      N = Size(pers)

      do i = 1, N
         if(Swap(pers, i, j)) k = i
      end do
      
      if(k /= j) then
         tempPers = pers(j)
         pers(j) = pers(k)
         pers(k) = tempPers
      end if

      if(j < N) call Drop_down(pers, j+1)
   end subroutine

   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function Swap(pers, j, k)
      type(people)  :: pers(:)
      integer        :: j, k
      intent(in)     pers, j, k

      Swap = .false.
      if(pers(j)%surname < pers(k)%surname) then
         Swap = .true.
      else if(pers(j)%surname == pers(k)%surname) then
         if(pers(j)%initials > pers(k)%initials) then
            Swap = .true.
         end if
      end if
   end function Swap
end module
