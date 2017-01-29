module People_Process
   ! модуль с ЧИСТЫМИ процедурами обработки данных
   use Environment
   use People_IO

   implicit none
   
contains
   pure subroutine Sort_people_list(pers)
      type(people), intent(inout) :: pers(:)
      type(people)                :: tempPers
      integer                     :: i, j, k

      ! сортировка методом "один со всеми"
      do i = 1, Size(pers) -1
         j = i
         do k = i + 1,  Size(pers)
         
            if(pers(j)%surname > pers(k)%surname) then
               j = k
            else if(pers(j)%surname == pers(k)%surname) then
               if(pers(j)%initials > pers(k)%initials) then
                  j = k
               end if
            end if
         end do
      
         ! меняем местами объекты в массиве
         if(i /= j) then
            tempPers = pers(i)
            pers(i) = pers(j)
            pers(j) = tempPers
         end if
      end do
   end subroutine
end module
