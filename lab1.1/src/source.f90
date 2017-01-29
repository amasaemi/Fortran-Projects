program onedotseven
   implicit none
   character(*), parameter                        :: output_file = "../bin/output.txt", E_ = "UTF-8"
   character(*), parameter                        :: input_file = "../data/input.txt"
   integer, parameter                             :: CH_= Selected_Char_Kind("ISO_10646")
   integer, parameter                             :: SURNAME_LEN = 15, INITIAL_LEN = 4, I_ = 4
   integer(I_)                                    :: i, j, k, amount_pers
   integer(I_)                                    :: In = 0, Out = 0

   type people
      character(SURNAME_LEN, kind=CH_)  :: surname
      character(INITIAL_LEN, kind=CH_)  :: initials
   end type people

   type(people), allocatable                      :: pers(:)
   type(people)                                   :: tempPers

   open (file=input_file, encoding=E_, newunit=In)
      read (In,*) amount_pers
      allocate(pers(amount_pers))
      read (In, '(2a)') (pers(i)%surname, pers(i)%initials, i=1,amount_pers)
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write(Out, '(/a)') "Исходный список:"
      write(Out,'(2a)') (pers(i)%surname, pers(i)%initials, i=1,amount_pers)
   close (Out)

   ! сортировка методом "один со всеми"
   do i = 1, amount_pers -1
      j = i
      do k = i + 1,  amount_pers
         
         if(pers(j)%surname > pers(k)%surname) then
            j = k
         else if(pers(j)%surname == pers(k)%surname) then
            if(pers(j)%initials > pers(k)%initials) then
               j = k
            end if
         end if

      end do
      
      ! меняем местами объекты в массиве
      tempPers = pers(i)
      pers(i) = pers(j)
      pers(j) = tempPers
   end do
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write(Out, '(/a)') "Отсортированный список:"
      write(Out,'(2a)') (pers(i)%surname, pers(i)%initials, i=1,amount_pers)
   close (Out)
end program
