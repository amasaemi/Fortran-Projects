program labtwodotseven
   implicit none
   character(*), parameter                          :: output_file = "../bin/output.txt"
   character(*), parameter                          :: input_file = "../data/input.txt", E_ = "UTF-8"
   integer, parameter                               :: CH_= Selected_Char_Kind("ISO_10646")
   integer, parameter                               :: SURNAME_LEN = 15, INITIAL_LEN = 5, I_ = 4, STUD_AMOUNT = 5

   character(kind=CH_)                              :: surname(STUD_AMOUNT, SURNAME_LEN)
   character(kind=CH_)                              :: initials(STUD_AMOUNT, INITIAL_LEN)
   
   call Read_list(input_file, surname, initials)
   
   call Sort_list(surname, initials)
   
   call Output_list(output_file, surname, initials)
   
   contains
         
      ! Вывод списка класса.
      subroutine Output_list(Output_File, surname, initials)
         character(*), intent(in)        :: Output_File
         character(kind=CH_), intent(in) :: surname(:, :), initials(:, :)
         integer                         :: Out, i
         character(:), allocatable       :: format
      
         open (file=output_file, encoding=E_, newunit=Out)
            write (out, '(/a)') "Отсортированный список"
            format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIAL_LEN // 'a1)'

            write (Out, format) (surname(i,:), initials(i,:), i=1,4)
         close (Out)
      end subroutine Output_list

      ! сортировка методом "один со всеми"
      pure subroutine Sort_list(surname, initials)
         character(kind=CH_), intent(inout) :: surname(:, :), initials(:, :)
         integer              :: i, j, k
         character(kind=CH_)  :: tempSurname(SURNAME_LEN)
         character(kind=CH_)  :: tempInitials(INITIAL_LEN)
         
         do i = 1, STUD_AMOUNT -1
            j = i
            do k = i + 1, STUD_AMOUNT
    
               if(GT(surname(j, :), surname(k, :))) then
                  j = k
               else if(EQ(surname(j, :), surname(k, :))) then
                  if(GT(initials(j, :), initials(k, :))) then
                     j = k
                  end if
                end if
            end do
         
            ! меняем местами в массиве
            tempSurname = surname(i, :)
            surname(i, :) = surname(j, :)
            surname(j, :) = tempSurname
    		 
            tempInitials = initials(i, :)
            initials(i, :) = initials(j, :)
            initials(j, :) = tempInitials
         end do
      end subroutine Sort_list
      
      ! Функция операции > для массивов символов.
      pure logical function GT(arr1, arr2)
         character(kind=CH_), intent(in) :: arr1(:), arr2(:)
    
         integer :: i 
    
         ! Поиск первого отличного символа или остановка на последнем символе.
         do i = 1, Min(Size(arr1), Size(arr2)) - 1
            if (arr1(i) /= arr2(i)) &
               exit
         end do
         GT = arr1(i) > arr2(i)  
      end function GT

      ! Функция операции == для массивов символов.
      pure logical function EQ(arr1, arr2)
         character(kind=CH_), intent(in) :: arr1(:), arr2(:)
         integer :: i
    
         EQ = .true.
         do i = 1, Min(Size(arr1), Size(arr2)) - 1
            if (arr1(i) /= arr2(i)) then
               EQ = .false.
               exit
            end if
         end do
      end function EQ
     
      ! процедура для чтения массива фамилий и инициалов из массива
      subroutine Read_list(Input_File, surname, initials)
         character(*), intent(in)         :: Input_File
         character(kind=CH_), intent(out) :: surname(:, :), initials(:, :)
    
         integer                          :: In, i
         character(:), allocatable        :: format
         
         open (file=Input_File, encoding=E_, newunit=In)
            format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIAL_LEN // 'a1)'
            read (In, format) (surname(i, :), initials(i, :), i = 1, STUD_AMOUNT)
         close (In)
      end subroutine Read_list
	 
end program
