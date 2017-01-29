program labtwo
   use Environment
   use File_Process
   use File_IO

   implicit none
   character(*), parameter                    :: output_file = "../bin/output.txt"
   character(*), parameter                    :: input_file = "../data/input.txt"

   type(SourceLine), pointer                  :: initialStrings
   type(SourceLine), pointer                  :: resultStrings
   
   integer                                    :: first, last

   ! Читаем исходный файл.
   initialStrings => Read_Source(input_file)

   write(*,*) "Введите диапазон строк, которые следует исключить"
   read(*,*) first, last

   if (((first > 0) .and. (last > first)) .and. Associated(initialStrings)) then
      resultStrings = RemoteStrings(initialStrings, first, last, 0)
      call Output_Source(output_file, initialStrings, "Исходный текст:")
      call Output_Source(output_file, resultStrings, "Полученный текст:")
   end if
end program
