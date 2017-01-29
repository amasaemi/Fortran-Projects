program threedotseven
   use Environment
   use People_Process
   use People_IO

   implicit none
   character(*), parameter                        :: output_file = "../bin/output.txt"
   character(*), parameter                        :: input_file = "../data/input.txt"
   character(*), parameter                        :: data_file = "../bin/class.dat"

   type(people), allocatable                      :: pers(:)

   ! создаем неформатированный файл с данными
   call Create_data_file(input_file, data_file)

   pers = Read_people_list(data_file)
write(*,*) pers

   ! записываем в выходной файл исходный список людей
   call Output_people_list(output_file, pers, "Входной список:", "rewind")

   ! производим сортировку
   call Sort_people_list(pers)

   ! записываем в файл отсортированный список
   call Output_people_list(output_file, pers, "Отсортированный список:", "append")
end program
