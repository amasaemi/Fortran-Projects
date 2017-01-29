
program lab_1_5
   use Environment
   use People_IO
   use People_Process
   
   implicit none

   character(:), allocatable  :: input_file, output_file

   type(people), pointer      :: Pers => Null()
 
   input_file = "../data/input.txt"
   output_file = "output.txt"

   Pers => Read_list(input_file)
   
   call Output_list(output_file, Pers, "Исходный список:","rewind")
  
   call Sort(Pers)

   call Output_list(output_file, Pers, "Сортированный список:", "append")

end program lab_1_5
