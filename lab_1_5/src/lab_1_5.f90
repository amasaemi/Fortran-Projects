
program lab_1_1
   use Environment
   use Group_IO
   use Group_Process
   
   implicit none

   character(:), allocatable  :: input_file, output_file

   type(people), pointer      :: Firts_Per => Null()
 
   input_file = "../data/input.txt"
   output_file = "output.txt"

   Firts_Per => Read_list(input_file)
   
   call Output_list(output_file, Firts_Per, "Исходный список:","rewind")
  
   call sort(Firts_Per, Firts_Per)

   call Output_list(output_file, Firts_Per, "Сортированный список:", "append")

 

end program lab_1_1
