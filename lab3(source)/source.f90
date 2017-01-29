program labtwo
   use Environment
   use File_Process
   use File_IO

   implicit none
   character(*), parameter                    :: File3 = "../bin/F3.txt"
   character(*), parameter                    :: File1 = "../data/F1.txt"
   character(*), parameter                    :: File2 = "../data/F2.txt"

   type(SourceLine), pointer                  :: F1
   type(SourceLine), pointer                  :: F2
   type(SourceLine), pointer                  :: F3

   allocate(F1, F2, F3)

   ! Читаем исходный файл.
   F1 => Read_Source(File1)
   F2 => Read_Source(File2)

   F3 = RemoteStrings(F1, F2)
   call Output_Source(File3, F3, "Полученный результат:")

end program
