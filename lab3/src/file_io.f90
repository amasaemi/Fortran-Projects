module File_IO
   use Environment

   implicit none

   ! Структура данных для хранения строк исходного текста.
   type SourceLine 
      character(:)                        :: String
      type(SourceLine), pointer           :: Next  => Null()
   end type SourceLine

contains
   ! Чтение исходного кода. 
   function Read_Source(InputFile) result (Code)
      type(SourceLine), pointer  :: Code
      character(*), intent(in)   :: InputFile
      integer  :: In
      
      open (file=InputFile, encoding=E_, newunit=In)
         Code => Read_Source_Line(In)
      close (In)
   end function Read_Source

   ! Чтение строки исходного кода.
   recursive function Read_Source_Line(in) result(Code)
      type(SourceLine), pointer  :: Code
      integer, intent(in)        :: In
      integer, parameter         :: max_len = 1024
      character(max_len, CH_)    :: tempString
      integer                    :: IO

      ! Чтение строки во временную строку бОльшей длины.
      read (In, "(a)", iostat=IO) tempString
      call Handle_IO_Status(IO, "reading line from source code")
      ! Проверяем, что строка не пустая.
      if (IO == 0) then
         allocate (Code)
         Code%String = Trim(tempString) ! Обрезаем хвост. пробелы.
         Code%Next => Read_Source_Line(in)
      else
         Code => Null()
      end if
   end function Read_Source_Line
 
   ! Вывод исходного кода.
   subroutine Output_Source(OutputFile, Code, Text)
      character(*), intent(in)      :: OutputFile , Text
      type(SourceLine), intent(in)  :: Code 
      integer  :: Out
      
      open (file=OutputFile, encoding=E_, newunit=Out)
         write(Out, "(/a)") Text
         call Output_Source_Line(out, Code)
      close (Out)
   end subroutine Output_Source

   ! Вывод строки исходного кода. invalid memory ref
   recursive subroutine Output_Source_Line(Out, Code)
      integer, intent(in)           :: Out
      type(SourceLine), intent(in)  :: Code
      integer  :: IO

      write (Out, "(a)", iostat=IO) Code%String
      call Handle_IO_Status(IO, "writing line to file")
      if (Associated(Code%next)) &
         call Output_Source_Line(Out, Code%next)
   end subroutine Output_Source_Line

end module File_IO
