module People_IO
   use Environment

   implicit none
   integer, parameter :: SURNAME_LEN = 15
   integer, parameter :: INITIAL_LEN = 4
   integer, parameter :: PEOPLE_AMOUNT = 4

   type people
      character(SURNAME_LEN, kind=CH_)  :: surname
      character(INITIAL_LEN, kind=CH_)  :: initials
   end type people

contains
   ! создание неформатированного файла данных
   subroutine Create_data_file(Input_File, Data_File)
      character(*), intent(in)   :: Input_File, Data_File
      
      type(people)               :: pers
      integer                    :: In, Out, i, recl
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
      recl = ((SURNAME_LEN + INITIAL_LEN)*CH_)
      open (file=Data_File, form='unformatted', newunit=Out, access='direct', recl=recl)
         format = '(2a)'
         do i = 1, PEOPLE_AMOUNT
            read (In, format) pers

            write (Out, rec=i) pers
         end do
      close (In)
      close (Out)
   end subroutine Create_data_file

   ! чтение списка: фамилии, инициалы
   function Read_people_list(Data_File) result(pers)
      type(people)               :: pers(PEOPLE_AMOUNT)
      character(*), intent(in)   :: Data_File

      integer In, recl
      
      recl = ((SURNAME_LEN + INITIAL_LEN + 1)*CH_) * PEOPLE_AMOUNT
      open (file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
         read (In, rec=1) pers
      close (In)
   end function Read_people_list
 
   ! вывод списка людей
   subroutine Output_people_list(Output_File, pers, List_name, Position)
      character(*), intent(in)   :: Output_File, Position, List_name
      type(people), intent(in)   :: pers(:)

      integer                    :: Out
      character(:), allocatable  :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(2a)'
         write (Out, format) pers
      close (Out)
   end subroutine Output_people_list
end module People_IO
