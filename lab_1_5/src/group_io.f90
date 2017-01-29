
module Group_IO
   use Environment

   implicit none  
   
   
   integer, parameter               ::  NAME_LEN = 15, TEL_LEN = 5
  
   type people
      character(NAME_LEN, kind=CH_) :: surname
      character(TEL_LEN, kind=CH_)  :: tel
      type(people), pointer         :: next => Null()
   end type people

contains
   
   function Read_list(Input_File) result(Firts_Per)
      type(people), pointer      :: Firts_Per
      character(*), intent(in)   :: Input_File
      integer  In

      open (file=Input_File, encoding=E_, newunit=In)
         Firts_Per => Read_person(In)
      close (In)
   end function Read_list

  
   recursive function Read_person(In) result(Person)
      type(people), pointer   :: Person
      integer, intent(in)     :: In
      integer  IO
      character(:), allocatable  :: format
      
      allocate (Person)
      format = '(2a)'
      read (In, format, iostat=IO) Person%surname, Person%tel
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
          Person%next => Read_person(In)
      else
         deallocate (Person)
         nullify (Person)
      end if
   end function Read_person


   subroutine Output_list(Output_File, Class_List, List_Name, Position)
      character(*), intent(in)   :: Position, Output_File, List_Name
      type(people), intent(in)  :: Class_List
      integer  :: Out
      
      open (file=Output_File, encoding=E_,position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_pers(Out, Class_List)
      close (Out)
   end subroutine Output_list

   recursive subroutine Output_pers(Out, Person)
      integer, intent(in)        :: Out
      type(people), intent(in)   :: Person
      
      integer  :: IO
      character(:), allocatable  :: format

      format = '(2a)'
      write (Out, format, iostat=IO) Person%surname, Person%tel
      call Handle_IO_status(IO, "writing")
      if (Associated(Person%next)) &
         call Output_pers(Out, Person%next)
   end subroutine Output_pers
end module Group_IO 
