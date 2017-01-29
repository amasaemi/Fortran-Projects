module People_IO
   use Environment

   implicit none  
   
   integer, parameter :: SURNAME_LEN = 15
   integer, parameter :: INITIAL_LEN = 4
   integer, parameter :: PEOPLE_AMOUNT = 4

   type people
      character(SURNAME_LEN, kind=CH_)  :: surname
      character(INITIAL_LEN, kind=CH_)  :: initials
      type(people), pointer             :: next
   end type people

contains
   
   function Read_list(Input_File) result(Pers)
      type(people), pointer      :: Pers
      character(*), intent(in)   :: Input_File
      integer  In

      open (file=Input_File, encoding=E_, newunit=In)
         Pers => Read_person(In)
      close (In)
   end function Read_list

  
   recursive function Read_person(In) result(Pers)
      type(people), pointer   :: Pers
      integer, intent(in)     :: In
      integer  IO
      character(:), allocatable  :: format
      
      allocate (Pers)
      format = '(2a)'
      read (In, format, iostat=IO) Pers%surname, Pers%initials
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
          Pers%next => Read_person(In)
      else
         deallocate (Pers)
      end if
   end function Read_person


   subroutine Output_list(Output_File, Pers, List_Name, Position)
      character(*), intent(in)   :: Position, Output_File, List_Name
      type(people), intent(in)   :: Pers
      integer  :: Out
      
      open (file=Output_File, encoding=E_,position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_pers(Out, Pers)
      close (Out)
   end subroutine Output_list

   recursive subroutine Output_pers(Out, Pers)
      integer, intent(in)        :: Out
      type(people), intent(in)   :: Pers
      
      integer  :: IO
      character(:), allocatable  :: format

      format = '(2a)'
      write (Out, format, iostat=IO) Pers%surname, Pers%initials
      call Handle_IO_status(IO, "writing")
      if (Associated(Pers%next)) &
         call Output_pers(Out, Pers%next)
   end subroutine Output_pers
end module People_IO 
