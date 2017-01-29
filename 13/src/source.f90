program eightdotseven
   implicit none
   character(*), parameter        :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"
   integer, parameter             :: I_ = 4
   integer(I_), allocatable       :: B(:,:)
   integer(I_)                    :: In = 0, Out = 0, m, i

   interface !описываем интерфейс процедуры rightzerо
      subroutine trzero(A)
         integer :: A(:,:)
      end subroutine
   end interface

   open(file = input_file, encoding = E_, newunit = In)
      read(In,*) m
      allocate(B(m,m))
      read(In,*) B
   close(In)

   call trzero(B)

   open(file = output_file, encoding = E_, newunit = Out)
      write(Out, "(10i4)") (B(:,i), i=1,m)
   close(Out)
end program

pure subroutine trzero(A)
   implicit none
   integer, parameter                       :: I_ = 4
   integer(I_), intent(inout)               :: A(:,:)
   integer(I_)                              :: j, i

   i = UBound(A, 1)

   do j = 2, i
      A(j, 1:j-1) = 0
   end do

   return
end subroutine
