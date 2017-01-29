program threedotfourb
   implicit none
   integer, parameter        :: M_ = 100, I_ = 4, R_ = 4
   integer(I_)               :: i, In = 0, Out = 0
   real(R_)                  :: summ
   real(R_), dimension(1:M_) :: m
   character(*), parameter   :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"

   open(file = input_file, encoding = E_, newunit = In)
      read (In, *) m
   close(In)

   do i = 5, M_, 3
      summ = Sum(m(5::3))
   end do

   open(file = output_file, encoding = E_, newunit = Out)
      write(Out,*) "Result=", summ
   close(Out)
end program
