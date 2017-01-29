program twodotseven
   implicit none
   integer, parameter      :: R_ = 4, I_ = 4
   integer(I_)             :: In = 0, Out = 0
   real(R_)                :: a, b, c
   character(*), parameter :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"

   open(file = input_file, encoding = E_, newunit = In)
      read (In, *) a, b
   close (In)

   if(a .lt. b) then
      c =(ABS(a) + ABS(b)) / 2
   else
      c = 1 + 2 * ABS(a)
   end if

   open(file = output_file, encoding = E_, newunit = Out)
      write(Out,*) "Result. При a=", a, ", b=", b, ", c=", c
   close(Out)
end program
