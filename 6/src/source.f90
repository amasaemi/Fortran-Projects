program sixdottwob
   implicit none
   integer, parameter      :: R_ = 4, I_ = 4
   integer(I_)             :: In = 0, Out = 0
   real(R_)                :: x, relerr, val, ln
   character(*), parameter :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"

   open(file = input_file, encoding = E_, newunit = In)
      read (In, *) x, relerr
   close(In)
	
   open(file = output_file, encoding = E_, newunit = Out)
      write(Out,*) "Входные данные. x=", x, "relerr", relerr

   val = ln(x, relerr)

      write(Out,*) "Result=", val
   close(Out)
end program

pure real function ln(x, relerr)
   implicit none
   integer, parameter   :: R_ = 4
   real(R_), intent(in) :: x, relerr
   real(R_)             :: res, cur
   integer              :: i

   res = x
   i = 2

   do
      cur = ((-1)**(i + 1)) * ((x**i) / i)
		
      if (abs(cur / res) > relerr) then
         res = res + cur
         i = i + 1
      else
         exit
      end if

   end do
	
   ln = res
   return
end function
