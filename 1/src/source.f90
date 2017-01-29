program onedotsevenb
   implicit none
   integer, parameter      :: I_ = 4
   integer(I_)             :: a, b, In = 0, Out = 0
   character(*), parameter :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"

   open(file = input_file, encoding = E_, newunit = In)
      read(In, *) a, b
   close(In)

   open(file = output_file, encoding = E_, newunit = Out)
      write(Out,*) "Исходные переменные. a =", a, "b =", b

   a = a + b
   b = a - b
   a = a - b

      write(Out,*) "Result. а =", a, "b =", b
   close(Out)
end program
