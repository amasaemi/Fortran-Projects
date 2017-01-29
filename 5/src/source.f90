program fivedotsix
   implicit none
   integer, parameter           :: I_ = 2
   integer(I_), allocatable     :: M(:)
   logical                      :: c = .false.
   integer(I_)                  :: k, m, In = 0, Out = 0
   character(*), parameter      :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"

   open(file = input_file, encoding = E_, newunit = In)
      read (In,*) k, m
      allocate (M(m))
      read (In,*) M
   close (In)

   c = count(M == k) > 1 ! если в массиве есть элементы, 
                           ! совпадающие с к, то pos присвоить true

   open(file = output_file, encoding = E_, newunit = Out)
      write(Out,*) c
   close(Out)
end program
