program sevendotfourty
   implicit none
   integer, parameter                     :: I_ = 4
   integer(I_), allocatable               :: C(:,:), B(:)
   integer(I_)                            :: k, In = 0, Out = 0, M_i, M_j
   logical, allocatable                   :: Mask(:,:)
   character(*), parameter                :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"

   open(file = input_file, encoding = E_, newunit = In)
      read(In,*) M_i, M_j
      allocate(C(M_i, M_j))
      allocate(Mask(M_i, M_j), source=.false.)
      read(In,*) C
   close(In)
   
   Mask(::2, ::2) = C(::2, ::2) > 1
   Mask(2::2, 2::2) = C(2::2, 2::2) > 1
   k = Count(Mask)
   allocate(B(k))
   B = Pack(C, Mask)

   open(file = output_file, encoding = E_, newunit = Out)
      write(Out, '(10i4)') B
   close(Out)
end program
