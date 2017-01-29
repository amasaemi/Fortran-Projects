program sevendottrityone
   implicit none
   integer, parameter                   :: I_ = 4
   integer(I_), allocatable             :: C(:,:), m2(:), m3(:)
   integer(I_)                          :: i, n, k, In = 0, Out = 0, M_i, M_j
   character(*), parameter              :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"

   open(file = input_file, encoding = E_, newunit = In)
      read(In,*) M_i, M_j
      allocate(C(M_i, M_j), m2(M_i), m3(M_j))
      read(In,*) C
   close(In)

   do i = 1, M_i
      m2(i) = i
   end do

   do k = 1, M_j
      do i = 1, M_i - 1

         if (C(i, 1) < C(i + 1, 1)) then
            
            m3 = C(i,:) ! меняем столбцы местами
            C(i,:) = C(i+1,:)
            C(i+1,:) = m3 

            n = m2(i) 
            m2(i) = m2(i + 1)
            m2(i + 1) = n
         end if

      end do
   end do
		
   open(file = output_file, encoding = E_, newunit = Out)
      write(Out, '(10i6)') m2
   close(Out)
end program
