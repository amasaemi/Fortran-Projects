program fivedotthree
   implicit none
   integer, parameter            :: M1_ = 25, M2_ = 50, M3_ = 75, I_ = 4
   integer(I_), dimension(1:M1_) :: m1
   integer(I_), dimension(1:M2_) :: m2
   integer(I_), dimension(1:M3_) :: m3
   integer(I_)                   :: n, i, j, In = 0, Out = 0
   character(*), parameter       :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"
	
   open(file = input_file, encoding = E_, newunit = In)
      read(In,*) m1, m2
   close(In)

   m3 = [m1, m2] !соединяем 2 массива

   open(file = output_file, encoding = E_, newunit = Out)
      write(Out,*) "Массив до сортировки:"
      write(Out, '(10i4)') m3

   do j = 1, M3_
      do i = 1, M3_

         if (m3(i) < m3(i+1)) then
            n = m3(i)
            m3(i) = m3(i+1)
            m3(i+1) = n
         end if

      end do
   end do

      write(Out,*) "Отсортированный массив:"
      write(Out, '(10i4)') m3
   close(Out)
end program
