program sevendotfiftythree
   implicit none
   integer, parameter               :: I_ = 4
   integer(I_), allocatable         :: Z(:,:)
   integer(I_)                      :: In = 0, Out = 0, M_i, M_j, i
   character(*), parameter          :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"

   interface !описываем интерфейс процедуры rightzerо
      subroutine rightzero(m_in)
         integer, dimension(:,:) :: m_in
      end subroutine
   end interface

   open(file = input_file, encoding = E_, newunit = In)
      read(In,*) M_i, M_j
      allocate(Z(M_i, M_j))
      read(In,*) Z
   close(In)

   call rightzero(Z)
 
   open(file = output_file, encoding = E_, newunit = Out)
      write(Out, "(5i3)") (Z(:,i), i=1,M_i)
   close(Out)
end program

!процедура rightzero обнуляет правый треугольник, образованный
!из пересечения главной и обратной диагоналей матрицы
pure subroutine rightzero(m_in)
   implicit none
   integer, parameter                          :: I_ = 4
   integer(I_), intent(inout)                  :: m_in(:,:)
   integer(I_)                                 :: M_i, M_j, i, n, k

   n = 2
   M_i = UBound(m_in, 1)
   M_j = UBound(m_in, 2)

   k = M_i / 2 + 2

   do i = M_i, k, -1 
      m_in(i, n:M_j - n + 1) = 0 
      n = n + 1 
   end do

   return
end subroutine
