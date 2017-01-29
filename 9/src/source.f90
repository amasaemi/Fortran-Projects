program sevendoteighteen
   implicit none
   integer, parameter                 :: I_ = 4
   integer(I_), allocatable           :: A(:,:), mas2(:), mas3(:), b(:)
   integer(I_)                        :: m, i, j, In = 0, Out = 0, max_, max_i, max_j
   character(*), parameter            :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"

   open(file = input_file, encoding = E_, newunit = In)
      read(In,*) m
      allocate(A(m,m), b(m*2), mas2(m), mas3(m))
      read(In,*) A
   close(In)

   ! записываем элементы главной и обратной диагоналей в массивы
   do i = 1, m
      mas2(i) = A(i,i)
      mas3(i) = A(m - i + 1, i)
   end do
   
   ! получаем один массив из элементов главной и обратной диагоналей
   b = [mas2, mas3]

   ! получаем номер максимального элемента
   ! если максимальный элемент лежал на главной диагонали - его
   ! координаты будут равны i и j, иначе если максимальный элемент
   ! лежал на обратной диагонали, вычисляем его координаты по формуле
   j = maxloc(b, dim=1)
   if (j <= m) then
      max_ = b(j)
      max_i = j
      max_j = j
   else
      max_ = b(j)
      max_j = 2*m - j + 1 ! нахождение j путем вычитания из размера
                          ! матрицы b позиции макс. эл. в матрице b
      max_i = m - max_j + 1 ! нахождение i путем вычитания из размера
                            ! исходной матрицы позиции j
   end if
   open(file = output_file, encoding = E_, newunit = Out)
      write(Out, '(10i4)') max_, max_i, max_j
   close(Out)
end program
