program sevendotnine
   implicit none
   character(*), parameter  :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"
   integer, parameter       :: I_ = 4
   integer(I_), allocatable :: A(:,:), B(:,:), C(:,:), P(:), Q(:), &
      Diag(:)
   logical, allocatable     :: Spec(:)
   integer(I_)              :: In = 0, Out = 0, n, j, i, k

   open(file = input_file, encoding = E_, newunit = In)
      read(In,*) n, k
      allocate(A(n,n), C(n, n - 1), B(n - 1, n), P(n), Q(n), Diag(n), &
         Spec(n))
      read(In,*) A
   close(In)

k=0
j=0
   do concurrent(i=1:n)
      C(i,i:) = abs(A(i, i+1:))
      C(i, 1:(i-1)) = abs(A(i, 1:(i-1)))
   end do

   do concurrent(i=1:n-1)
      B(i,i:) = abs(A(i, i:))
      B(i,1:i) = abs(A(i+1, 1:i))
   end do

   P = sum(B,2)
   Q = sum(C,1)

   do i = 1,n
      Diag(i) = abs(A(i,i))
   end do

   Spec = (Diag > ((P**k)*(Q**(1-k))))

   open(file = output_file, encoding = E_, newunit = Out)
      write(Out, *) Spec
   close(Out)
end program
