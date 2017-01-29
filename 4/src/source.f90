program fourdottwov
   implicit none
   integer, parameter      :: R_ = 4, I_ = 4
   real, parameter         :: con1 = 2.3754, e = 2.718
   real(R_)                :: y1, y2, x1, x2, step_x, step_y
   integer(I_)             :: In = 0, Out = 0, i = 0, j = 0, N = 0, Nx = 0, Ny = 0
   real(R_), allocatable   :: X(:), Y(:), Small_Y(:), F(:)
   character(*), parameter :: input_file = "../data/input.txt", output_file = "../bin/output.txt", E_ = "UTF-8"

   open(file = input_file, encoding = E_, newunit = In)
      read (In, *) x1, x2, y1, y2, step_x, step_y
   close(In)

   Nx = Int((x2-x1) / step_x + 0.5) + 1
   Ny = Int((y2-y1) / step_y + 0.5) + 1

   N = Nx * Ny
   allocate (X(N), Y(N), Small_Y(Ny), F(N))

   forall (i = 1:Nx)&
      X(1+Ny*(i-1):Ny*i) = x1 + step_x*(i-1)
   
   forall (j = 1:Ny)&
      Small_Y(j) = y1 + step_y*(j-1)

   forall (j = 1:Nx)&
      y(1+Ny*(j-1):Ny*j) = Small_Y

   F = sqrt(Y)*e**X+con1*X**3

   open(file = output_file, encoding = E_, newunit = Out)
      write (Out, '(f0.4)') (f(i), i = 1, Nx*Ny)
   close(Out)
end program
