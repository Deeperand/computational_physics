program main
    implicit none
    integer :: N=1000, i
    real(8) :: y(0:1000), x, a, b, h
    y(0) = 0.0
    a = 0.0
    b = 2.0D0
    x = a
    h = (b - a) / N
    do i = 1, N
        y(i) = y(i-1) + (3*x**2)*h
        x = x + h
    end do
    write(*,*) y(N)
end program main