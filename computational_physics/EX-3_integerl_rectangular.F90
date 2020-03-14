program main
    implicit none
    integer i, j
    real(8) a, b, N, num, integrate, delta_x, xi

    a = -1.0D0
    b = 2.0D0

    do i = 1, 20
        N = 2.0**i
        num = N + 1
        delta_x = (b - a) / N
        xi = a + delta_x

        integrate = 0.5 * f(a)
        do j = 1, int(num - 2)
            integrate = integrate + f(xi)
            xi = xi + delta_x
        end do
        integrate = integrate + 0.5 * f(b)
        integrate = integrate * delta_x

        write(*,'(I10, F15.7)') int(N), integrate
    end do

    contains
        function f(x)
            implicit none
            real(8) f, x
            if (x >= 0) then
                f = x**2 + x**4 + x**6 + x**8
            else
                f = x + x**3 + x**5 + x**7
            end if
        end function f
end program main
