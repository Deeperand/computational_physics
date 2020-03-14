program Taylor_cos
    implicit none
    real(8), parameter :: pi = 4.0*atan(1.D0)
    real(8) :: term = 1.0, result = 1.0, x, i = 0, factorial = 1.0, sign = 1.0

    x = pi/3
    do while (abs(term) >= 1.0d-8)
        i = i+2
        factorial = factorial * i * (i-1)
        term = sign /factorial * x**i
        result = result + term
        sign = -1.0 * sign
    enddo
    write(*,*) result, term
end program Taylor_cos