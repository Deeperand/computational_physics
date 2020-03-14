program main
    implicit none
    real(8) :: T, a = 1.D-4, b = 50.D0, inte
    integer :: N = 10**6, i
    do i = 1, 3
        T = real(i)
        write(*, '(A3, I2, F12.4)') "T = ", i, inte(T, a, b, N) / T**4
    end do
end program main

function f(x, T)
    real(8) x, T, f
    f = 2 / (x**5 * (exp(1 / (x * T)) - 1))
end function f

function inte(T, a, b, N) result(integration)
    implicit none
    real(8) :: T, a, b
    integer N, i
    real(8) :: integration
    real(8) f, h

    h = (b - a) / real(N)
    integration = a
    do i = 1, N / 2
        integration = integration + 4 * f(a + (2.D0*i - 1) * h, T) + 2 * f(a + 2.D0*i * h, T)
    end do
    integration = integration - f(b, T)
    integration = integration * h / 3.0
end function inte
