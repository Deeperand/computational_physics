program main
    implicit none
    real(8) :: x0 = 13.D0, M = 2.D30, lambda = 16807.D0
    integer :: i, N = 1000
    real(8), dimension(0:1000) :: rand_num, x

    !function
    real(8) f

    x(0) = x0
    rand_num(0) = x0/M
    do i = 1, N
        x(i) = f(lambda, M, x(i-1))
        rand_num(i) = x(i)/M
        write(*,'(F8.4)') rand_num(i)
    end do
end program

function f(lambda, M, x) result(output)
    real(8) lambda, M, x, output
    output = mod(lambda * x, M)
end function