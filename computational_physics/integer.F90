program integer
    implicit none
    integer times
    real(8):: x1, x2, y = 0
    !    real(8), external:: integ
    read(*,*) x1, x2, times
    y = integ(x1, x2, times)
    write(*,*) y

contains
    function integ(a, b, N)
        implicit none
        integer:: N, i
        real(8) a, b, integ, interval
        integ = exp(a)
        interval = (b-a)/N
        do i = 1, N
        integ = integ+interval*exp(a)
        a = a+interval
        end do
    end function integ
end program integer
