program taylor_e
    implicit none
    integer n, i, j
    real x, a, result
    read(*,*) n, x
    result = 0
    do i = 1, n
        if (i > 1) then
            a = 1
            do j = 1, i-1
                a = a*j
            end do
            result = result + x**(i-1)/a
        else
            result = 1
        end if
    end do
    write(*,*) result
end program taylor_e