program main
    implicit none
    real(8) :: x, f, y, sep
    integer, parameter :: sep_num = 10**3, N = 10**5
    integer :: i = 0, j = 0
    integer counter(1:sep_num)

    sep = 2.D0 / sep_num

    do i = 1, sep_num
        counter(i) = 0
    end do

    do i = 1, N
        call random_number(x)
        y = f(x)

        do j = 1, sep_num
            if (y > -1.D0 + sep*(j-1) .AND. y < -1.D0 + sep*j) then
                counter(j) = counter(j) + 1
                exit
            end if
        end do
    end do

    open(unit = 20, file = 'practice_0306.txt')

    do j = 1, sep_num
        write(*, '(F8.4, A2, F8.4)') -1 + j*sep, ",", counter(j)/real(sep_num)
        write(20, '(F8.4, A2, F8.4)') -1 + j*sep, ",", counter(j)/real(sep_num)
    end do
    close(20)

end program main

function f(x)
    real(8) x, f, a
    a = 2.D0*x - 1.D0
    if (a >= 0) then
        f = (2.D0*x - 1.D0)**(1.D0/3.D0)
    else
        f = -(1.D0 - 2.D0*x)**(1.D0/3.D0)
    endif
end function f
