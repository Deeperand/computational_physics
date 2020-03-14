#define PI 3.1415926
program main
    implicit none
    real(8) :: f, y, sep
    real(8) :: left_lim = -4.D0, right_lim = 4.D0
    integer, parameter :: sep_num = 10**3, N = 10**5
    integer :: i = 0, j = 0
    integer counter(1:sep_num)

    sep = (right_lim - left_lim) / sep_num

    ! seg 
    do i = 1, sep_num
        counter(i) = 0
    end do

    ! generate distribution
    do i = 1, N
        y = f()

        do j = 1, sep_num
            if (y > left_lim + sep*(j-1) .AND. y < left_lim + sep*j) then
                counter(j) = counter(j) + 1
                exit
            end if
        end do
    end do

    ! output distribution
    open(unit = 20, file = 'practice_0311_2.txt')
    do j = 1, sep_num
        write(*, '(F8.4, A2, F8.4)') left_lim + j*sep, ",", counter(j)/real(sep_num)
        write(20, '(F8.4, A2, F8.4)') left_lim + j*sep, ",", counter(j)/real(sep_num)
    end do
    close(20)

end program main

function f()
    real(8) f
    real(8) zeta_1, eta_h, zeta_2

    ! initial value
    call random_number(zeta_1)
    call random_number(zeta_2)

    eta_h = -log(zeta_1)

    ! loop
    do while (x2 > gx)
        call random_number(x)
        call random_number(x2)
        y1 = -log(x)
        gx = 1*0.5D0 * exp(-0.5D0 * (x2 - 1)**2.D0)
    end do

    f = gx
end function f
