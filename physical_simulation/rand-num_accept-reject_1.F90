#define PI 3.1415926
program main
    implicit none
    real(8) :: f, x, sep
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
        x = f()

        do j = 1, sep_num
            if (x > left_lim + sep*(j-1) .AND. x < left_lim + sep*j) then
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
    real(8) zeta_1, eta, zeta_2, g

    ! initial value
    call random_number(zeta_1)
    ! write(*,*) "zeta_1 = ", zeta_1
    eta = -log(zeta_1)
    g = 0.5D0 * exp(-0.5D0 * (eta - 1.D0)**2.D0)

    call random_number(zeta_2)
    ! write(*,*) "zeta_2 = ", zeta_2

    ! loop
    do while (zeta_2 > g)
        call random_number(zeta_1)
        eta = -log(zeta_1)
        g = 0.5D0 * exp(-0.5D0 * (eta - 1.D0)**2)
    end do

    f = eta
end function f
