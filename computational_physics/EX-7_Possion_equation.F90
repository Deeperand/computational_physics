program main
    implicit none
    ! input var
        integer, parameter :: seg_num = 100 ! segment number
        real(8), parameter :: eps = 1.D-4
        real(8), dimension(0:seg_num, 0:seg_num) :: phi, rho
        real(8) omega ! relaxation paremeter
        real(8) delta_x, delta_y ! interval
        real(8) :: x_length = 3.D0, y_length = 4.D0

    ! loop var
    integer i ! loop parameter

    ! out put var
    integer times ! iteration times

    ! function
    integer solve_equation

    delta_x = x_length / dble(seg_num); delta_y = y_length / dble(seg_num)
    write(*,'(2A8)') 'omega', 'times'
    open(unit = 21, file = 'EX_7_data.txt')
    do i = 1, 20
        omega = 1.D0 + 0.05D0 * i
        call ini_point(phi, rho, seg_num)
        times = solve_equation(omega, phi, rho, eps, seg_num, x_length, y_length)
        write(*, '(F8.4, I6)') omega, times
        write(21,'(F8.4, A3, I6, A3)') omega, '&', times, '\\'
    end do
    close(21)
end program main

function solve_equation(omega, phi, rho, eps, seg_num, x_length, y_length) result(times_result)
    implicit none
    ! input/output var
        integer times_result
        real(8) omega, eps, x_length, y_length
        integer seg_num
        real(8), dimension(0:seg_num, 0:seg_num) :: phi, rho

    ! local var
        real(8) ErrSum, phiN, delta_x, delta_y
        integer, parameter :: times_max = 10**4, chk_step = 20
        integer i, j, times ! iteration related
        real(8) tmp1, tmp2, tmp3, f_ij

    delta_x = x_length / dble(seg_num - 1)
    delta_y = y_length / dble(seg_num - 1)
    do times = 1, times_max
        ErrSum = 0.D0
        do j = 1, seg_num
            do i = 1, seg_num
                tmp1 = 0.5D0 * delta_x**2 * delta_y**2 / (delta_x**2 + delta_y**2)
                tmp2 = 1.D0 / delta_x**2 * (phi(i+1, j) + phi(i-1, j))
                tmp3 = 1.D0 / delta_y**2 * (phi(i, j+1) + phi(i, j-1))
                f_ij = -rho(i, j)
                phiN = tmp1 * (tmp2 + tmp3 - f_ij)
                ErrSum = ErrSum + abs(phi(i, j) - phiN)
                phi(i, j) = (1.D0 - omega) * phi(i, j) + omega * phiN
            end do
        end  do
        if (ErrSum < eps) exit
    end do

    times_result = times
end function

subroutine ini_point(phi, rho, seg_num)
    implicit none
    ! input var
        integer seg_num
        real(8), dimension(0:seg_num, 0:seg_num) :: phi, rho
        real(8), parameter :: PI = 4.D0*atan(1.D0)

    phi(:, :) = 0.D0
    phi(0, :) = 1.D0; phi(seg_num, :) = 1.D0; phi(:, 0) = 1.D0; phi(:, seg_num) = 1.D0
    rho(:, :) = 0.D0
    rho(seg_num/2, seg_num/2) = 400.D0
end subroutine
