# define l 1.D0
# define g 9.8D0
# define PI 3.1415926
program main
    implicit none
    integer :: N = 10**5, i
    real(8) :: angle_rad
    real(8) :: T = 2 * PI * sqrt(l/g), T_caculate(1:5)
    real(8) :: h, eps, pendulum_period
    real(8), dimension(1:5) :: theta_0 = (/3.D0, 30.D0, 60.D0, 90.D0, 120.D0/)
    do i = 1, 5 ! change the unit
        theta_0(i) = angle_rad(theta_0(i))
    end do
    h = T / N
    eps = theta_0(1) * 1.D-4

    write(*, '(2A13)') 'approximate', 'simulation'
    do i = 1, 5
        T_caculate(i) = pendulum_period(eps, h, theta_0(i), 0.D0)
    write(*, '(2F12.4)') T, T_caculate(i)
    end do
end program main

function f1(theta, theta_dot) result(output)
    implicit none
    real(8) :: theta, theta_dot
    real(8) :: output
    output = - g / l * sin(theta)
end function f1

function f2(theta, theta_dot) result(output)
    implicit none
    real(8) :: theta, theta_dot
    real(8) :: output
    output = theta_dot
end function f2

function pendulum_period(h, eps, theta_0, theta_dot_0) result(output)
    implicit none
    real(8) :: h, eps, theta_0, theta_dot_0
    real(8) :: output
    real(8) :: theta, theta_dot, time
    real(8), dimension(1:4) :: R_theta, R_theta_dot, flag = (/1.D0, 0.5D0, 0.5D0, 1.D0/)
    real(8) :: f1, f2 ! function using
    integer :: j

    theta = theta_0
    theta_dot = theta_dot_0
    time = 0.D0
    do while (abs(theta) > eps)
        R_theta_dot(1) = f1(theta, theta_dot) * h
        R_theta(1) = f2(theta, theta_dot) * h

        do j = 2, 4
            R_theta_dot(j) = f1(theta + flag(j) * R_theta(j-1), theta_dot + flag(j) * R_theta_dot(j-1)) * h
            R_theta(j) = f2(theta + flag(j) * R_theta(j-1), theta_dot + flag(j) * R_theta_dot(j-1)) * h
        end do

        theta_dot = theta_dot + (R_theta_dot(1) + 2D0*(R_theta_dot(2) + R_theta_dot(3)) + R_theta_dot(4)) / 6
        theta = theta + (R_theta(1) + 2D0*(R_theta(2) + R_theta(3)) + R_theta(4)) / 6

        time = time + h
    end do

    output = time * 4D0
end function pendulum_period

function angle_rad(angle) result(output)
    implicit none
    real(8) :: angle
    real(8) :: output
    output = angle / 180.D0 * PI
end function angle_rad