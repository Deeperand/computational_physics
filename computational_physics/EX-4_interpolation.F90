program main
    implicit none
    real(8), dimension(1:10) :: Px, Py
    real(8), dimension(1:5) :: test
    integer ii
    real(8) tmp1, tmp2
    Px = (/4.D0, 9.D0, 16.D0, 25.D0, 36.D0, 49.D0, 64.D0, 81.D0,100.D0, 121.D0/)
    Py = (/2.D0, 3.D0, 4.D0, 5.D0, 6.D0, 7.D0, 8.D0, 9.D0, 10.D0, 11.D0/)
    test = (/5.D0, 13.D0, 55.D0, 72.D0, 103.D0/)
    write(*,'(5A16)') '测试数据','插值结果', '精确结果', '绝对误差', '相对误差'
    do ii = 1, size(test)
        tmp1 = LIntp(Px, Py, test(ii))
        tmp2 = sqrt(test(ii))
        write(*,'(5F12.4)') test(ii) ,tmp1, tmp2, tmp2 - tmp1, (tmp2 - tmp1)/tmp2
    end do

    contains
    function LIntp(points_x, points_y, x)
        integer :: p_num, label_st, i, j, k
        real(8) :: LIntp, x, pcoff
        real(8), dimension(:) :: points_x, points_y

        p_num = size(points_x)
        if (x <= points_x(2)) then
            label_st = 1
        elseif (x > points_x(p_num - 1)) then
            label_st = p_num - 2
        else
            do i = 2, p_num - 2
                if (points_x(i) < x .and. x <= points_x(i + 1)) then
                    if ( x < (points_x(i) + points_x(i + 1))/2 ) then
                        label_st = i - 1
                    else
                        label_st = i
                    endif
                    exit
                endif
            enddo
        endif

        LIntp = 0.D0
        do j = label_st, label_st + 2
            pcoff = 1.0D0
            do k = label_st, label_st + 2
                if (j == k) cycle
                pcoff = pcoff * (x - points_x(k))/(points_x(j) - points_x(k))
            end do
            LIntp = LIntp + pcoff*points_y(j)
        end do
    end function LIntp
end program main
