! Experiment_2 File and array
program main
    implicit none
    real :: stu_score(10), stu_ave=0, stu_std=0, tmp1
    character(len=8) :: stu_name(10), tmp2
    integer :: i, j, stu_num=10

    open(unit = 20, file = 'EX_2_data.txt')
    open(unit = 21, file = 'EX_2_result.txt')

    do i = 1, stu_num
        read(20, *) stu_name(i), stu_score(i)
    end do
    ! stu_name = (/'张一', '张二', '张三', '张四', '张五', '张六', '张七', '张八', '张九', '张十'/)
    ! stu_score = (/67.00, 43.00, 57.00, 43.00, 98.00, 74.00, 89.00, 85.00, 95.00, 88.00/)

    ! average
    do i = 1, stu_num
        stu_ave = stu_score(i) + stu_ave
    end do
    stu_ave = stu_ave / stu_num

    ! std
    do i = 1, stu_num
        stu_std = (stu_score(i) - stu_ave)**2 + stu_std
    end do
    stu_std = sqrt(stu_std / stu_num)

    ! sort
    do i = 1, stu_num
        do j = i, stu_num
            if (stu_score(j) > stu_score(i)) then
                ! exchange score
                tmp1 = stu_score(i)
                stu_score(i) = stu_score(j)
                stu_score(j) = tmp1
                ! exchange name
                tmp2 = stu_name(i)
                stu_name(i) = stu_name(j)
                stu_name(j) = tmp2
            end if
        end do
    end do

    do i = 1, stu_num
        write(21,*) stu_name(i), int(stu_score(i))
        write(*,*) stu_name(i), int(stu_score(i))
    end do
    write(*, '(A6, F8.2)') 'ave =', stu_ave
    write(*, '(A6, F8.2)') 'std =', stu_std
    write(21, *) 'ave =', stu_ave
    write(21, *) 'std =', stu_std

    close(20)
    close(21)
end program main