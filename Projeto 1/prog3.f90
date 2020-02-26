program prog3

implicit none

integer :: N, M, i, j
real :: aux
real, dimension(:), allocatable :: NumList

open (unit=10, file='prog3.listnum')
open (unit=11, file='prog3.out')

read (10,*) N
allocate (NumList(N))
read (10,*) NumList
close(10)
print *, 'Digite o valor de M (Inteiro):'
read (*,*) M

do i = N, 1, -1
    do j = N, 2, -1
        if (NumList(j) < NumList(j-1)) then
            aux = NumList(j)
            NumList(j) = NumList(j-1)
            NumList(j-1) = aux
        end if
    end do
end do

do i = 1, M
    write (11,*) NumList(i)
end do
close (11)

end program prog3
