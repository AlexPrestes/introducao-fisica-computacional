program prog4

implicit none

integer :: N, i, j, resto
real, dimension(:), allocatable :: primos

open(unit=11, file='prog4.out')

print *, 'Digite um inteiro N:'
read (*,*) N

allocate(primos(N-1))

do i = 2, N
    primos(i-1) = i
end do

do i = 1, N-1
    do j = i+1, N-1
        if (mod(primos(j), primos(i)) == 0) then
            primos(j) = 0
        end if
    end do
    if (primos(i) /= 0) then
        write (11,*) int(primos(i))
    end if
end do

end program prog4
