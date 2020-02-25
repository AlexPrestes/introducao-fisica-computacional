program prog1

implicit none

integer :: n
real*8 :: a, b, c, delta, x1, x2

print *,'Digite a, b e c, respectivamente:'
read(*,*) a, b, c

delta = (b**2) - (4*a*c)

if (delta == 0) then
    print *, 1, (-b + sqrt(delta))/(2*a)
else if (delta > 0) then
    print *, 2, (-b + sqrt(delta))/(2*a), (-b - sqrt(delta))/(2*a)
else
    print *, 0
end if

end program prog1
