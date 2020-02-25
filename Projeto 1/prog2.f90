program prog2

implicit none

real*8, dimension(3) :: v1, v2
real*8 :: area

print *, 'Digite v1: x1 y1 z1'
read(*,*) v1
print *, 'Digite v2: x2 y2 z2'
read(*,*) v2

area = (v1(2)*v2(3) - v2(2)*v1(3))**2 + (v2(1)*v1(3) - v1(1)*v1(3))**2 + (v1(1)*v2(2) - v2(1)*v1(2))**2
area = sqrt(area)/2.d0

print *, area

end program prog2
