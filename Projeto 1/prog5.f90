program prog5

implicit none

integer :: n
real*8 :: lnx, x, erro
real*8, parameter :: eprec = 10e-5

erro = 2*eprec
lnx = 0
n = 1

print *, 'Digite um x:'
read (*,*) x

do while (erro >= eprec)
    lnx = lnx + (-(1 - x)**n)/n
    erro = abs(log(x)-lnx)
    print *, log(x), lnx, erro, eprec
    n = n + 1
end do

end program prog5
