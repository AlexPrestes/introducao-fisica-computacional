program prog5

    implicit none

    integer :: n
    real*8 :: lnx, x, erro, logx
    real*8, parameter :: eprec = 10e-5

    erro = 2*eprec
    lnx = 0
    n = 0

    print *, 'Digite um x Real entre 0 e 2:'
    read (*,*) x

    logx = log(x)

    do while (erro >= eprec)
        n = n + 1
        lnx = lnx -(1 - x)**n/n
        erro = abs(logx - lnx)
        print *, logx, lnx, erro, eprec, n
    end do

end program prog5
