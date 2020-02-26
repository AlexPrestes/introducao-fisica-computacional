program prog5

    implicit none
    
    integer :: n
    real :: lnx, x, erro, logx
    real, parameter :: eprec = 1e-5
    
    real*8 :: dlnx, dx, derro, dlogx, deprec
    
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
    end do
    
    print *, 'Precisão Simples'
    print *, 'Num iter: ', n
    print *, 'log(x):   ', logx
    print *, 'Aprox:    ', lnx
    print *, 'erro:    ', erro
    
    
    dx = x
    deprec = epsilon(dx)
    derro = 2*deprec
    dlnx = 0
    n = 0
    
    dlogx = dlog(dx)
    
    do while (derro > deprec)
        n = n + 1
        dlnx = dlnx -(1 - dx)**n/n
        derro = abs(dlogx - dlnx)
    end do
    
    print *, ''
    print *, ''
    print *, 'Precisão Dupla'
    print *, 'Num iter: ', n
    print *, 'dlog(x):   ', dlogx
    print *, 'Aprox:    ', dlnx
    print *, 'erro:    ', derro
    
end program prog5
