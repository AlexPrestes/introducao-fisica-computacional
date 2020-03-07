program tarefa5
    
    real*8 :: dlnx, dx, derro, dlogx
    
    eprec = 10e-5
    erro = 2*eprec
    slnx = 0e0
    n = 0
    
    print *, 'Digite um x Real entre 0 e 2:'
    read (*,*) x
    slogx = log(x)
    
    do while (erro >= eprec)
        n = n + 1
        slnx = slnx -(1e0 - x)**n/n
        erro = abs(slogx - slnx)
    end do
    
    print *, 'Precisão Simples'
    print *, 'Num iter: ', n
    print *, 'log(x):   ', slogx
    print *, 'Aprox:    ', slnx
    print *, 'erro:    ', erro
    
    
    dx = x
    derro = 0d0
    dlnx = 0d0
    n = 0
    
    dlogx = dlog(dx)
    
    do while (derro /= abs(dlogx - dlnx))
        derro = abs(dlogx - dlnx)
        n = n + 1
        dlnx = dlnx -(1 - dx)**n/n
    end do
    
    print *, ''
    print *, ''
    print *, 'Precisão Dupla'
    print *, 'Num iter: ', n-1
    print *, 'dlog(x):   ', dlogx
    print *, 'Aprox:    ', dlnx
    print *, 'erro:    ', derro
    
end program tarefa5
