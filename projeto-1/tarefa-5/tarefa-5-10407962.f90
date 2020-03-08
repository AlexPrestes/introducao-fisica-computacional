program tarefa5
   
    ! Definindo a variáveis com precisão dupla 
    real*8 :: dlnx, dx, derro, dlogx
    
    ! definindo precisão e erro inicial 
    eprec = 10e-5
    erro = 2*eprec
    slnx = 0e0
    n = 0
    
    ! Recebe o valor de x
    print *, 'Digite um x Real, 0 < x < 2:'
    read (*,*) x

    ! verifica se está dentro do raio de convergencia
    if ((x <= 0) .or. (2e0 <= x)) then
        print *, 'x está fora do raio de convergencia'
        ! sai do programa
        stop
    end if

    slogx = log(x)

    ! calcula a aproximação até atingir o erro eprec
    ! ou até que não haja mais variação no erro    
    do while ((erro >= eprec) .and. (erro /= abs(slogx - slnx)))
        erro = abs(slogx - slnx)
        n = n + 1
        slnx = slnx -(1e0 - x)**n/n
    end do
    
    print *, 'Precisão Simples'
    print *, 'Num iter: ', n
    print *, 'log(x):   ', slogx
    print *, 'Aprox:    ', slnx
    print *, 'erro:    ', erro
    
    ! valores iniciais para a precisão dupla
    dx = x
    derro = 0d0
    dlnx = 0d0
    n = 0
    
    dlogx = dlog(dx)
    
    ! calcula a aproximação até que não haja mais variação do erro
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
