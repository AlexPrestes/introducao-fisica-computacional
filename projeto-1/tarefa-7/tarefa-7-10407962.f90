program tarefa7
    ! Definindo valor de pi
    pi = 4e0*atan(1e0)
    
    ! Variável inteira id que recebe o número da dimensão
    print *, 'Digite o número de dimensões d:'
    read (*,*) id
    
    ! Número M aleátorios 
    print *, ''
    print *, 'Digite o número de iterações M:'
    read (*,*) M
    
    ! Contador N de pontos dentro da d-esfera
    N = 0
    
    ! loop de ;m iterações
    do i = 1, M
        ! Variável que irá armazenar o modulo ao quadrado de cada ponto
        rmod = 0
        ! Coordenadas aleátorias do ponto
        do j = 1, id
            rmod = rmod + rand()**2
        end do
        ! Soma +1 em N, se o ponto estiver dentro da d-esfera
        if (rmod <= 1e0) then
            N = N + 1
        end if
    end do
    
    ! Volume obtido pelo metodo de monte carlo
    Vmc = (float(N)/float(M)) * 2e0**id
    
    ! arg = d/2 + 1
    ! dgamma(arg) = (arg-1)*dgamma(arg-1)
    arg = (id/2e0 + 1e0)
    dgamma = 1e0
    
    arg = arg - 1e0
    ! O Cálculo da função gamma é reproativo,
    ! então precisa verificar se a última iteração será gamma(1) ou gamma(1/2)
    do while (arg > 0e0)
        if (arg >= 1e0) then
            dgamma = arg*dgamma
            arg = arg - 1e0
        else
            dgamma = arg*sqrt(pi)*dgamma
            arg = 0e0
        end if
    end do

    ! Cálculo do volume de d-esfera de raio 1
    Vd = pi**(id/2e0) / dgamma
    
    ! Imprime os volumes da tela
    print '(A,F0.6)', 'Volume por Monte Carlo: ', Vmc
    print '(A,F0.6)', 'Volume por n-esferas:   ', Vd
    
end program tarefa7
