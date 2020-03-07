program tarefa3
    
    !definindo um n maximo para o arquivo de entrada
    parameter (nmax=10000)
    !vetor que irá receber os valores do arquivo de entrada
    dimension ANumList(nmax)
    
    !definindo os arquivos
    open(unit=10, file='entrada-3-10407962')
    open(unit=11, file='saida-3-10407962')
    
    !loop para atribuir valores do arquivo para o vetor
    do n = 1, nmax
        read(10, *, end=1) ANumList(n)
    end do
1   close(10)
    !como o n final é +1 que o número de valores é necessário corrigir
    n = n-1

    !recebe o número inteiro M
    print *, 'Digite o valor de M (Inteiro):'
    read (*,*) M
    
    !Algoritmo de ordenação, verifica termo a termo
    do i = n, 1, -1
        do j = n, 2, -1
            if (ANumList(j) < ANumList(j-1)) then
                aux = ANumList(j)
                ANumList(j) = ANumList(j-1)
                ANumList(j-1) = aux
            end if
        end do
    end do
    
    !gera o arquivo de saida com os M primeiros números
    do i = 1, M
        write (11,*) ANumList(i)
    end do
    close (11)

end program tarefa3
