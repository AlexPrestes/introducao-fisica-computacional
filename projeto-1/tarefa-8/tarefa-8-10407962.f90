program tarefa8

    ! Definindo o valor de pi
    pi = 4e0*atan(1e0)
    
    ! Definindo unit para o arquivo de saida
    open(unit=10, file='saida-8-10407962')

    ! Recebe o valor do raio
    print *, 'Digite o raio R:'
    read (*,*) R

    ! Recebe o valor da dimensão
    print *, 'Digite o número de dimensões d:'
    read (*,*) id
    
    ! loop que para mudar a dimensão de 2 até d
    do i = 2, id
        
        ! definindo arg e dgamma inicial
        ! aqui está sem as redundancia da tarefa-7
        arg = i/2e0
        dgamma = 1e0

        ! loop que calcula o dgamma
        do while (arg > 0e0)
            if (arg >= 1e0) then
                dgamma = arg*dgamma
                arg = arg-1e0
            else
                dgamma = arg*sqrt(pi)*dgamma
                arg = 0e0
            end if
        end do
    
        ! calculo do volume d
        V = (pi**(i/2e0) * R**i) / dgamma

        ! escreve a dimensão e o volume no arquivo de saida
        write (10,'(I0,", ",F0.6)') i, V
    end do

    ! fecha o arquivo de saida
    close (10)
    
end program tarefa8
