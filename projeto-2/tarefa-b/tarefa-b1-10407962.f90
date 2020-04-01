program tarefab1
    
    ! Número de passos N
    parameter (N = 1000)
    ! vetor posição
    dimension iposicao(-N:N)

    ! definindo arquivo de saida
    open(10, file='saida-b1-10407962')

    ! recebe a probabilidade p de ir para a direita
    write(*,*) "valor p:"
    read(*,*) p

    ! recebe o número de andarilhos M
    print *, "número de andarilhos M:"
    read(*,*) M

    ! defini inicialmente como zero as variáveis
    iposicao = 0
    iandarilho = 0
    soma = 0
    soma2 = 0

    ! loop dos andarilhos
    do j = 1, M
        ! zera a posição do andarilho
        ix = 0

        ! loop dos passos
        do i = 1, N

            ! condicional para direção do passo
            if (rand().lt.p) then
                ! <= p, vai para direita
                ix = ix + 1
            else
                ! > p, vai para esquerda
                ix = ix - 1
            end if
        end do
        
        ! soma 1 andarilhos na posição que parou
        iposicao(ix) = iposicao(ix) + 1

        ! soma para <x>
        soma = soma + ix
        ! soma2 para <x**2>
        soma2 = soma2 + ix**2
    end do

    ! loop para salvar as posições no arquivo de saida
    do i = -N, N

        ! condicional para o caso de não tiver andarilho na posição
        ! não salva no arquivo
        if ( iposicao(i).ne.0 ) then
            write(10,'(I0," ",I0)') i, iposicao(i)
        end if
    end do

    ! escreve no terminal o número de passos, <x> e <x**2>
    write(*,'("N: ",I0,"  <x>: ",1F0.3," <x**2>: ",1F0.3)') N, abs(soma)/M, abs(soma2)/M

    ! fecha o arquivo de saida
    close(10)

 end program tarefab1
