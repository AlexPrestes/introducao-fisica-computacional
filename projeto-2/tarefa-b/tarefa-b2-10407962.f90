program tarefab2

    ! númeor de passos N    
    parameter (N = 1000)
    ! vetores posição e probabilidade (indice com referencia ao unit dos arquivos de saida)
    dimension iposicao(-N:N), p(10:12)

    ! declarando arquivos de saida
    open(10, file='saida-b2-1-10407962')
    open(11, file='saida-b2-2-10407962')
    open(12, file='saida-b2-3-10407962')
    
    ! valores de cada propabilidade a ser testada
    p = (/1e0/3, 1e0/4, 1e0/5/)

    ! recebe o número de andarilhos M
    write(*,*) 'Digite o valor M de bebados:'
    read(*,*) M

    ! loop das probabilidades
    do k = 10, 12

        ! valores iniciais das variáveis
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
                if (rand().lt.p(k)) then
                    ! <= p, passo para direita
                    ix = ix + 1
                else
                    ! > p, passo para esquerda
                    ix = ix - 1
                end if
            end do

            ! soma os andarilhos na posição final
            iposicao(ix) = iposicao(ix) + 1

            ! soma para <x>
            soma = soma + ix
            ! soma2 para <x**2>
            soma2 = soma2 + ix**2
        end do

        ! loop para gerar o arquivo de saida
        do i = -N, N
            ! condicional para retirar as posições sem andarilhos
            if ( iposicao(i).ne.0 ) then
                write(k,'(I0," ",I0)') i, iposicao(i)
            end if
        end do
        
        ! escreve no terminal <x> e <x**2>
        write(*,'("<x>: ",1F0.3," <x**2>: ",1F0.3)') abs(soma)/M, abs(soma2)/M
    
    end do

end program tarefab2
