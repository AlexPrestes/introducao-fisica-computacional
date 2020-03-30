program tarefac

    character(22) filename ! variavel que irá receber o nome do arquivo de saida
    parameter (Mmax = 10000000, pi = 4d0*atan(1d0)) ! constantes Mmax='número maixmo de andarilhos' e pi='valor do pi'
    complex, dimension(Mmax) :: zAndPos ! Vetor complexo da posicao de cada andarilho
    complex :: zPasso ! variavel que irá receber o passo do andarilho

    write(*,*) 'Digite o número de passos (Nmax=10e6):'
    read(*,*) N

    if (N.ge.10e6) then ! condicional para encerrar o programa
        write(*,*) 'Valor incorreto'
        stop
    end if

    write(*,*) 'Digite o número de andarilhos:'
    read(*,*) M ! recebe o número de andarilhos

    zAndPos = 0 ! zera o vetor de passos

    do j = 1, N ! loop do número de passos

        rmedio = 0
        delta2 = 0
        do i = 1, M ! loop do número de andarilhos

            nrand = 4*rand()                                    ! gera números inteiros aleatórios de 0 até 3
            arg = 0.5*pi*nrand                                  ! gera o argumento para fazer uma rotação no plano complexo
            zPasso = cmplx(int(cos(arg)), int(sin(arg)))        ! cálcula o passo
            zAndPos(i) = zAndPos(i) + zPasso                    ! soma o passo na posição do andarilho

        end do

        
        ! Esse trecho do código é onde serão gerados os arquivos de saida para N = 10e0, 10e1, 10e2, 10e3, 10e4, 10e5 até 10e6

        exp10 = dlog10(dfloat(j)) ! expoente

        if (mod(exp10, 1.).eq.0) then ! verifica se o valor é inteiro
            write(filename, '(A11,I0,A9)') 'saida-c-10e', int(exp10), '-10407962' ! gera o nome do arquivo de saida
            open(10, file=filename) ! abre o arquivo
            
            do k = 1, M ! loop para gravar a coordenado dos andarilhos por linha e <r> delta**2
                r = abs(zAndPos(k))
                rmedio = rmedio + r
                delta2 = delta2 + r**2

                write(10,'(F0.1,", ",F0.1)') zAndPos(k)
            end do

            rmedio = rmedio/M
            delta2 = delta2/M - rmedio**2

            write(*,'(A6,I0,A6,F0.3,A11,F0.3)') 'N: 10e', int(exp10), ' <r>: ', rmedio, ' delta**2: ', delta2
            
            close(10) ! fecha o arquivo
        end if

    end do

end program tarefac
