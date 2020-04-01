program tarefac
    
    ! variavel que para personalizar o nome dos arquivos de saidas
    character(22) filename
    ! constantes
    parameter (Mmax = 10000000, pi = 4d0*atan(1d0))
    ! vetor AndPos, matrix para receber a coordenada de cada andarilho
    ! vetor I_passo, vetor de direção do passo
    dimension AndPos(Mmax,2), I_passo(2)

    ! recebe o número de passos N
    write(*,*) 'Digite o número de passos N:'
    read(*,*) N

    ! recebe o número de andarilhos M
    write(*,*) 'Digite o número de andarilhos M:'
    read(*,*) M

    ! valor inicial da variavel
    AndPos = 0

    ! loop dos passo
    do j = 1, N

        ! loop dos andarilhos
        do i = 1, M

            ! nrand, parte inteira do rand() dilatado
            nrand = 4*rand()
            ! arg, argumento que fará a rotação do passo no plano (x,y)
            arg = nrand*pi/2
            ! I_passo, recebe o valor do passo como vetor
            I_passo = (/cos(arg), sin(arg)/)
            ! soma o passo para cada andarilho
            AndPos(i,:) = AndPos(i,:) + I_passo

        end do
        
        ! trecho do código para gerar o arquivo de saida para N = 10**j, se j for inteiro
        exp10 = dlog10(dfloat(j))
        i_exp10 = exp10

        ! condiciona para verificar a hipotese acima
        if (mod(exp10, 1.).eq.0) then
            
            ! filename é alterada para o expoente correspondente
            write(filename, '(A11,I0,A9)') 'saida-c-10e', i_exp10, '-10407962'
            open(10, file=filename)
            
            ! loop para gerar o arquivo de saida
            do k = 1, M
                write(10,'(I0,", ",I0)') int(AndPos(k,:))
            end do

            ! r2, soma do r**2
            r2 = (sum(AndPos(:,1)**2) + sum(AndPos(:,2)**2))
            
            ! r = raiz de r2
            r = sqrt(r2)
            
            ! calculando delta**2
            delta2 = r2/M - (r/M)**2

            ! escreve no terminal o valor de N, <r> e delta**2
            write(*,'("N: 10e",I0,", <r>: ",1F0.3,", delta**2: ",1F0.3)') i_exp10, abs(r/M), abs(delta2)

            close(10)
        end if

    end do

end program tarefac
