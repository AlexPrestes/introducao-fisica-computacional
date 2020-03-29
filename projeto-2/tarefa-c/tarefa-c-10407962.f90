program tarefac

    character*22 :: filename
    parameter (Mmax=10000000)
    complex, dimension(Mmax) :: zAndPos ! Vetor complexo da posicao de cada andarilho

    write(*,*) 'Digite o número de passos (Nmax=10e6):'
    read(*,*) N

    if (N.ge.10e6) then
        write(*,*) 'Valor incorreto'
        stop
    end if

    write(*,*) 'Digite o número de andarilhos:'
    read(*,*) M

    zAndPos = 0

    do j = 1, N
        do i = 1, M

            nrand = 4*rand()
            
            if (nrand.eq.0) then ! Norte
                zAndPos(i) = zAndPos(i) + ( 0,  1)
            else if (nrand.eq.1) then ! Sul
                zAndPos(i) = zAndPos(i) + ( 0, -1)
            else if (nrand.eq.2) then ! Leste
                zAndPos(i) = zAndPos(i) + ( 1,  0)
            else ! Oeste
                zAndPos(i) = zAndPos(i) + (-1,  0)
            end if

        end do

        exp10 = dlog10(dfloat(j))

        if (mod(exp10, 1.).eq.0) then
            write(filename, '(A11,I0,A9)') 'saida-c-10e', int(exp10), '-10407962'
            open(10, file=filename)
            
            do k = 1, M
                write(10,'(F0.1,", ",F0.1)') zAndPos(k)
            end do
            
            close(10)
        end if

    end do

end program tarefac
