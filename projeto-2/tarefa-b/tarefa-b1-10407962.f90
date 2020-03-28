program tarefab1
    
    parameter (N = 1000)
    dimension iposicao(-N:N)

    open(10, file='saida-b1-10407962')

    print *, "valor p:"
    read(*,*) p

    print *, "número de bebados M:"
    read(*,*) M

    q = 1 - p
    iposicao = 0
    iandarilho = 0
    soma = 0
    soma2 = 0

    do j = 1, M !quantidade de bebados
        ix = 0
        do i = 1, N !quatidade de passos
            if (rand().lt.p) then !direcao do passo
                ix = ix + 1
            else
                ix = ix - 1
            end if
        end do
        iposicao(ix) = iposicao(ix) + 1

        soma = soma + ix
        soma2 = soma2 + ix**2
    end do

    do i = -N, N
        if ( iposicao(i).ne.0 ) then
            write(10,'(I0," ",I0)') i, iposicao(i)
        end if
    end do

    write(*,'("<x>: ",1F5.3," <x**2>: ",1F0.3)') abs(soma)/M, abs(soma2)/M

 end program tarefab1
