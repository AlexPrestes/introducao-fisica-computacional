program tarefab2
    
    parameter (N = 1000)
    dimension iposicao(-N:N)
    dimension p(10:12)

    open(10, file='saida-b2-1-10407962')
    open(11, file='saida-b2-2-10407962')
    open(12, file='saida-b2-3-10407962')
    
    p = (/1e0/3, 1e0/4, 1e0/5/)
    print *, p

    write(*,*) 'Digite o valor M de bebados:'
    read(*,*) M

    do k = 10, 12

        iposicao = 0
        iandarilho = 0
        soma = 0
        soma2 = 0

        do j = 1, M !quantidade de bebados
            ix = 0
            do i = 1, N !quatidade de passos
                if (rand().lt.p(k)) then !direcao do passo
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
                write(k,'(I0," ",I0)') i, iposicao(i)
            end if
        end do

        write(*,'("<x>: ",1F5.3," <x**2>: ",1F0.3)') abs(soma)/M, abs(soma2)/M
    
    end do

 end program tarefab2
