program tarefa4
    
    open(unit=11, file='saida-4-10407962')
    
    ! Recebe um inteiro N
    print *, 'Digite um inteiro N:'
    read (*,*) N
    
    ! Loop i indo de 2 até N
    do i = 2, N
        ! Variável k auxiliar, recebe o valor de i
        k = i
        ! Loop j procura por 2 até i/2
        do j = 2, i/2
            ! Verifica se i/j é divisivel
            if (mod(i, j) == 0) then
                ! caso seja divisivel k = 0,
                ! para que não seja escrito no arquivo de saida
                k = 0
            end if
        end do
        ! condição para escrever no arquivo de saida
        if (k /= 0) then
            write (11, '(I0)') i
        end if
    end do
    
    close(11)

end program tarefa4
