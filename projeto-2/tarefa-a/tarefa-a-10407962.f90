program tarefaa
    print *, "Digite o valor inteiro de N:"
    read (*,*) N
   
    ! Número de "Andarilhos" M 
    M = 1000
    
    ! loop do passo
    do i = 1, N
        ! zera a soma
        soma = 0e0 
        
        ! loop do andarilho
        do j = 1, M
            ! soma os passos de cada andarilho
            soma = soma + rand()**i
        end do
        
        ! Escreve a media no terminal
        write (*,'(A,I0,A,1F5.3)') "<x^",i,">: ", soma/M
    end do

end program tarefaa
