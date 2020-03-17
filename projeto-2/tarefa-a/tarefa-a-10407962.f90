program tarefaa
    print *, "Digite o valor inteiro de N:"
    read (*,*) N
    
    M = 10000
    
    do i = 1, N
        soma = 0e0
        
        do j = 1, M
            soma = soma + rand()**i
        end do
        
        write (*,'(A,I0,A,1F5.3)') "<x^",i,">: ", soma/M
    end do

end program tarefaa
