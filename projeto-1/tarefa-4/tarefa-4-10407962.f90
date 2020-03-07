program tarefa4
    
    open(unit=11, file='saida-4-10407962')
    
    print *, 'Digite um inteiro N:'
    read (*,*) N
    
    do i = 2, N
        k = i
        do j = 2, i/2
            if (mod(i, j) == 0) then
                k = 0
            end if
        end do
        if (k /= 0) then
            write (11, *) i
        end if
    end do
    
    close(11)

end program tarefa4
