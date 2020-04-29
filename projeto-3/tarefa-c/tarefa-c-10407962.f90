program tarefac
    implicit real*8 (a-h,o-z)
    ! variaveis auxiliaris para indicar parada de cada metodo
    logical flag_dir, flag_new, flag_sec
    
    erro = 10d-6   ! erro
    nmax = 100     ! iteração maxima
    h_sec = 0.005  ! valor de h para o metodo da secante
    h_dir = 3      ! valor de h para definir intervalo a b do metodo direto
    n = 0          ! valor da iteração

    ! variaveis de desvio
    desv_dir = 2*erro
    desv_new = 2*erro
    desv_sec = 2*erro

    ! flags
    flag_dir = .true.
    flag_new = .true.
    flag_sec = .true.

    ! recebe o valor para iniciar
    write(*,*) 'Digite um valor para iniciar a busca:'
    read(*,*) a

    ! define o valor de b do intervalo
    b = a + h_dir

    ! loop para encontrar o melhor intervalo
    do while ( f(a)*f(b) .gt. 0 )
        a = b
	b = b + h_dir
    end do

    ! escreve o valor de que irá iniciar todos os metodos
    write(*,'(A,1F0.11)') 'Valor inicial encontrado = ', a

    ! definindo valor inicial para newton e secante
    x0_new = a
    x0_sec = a

    ! loop que itera cada metodo, e para quando todas as flags indicam false ou atingiu o nmax
    do while ( (n <= nmax) .and. (flag_dir .or. flag_new .or. flag_sec) )
        
	! metodo direto
	! verifica se o desvio já atingiu o erro
	if ( desv_dir .gt. erro ) then
	    ! valor medio de ab
            x_dir = (a+b)/2
            ! verifica se o produto e redefine a ou b
            if ( f(x_dir)*f(a) .gt. 0 ) then
                a = x_dir
            else
                b = x_dir
            end if
            ! atualiza o valor do desvio
            desv_dir = abs(f(a)-f(b))
        else
            flag_dir = .false.
        end if

	! metodo de newton
	! verifica se o desvio já atingiu o erro
        if ( desv_new .gt. erro ) then
            x_new = x0_new - f(x0_new)/df(x0_new)
	    ! atualiza o valor do desvio
            desv_new = abs(f(x_new) - f(x0_new))
	    ! redefine x0
            x0_new = x_new
        else
            flag_new = .false.
        end if
        

	! metodo da secante
        ! verifica se o desvio já atingiu o erro
        if ( desv_sec .gt. erro ) then
	    ! derivada numerica de f
            df_sec = ( f(x0_sec + h_sec) - f(x0_sec) ) / h_sec
            x_sec = x0_sec - f(x0_sec)/df_sec
	    ! atualiza o desvio
            desv_sec = abs(f(x_sec) - f(x0_sec))
	    ! redefine x0
            x0_sec = x_sec
        else
            flag_sec = .false.
        end if

	! escreve na tela o numero da iteração e o ultimo x encontrado
        write(*,*) n, x_dir, x_new, x_sec
	! soma 1 em n
        n = n + 1
    end do

contains ! funções utilizadas f(x) e a derivada de f(x)

    function f(x)
        f = x**3 -21*x -20
        return
    end function

    function df(x)
        df = 3*x**2 -21
        return
    end function

end program tarefac
