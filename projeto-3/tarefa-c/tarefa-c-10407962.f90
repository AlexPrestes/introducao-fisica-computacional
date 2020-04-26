program tarefac
    implicit real*8 (a-h,o-z)
    logical flag_dir, flag_new, flag_sec
    
    erro = 10d-6
    nmax = 100
    h = 0.005
    n = 0

    desv_dir = 2*erro
    desv_new = 2*erro
    desv_sec = 2*erro

    flag_dir = .true.
    flag_new = .true.
    flag_sec = .true.

    write(*,*) 
    read(*,*) a, b, x0_new, x0_sec

    do while ( (n <= nmax) .and. (flag_dir .or. flag_new .or. flag_sec) )
        if ( desv_dir**2 .gt. erro**2 ) then
            x_dir = (a+b)/2
            if ( f(x_dir)*f(a) .gt. 0 ) then
                a = x_dir
            else
                b = x_dir
            end if
            desv_dir = f(a)-f(b)
        else
            flag_dir = .false.
        end if

        if ( desv_new**2 .gt. erro**2 ) then
            x_new = x0_new - f(x0_new)/df(x0_new)
            desv_new = f(x_new) - f(x0_new)
            x0_new = x_new
        else
            flag_new = .false.
        end if
            
        if ( desv_sec**2 .gt. erro**2 ) then
            df_sec = ( f(x0_sec + h) - f(x0_sec) ) / h
            x_sec = x0_sec - f(x0_sec)/df_sec
            desv_sec = f(x_sec) - f(x0_sec)
            x0_sec = x_sec
        else
            flag_sec = .false.
        end if

        print *, n, x_dir, x_new, x_sec
        n = n + 1
    end do

contains

    function f(x)
        f = x**3 -21*x -20
        return
    end function

    function df(x)
        df = 3*x**2 -21
        return
    end function

end program tarefac
