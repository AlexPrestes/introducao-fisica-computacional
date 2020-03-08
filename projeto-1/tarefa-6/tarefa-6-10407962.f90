program tarefa6

    ! Definindo pi 
    pi = 4*atan(1e0)
    
    ! Recebe o valor inteiro de N
    print *, 'Raízes da equação (Z - 2)**N = 3'
    print *, 'Digite o valor inteiro de N:'
    read (*,*) N
    
    ! loop k para solução geral z = |z|**(1/n) * ( cos( (theta+2*pi*k)/n ) + i*sin( (theta+2*pi*k)/n ) )
    ! forma polar do complexo z
    ! na equação theta = 0
    ! arg = 2*pi*k/n
    ! zmod = |z|
    do k = 1, N
        arg = 2e0*pi*k/N
        zmod = 3e0**(1e0/N)
        x = zmod*cos(arg) + 2e0
        yi = zmod*sin(arg)
        print '("Z"I0,": ",F8.5," ",SP,F8.5,"i")', k, cmplx(x, yi)
    end do
    
end program tarefa6
