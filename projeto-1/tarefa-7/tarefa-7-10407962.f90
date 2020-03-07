program prog7

    implicit none
    
    integer*8 :: i, j
    integer*8 :: d, M, N
    real*8 :: rmod, R, Vd, dgamma, arg
    real*8, parameter :: pi = 4d0*atan(1d0)
    real*8, dimension(:), allocatable :: p
    
    print *, 'Digite o número de dimensões d:'
    read (*,*) d
    
    print *, ''
    print *, 'Digite o número de iterações M:'
    read (*,*) M
    
    allocate(p(d))
    
    N = 0
    R = 1d0
    
    print *, 'iniciando...'
    
    do i = 1, M
        do j = 1, d
            p(j) = rand()
        end do
        
        rmod = 0
        
        do j = 1, d
            rmod = rmod + p(j)**2
        end do
        
        if (rmod <= 1d0) then
            N = N + 1
        end if
    end do
    print *, 'Finalizado...'
    print *, (dfloat(N)/dfloat(M))*((2d0*R)**d)
    
    arg = (d/2d0 + 1d0)
    dgamma = 1d0
    
    arg = arg - 1
    
    !print *, sqrt(pi)
    
    do while (arg > 0d0)
        if (arg >= 1d0) then
            dgamma = arg*dgamma
            arg = arg - 1d0
        else
            dgamma = arg*sqrt(pi)*dgamma
            arg = 0
        end if
    end do
    
    Vd = (pi**(d/2) * R**d) / dgamma
    
    print *, Vdd
    
end program prog7
