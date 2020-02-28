program prog7

    implicit none
    
    integer*8 :: i, j
    integer*8 :: d, M, N
    real*8 :: rmod, R
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
    
end program prog7
