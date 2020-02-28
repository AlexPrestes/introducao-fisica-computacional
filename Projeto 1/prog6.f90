program prog6

    implicit none
    
    integer :: N, k
    real*8, parameter :: pi = 4*atan(1d0)
    real*8 :: x, yi, arg, zmod
    complex, dimension(:), allocatable :: zroots
    
    print *, 'Digite o valor inteiro de N:'
    read (*,*) N
    
    allocate(zroots(N))
    
    do k = 1, N
        arg = 2d0*pi*k/N
        zmod = 3d0**(1d0/N)
        x = zmod*cos(arg) + 2d0
        yi = zmod*sin(arg)
        zroots(k) = cmplx(x, yi)
        print *, k, zroots(k)
    end do
    
end program prog6
