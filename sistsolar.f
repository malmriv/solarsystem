      program sistsolar
        implicit none
        real*8, dimension (:,:), allocatable :: x,y,vx,vy,ax,ay,KE,UE
        real*8, dimension (:), allocatable :: m, e
        real*8 h, t, tmax, angmomentum
        real*8 calc_x, calc_v, calc_a, aux, total_KE, total_UE
        integer i, j, k, n_bodies, N

c       Length of each step (in seconds) = max time to compute / frames
        n_bodies = 10
        N = 100000
        tmax = 6.2865 !An Earth year
        t = 0.d0
        h = tmax/float(N)

c       Allocate memory for each vector
        allocate(x(1:n_bodies,1:N),y(1:n_bodies,1:N))
        allocate(vx(1:n_bodies,1:N),vy(1:n_bodies,1:N))
        allocate(ax(1:n_bodies,1:N),ay(1:n_bodies,1:N))
        allocate(KE(1:n_bodies,1:N),UE(1:n_bodies,1:N))
        allocate(m(1:n_bodies),e(1:n_bodies))

c       Initialize the vectors.
        t = 0.d0
        KE = 0.d0
        UE = 0.d0
        m = 0.d0
        x = 0.d0
        y = 0.d0
        vx = 0.d0
        vy = 0.d0
        ax = 0.d0
        ay = 0.d0

c       Read the normalised data. The normalisation is performed by an R script.
        open(100,file="data_norm.txt",status='old')
        do i=1,n_bodies
            read(100,*) m(i), x(i,1), e(i)
        end do
        close(100)

c       Compute the orbital velocity (see notebook).
        do i=2,n_bodies
          vy(i,1) = dsqrt(x(4,1)/x(i,1)*(1.0-e(i)))
        end do

c       We open different files for different magnitudes. This may seem
c       inefficient for a small number of bodies, but a simulation with
c       hundreds of bodies, each one evaluated thousands of times,
c       would quickly yield a very unmanageable .txt file if we were to
c       store everything in one go.
        call system("mkdir ./results")
        open(10,file="./results/x.txt")
        open(20,file="./results/y.txt")
        open(30,file="./results/vx.txt")
        open(40,file="./results/vy.txt")
        open(50,file="./results/ax.txt")
        open(60,file="./results/ay.txt")
        open(70,file="./results/t.txt")
        open(80,file="./results/energy.txt")
        open(90,file="./results/angmomentum.txt")


c       Evaluate accelerations to set initial conditions
        i = 1
        do j=1,n_bodies !j = body whose motion is being computed
          do k=1,n_bodies !k = its interactions with the k-th body
            if(j .ne. k) then !Avoid self-interactions
            ax(j,i) = ax(j,i) + calc_a(m(k),x(j,i),x(k,i),y(j,i),y(k,i))
            ay(j,i) = ay(j,i) + calc_a(m(k),y(j,i),y(k,i),x(j,i),x(k,i))
            end if
          end do
        end do

c       Save the initial time
        write(70,*) t

c       Iterative process computing motion magnitudes for each step
        do i=2,N !Step number. Indexes mean the same as before.

          do j=1,n_bodies
c           Compute positions
            x(j,i) = calc_x(h,x(j,i-1),vx(j,i-1),ax(j,i-1))
            y(j,i) = calc_x(h,y(j,i-1),vy(j,i-1),ay(j,i-1))
          end do

          do j=1,n_bodies
c           Compute accelerations and potential energy (same type of sum)
            do k=1,n_bodies
            if(j .ne. k) then !Avoid self-interactions
            ax(j,i) = ax(j,i) + calc_a(m(k),x(j,i),x(k,i),y(j,i),y(k,i))
            ay(j,i) = ay(j,i) + calc_a(m(k),y(j,i),y(k,i),x(j,i),x(k,i))
            end if
            end do
          end do

          do j=1,n_bodies
c           Compute the potential energy
            do k=j+1,n_bodies
              aux = dsqrt((x(j,i)-x(k,i))**2.0+(y(j,i)-y(k,i))**2.0)
              UE(j,i) = UE(j,i) - (m(j)*m(k))/aux
            end do
          end do

          angmomentum = 0.d0
          do j=1,n_bodies
c           Compute velocities, kinetic energy and angular momentum
            vx(j,i) = calc_v(h,vx(j,i-1),ax(j,i-1),ax(j,i))
            vy(j,i) = calc_v(h,vy(j,i-1),ay(j,i-1),ay(j,i))
            KE(j,i) = (1.0/2.0)*m(j)*(vx(j,i)**2.0 + vy(j,i)**2.0)
            angmomentum = angmomentum + m(j)*(x(j,i)*vy(j,i)-
     &      y(j,i)*vx(j,i))
          end do
          write(90,*) angmomentum

          t=t+h
          write(70,*) t
        end do

c       Save results. i-th row = i-th iteration, j-th column = j-th object
        do i=1,N
              write(10,*) (x(j,i),j=1,n_bodies)
              write(20,*) (y(j,i),j=1,n_bodies)
              write(30,*) (vx(j,i),j=1,n_bodies)
              write(40,*) (vy(j,i),j=1,n_bodies)
              write(50,*) (ax(j,i),j=1,n_bodies)
              write(60,*) (ay(j,i),j=1,n_bodies)
        end do

        do i=2,N
          total_KE = 0.d0
          total_UE = 0.d0
          do j=1,n_bodies
            total_KE = total_KE + KE(j,i)
            total_UE = total_UE + UE(j,i)
          end do
          write(80,*) i,total_KE,total_UE,total_KE+total_UE
        end do

c       Close every file
        do i=10,90,10
        close(i)
        end do
        write(*,*) "Datos guardados en directorio ./results/"
      end program sistsolar

c     These functions evaluate angular position, velocity and accelera-
c     tion. Defining them here will make this program more easy to
c     addapt to different situations.

      function calc_x(h,x,v,a) result(ans)
        implicit none
        real*8 h, x, v, a, ans
        ans = x + h*v + ((h**2.0)/2.0)*a
        return
      end function calc_x

      function calc_v(h,v,a1,a2) result(ans)
        implicit none
        real*8 h, v, a1, a2, ans
        ans = v + (h/2.0)*(a1+a2)
        return
      end function calc_v

      function calc_a(m,x1,x2,y1,y2) result(ans)
        implicit none
        real*8 m,x1,x2,y1,y2,dist,ans
        dist = dsqrt((x1-x2)**2.0+(y1-y2)**2.0)**3.0
        ans =  -m*(x1-x2)/dist
        return
      end function calc_a
