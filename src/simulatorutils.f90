module simulatorutils
	implicit none
    integer,parameter :: rk=selected_real_kind(10,20)
    real(rk),parameter :: G=6.67408e-11   ! m^3 kg^-1 s^-2 gravitational constant

    type :: planet
        real(rk) :: mass                  !mass of the planet
        real(rk),dimension(3) :: r,v,a    !position, velocity, accleration
        character(80) :: name
    end type planet

	contains 
	


    subroutine checkforerror(ios,tryingtoread,fname)
        implicit none
        character(*) :: tryingtoread,fname
        integer :: ios
        if(ios<0) then
            write(6,*) &
            "error when trying to read ",tryingtoread,", file: ",fname
            stop
        end if
    end subroutine checkforerror

    subroutine velocity_verlet(planets,planets_dim,dt,constraint)
        implicit none
        integer,intent(in) :: planets_dim
        type(planet),dimension(planets_dim) :: planets
        real(rk),intent(in) :: dt,constraint
        real(rk),dimension(3) :: atemp
        integer :: i,k

        !------- [calculating: x(t + dt) = x(t) + v(t)*dt + 1/2*a(t)*dt^2]
        do i=1,planets_dim
            planets(i)%r = planets(i)%r + planets(i)%v*dt + 0.5*planets(i)%a*dt**2
        end do

        !-------[calculating: a(t + dt) and v(t + dt) = v(t) + 1/2(a(t) + a(t + dt))*dt]
        do i=1,planets_dim
            atemp = planets(i)%a
            planets(i)%a = [0,0,0]
            do k=1,planets_dim
                if (k==i) then
                cycle
                end if
                planets(i)%a = planets(i)%a + accel(planets(k),planets(i),constraint)
            end do
            planets(i)%v = planets(i)%v + 0.5*(atemp + planets(i)%a)*dt
        end do
    end subroutine velocity_verlet


    subroutine initaccel(planets,planets_dim,constraint) ! calculate the initial accelerations from input
        implicit none
        integer,intent(in) :: planets_dim
        real(rk),intent(in) :: constraint
        type(planet),dimension(planets_dim) :: planets
        integer :: i,k
        do i=1,planets_dim
            do k=1,planets_dim
                if (k==i) then
                cycle
                end if
                planets(i)%a = planets(i)%a + accel(planets(k),planets(i),constraint)
            end do
        end do
    end subroutine initaccel


    function accel(planet1,planet2,constraint) ! vector: acceleration of planet2 due to planet1
        implicit none
        real(rk) :: constraint
        real(rk),dimension(3) :: accel
        type(planet) :: planet1,planet2
        accel = -(G*planet1%mass/(( constraindist(distbtwn_planets(planet1,planet2),constraint))**2))*unitvec_r(planet1,planet2)
    end function accel

    real(rk) function distbtwn_planets(p1,p2) ! distance between p1 and p2
        implicit none
        type(planet) :: p1,p2
            distbtwn_planets = sqrt((p2%r(1)-p1%r(1))**2+(p2%r(2)-p1%r(2))**2+(p2%r(3)-p1%r(3))**2)


    end function distbtwn_planets

    function unitvec_r(p1,p2) ! unit vector from p1 to p2
        implicit none
        real(rk),dimension(3) :: unitvec_r
        type(planet) :: p1,p2
        unitvec_r = (p2%r - p1%r)/(distbtwn_planets(p1,p2))
    end function unitvec_r

    real(rk) function constraindist(r,constraint) ! constrains r to be >= than constraint
        real(rk),intent(in) :: r,constraint
        if (r<constraint) then
            constraindist = constraint
        else
            constraindist = r
        end if
    end function constraindist


end module simulatorutils
		