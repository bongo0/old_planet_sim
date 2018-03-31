module savedata
    use simulatorutils
    implicit none
    character(80) :: fname
    contains

    subroutine newfile(filename)   ! creates a new file for saving data
        implicit none
        integer :: ios
        character(*),intent(in) :: filename
        fname = filename
        open(unit=2,file=fname,iostat=ios,status='new')
        if (ios/=0) then
            write(6,*) "Error creating a file ",fname
            stop
        end if
        close(2)
    end subroutine newfile

    subroutine savepos(planets,planets_dim,time)  ! saves positions of the planets in following format:
        implicit none                        ! x y z 'p1' x2 y2 z2 'p2'...     xyz-first planets coords
                                             !                                 x2 y2 z2 -second planets etc...
        integer :: ios,i,planets_dim
        real(rk) :: time
        type(planet),dimension(planets_dim) :: planets
        open(unit=2,file=fname,iostat=ios,status='old',access='append')
        if (ios/=0) then
            write(6,*) "Error opening a file ",fname
        stop
        end if

        ! writing positions to file
        ! names the planets p1, p2 ,p3 ... respectively with the order of input
        do i=1,planets_dim
            write(2,'((g14.8),a1,(g14.8),a1,(g14.8),a1,a5,$)') &
            planets(i)%r(1)," ",planets(i)%r(2)," ",planets(i)%r(3)," ",planetname(i)
        end do
            write(2,'((g14.8))') time
            close(2)
    end subroutine savepos


    character(80) function planetname(i)
        implicit none
        character(80) :: char
        integer,intent(in) :: i
        write(char,'(a,i0)') 'p',i
        planetname = trim(char)
    end function planetname

    subroutine print_pos(planets,planets_dim) ! prints to console the positions of planets
        implicit none
        integer :: planets_dim,i
        type(planet),dimension(planets_dim) :: planets
        do i=1,planets_dim
            write(6,'(a,a,(g14.8),a,(g14.8),a,(g14.8),a,a)') &
            trim(planets(i)%name)," ",planets(i)%r(1)," ",planets(i)%r(2)," ",planets(i)%r(3)," ",planetname(i)
        end do
    end subroutine

    subroutine printupdates(planets_dim,timepassed,stepsmade,writingstofile,planets)
        implicit none
        integer :: planets_dim,stepsmade,writingstofile
        type(planet),dimension(planets_dim) :: planets
        real(rk) :: timepassed
        write(6,'(a,i0,a,f14.4,a,i0,a,i0)') &
                'number of planets: ',planets_dim,' time passed: ',timepassed &
                ,' steps made: ',stepsmade,' writings to file: ',writingstofile
        write(6,*) 'positions of planets:'
        call print_pos(planets,planets_dim)
    end subroutine

end module savedata

