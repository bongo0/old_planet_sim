module dataplot
    use simulatorutils
    implicit none

    contains

    subroutine newplotcmd(animfile) ! creates a new file called animfile for plotting commands
        implicit none              ! and sets basic plotting stuff
        character(*),intent(in) :: animfile
        integer :: ios
        open(unit=1,file=animfile,iostat=ios,status='replace')
            if(ios/=0) then
                write(6,*) "error creating a file",animfile
                stop
            end if
            write(1,*) "set terminal pngcairo enhanced font ""arial,12"" size 1280,1280"
            write(1,*) "set ticslevel 0"
            write(1,*) "set grid ls -1 dt 4"
            write(1,*) "set grid ztics ls -1 dt 4"
            write(1,*) 'set output "/dev/null"'
            close(1)
    end subroutine newplotcmd

    subroutine init_axes(animfile,n_planets,planets,datafile) ! plots the data from datafile to
        character(*) :: animfile,datafile                     ! set the ranges for axes

        integer :: n_planets,ios,i
        type(planet),dimension(n_planets) :: planets
        call open_animfile(1,animfile)
        write(1,'(a,a,a,$)') 'splot "',trim(datafile),'" '
            do i=1,n_planets-1
                write(1,'(a,a,$)') trim(data_pos_planet_n(i)),",'' "
            end do
                write(1,*) data_pos_planet_n(n_planets)
                write(1,*) "set xlabel 'x (m)'"
                write(1,*) "set ylabel 'y (m)'"
                write(1,*) "set zlabel 'z (m)'"
                write(1,*) "set xrange [GPVAL_X_MIN:GPVAL_X_MAX]"
                write(1,*) "set yrange [GPVAL_Y_MIN:GPVAL_Y_MAX]"
                write(1,*) "set zrange [GPVAL_Z_MIN:GPVAL_Z_MAX]"

                write(1,*) "set key at graph -0.05,-0.05,1.65"

                write(1,'(a,$)') 'set label 2 "'
                do i=1,n_planets
                write(1,'(a,a,$)') trim(planets(i)%name),"\n"
                end do
                write(1,*) '" at graph -0.06,-0.06,1.638 font "arial,12"'

        close(1)
    end subroutine


    function data_pos_planet_n(n,with_label,xyz) ! returns the location of the data for nth planet
        integer,intent(in) :: n              ! in format that gnuplot can understand
        character(80) :: data_pos_planet_n   ! with option to get a label for the data
        character(2),optional :: xyz         ! and a option to get 2d data xy,xz,yz
        logical,optional :: with_label

        if(present(with_label)) then
            if(with_label) then
                write(data_pos_planet_n,'(a,i0,a,i0,a,i0,a,i0)') &
                "using ",1+(n-1)*4,":",2+(n-1)*4,":",3+(n-1)*4,":",4+(n-1)*4
                data_pos_planet_n = trim(data_pos_planet_n)
                return
            else
                write(data_pos_planet_n,'(a,i0,a,i0,a,i0)') &
                "using ",1+(n-1)*4,":",2+(n-1)*4,":",3+(n-1)*4
                data_pos_planet_n = trim(data_pos_planet_n)
                return
            end if
        end if

        if(present(xyz)) then
            select case(xyz)
                    case("xy")
                    write(data_pos_planet_n,'(a,i0,a,i0)') &
                    'using ',1+(n-1)*4,":",2+(n-1)*4
                    data_pos_planet_n = trim(data_pos_planet_n)
                    return

                    case("xz")
                    write(data_pos_planet_n,'(a,i0,a,i0)') &
                    'using ',1+(n-1)*4,":",3+(n-1)*4
                    data_pos_planet_n = trim(data_pos_planet_n)
                    return

                    case("yz")
                    write(data_pos_planet_n,'(a,i0,a,i0)') &
                    'using ',2+(n-1)*4,":",3+(n-1)*4
                    data_pos_planet_n = trim(data_pos_planet_n)
                    return
            end select
        end if

        write(data_pos_planet_n,'(a,i0,a,i0,a,i0)') &
        "using ",1+(n-1)*4,":",2+(n-1)*4,":",3+(n-1)*4
        data_pos_planet_n = trim(data_pos_planet_n)

    end function



    subroutine mkdir_for_frames(foldername)     ! creates a folder for the frames
        character(*) :: foldername
        character(80) :: mkdir
        write(mkdir,'(a,a)') 'mkdir ',foldername
        call system(mkdir)
    end subroutine

    subroutine setframe_n_output(n,foldername,animfile) ! sets output for the nth plotted frame
        integer,intent(in) :: n
        character(*),intent(in) :: foldername,animfile
        integer :: ios
        character(1000) :: framename
        call open_animfile(1,animfile)
        write(framename,'(a,i0,a)') 'frame',n,'.png'
        write(1,'(6(a))') 'set output ','"./',trim(foldername),'/',trim(framename),'"'
    end subroutine

    subroutine plotplanets_frame_m(n_planets,data_m,animfile,datafile,foldername,frame_m) ! sets commands how the
        character(*),intent(in) :: animfile,datafile,foldername                    ! mth frame should be
        integer,intent(in) :: data_m,n_planets,frame_m                             ! plotted
        integer :: i
        call open_animfile(1,animfile)

        write(1,'(3(a),i0,a,i0,a,i0,a)') &      
        "plot '",trim(datafile),"' every ::",data_m,"::",data_m," using (0):(t=$",(4*n_planets+1),")"
        call setframe_n_output(frame_m,foldername,animfile)
        write(1,'(a)') "set label 1 sprintf('%e s',t) at graph -0.1,-0.1,-0.2"

        write(1,'(3(a),$)') 'splot "',trim(datafile),'" '
        do i=1,n_planets-1
            call plotplanet_n_frame_m(i,data_m,1)
            write(1,*) '\'
            write(1,'(a,$)') ",''"
        end do
            call plotplanet_n_frame_m(n_planets,data_m,1)
            write(1,*)
        close(1)
    end subroutine

    subroutine plotplanet_n_frame_m(planet_n,frame_m,unit1) ! sets how nth planet should be
        integer,intent(in) :: planet_n,frame_m,unit1        ! plotted in the mth frame

        integer :: column,x,z

        column = 4 + (planet_n - 1)*4
        x = 1 + (planet_n - 1)*4
        z = 3 + (planet_n - 1)*4

    write(unit1,'(a,i0,3(a),i0,a,$)') &
    " every ::0::",frame_m,' ',trim(data_pos_planet_n(planet_n)),' with lines lc ',planet_n,' notitle'

    write(unit1,*) '\'

    write(unit1,'(a,i0,a,i0,3(a),i0,a,i0,a,$)') &
    ",''every ::",frame_m,'::',frame_m,' ',trim(data_pos_planet_n(planet_n)) &
    ,' with points pt 7 lc ',planet_n,' title column(',column,')'

    write(unit1,*) '\'

    write(unit1,'(a,i0,a,i0,3(a),$)') &
    ",''every ::",frame_m,'::',frame_m,' ',trim(data_pos_planet_n(planet_n,.true.)),' with labels offset 0,-1 notitle'

    write(unit1,*) '\'

    write(unit1,'(a,i0,a,i0,3(a),i0,a,$)') &
    ",''every ::",frame_m,'::',frame_m,' ',trim(data_pos_planet_n(planet_n)) &
    ,':(0):(0):(-($',z,' +abs(GPVAL_Z_MIN))) with vectors nohead ls -1 dt 3 notitle '

    write(unit1,*) '\'

    write(unit1,'(a,i0,a,i0,3(a),i0,a,$)') &
    ",''every ::",frame_m,'::',frame_m,' ',trim(data_pos_planet_n(planet_n)) &
    ,':(-($',x,'+abs(GPVAL_X_MIN))):(0):(0) with vectors nohead ls -1 dt 3 notitle'

    end subroutine


    subroutine open_animfile(unit1,animfile)  ! opens the animfile with unit= unit1
        integer,intent(in) :: unit1
        character(*),intent(in) :: animfile
        integer :: ios
        open(unit=unit1,file=animfile,iostat=ios,status='old',access='append')
        if(ios/=0) then
        write(6,*) "error opening file",animfile
        stop
        end if
    end subroutine

    subroutine plot_2d(xyz,n_planets,planets,animfile,datafile,lastdatapoint)
        implicit none                            ! makes the command for plotting 2d plot to file: animfile
        integer,intent(in) :: n_planets,lastdatapoint   ! xyz = 'xy','xz' or 'yz' selects which
        type(planet),dimension(n_planets) :: planets    ! projection we want to plot
        character(*),intent(in) :: animfile,datafile    ! output is set to plotxy.png/plotxz.png/plotyz.png
        character(2),intent(in) :: xyz
        integer :: i

        call open_animfile(1,animfile)

        select case(xyz)
            case("xy")
            write(1,*) "set xlabel 'x (m)'"
            write(1,*) "set ylabel 'y (m)'"

            case("xz")
            write(1,*) "set xlabel 'x (m)'"
            write(1,*) "set ylabel 'z (m)'"

            case("yz")
            write(1,*) "set xlabel 'y (m)'"
            write(1,*) "set ylabel 'z (m)'"
        end select

        write(1,*) "set key at graph 0.07,0.99"
        write(1,'(a,$)') 'set label 2 "'
        do i=1,n_planets
        write(1,'(a,a,$)') trim(planets(i)%name),"\n"
        end do
        write(1,*) '" at graph 0.07,0.985 font "arial,12"'

        write(1,*) 'set output "plot',xyz,'.png"'
        write(1,'(3(a),$)') 'plot "',trim(datafile),'" '

            do i=1,n_planets-1
        write(1,'(2(a),i0,a)') &
        trim(data_pos_planet_n(i,xyz=xyz)),' with lines lc ',i,' notitle \'
        write(1,'(3(a))') &
            ",'' every ::0::0 ",trim(data_pos_planet_n(i,xyz=xyz)) &
            ,' with points pt 7 lt rgb "black"  notitle \'
        write(1,'(a,i0,a,i0,a,a,i0,a,i0,a)') &
        ",'' every ::",lastdatapoint,"::",lastdatapoint,trim(data_pos_planet_n(i,xyz=xyz)) &
        ,' with points pt 7 lc ',i,' title column(',4+(i-1)*4,') \'
        write(1,'(a,$)') ",'' "
            end do
        write(1,'(2(a),i0,a)') &
        trim(data_pos_planet_n(n_planets,xyz=xyz)),' with lines lc ',n_planets,' notitle \'
        write(1,'(3(a))') &
        ",'' every ::0::0 ",trim(data_pos_planet_n(n_planets,xyz=xyz)) &
        ,' with points pt 7 lt rgb "black" notitle \'
        write(1,'(a,i0,a,i0,a,a,i0,a,i0,a)') &
        ",'' every ::",lastdatapoint,"::",lastdatapoint,trim(data_pos_planet_n(n_planets,xyz=xyz)) &
        ,' with points pt 7 lc ',n_planets,' title column(',4+(n_planets-1)*4,') '
        close(1)

    end subroutine



end module dataplot