program test
    use simulatorutils
    use savedata
    use dataplot
    implicit none

    type(planet),allocatable :: planets(:)
    integer :: n_planets=0,iterations=1000000,update_step=100000,save_step=1000,frame_step=10
    real(rk) :: totaltime = 3.154e7,constraint = 0
    character(80) :: inputfile,anim_filename="simulation",outputfile="data.txt"
    logical :: doanimation = .true.

!-------MAIN PROGRAM-----------------------------------------
    call get_command_argument(1,inputfile)
    call readvalues()
    write(6,'(a,g14.8,3(a,i0))') "time step: ",totaltime/iterations," n planets:",n_planets &
    ," iterations: ",iterations," update_step: ",update_step
    write(6,'(a,i0,a,i0,5(a),g14.8)')" save_step: ",save_step," frame_step: " &
    ,frame_step," video name: ",anim_filename," outputfile: ",outputfile," total_time: ",totaltime
    call newfile(outputfile)
    call simulation()

    if(doanimation) then
    write(6,*) "making frames..."
    call makeanimation(frame_step)
    end if

    call mk2dplot("xy")
    call mk2dplot("xz")
    call mk2dplot("yz")
!------------------------------------------------------------
    contains

    subroutine simulation()         ! main loop for running the simulation
        implicit none               ! runs the velocity verlet and saves data
        integer :: i,k,m,wf=0
        real(rk) :: dt,t=0
        k = save_step
        m = update_step
        dt = totaltime/iterations

        do i=1,iterations
            if(k==save_step) then
                call savepos(planets,n_planets,t)
                wf = wf + 1
                k=0
            end if
            if(m==update_step) then
                call printupdates(n_planets,t,i,wf,planets)
                m=0
            end if
            call velocity_verlet(planets,n_planets,dt,constraint)
            m = m + 1
            k = k + 1
            t = t + dt
        end do
            call savepos(planets,n_planets,t) ! for some reason gnuplot doesent want to plot
            call savepos(planets,n_planets,t) ! the last line in data file.. thats why last pos
    end subroutine simulation                 ! is saved twice.

    subroutine makeanimation(frame_step)  ! main loop for making the frames for the animation
        implicit none                     ! makes the animation in the end
        character(80) :: mk_anim_cmd
        integer :: frame_step,i,nframes
        write(mk_anim_cmd,'(3(a))') &
                "ffmpeg -i frames/frame%d.png -pix_fmt yuv420p ",trim(anim_filename),".mp4 </dev/null"
        nframes = (iterations/save_step)/frame_step

        call mkdir_for_frames("frames")
        call newplotcmd("framescmd.txt")
        call init_axes("framescmd.txt",n_planets,planets,trim(outputfile))
            do i=1,nframes
            call plotplanets_frame_m(n_planets,i*frame_step,"framescmd.txt",trim(outputfile),"frames",i)
            end do
        call system("gnuplot framescmd.txt")
        call system(mk_anim_cmd)
    end subroutine

    subroutine mk2dplot(xyz) ! makes a 2d plot projected to xyz-plane xyz = 'xy','xz' or 'yz'
        implicit none
        character(2) :: xyz
        call newplotcmd("plot2d.txt")
        call plot_2d(xyz,n_planets,planets,"plot2d.txt",outputfile,iterations/save_step)
        call system("gnuplot 'plot2d.txt'")
    end subroutine

    subroutine readvalues() ! reads values from the inputfile that is given in command arg
        implicit none
        character(80) :: readin,temp
        real(rk) :: mass
        real(rk),dimension(3) :: r,v,a=[0,0,0]
        integer :: ios,i

        open(unit=1, file=inputfile, iostat=ios)
        call checkforerror(ios," ",inputfile)

do while(ios == 0)
        read(1,*,iostat=ios) readin

    select case(readin)
            case("[planets]")
                read(1,*,iostat=ios) n_planets
                call checkforerror(ios,"integer number of planets",inputfile)
                allocate(planets(n_planets))
                do i=1,n_planets
                    read(1,*,iostat=ios) mass,r,v
                    call checkforerror(ios,"planet initial data: (mass, position, velocity)",inputfile)
                    planets(i) = planet(mass,r,v,a,"")
                end do

            case("[iterations]")
                read(1,*,iostat=ios) iterations
                call checkforerror(ios,"integer iteration steps",inputfile)

            case("[total_time]")
                read(1,*,iostat=ios) totaltime
                call checkforerror(ios,"real number total time",inputfile)

            case("[update_step]")
                read(1,*,iostat=ios) update_step
                call checkforerror(ios,"integer update step",inputfile)

            case("[save_step]")
                read(1,*,iostat=ios) save_step
                call checkforerror(ios,"integer save step",inputfile)

            case("[frame_step]")
                read(1,*,iostat=ios) frame_step
                call checkforerror(ios,"integer frame step",inputfile)

            case("[outputfile]")
                read(1,*,iostat=ios) outputfile
                call checkforerror(ios,"string name of output file",inputfile)

            case("[do_animation]")
                read(1,*,iostat=ios) doanimation
                call checkforerror(ios,"logical variable do_animation (true/false)",inputfile)

            case("[anim_filename]")
                read(1,*,iostat=ios) anim_filename
                call checkforerror(ios,"string name of the animation file",inputfile)

            case("[planet_names]")
                do i=1,n_planets
                read(1,*,iostat=ios) planets(i)%name
                call checkforerror(ios,"planet names",inputfile)
                end do

            case("[distance_constraint]")
                read(1,*,iostat=ios) constraint
                call checkforerror(ios,"real number distance constraint",inputfile)

        end select
end do
    close(1)
        call initaccel(planets,n_planets,constraint) ! update initial accelarations

end subroutine readvalues

end program test