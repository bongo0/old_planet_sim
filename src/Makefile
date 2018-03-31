
FC=gfortran

OBJ = simulator.o simulatorutils.o savedata.o dataplot.o
GNUPLOT:=$(shell command -v gnuplot 2> /dev/null)
FFMPEG:=$(shell command -v ffmpeg 2> /dev/null)

sim: $(OBJ)	
ifndef FFMPEG
	$(error "ffmpeg not installed. Please install ffmpeg. apt-get install ffmpeg")
endif
ifndef GNUPLOT
	$(error "gnuplot not installed. Please install gnuplot. apt-get install gnuplot")
endif
	$(FC) $^ -o $@
	-mkdir ../run
	mv sim ../run/sim

%.o: %.f90
	$(FC) -c $^ -o $@

simulator.o: simulatorutils.o savedata.o dataplot.o
	$(FC) -c simulator.f90

savedata.o: simulatorutils.o
	$(FC) -c savedata.f90

dataplot.o: simulatorutils.o
	$(FC) -c dataplot.f90

clean:
	rm -f *.o *.mod
