F90=gfortran -Jlib -Ilib -L/home/mints/.local/lib -I/home/mints/.local/lib  -L. -fno-range-check

all: gnuout average_files join_num

gnuout: src/gnuout.F90 lib/comline.o lib/logs.o lib/tcGlobals.o lib/array_works.o lib/file_io.o lib/operators.o lib/StringArray.o lib/StringUtils.o
	$(F90) -lgnuplotfortran -lfortranposix -o gnuout src/gnuout.F90 lib/comline.o lib/logs.o lib/tcGlobals.o lib/array_works.o lib/file_io.o lib/operators.o lib/StringArray.o lib/StringUtils.o
	
average_files: src/average_files.f90 lib/operators.o lib/StringArray.o lib/StringUtils.o lib/quickSort.o lib/logs.o lib/comline.o
	$(F90) -o average_files src/average_files.f90 lib/operators.o lib/StringArray.o lib/StringUtils.o lib/quickSort.o lib/logs.o lib/comline.o
	
join_num: src/join_num.f90 lib/comline.o lib/logs.o lib/tcGlobals.o lib/array_works.o lib/file_io.o lib/operators.o lib/StringArray.o lib/StringUtils.o lib/quickSort.o
	$(F90) -o join_num src/join_num.f90 lib/comline.o lib/logs.o lib/tcGlobals.o lib/array_works.o lib/file_io.o lib/operators.o lib/StringArray.o lib/StringUtils.o lib/quickSort.o
	
lib/comline.o: src/comline.f90 lib/logs.o lib/operators.o
	$(F90) -c -o lib/comline.o src/comline.f90
	
lib/array_works.o: src/array_works.f90 lib/logs.o lib/tcGlobals.o lib/file_io.o lib/operators.o lib/StringUtils.o lib/StringArray.o
	$(F90) -c -o lib/array_works.o src/array_works.f90
	
lib/StringArray.o: src/StringArray.f90 lib/operators.o
	$(F90) -c -o lib/StringArray.o src/StringArray.f90
	
lib/logs.o: src/logs.f90 
	$(F90) -c -o lib/logs.o src/logs.f90
	
lib/StringUtils.o: src/StringUtils.f90 
	$(F90) -c -o lib/StringUtils.o src/StringUtils.f90
	
lib/file_io.o: src/file_io.f90 
	$(F90) -c -o lib/file_io.o src/file_io.f90
	
lib/tcGlobals.o: src/tcGlobals.F90 lib/StringArray.o lib/logs.o lib/operators.o
	$(F90) -c -o lib/tcGlobals.o src/tcGlobals.F90
	
lib/operators.o: src/operators.f90 
	$(F90) -c -o lib/operators.o src/operators.f90
	
lib/quickSort.o: src/quickSort.f90 
	$(F90) -c -o lib/quickSort.o src/quickSort.f90
