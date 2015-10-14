#    This file is a part of the gnuplotfortran library. This provides an interface between Fortran 90/95 and GNUPlot
#    Copyright (C) 2004  Madhusudan Singh
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#    I would strongly welcome bug reports, feature enhancement requests, compliments, donations and job offers :) My email address : msc@ieee.org

# USES GNU make - gmake

# Name of the library

LIBRARYFILE=libgnuplotfortran.so
DEBUGLIBRARYFILE=libgnuplotfortrandbg.so

RM=rm -f

# Your fortran compiler
F95C=gfortran -fPIC
# Option for generating shared libraries on your Fortran compiler
F95SHAREDOPTION=-shared 
F95DEBUGOPTIONS=-g -CB

compf95=$(F95C) $(F95SHAREDOPTION) $(@F:.so=.f90)

all: $(LIBRARYFILE)

clean :
	$(RM) *.o

debug : $(DEBUGLIBRARYFILE)

$(LIBRARYFILE) ::
	$(F95C) $(F95SHAREDOPTION) gnuplot_fortran95.f90 -o $(LIBRARYFILE)

$(DEBUGLIBRARYFILE) ::
	$(F95C) $(F95DEBUGOPTIONS) $(F95SHAREDOPTION) gnuplot_fortran95.f90 -o $(DEBUGLIBRARYFILE)

doc :
	makeinfo --html gnuplotfortran.texi
	makeinfo gnuplotfortran.texi
	texi2pdf gnuplotfortran.texi