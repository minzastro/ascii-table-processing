!!$ gnuplot_fortran95_testbench.f90  - Testbench for Fortran 95 interface to some gnuplot functions
!!$    This file is a part of the gnuplotfortran library. This provides an interface between Fortran 90/95 and GNUPlot
!!$    Copyright (C) 2004  Madhusudan Singh
!!$
!!$
!!$    This library is free software; you can redistribute it and/or
!!$    modify it under the terms of the GNU Lesser General Public
!!$    License as published by the Free Software Foundation; either
!!$    version 2.1 of the License, or (at your option) any later version.
!!$
!!$    This library is distributed in the hope that it will be useful,
!!$    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!$    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!!$    Lesser General Public License for more details.
!!$
!!$    You should have received a copy of the GNU Lesser General Public
!!$    License along with this library; if not, write to the Free Software
!!$    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!!$
!!$    I would strongly welcome bug reports, feature enhancement requests, compliments, donations and job offers :) My email address : msc@ieee.org



      program gnuplot_fortran95_testbench
        use datatypes, only : i4b,dp,dpc,lgc
        use gnuplot_module_data, only : PI_D,gnuplot_ctrl,GNUPLOT_SHOWDEBUG,GNUPLOT_SHOWWARNINGS,GP_CMD_SIZE
        use gnuplot_module
        implicit none

        integer(i4b), external :: fortran_getchar

        integer(i4b) :: numpoints=50,i=0,j=0
        integer(i4b) :: status=0,ierror=0
        type(gnuplot_ctrl), pointer :: ptr_gctrl

        real(dp), dimension(:), allocatable :: x,y1,y2,y3,y4,y
        complex(dpc), dimension(:), allocatable :: cp
        real(dp), dimension(:,:), allocatable :: z1,z2,z3,z4
        character(len=1), dimension(2) :: vialist1=(/'a','b'/)
        real(dp), dimension(2) :: paraminit1=(/1.2,1.3/)
        character(len=1), dimension(3) :: vialist2=(/'a','b','c'/)
        real(dp), dimension(3) :: paraminit2=(/1.2,1.3,1.5/)

        character(len=1) :: debug
        character(len=GP_CMD_SIZE) :: cmd

        write(*,'(A)',advance='no') "Do you wish to see the actual gnuplot commands issued as testbench proceeds (y/n) ? "
        read(*,'(a1)') debug

        if(debug.eq.'y') then
           GNUPLOT_SHOWDEBUG=.true.
        else
           GNUPLOT_SHOWDEBUG=.false.
        end if

        print*, 'Disabling warnings for this run ...'
        GNUPLOT_SHOWWARNINGS=.false.

        allocate(x(numpoints), stat=status)
        if(status.ne.0) stop 'Failed to allocate memory for x in gnuplot_fortran95_testbench'

        allocate(y(numpoints), stat=status)
        if(status.ne.0) stop 'Failed to allocate memory for x in gnuplot_fortran95_testbench'

        allocate(y1(numpoints), stat=status)
        if(status.ne.0) stop 'Failed to allocate memory for y1 in gnuplot_fortran95_testbench'
        allocate(y2(numpoints), stat=status)
        if(status.ne.0) stop 'Failed to allocate memory for y2 in gnuplot_fortran95_testbench'
        allocate(y3(numpoints), stat=status)
        if(status.ne.0) stop 'Failed to allocate memory for y3 in gnuplot_fortran95_testbench'
        allocate(y4(numpoints), stat=status)
        if(status.ne.0) stop 'Failed to allocate memory for y4 in gnuplot_fortran95_testbench'

        allocate(cp(numpoints), stat=status)
        if(status.ne.0) stop 'Failed to allocate memory for cp in gnuplot_fortran95_testbench'

        allocate(z1(numpoints,numpoints), stat=status)
        if(status.ne.0) stop 'Failed to allocate memory for y1 in gnuplot_fortran95_testbench'
        allocate(z2(numpoints,numpoints), stat=status)
        if(status.ne.0) stop 'Failed to allocate memory for y2 in gnuplot_fortran95_testbench'
        allocate(z3(numpoints,numpoints), stat=status)
        if(status.ne.0) stop 'Failed to allocate memory for y3 in gnuplot_fortran95_testbench'
        allocate(z4(numpoints,numpoints), stat=status)
        if(status.ne.0) stop 'Failed to allocate memory for y4 in gnuplot_fortran95_testbench'

        definepoints : do i=1,numpoints
           x(i)=(real(i-1)/real(numpoints-1))*2.0_dp*PI_D
           y1(i)=2.0*sin(10.0_dp*x(i))
           y2(i)=cos(x(i))
           y3(i)=atan(x(i))
           y4(i)=sqrt(x(i))
           cp(i)=cmplx(cos(x(i)),sin(x(i)),dpc)
           do j = 1,numpoints
              y(j)=(real(j-1)/real(numpoints-1))*2.0_dp*PI_D
              z1(i,j)=cos(y(j))*y1(i)
              z2(i,j)=sin(y(j))*y2(i)
              z3(i,j)=cos(y(j))*y3(i)
              z4(i,j)=cos(y(j))*y4(i)
           end do
        end do definepoints

        print*, 'Invoking gnuplot_init ...'
        ptr_gctrl=>gnuplot_init('-persist')
        if(.not.associated(ptr_gctrl)) stop 'Failed : to initiate a gnuplot session in gnuplot_fortran95_testbench'

        print*, 'Press Enter to continue ...'

        status=fortran_getchar(debug)

        print*, 'Invoking gnuplot_setrange (s)'
        status=gnuplot_setrange(ptr_gctrl,'x',0.0_dp,PI_D)
        if(status.ne.0) stop 'Failed : to set plot xrange (2D) in gnuplot_fortran95_testbench'

        status=gnuplot_setrange(ptr_gctrl,'y',-5.0_dp,5.0_dp)
        if(status.ne.0) stop 'Failed : to set plot yrange (2D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_settitle'
        status=gnuplot_settitle(ptr_gctrl,'Testbench plot')
        if(status.ne.0) stop 'Failed : to set plot title (2D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_setlabel (s)'
        status=gnuplot_setaxislabel(ptr_gctrl,'x','x')
        if(status.ne.0) stop 'Failed : to set plot xlabel (2D) in gnuplot_fortran95_testbench'

        status=gnuplot_setaxislabel(ptr_gctrl,'y','sin,cos,atan,sqrt')
        if(status.ne.0) stop 'Failed : to set plot ylabel (2D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_setscale (s)'
        status=gnuplot_setscale(ptr_gctrl,'x','NLG')
        if(status.ne.0) stop 'Failed : to set plot xscale (2D) in gnuplot_fortran95_testbench'

        status=gnuplot_setscale(ptr_gctrl,'y','NLG')
        if(status.ne.0) stop 'Failed : to set plot yscale (2D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_setstyle'
        status=gnuplot_setstyle(ptr_gctrl,'linespoints')
        if(status.ne.0) stop 'Failed : to set plot scales (2D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_plot2d'
        status=gnuplot_plot2d(ptr_gctrl,numpoints,x,y1,'sin(x)',y2=y2,y2title='cos(x)',y3=y3,y3title='atan(x)',y4=y4,y4title='sqrt(x)')
        if(status.ne.0) stop 'Failed : to plot (2D) in gnuplot_fortran95_testbench'

        print*, 'Press Enter to continue ...'

        status=fortran_getchar(debug)

        print*, 'Invoking gnuplot_setstyle'
        status=gnuplot_setstyle(ptr_gctrl,'lines')
        if(status.ne.0) stop 'Failed : to set plot scales (2D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_plotslope'
        status=gnuplot_plotslope(ptr_gctrl,1.0_dp,0.1_dp,'Slope',1.2_dp,1.4_dp)
        if(status.ne.0) stop 'Failed : to plot slope (2D) in gnuplot_fortran95_testbench'

        print*, 'Press Enter to continue ...'

        status=fortran_getchar(debug)

        print*, 'Invoking gnuplot_plotequation2d'
        status=gnuplot_plotequation2d(ptr_gctrl,'x-1+x**2.0-(x**3.0)*sin(7*x)','Equation',1.0_dp,1.5_dp)
        if(status.ne.0) stop 'Failed : to plot equation (2D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_resetsession'
        status=gnuplot_resetsession(ptr_gctrl)
        if(status.ne.0) stop 'Failed : to reset plot (2D) session in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_reset'
        status=gnuplot_reset(ptr_gctrl)
        if(status.ne.0) stop 'Failed : to reset plot (2D) session in gnuplot_fortran95_testbench'

        print*, 'Press Enter to continue ...'

        status=fortran_getchar(debug)

        print*, 'Invoking gnuplot_setcoordinatesystem (s)'
        status=gnuplot_setcoordinatesystem(ptr_gctrl,'polar')
        if(status.ne.0) stop 'Failed : to set polar coordinates in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_setrange (s)'
        status=gnuplot_setrange(ptr_gctrl,'t',-PI_D,PI_D)
        if(status.ne.0) stop 'Failed : to set plot trange (2D) in gnuplot_fortran95_testbench'

        status=gnuplot_setrange(ptr_gctrl,'x',-5.0_dp,5.0_dp)
        if(status.ne.0) stop 'Failed : to set plot xrange (2D) in gnuplot_fortran95_testbench'

        status=gnuplot_setrange(ptr_gctrl,'y',-5.0_dp,5.0_dp)
        if(status.ne.0) stop 'Failed : to set plot yrange (2D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_settitle'
        status=gnuplot_settitle(ptr_gctrl,'Testbench radial plot')
        if(status.ne.0) stop 'Failed : to set plot title (2D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_set'
        status=gnuplot_set(ptr_gctrl,'size square')
        if(status.ne.0) stop 'Failed : to set size square in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_plotcomplex'
        status=gnuplot_plotcomplex(ptr_gctrl,numpoints,cp,'A circle ?')
        if(status.ne.0) stop 'Failed : to plot complex (2D) in gnuplot_fortran95_testbench'

        print*, 'Press Enter to continue ...'

        status=fortran_getchar(debug)

        print*, 'Invoking gnuplot_plotpolarequation'
        status=gnuplot_plotpolarequation(ptr_gctrl,'t-1+t**2.0-(t**3.0)*sin(7*t)','Equation 2')
        if(status.ne.0) stop 'Failed : to plot equation (2D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_resetsession'
        status=gnuplot_resetsession(ptr_gctrl)
        if(status.ne.0) stop 'Failed : to reset plot (2D) session in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_reset'
        status=gnuplot_reset(ptr_gctrl)
        if(status.ne.0) stop 'Failed : to reset plot (2D) session in gnuplot_fortran95_testbench'

        print*, 'Press Enter to continue ...'

        status=fortran_getchar(debug)

        print*, 'Now a PDF file will be generated for a surface plot. &
             &You may view it later using xpdf or something like that'

        print*, 'Invoking gnuplot_hardcopy'
        status=gnuplot_hardcopy(ptr_gctrl,'PDF','test3.pdf','color')
        if(status.ne.0) stop 'Failed : to set plot terminal type in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_setcoordinatesystem (s)'
        status=gnuplot_setcoordinatesystem(ptr_gctrl,'nopolar')
        if(status.ne.0) stop 'Failed : to set cartesian coordinates in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_setstyle'
        status=gnuplot_setstyle(ptr_gctrl,'linespoints')
        if(status.ne.0) stop 'Failed : to set plot scales (2D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_plot3d'
        status=gnuplot_plot3d(ptr_gctrl,numpoints,numpoints,x,y,z1,'2 sin(10x) cos(y)',z2,'cos(x) sin(y)',&
             z3,'atan(x) cos(y)',z4,'sqrt(x) cos(y)')
        if(status.ne.0) stop 'Failed : to splot (3D) in gnuplot_fortran95_testbench'

        print*, 'Invoking set output - necessitated by gnuplot ver 4.0'
        cmd='set output'
        status=gnuplot_cmd(ptr_gctrl,cmd)
        if(status.ne.0) stop 'Failed : to close output file in gnuplot_fortran95_testbench'

        status=gnuplot_genpdf(ptr_gctrl)
        if(status.ne.0) stop 'Failed : to generate PDF in gnuplot_fortran95_testbench'

        status=gnuplot_hardcopy(ptr_gctrl,'X11','')
        status=gnuplot_replot(ptr_gctrl)

        print*, 'Press Enter to continue ...'

        status=fortran_getchar(debug)

        print*, 'Invoking gnuplot_setstyle'
        status=gnuplot_setstyle(ptr_gctrl,'lines')
        if(status.ne.0) stop 'Failed : to set splot scales (3D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_set'
        status=gnuplot_set(ptr_gctrl,'parametric')
        if(status.ne.0) stop 'Failed : to set parametric (3D) in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_plotequation3d'
        status=gnuplot_plotequation3d(ptr_gctrl,'tanh(u),cos(u)*sin(v),tanh(v)','Equation')
        if(status.ne.0) stop 'Failed : to splot equation (3D) in gnuplot_fortran95_testbench'

        print*, 'Press Enter to continue ...'

        status=fortran_getchar(debug)

        print*, 'Invoking gnuplot_resetsession'
        status=gnuplot_resetsession(ptr_gctrl)
        if(status.ne.0) stop 'Failed : to reset plot (2D) session in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_reset'
        status=gnuplot_reset(ptr_gctrl)
        if(status.ne.0) stop 'Failed : to reset plot (2D) session in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_fitcurve'
        status=gnuplot_fitcurve(ptr_gctrl,numpoints,2,x,y2,'a*cos(b*x)',vialist=vialist1,paraminit=paraminit1,&
             xlo=0.1_dp,xhi=1.3_dp,fitlog='myfit1.log',plotresult=1)
        if(status.ne.0) print*, 'Failed : to fit and plot curve in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_resetsession'
        status=gnuplot_resetsession(ptr_gctrl)
        if(status.ne.0) stop 'Failed : to reset plot (2D) session in gnuplot_fortran95_testbench'

        print*, 'Invoking gnuplot_reset'
        status=gnuplot_reset(ptr_gctrl)
        if(status.ne.0) stop 'Failed : to reset plot (2D) session in gnuplot_fortran95_testbench'

        print*, 'Press Enter to continue ...'

        status=fortran_getchar(debug)

        print*, 'Invoking gnuplot_fitsurface'
        status=gnuplot_fitsurface(ptr_gctrl,numpoints,numpoints,3,x,y,z4,'a*sqrt(b*x)*cos(c*y)',vialist=vialist2,&
             paraminit=paraminit2,xlo=0.1_dp,xhi=4.3_dp,ylo=0.1_dp,yhi=5.0_dp,fitlog='myfit2.log',plotresult=1)
        if(status.ne.0) print*, 'Failed : to fit and plot surface in gnuplot_fortran95_testbench'

        print*, 'Press Enter to continue ...'

        status=fortran_getchar(debug)

        print*, 'Invoking gnuplot_close'
        status=gnuplot_close(ptr_gctrl)
        if(status.ne.0) stop 'Failed : to close a gnuplot session in gnuplot_fortran95_testbench'


        deallocate(x,y,y1,y2,y3,y4,cp,z1,z2,z3,z4)

      end program gnuplot_fortran95_testbench
