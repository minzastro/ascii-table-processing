!!$ gnuplot_fortran95_interfaces  - GNUPlot - Fortran 95 interfaces
!!$ Date - 1/27/2004 Version 0.1
!!$ Copyright(C) 2004 Madhusudan Singh
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
!!$

!!$          Function : gnuplot_init
!!$
!!$          Inputs :
!!$
!!$                 1. xoption : character string that passes any X11 options to the GNUPlot session, such as -persist,-background,-clear,-raise,-noraise,-tvtwm,-gray,or -mono.
!!$
!!$          Outputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$
!!$          Purpose : Initiates a gnuplot session
!!$
!!$          Diagnostics :
!!$
!!$                 0. ptr_gctrl = associated : Success
!!$                 1. ptr_gctrl = NULL : Failure
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
             interface
                function gnuplot_init(xoption) result(ptr_gctrl)
                  character(len=*), intent(in) :: xoption
                end function gnuplot_init
             end interface

!!$          Function : gnuplot_close
!!$
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Concludes a gnuplot session
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -8888 : PDF conversion requested and failed.
!!$                 3. status = -7777 : Cannot close communication pipe to gnuplot
!!$                 4. status = -1 : Could not remove temporary directory
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
             interface
                integer function gnuplot_close(ptr_gctrl) result(status)
                  type(gnuplot_ctrl), pointer :: ptr_gctrl
                end function gnuplot_close
             end interface

!!$          Function : gnuplot_setstyle
!!$
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. plotstyle : character string (plot style - see VALIDPLOTSTYLE)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Sets the plotting style of a gnuplot session
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = 22 : Invalid plotting style requested, replaced with points
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_setstyle(ptr_gctrl,plotstyle) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: plotstyle
             end function gnuplot_setstyle
          end interface

!!$          Function : gnuplot_setscale
!!$
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. axis : character string indicating the axis, such as x,x2,y,y2,z,z2,v,t,u, etc.
!!$                 3. scale : 3-character string (choices - 'LOG' or 'NLG')
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Sets scaling along different axes
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Cannot set requested scaling for the specified axis
!!$
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_setscale(ptr_gctrl,axis,scale) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: axis
               character(len=3), intent(in) :: scale
             end function gnuplot_setscale
          end interface

!!$          Function : gnuplot_setaxislabel
!!$
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. axis : character string indicating the axis
!!$                 3. axislabel : character string indicating the label
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Sets axis label
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Cannot set requested label for the axis
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_setaxislabel(ptr_gctrl,axis,axislabel) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: axis,axislabel
             end function gnuplot_setaxislabels
          end interface

!!$          Function : gnuplot_settitle
!!$
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. title : character string (title for the plot)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Sets the plot title
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Cannot set title
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_settitle(ptr_gctrl,title) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: title
             end function gnuplot_settitle
          end interface

!!$          Function : gnuplot_resetsession
!!$
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Resets the session
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_resetsession(ptr_gctrl) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
             end function gnuplot_resetsession
          end interface

!!$          Function : gnuplot_set
!!$
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. setstring : character string (can be anything that has not been covered by other gnuplot_set type of commands)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Sets a GNUPlot variable
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Cannot set with requested string
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_set(ptr_gctrl,setstring) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: setstring
             end function gnuplot_set
          end interface

!!$          Function : gnuplot_unset
!!$
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. setstring : character string (can be anything that has not been covered by other gnuplot_set type of commands)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Unsets a GNUPlot variable
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Cannot set with requested string
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_unset(ptr_gctrl,unsetstring) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: unsetstring
             end function gnuplot_unset
          end interface

!!$          Function : gnuplot_reset
!!$
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Resets the parameters for the current session
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Failed to reset parameters
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_reset(ptr_gctrl) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
             end function gnuplot_reset
          end interface

!!$          Function : gnuplot_load
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. loadfile : character string : name of the load file
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Runs a gnuplot load file
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Cannot find the specified load file
!!$                 3. status = -2 : You do not have read permissions for the load file
!!$                 4. status = -3 : Cannot run the specified load file
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_load(ptr_gctrl,loadfile) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: loadfile
             end function gnuplot_loadfile
          end interface

!!$          Function : gnuplot_replot
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Send a replot command to the session
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Failed to replot
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_replot(ptr_gctrl) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
             end function gnuplot_replot
          end interface

!!$          Function : gnuplot_setaxisformat
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. axis : character string indicating the axis
!!$                 3. axisformat : character string indicating the format
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Set the format for an axis
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Failed to set format for the specified axis
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_setaxisformat(ptr_gctrl) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: axis,axisformat
             end function gnuplot_setaxisformat
          end interface

!!$          Function : gnuplot_setcoordinatesystem
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. coordsystem : character string indicating the kind of coordinate system (polar or nopolar)
!!$
!!$          Outputs :
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Sets the coordinate system
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Cannot set requested coordinate system
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_setcoordsystem(ptr_gctrl,coordsystem) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: coordsystem
             end function gnuplot_setcoordsystem
          end interface

!!$          Function : gnuplot_setangleunits
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. angleunits : character string indocating the units of angle measurement (radians or degrees) - this may effect some transcendental functions - consult GNUPlot help
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Sets the angle units
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Cannot set requested angle units
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_setangleunits(ptr_gctrl,angles) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: angleunits
             end function gnuplot_setangleunits
          end interface

!!$          Function : gnuplot_setrange
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. axis : character string indicating the axis
!!$                 3. lo : real (lower limit of the range)
!!$                 4. hi : real (upper limit of the range)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Sets axes ranges
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Cannot set requested range for the axis
!!$                 3. status = -8888 : Invalid range specification
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_setrange(ptr_gctrl,axis,lo,hi) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: axis
               real(dp), intent(in) :: lo,hi
             end function gnuplot_setrange
          end interface

!!$          Function : gnuplot_pause
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. t : integer time for which to pause
!!$                 3. displaystring : string to be displayed (optional)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Sets the coordinate system
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Cannot pause
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_pause(ptr_gctrl,t,displaystring) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: t
               character(len=*), intent(in), optional :: displaystring
             end function gnuplot_pause
          end interface

!!$          Function : gnuplot_plot2d
!!$
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. n : integer (number of points to be plotted)
!!$                 3. x : real, dimension(n) (abcissae)
!!$                 4. y1 : real, dimension(n) (first ordinate)
!!$                 5. y1title : character string (title for y1) (optional)
!!$                 6. y1axis : character string (axis for y1) (optional)
!!$                 7. y2 : real, dimension(n) (second ordinate) (optional)
!!$                 8. y2title : character string (title for y2) (optional)
!!$                 9. y2axis : character string (axis for y2) (optional)
!!$                 10. y3 : real, dimension(n) (third ordinate) (optional)
!!$                 11. y3title : character string (title for y3) (optional)
!!$                 12. y3axis : character string (axis for y3) (optional)
!!$                 13. y4 : real, dimension(n) (fourth ordinate) (optional)
!!$                 14. y4title : character string (title for y4) (optional)
!!$                 15. y4axis : character string (axis for y4) (optional)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Plots x,y1,y2,y3,y4 plot (max of 4 plots possible)
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -3 : Called with less than one point
!!$                 3. status = -1 : Impossible error
!!$                 4. status = -2 : Cannot create temporary file
!!$                 6. status = 22 : Failed to delete temporary file
!!$                 7. status = -4 : Failed to write to temporary file
!!$                 8. status = -5 : Failed to open temporary file in temporary directory
!!$                 9. status = -6 : Failed to execute the plot / replot command
!!$                10. status = -7 : Invalid y axis specification supplied
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
!!$          interface
            integer function gnuplot_plot2d(ptr_gctrl,n,x,y1,y1title,y1axis,y2,y2title,y2axis,&
               y3,y3title,y3axis,y4,y4title,y4axis) result(status)
              type(gnuplot_ctrl), pointer :: ptr_gctrl
              real(dp), dimension(:), intent(in) :: x,y1
              real(dp), dimension(:), intent(in), optional :: y2,y3,y4
              integer(i4b), intent(in) :: n
              character(len=*), intent(in), optional :: y1title,y2title,y3title,y4title
              character(len=*), intent(in), optional :: y1axis,y2axis,y3axis,y4axis
            end function gnuplot_plot2d
          end interface


!!$          Function : gnuplot_fitcurve
!!$
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. n : integer (number of points to be plotted)
!!$                 3. np : integer (number of adjustable parameters)
!!$                 4. x : real, dimension(n) (abcissae)
!!$                 5. y : real, dimension(n) (ordinate)
!!$                 6. ffit : character string : function to be fit (must be the RHS of the expression f(x)=...)
!!$                 7. vialist : character(len=1), dimension(np) : the via list (like 'a,b,c' (constants used in ffit)
!!$                 8. paraminit : real, dimension(np) : array containing initial values for the parameters (optional)
!!$                 9. fitlimit : real : FIT_LIMIT , default is usually 1e-6 (depends upon your gnuplot installation) (optional)
!!$                 10. fitlog : character string : filename of the fitting log (optional)
!!$                 11. plotresult : integer whether to plot the result (=1) or not (=0) with the data (optional)
!!$                 12. xlo : real xrange low (optional)
!!$                 13. xhi : real xrange high (optional)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Fits f(x) to supplied y vs x and plots the result if desired
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -3 : Called with less than one point
!!$                 3. status = -1 : Impossible error
!!$                 4. status = -2 : Cannot create temporary file
!!$                 6. status = 22 : Failed to delete temporary file
!!$                 7. status = -4 : Failed to write to temporary file
!!$                 8. status = -5 : No function specified
!!$                 9. status = -7 : Invalid function specification
!!$                 10. status = -8 : Invalid plot directive
!!$                 11. status = 23 : Specified fit log file will overwrite an existing file
!!$                 12. status = -9 : Failed to open temporary file temporary directory
!!$                 13. status = -10 : Could not execute the fit
!!$                 14. status = -11 : Could not plot the result
!!$                 15. status = -12 : Wrong initial paraminit array size
!!$                 16. status = -13 : Wrong vialist size
!!$                 17. status = -14 : Invalid input xrange
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_fitcurve(ptr_gctrl,n,np,x,y,ffit,vialist,paraminit,xlo,xhi,fitlimit,fitlog,plotresult) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               real(dp), dimension(:), intent(in) :: x,y
               character(len=*), intent(in) :: ffit
               character(len=1), dimension(:) :: vialist
               real(dp), intent(in), optional :: fitlimit
               integer(i4b), intent(in) :: n,np
               integer(i4b), intent(in), optional :: plotresult
               real(dp), dimension(:), intent(in), optional :: paraminit
               character(len=*), intent(in), optional :: fitlog
             end function gnuplot_fitcurve
          end interface

!!$          Function : gnuplot_plot3d
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. n : integer (number of points along x axis to be plotted)
!!$                 3. m : integer (number of points along y axis to be plotted)
!!$                 4. x : real, dimension(n) (abcissae)
!!$                 5. y : real, dimension(m) (abcissae)
!!$                 6. z1 : real, dimension(n X m) (first surface)
!!$                 7. z1title : character string (title for z1) (optional)
!!$                 8. z2 : real, dimension(n X m) (second surface) (optional)
!!$                 9. z2title : character string (title for z2) (optional)
!!$                 10. z3 : real, dimension(n X m) (third surface) (optional)
!!$                 11. z3title : character string (title for z3) (optional)
!!$                 12. z4 : real, dimension(n X m) (fourth surface) (optional)
!!$                 13. z4title : character string (title for z4) (optional)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Plots x,y,z1,z2,z3,z4 plot (max of 4 surface plots possible)
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -3 : Called with less than one point
!!$                 3. status = -1 : Impossible error
!!$                 4. status = -2 : Cannot create temporary file
!!$                 6. status = 22 : Failed to delete temporary file
!!$                 7. status = -4 : Failed to write to temporary file
!!$                 8. status = 24 : Failed to unset previously set acknowledgement string
!!$                 9. status = 23 : Failed to set acknowledgement string
!!$                 10. status = -5 : Failed to open temporary file in temporary directory
!!$                 11. status = -6 : Failed to execute the splot / replot command
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_plot3d(ptr_gctrl,n,m,x,y,z1,z1title,z2,z2title,z3,z3title,z4,z4title) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               real(dp), dimension(:), intent(in) :: x,y
               real(dp), dimension(:,:), intent(in) :: z1
               real(dp), dimension(:,:), intent(in), optional :: z2,z3,z4
               integer(i4b), intent(in) :: n.m
               character(len=*), intent(in), optional :: z1title,z2title,z3title,z4title
             end function gnuplot_plot3d
          end interface

!!$          Function : gnuplot_fitsurface
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. n : integer (number of points to be plotted along x)
!!$                 3. m : integer (number of points to be plotted along y)
!!$                 4. np : integer (number of adjustable parameters)
!!$                 5. x : real, dimension(n) (abcissae)
!!$                 6. y : real, dimension(m) (abcissae)
!!$                 7. z : real, dimension(n X m) (ordinate)
!!$                 8. gfit : character string : function to be fit (must be the RHS of the expression g(x,y)=...)
!!$                 9. vialist : character(len=1), dimension(np) : the via list (like 'a,b,c' (constants used in ffit)
!!$                 10. paraminit : real, dimension(np) : array containing initial values for the parameters (optional)
!!$                 11. fitlimit : real : FIT_LIMIT , default is usually 1e-6 (depends upon your gnuplot installation) (optional)
!!$                 12. fitlog : character string : filename of the fitting log (optional)
!!$                 13. plotresult : integer whether to plot the result (=1) or not (=0) with the data (optional)
!!$                 14. xlo : real : xrange low (optional)
!!$                 15. xhi : real : xrange high (optional)
!!$                 16. ylo : real : yrange low (optional)
!!$                 17. yhi : real : yrange high (optional)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Fits g(x,y) to supplied z vs x,y and plots the result if desired
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -3 : Called with less than one point
!!$                 3. status = -1 : Impossible error
!!$                 4. status = -2 : Cannot create temporary file
!!$                 6. status = 22 : Failed to delete temporary file
!!$                 7. status = -4 : Failed to write to temporary file
!!$                 8. status = -5 : No function specified
!!$                 9. status = -7 : Invalid function specification
!!$                 10. status = -8 : Invalid plot directive
!!$                 11. status = 23 : Specified fit log file will overwrite an existing file
!!$                 12. status = -9 : Failed to open temporary file temporary directory
!!$                 13. status = -10 : Could not execute the fit
!!$                 14. status = -11 : Could not plot the result
!!$                 15. status = -12 : Wrong initial paraminit array size
!!$                 16. status = -13 : Wrong vialist size
!!$                 17. status = -14 : Invalid input x/y range
!!$                 18. status = 24 : Failed to unset previously set acknowledgement string
!!$                 19. status = 25 : Failed to set acknowledgement string
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_fitsurface(ptr_gctrl,n,m,np,x,y,z,gfit,vialist,paraminit,xlo,xhi,ylo,yhi,fitlimit,fitlog,plotresult) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               real(dp), dimension(:), intent(in) :: x,y
               real(dp), dimension(:,:), intent(in) :: z
               character(len=*), intent(in) :: gfit
               character(len=1), dimension(:) :: vialist
               real(dp), intent(in), optional :: fitlimit
               integer(i4b), intent(in) :: n,m,np
               integer(i4b), intent(in), optional :: plotresult
               real(dp), dimension(:), intent(in), optional :: paraminit
               character(len=*), intent(in), optional :: fitlog
             end function gnuplot_fitcurve
          end interface

!!$          Function : gnuplot_plotslope
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. a : real (in a*x+b)
!!$                 3. b : real (in a*x+b)
!!$                 4. xlo : real (lower limit of xrange) (optional) ignored if a previous plot exists
!!$                 5. xhi : real (upper limit of xrange) (optional) ignored if a previous plot exists
!!$                 6. slopetitle : character string (title for slope)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Plots the specified slope line
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Called with invalid range (if present)
!!$                 3. status = -2 : Failed to plot slope
!!$                 4. status = -4 : Missing one of the two xrange specifications (if present)
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_plotslope(ptr_gctrl,a,b,slopetitle,xlo,xhi) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               real(dp), intent(in) :: a,b
               real(dp), intent(in), optional :: xlo,xhi
               character(len=*), intent(in) :: slopetitle
             end function gnuplot_plotslope
          end interface

!!$          Function : gnuplot_plotequation2d
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. equationstring : character string (string defining the function to be plotted)
!!$                 3. equationtitle : character string (title for equation)
!!$                 4. xlo : real (lower limit of xrange) (optional) ignored if a previous plot exists
!!$                 5. xhi : real (upper limit of xrange) (optional) ignored if a previous plot exists
!!$
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Plots a specified equation
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Called with invalid range (if present)
!!$                 3. status = -2 : Failed to plot equation
!!$                 4. status = -4 : Missing one of the two xrange specifications (if present)
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_plotequation2d(ptr_gctrl,equationstring,equationtitle,xlo,xhi) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*) :: equationstring
               real(dp), intent(in), optional :: xlo,xhi
               character(len=*), intent(in) :: equationtitle
             end function gnuplot_plotequation2d
          end interface

!!$          Function : gnuplot_plotequation3d
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. equationstring : character string (string defining the function to be plotted)
!!$                 3. equationtitle : character string (title for equation)
!!$                 4. xlo : real (lower limit of xrange) (optional) ignored if a previous plot exists
!!$                 5. xhi : real (upper limit of xrange) (optional) ignored if a previous plot exists
!!$                 6, ylo : real (lower limit of yrange) (optional) ignored if a previous plot exists
!!$                 7. yhi : real (upper limit of yrange) (optional) ignored if a previous plot exists
!!$
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Plots a specified equation
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Called with invalid range (if present)
!!$                 3. status = -2 : Failed to plot equation
!!$                 4. status = -4 : Missing one of the two x/y range specifications (if present)
!!$                 5. status = 24 : Failed to unset previously set acknowledgement string
!!$                 6. status = 23 : Failed to set acknowledgement string
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_plotequation3d(ptr_gctrl,equationstring,equationtitle,xlo,xhi) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*) :: equationstring
               real(dp), intent(in), optional :: xlo,xhi,ylo,yhi
               character(len=*), intent(in) :: equationtitle
             end function gnuplot_plotequation32
          end interface

!!$          Function : gnuplot_plotpolarequation
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. equationstring : character string (string defining the function to be plotted)
!!$                 3. equationtitle : character string (title for equation)
!!$
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Plots a specified equation r=f(t) in polar coordinates - invoke gnuplot_setcoordinatesystem first to set the polar coordinate system
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 3. status = -2 : Failed to plot equation
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_plotpolarequation(ptr_gctrl,equationstring,equationtitle) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*) :: equationstring
               character(len=*), intent(in) :: equationtitle
             end function gnuplot_plotpolarequation
          end interface

!!$          Function : gnuplot_plotcomplex
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. n : integer (number of points to be plotted)
!!$                 3. c1 : complex, dimension(n)
!!$                 4. c1title : character string (title for c1) (optional)
!!$                 5. c2 : complex, dimension(n) (optional)
!!$                 6. c2title : character string (title for c2) (optional)
!!$                 7. c3 : complex, dimension(n) (optional)
!!$                 8. c3title : character string (title for c3) (optional)
!!$                 9. c4 : complex, dimension(n) (optional)
!!$                 10. c4title : character string (title for c4) (optional)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Polar complex plots c1,c2,c3,c4 plot (max of 4 plots possible) in the z plane
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -3 : Called with less than one point
!!$                 3. status = -1 : Impossible error
!!$                 4. status = -2 : Cannot create temporary file
!!$                 5. status = -4 : Cannot allocate an internal array
!!$                 6. status = -5 : Cannot deallocate an internal array
!!$                 7. status = 22 : Failed to delete temporary file
!!$                 8. status = -6 : Failed to write to temporary file
!!$                 9. status = -7 : Failed to open temporary file in temporary directory
!!$                10. status = -8 : Failed to run plot / replot command
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_plotcomplex(ptr_gctrl,n,c1,c1title,c2,c2title,c3,c3title,c4,c4title) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               complex(dp), dimension(:), intent(in) :: c1
               complex(dp), dimension(:), intent(in), optional :: c2,c3,c4
               integer(i4b), intent(in) :: n
               character(len=*), intent(in), optional :: c1title,c2title,c3title,c4title
             end function gnuplot_plotcomplex
          end interface

!!$          Function : gnuplot_hardcopy
!!$          
!!$          Inputs :
!!$
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$                 2. plotfileformat : character string (format of the requested file)
!!$                 3. plotfilename : character string (file name for the plot file)
!!$                 4. extraarguments : character string (any user specified commands that come after "set terminal <terminal-type>", 
!!$                                     such as the enhanced mode for PostScript) (optional)
!!$                 5. pubflag : 3 character string (optional) (if set to 'PUB', the acknowledgement and date string will be suppressed)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Produces a hard(soft? :) )copy of the plot in a user specified format. Valid file formats are :
!!$                   a. 'PS' : PostScript
!!$                   b. 'EPS' : Encapsulated PostScript
!!$                   c. 'PDF' : Portable Document Format
!!$                   d. 'PNG' : Portable Network Graphics
!!$                   e. 'LATEX' : LaTeX format
!!$                   f. 'PSLATEX'
!!$                   g. 'EPSLATEX'
!!$                   h. 'PSTRICKS'
!!$                   i. 'HPGL' : HP Graphics Language
!!$                   j. 'HPDJ'
!!$                   k. 'FIG'
!!$                   l. 'DXF' : AutoCad format
!!$                   m. 'EEPIC'
!!$
!!$                  in addition to x11, which of course does not generate any "hardcopy" :)
!!$
!!$                    This function must be called immediately after gnuplot_init and before any plotting or set commands. 
!!$                    In case an unknown format is chosen, the function reverts to PostScript.
!!$                    If you choose the PDF format, you must have the epstopdf command available in your $PATH variable. The actual
!!$                    conversion from intermediary ps to pdf takes place in gnuplot_genpdf. 
!!$                    So gnuplot_genpdf must be called after this to ensure that the PDF file is generated
!!$                    The function does no file extension checking, so make sure that you supply the correct output filename.
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = 22 : Unknown format requested
!!$                 3. status = -2 : Failed to set requested (valid) terminal type
!!$                 4. status = -3 : Failed to set output file
!!$                 5. status = 23 : Failed to set acknowledgement string
!!$                 6. status = 24 : Failed to unset prior acknowledgement label
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_hardcopy(ptr_gctrl,plotfileformat,plotfilename,extraarguments,pubflag) result(status)
               type(gnuplot_ctrl), pointer :: ptr_gctrl
               character(len=*), intent(in) :: plotfileformat,plotfilename
               character(len=*), intent(in), optional :: extraarguments
               character(len=3), intent(in), optional :: pubflag
             end function gnuplot_hardcopy
          end interface

!!$          Function : gnuplot_genpdf
!!$          
!!$          Inputs :
!!$                 1. ptr_gctrl : pointer to structure of type gnuplot_ctr
!!$
!!$          Outputs :
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Produces a pdf - must be called after all the plot commands you want included have been executed
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Cannot generate PDF from the PS file.
!!$                 3. status = 22 : Called when the o/p format was not PDF
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
            integer function gnuplot_genpdf(ptr_gctrl) result(status)
              type(gnuplot_ctrl), pointer :: ptr_gctrl
            end function gnuplot_genpdf
          end interface

!!$          Function : gnuplot_plot3dpanel
!!$
!!$          Inputs :
!!$
!!$                 1. pf : character string (plot file name)
!!$                 2. pformat : character string (format of hardcopy)
!!$                 3. commontitle : character string (title used for the whole plot)
!!$                 4. xarray : real, dimension(n) (x array)
!!$                 5. xlabel : character string (x label)
!!$                 6. yarray : real, dimension(m) (y array)
!!$                 7. ylabel : character string (y label)
!!$                 8. zarray : real, dimension (n X m) (z array)
!!$                 9. zlabel : character string (z label)
!!$                 10. zarray1 : real, dimension (n X m) (z1 array) (optional)
!!$                 11. zlabel1 : character string (z1 label) (optional)
!!$                 12. zarray2 : real, dimension (n X m) (z2 array) (optional)
!!$                 13. zlabel2 : character string (z2 label) (optional)
!!$                 14. zarray3 : real, dimension (n X m) (z3 array) (optional)
!!$                 15. zlabel3 : character string (z3 label) (optional)
!!$                 16. logz : character string (len=3) (log scale or not) (optional)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Plots x,y,z,z1,z2,z3 plot (max of 4 surface plots possible)
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Array size mismatch
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_plot3dpanel(pf,pformat,commontitle,xarray,xlabel,yarray,ylabel,zarray,zlabel,&
                  zarray1,zlabel1,zarray2,zlabel2,zarray3,zlabel3,logz) result(status)
               character(len=*), intent(in) :: pf,pformat,commontitle
               real(dp), dimension(:), intent(in) :: xarray,yarray
               character(len=*), intent(in) :: xlabel,ylabel,zlabel
               real(dp), dimension(:,:), intent(in) :: zarray
               real(dp), dimension(:,:), intent(in), optional :: zarray1,zarray2,zarray3
               character(len=*), intent(in), optional :: zlabel1,zlabel2,zlabel3
               character(len=*), intent(in), optional :: logz
             end function gnuplot_plot3dpanel
          end interface

!!$          Function : gnuplot_plot2dset
!!$
!!$          Inputs :
!!$
!!$                 1. pf : character string (plot file name)
!!$                 2. pformat : character string (format of hardcopy)
!!$                 3. commontitle : character string (title used for the whole plot)
!!$                 4. xarray : real, dimension(n) (x array)
!!$                 5. xlabel : character string (x label)
!!$                 6. ylabel : character string (label for the y1 axis)
!!$                 7. y2label : character string (label for the y2 axis) (optional)
!!$                 8. yarray :  real, dimension(n) (y array)
!!$                 9. yaxis : character string (len=2) (axis used - y1 or y2) (optional)
!!$                 10. ykey : character string (ykey)
!!$                 11. yarray1 : real, dimension(n) (yarray1) (optional)
!!$                 12. yaxis1 : character string (len=2) (axis used - y1 or y2) (optional)
!!$                 13. ykey1 : character string (ykey1) (optional)
!!$                 14. yarray2 : real, dimension(n) (yarray2) (optional)
!!$                 15. yaxis2 : character string (len=2) (axis used - y1 or y2) (optional)
!!$                 16. ykey2 : character string (ykey2) (optional)
!!$                 17. yarray3 : real, dimension(n) (yarray3) (optional)
!!$                 18. yaxis3 : character string (len=2) (axis used - y1 or y2) (optional)
!!$                 19. ykey3 : character string (ykey3) (optional)
!!$                 20. logy1 : character string (len=3) (log scale or not -- for y1 axis) (optional)
!!$                 21. logy2 : character string (len=3) (log scale or not -- for y2 axis) (optional)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer (error flag)
!!$
!!$          Purpose : Plots x,y,y1,y2,y3 plot (max of 4 line plots possible)
!!$
!!$          Diagnostics :
!!$
!!$                 0. status = 0 : Success
!!$                 1. status = BAD_SIGNAL : Called with a dissociated pointer
!!$                 2. status = -1 : Array size mismatch
!!$
!!$          Type : public
!!$
!!$          Interface :
!!$
          interface
             integer function gnuplot_plot2dset(pf,pformat,commontitle,xarray,xlabel,ylabel,y2label,yarray,yaxis,ykey,&
                  yarray1,yaxis1,ykey1,yarray2,yaxis2,ykey2,yarray3,yaxis3,ykey3,logy1,logy2) result(status)
               character(len=*), intent(in) :: pf,pformat,commontitle
               real(dp), dimension(:), intent(in) :: xarray,yarray
               character(len=*), intent(in) :: xlabel,ylabel,ykey
               real(dp), dimension(:), intent(in), optional :: yarray1,yarray2,yarray3
               character(len=*), intent(in), optional :: y2label,ykey1,ykey2,ykey3
               character(len=*), intent(in), optional :: yaxis,yaxis1,yaxis2,yaxis3
               character(len=*), intent(in), optional :: logy1,logy2
             end function gnuplot_plot2dset
          end interface
