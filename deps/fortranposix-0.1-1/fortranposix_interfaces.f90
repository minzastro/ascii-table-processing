!!$ fortranposix_interfaces.f90 - Contains canned interfaces for the fortranposix library
!!$
!!$     This file is a part of the fortranposix library. This makes system calls on Linux and Unix like systems available to Fortran programs.
!!$     Copyright (C) 2004  Madhusudan Singh 
!!$
!!$     This library is free software; you can redistribute it and/or 
!!$     modify it under the terms of the GNU Lesser General Public 
!!$     License as published by the Free Software Foundation; either 
!!$     version 2.1 of the License, or (at your option) any later version.
!!$
!!$     This library is distributed in the hope that it will be useful,
!!$     but WITHOUT ANY WARRANTY; without even the implied warranty of 
!!$     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
!!$     Lesser General Public License for more details. 
!!$
!!$     You should have received a copy of the GNU Lesser General Public 
!!$     License along with this library; if not, write to the Free Software 
!!$     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 
!!$
!!$       I would strongly welcome bug reports, feature enhancement requests, compliments, donations and job offers :) My email address : msc@ieee.org
!!$

!!$          Function : fortran_pipebufsize
!!$
!!$          Inputs :
!!$
!!$                 None
!!$
!!$          Outputs :
!!$
!!$                 1. pipe_buf_size : integer : size of the buffer that can be atomically written to a pipe
!!$
!!$          Purpose : Finds the size of the buffer that can be atomically written to a pipe
!!$
!!$          Diagnostics :
!!$
!!$                 None
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_pipebufsize() result(pipe_buf_size)
                   end function fortran_pipebufsize
                 end interface

!!$          Function : fortran_popen
!!$
!!$          Inputs :
!!$
!!$                 1. cmd : character string : Command to be piped
!!$                 2. len : character string (len = 1) : type of pipe to be opened
!!$
!!$          Outputs :
!!$
!!$                 1. fd : integer : file descriptor for the pipe
!!$
!!$          Purpose : Mimics the popen() system call
!!$
!!$          Diagnostics (incomplete) :
!!$
!!$                 1. fd = -1 : failed to initiate the popen() call
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_popen(cmd,type) result(fd)
                     character(len=*), intent(in) :: cmd
                     character(len=*), intent(in) :: type
                   end function fortran_popen
                 end interface

!!$          Function : fortran_pclose
!!$
!!$          Inputs :
!!$
!!$                 1. fd : integer : file descriptor for the pipe to be closed
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer : error flag
!!$
!!$          Purpose : Mimics the pclose() system call
!!$
!!$          Diagnostics :
!!$
!!$                 1. status = -1 : failed to close pipe
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_pclose(fd) result(status)
                     integer, intent(in) :: fd
                   end function fortran_pclose
                 end interface

!!$          Function : fortran_access
!!$
!!$          Inputs :
!!$
!!$                 1. pathname : character string : pathname for the file to be checked
!!$                 2. mode : integer : 
!!$                                     4 - check for read permissions
!!$                                     2 - check for write permissions
!!$                                     1 - check for execute permissions
!!$                                     0 - check for existence
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer : error flag
!!$
!!$          Purpose : Mimics the access() system call
!!$
!!$          Diagnostics :
!!$
!!$                 1. status = -1 : failed to check for permissions
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_access(pathname,mode) result(status)
                     character(len=*), intent(in) :: pathname
                     integer, intent(in) :: mode
                   end function fortran_access
                 end interface

!!$          Function : fortran_fflush
!!$
!!$          Inputs :
!!$
!!$                 1. fd : integer : file descriptor for the pipe to be flushed
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer : error flag
!!$
!!$          Purpose : Mimics the fflush() system call
!!$
!!$          Diagnostics :
!!$
!!$                 1. status = -1 : failed to flush the pipe
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_fflush(fd) result(status)
                     integer, intent(in) :: fd
                   end function fortran_fflush
                 end interface

!!$          Function : fortran_getcwd
!!$
!!$          Inputs :
!!$
!!$                 None
!!$
!!$          Outputs :
!!$
!!$                 1. buf : character string : contains the CWD upon successful exit
!!$                 2. status : integer : error flag
!!$
!!$          Purpose : Mimics the getcwd() system call
!!$
!!$          Diagnostics :
!!$
!!$                 1. status = -1 : failed to get current working directory
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_getcwd(buf) result(status)
                     character(len=*), intent(out) :: buf
                   end function fortran_getcwd
                 end interface

!!$          Function : fortran_gethostname
!!$
!!$          Inputs :
!!$
!!$                 None
!!$
!!$          Outputs :
!!$
!!$                 1. buf : character string : contains the hostname upon successful exit
!!$                 2. status : integer : error flag
!!$
!!$          Purpose : Mimics the gethostname() system call
!!$
!!$          Diagnostics :
!!$
!!$                 1. status = -1 : failed to get hostname
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_gethostname(hostname) result(status)
                     character(len=*), intent(out) :: hostname
                   end function fortran_gethostname
                 end interface

!!$          Function : fortran_getlogin
!!$
!!$          Inputs :
!!$
!!$                 None
!!$
!!$          Outputs :
!!$
!!$                 1. buf : character string : contains the login of the current user upon successful exit
!!$                 2. status : integer : error flag
!!$
!!$          Purpose : Mimics the getlogin() system call safely through the getpwuid(getuid) mechanism
!!$
!!$          Diagnostics :
!!$
!!$                 1. status = -1 : failed to get current user's login name
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_getlogin(loginstring) result(status)
                     character(len=*), intent(out) :: loginstring
                   end function fortran_getlogin
                 end interface

!!$          Function : fortran_getenv
!!$
!!$          Inputs :
!!$
!!$                 1. varnamestring : character string containing the name of the environment variable
!!$
!!$          Outputs :
!!$
!!$                 1. environmentstring : character string : contains the environment variable upon successful exit
!!$                 2. status : integer : error flag
!!$
!!$          Purpose : Mimics the getenv() system call
!!$
!!$          Diagnostics :
!!$
!!$                 1. status = -1 : failed to get the value of the requested environment variable
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_getenv(varnamerequested,environmentstring) result(status)
                     character(len=*), intent(in) :: varnamerequested
                     character(len=*), intent(out) :: environmentstring
                   end function fortran_getenv
                 end interface

!!$          Function : fortran_chdir
!!$
!!$          Inputs :
!!$
!!$                 1. pathname : character string : directory to change to
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer : error flag
!!$
!!$          Purpose : Mimics the chdir() system call
!!$
!!$          Diagnostics :
!!$
!!$                 1. status = -1 : failed to change directory
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_chdir(pathname) result(status)
                     character(len=*), intent(in) :: pathname
                   end function fortran_chdir
                 end interface

!!$          Function : fortran_fputs
!!$
!!$          Inputs :
!!$
!!$                 1. buf : character string : buffer to be written to fd
!!$                 2. fd : integer : file descriptor
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer : error flag
!!$
!!$          Purpose : Mimics the fputs() system call
!!$
!!$          Diagnostics :
!!$
!!$                 1. status = -1 : failed to write the string
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_fputs(buf,fd) result(status)
                     character(len=*), intent(in) :: buf
                     integer, intent(in) :: fd
                   end function fortran_fputs
                 end interface

!!$          Function : fortran_fgets
!!$
!!$          Inputs :
!!$
!!$                 1. fd : integer : file descriptor
!!$
!!$          Outputs :
!!$
!!$                 1. buf : character string : buffer to be read from fd
!!$                 2. status : integer : error flag
!!$
!!$          Purpose : Mimics the fgets() system call
!!$
!!$          Diagnostics :
!!$
!!$                 1. status = -1 : failed to read the string
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_fgets(buf,fd) result(status)
                     character(len=*), intent(out) :: buf
                     integer, intent(in) :: fd
                   end function fortran_fgets
                 end interface

!!$          Function : fortran_mkdir
!!$
!!$          Inputs :
!!$
!!$                 1. dirname : character string : name of the directory to make
!!$                 2. mode : character string : string containing the usual chmod style permission bits (octal)
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer : error flag
!!$
!!$          Purpose : Mimics the mkdir() system call
!!$
!!$          Diagnostics :
!!$
!!$                 1. status = -1 : failed to make directory
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_mkdir(dirname,mode) result(status)
                     character(len=*), intent(in) :: dirname,mode
                   end function fortran_mkdir
                 end interface

!!$          Function : fortran_rmdir
!!$
!!$          Inputs :
!!$
!!$                 1. dirname : character string : name of the directory to remove
!!$
!!$          Outputs :
!!$
!!$                 1. status : integer : error flag
!!$
!!$          Purpose : Mimics the rmdir() system call
!!$
!!$          Diagnostics :
!!$
!!$                 1. status = -1 : failed to remove directory
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_rmdir(dirname) result(status)
                     character(len=*), intent(in) :: dirname
                   end function fortran_rmdir
                 end interface

!!$          Function : fortran_getpid
!!$
!!$          Inputs :
!!$
!!$                 None
!!$
!!$          Outputs :
!!$
!!$                 1. pid : Process ID of the calling process
!!$
!!$          Purpose : Mimics the getpid() system call
!!$
!!$          Diagnostics :
!!$
!!$                 None
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_getpid() result(pid)
                   end function fortran_getpid
                 end interface

!!$          Function : fortran_getppid
!!$
!!$          Inputs :
!!$
!!$                 None
!!$
!!$          Outputs :
!!$
!!$                 1. ppid : Parent's Process ID of the calling process
!!$
!!$          Purpose : Mimics the getppid() system call
!!$
!!$          Diagnostics :
!!$
!!$                 None
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_getppid() result(ppid)
                   end function fortran_getppid
                 end interface

!!$          Function : fortran_getchar
!!$
!!$          Inputs :
!!$
!!$                 None
!!$
!!$          Outputs :
!!$
!!$                 1. readchar : character
!!$
!!$          Purpose : Reads a character from stdin
!!$
!!$          Diagnostics :
!!$
!!$                 None
!!$
!!$          Interface :
!!$
                 interface
                   integer function fortran_getchar(readchar) result(status)
                     character(len=1), intent(out) :: readchar
                   end function fortran_getchar
                 end interface
