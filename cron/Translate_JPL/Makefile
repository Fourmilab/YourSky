# MAKEFILE
#
# Input file for 'make' utility. Builds DASTCOM5 library and executables. 
#
# Review settings below and customize for local system if necessary by setting
# variables and selecting compiler. When configured as desired, build software
# by typing: 'make all'
#
# Explanation:
#
#  (1) The 'LIBDXREAD' and 'EXEDXREAD' variables can be altered to point to 
#       a different destination for the library archive and executables, if 
#       desired.
#
#  (2) 'BITDXREAD' can be altered as necesary to select which memory model 
#       (32 or 64-bit) is required for compatibility with user applications. 
#       Options are '-m32' or '-m64'
#
#  (3) Example settings for four FORTRAN compilers are shown (FC and FFLAGS
#       variables). It may be possible to simply alter comment-markers to 
#       select the desired compiler. If not, create new FC and FFLAGS as 
#       needed.
#
#  (4) Once Makefile is customized for the local system, it can be renamed
#       'makefile' (lower-case). This will prevent it from being 
#       over-written during future releases while giving local changes 
#       execution precedence over the default distribution 'Makefile' 
#       (upper-case). However, keep an eye on "news.txt" in case an 
#       additional program or subroutine is added to the distribution, 
#       and the distribution Makefile is changed to include it.
#
# DO NOT USE COMPILER OPTIONS THAT CHANGE DATA-TYPE LENGTH: NO PROMOTIONS 
# FROM INTEGER*4 TO INTEGER*8, OR REAL*8 TO REAL*10 OR REAL*16, ETC.
#
# Modification History:
#
#  DATE         Who  Change
#  -----------  ---  -------------------------------------------------------
#  2013-Aug-05  JDG  Version 1.0
#  2013-Aug-13  JDG  Added optional Intel FORTRAN compile; made memory model
#                     a variable setting
#  2014-Mar-28  JDG  Tweak Makefile comments
#
# Key:
#  JDG= Jon.D.Giorgini@jpl.nasa.gov
#
# Jet Propulsion Laboratory, NASA/California Institute of Technology,
# Solar System Dynamics Group, 4800 Oak Grove Drive, Pasadena, CA 91109 USA
#---------------------------------------------------------------------------
#
# Location to place local subroutine library and executables
#
 LIBDXREAD = .
 EXEDXREAD = .
#
# Specify local memory model for compiled objects ('-m32' or '-m64' for 32 
#  or 64-bit respectively); whichever is needed for compatibility with the 
#  user applications LIBDXREAD will be linked to on the user system.
#
 BITDXREAD = -m64
#
# Compiler selection and settings:
#
#------------------------------------------------------------------------
#
# GFORTRAN compile/link flags
   FFLAGS= -Bstatic
   FC    = gfortran $(BITDXREAD)
#
#
# List of executables in package
 PROGRAMS  =  trans_ast trans_comet
#
# List of object files in library
 OBJECTS   = dxread.o d3legr.o d4legr.o dltxch.o dxcasgn.o dxcodlk.o \
             dxnasgn.o dxsump.o dxsuplq.o getuni2.o i2swap.o i4swap.o \
             ladjust.o lentrm.o lfttrm.o ltlend.o r4swap.o r8swap.o ucase3.o \
             getflnm.o greg2jd.o jul2jd.o
#
# Build all
 all: $(OBJECTS) $(PROGRAMS)
#
# Remove build
 clean:
	rm -rf *.o $(PROGRAMS) $(LIBDXREAD)/libdxread.a
#
# Executables
#
        
 trans_ast: trans_ast.o $(OBJECTS)
	$(FC) $(FFLAGS) trans_ast.o $(LIBDXREAD)/libdxread.a -o $(EXEDXREAD)/trans_ast 
        
 trans_comet: trans_comet.o $(OBJECTS)
	$(FC) $(FFLAGS) trans_comet.o $(LIBDXREAD)/libdxread.a -o $(EXEDXREAD)/trans_comet 

#
# Object files for library
#
 dxread.o: dxread.f
	$(FC) -c dxread.f
	ar -crs $(LIBDXREAD)/libdxread.a dxread.o

 d3legr.o: d3legr.f
	$(FC) -c d3legr.f
	ar -crs $(LIBDXREAD)/libdxread.a d3legr.o

 d4legr.o: d4legr.f
	$(FC) -c d4legr.f
	ar -crs $(LIBDXREAD)/libdxread.a d4legr.o

 dltxch.o: dltxch.f
	$(FC) -c dltxch.f
	ar -crs $(LIBDXREAD)/libdxread.a dltxch.o

 dxcasgn.o: dxcasgn.f
	$(FC) -c dxcasgn.f
	ar -crs $(LIBDXREAD)/libdxread.a dxcasgn.o

 dxcodlk.o: dxcodlk.f
	$(FC) -c dxcodlk.f
	ar -crs $(LIBDXREAD)/libdxread.a dxcodlk.o

 dxnasgn.o: dxnasgn.f
	$(FC) -c dxnasgn.f
	ar -crs $(LIBDXREAD)/libdxread.a dxnasgn.o

 dxsump.o: dxsump.f
	$(FC) -c dxsump.f
	ar -crs $(LIBDXREAD)/libdxread.a dxsump.o

 dxsuplq.o: dxsuplq.f
	$(FC) -c dxsuplq.f
	ar -crs $(LIBDXREAD)/libdxread.a dxsuplq.o

 getuni2.o: getuni2.f
	$(FC) -c getuni2.f
	ar -crs $(LIBDXREAD)/libdxread.a getuni2.o

 i2swap.o: i2swap.f
	$(FC) -c i2swap.f
	ar -crs $(LIBDXREAD)/libdxread.a i2swap.o

 i4swap.o: i4swap.f
	$(FC) -c i4swap.f
	ar -crs $(LIBDXREAD)/libdxread.a i4swap.o

 ladjust.o: ladjust.f
	$(FC) -c ladjust.f
	ar -crs $(LIBDXREAD)/libdxread.a ladjust.o

 lentrm.o: lentrm.f
	$(FC) -c lentrm.f
	ar -crs $(LIBDXREAD)/libdxread.a lentrm.o

 lfttrm.o: lfttrm.f
	$(FC) -c lfttrm.f
	ar -crs $(LIBDXREAD)/libdxread.a lfttrm.o

 ltlend.o: ltlend.f
	$(FC) -c ltlend.f
	ar -crs $(LIBDXREAD)/libdxread.a ltlend.o

 r4swap.o: r4swap.f
	$(FC) -c r4swap.f
	ar -crs $(LIBDXREAD)/libdxread.a r4swap.o

 r8swap.o: r8swap.f
	$(FC) -c r8swap.f
	ar -crs $(LIBDXREAD)/libdxread.a r8swap.o

 ucase3.o: ucase3.f
	$(FC) -c ucase3.f
	ar -crs $(LIBDXREAD)/libdxread.a ucase3.o

 getflnm.o: getflnm.f
	$(FC) -c getflnm.f
	ar -crs $(LIBDXREAD)/libdxread.a getflnm.o

 greg2jd.o: greg2jd.f
	$(FC) -c greg2jd.f
	ar -crs $(LIBDXREAD)/libdxread.a greg2jd.o

 jul2jd.o: jul2jd.f
	$(FC) -c jul2jd.f
	ar -crs $(LIBDXREAD)/libdxread.a jul2jd.o
