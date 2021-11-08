## =============================================================================
## Filename          : $RCSfile: Makefile,v $
## Version           : $Revision: 1.1.1.1 $
## Release           : $Name:  $
##
## Original author   : Peter Dreisiger, MOD, DSTO Stirling
## Original date     : 12-Feb-2009
## Last modified by  : $Author: prd $
## Last modified on  : $Date: 2009/03/12 06:03:32 $
##
## Security Class.   : UNCLASSIFIED
## Copyright         : DSTO
##
## Purpose           : Makefile for the FGREP episode generator
##
## Revision History  :
##
## Note              : Debug flags include: __DEBUG (generic), 
##                     __DEBUG__INTERACTIVE_TESTS (to enable the TestRunner's
##                     interactive tests)
## =============================================================================
CXX = 		g++
LD = 		g++

INCLUDES =      -I/usr/local/include -I../../include
CPPFLAGS = 	$(INCLUDES) -Wall 
CPPFLAGS_TEST = $(CPPFLAGS)

RELEASEFLAGS =	-O2
DEBUGFLAGS =	-g -D__DEBUG # -D__DEBUG__INTERACTIVE_TESTS
LIBS =          -L../../lib -lFGREP
LIBS_TEST =	-L/usr/local/lib/cppunit/lib -L../../lib/debug -lcppunit


OBJS = 		\
		Concept.o
		


DOBJS =		$(OBJS:.o=.do)


DOBJS_TEST =	


all:		experiment-1a

test:		$(DOBJS_TEST) test-$(TARGET).do
		$(CXX) $(DOBJS_TEST) test-$(TARGET).do $(LIBS_TEST) -o test-$(TARGET)

experiment-1a:	$(OBJS) Experiment-1A--Entities.o Experiment-1A--Generator.o
		mkdir -p ../bin
		$(CXX) $(OBJS) Experiment-1A--Entities.o Experiment-1A--Generator.o $(LIBS) -o experiment-1a
		cp experiment-1a ../bin

install:
	

%.o:		%.cpp %.hpp
		$(CXX) $(CPPFLAGS) $(RELEASEFLAGS) -c $< -o $*.o

%.do:		%.cpp %.hpp
		$(CXX) $(CPPFLAGS) $(DEBUGFLAGS) -c $< -o $*.do

%.do:		%.cpp
		$(CXX) $(CPPFLAGS) $(DEBUGFLAGS) -c $< -o $*.do


clean:
		rm -f *.o *.do *.*~ test-$(TARGET)

cleaner:
		rm -f *.o *.do *.*~ test-$(TARGET) generator
