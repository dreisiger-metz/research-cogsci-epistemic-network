## =============================================================================
## Filename          : $RCSfile: Makefile,v $
## Version           : $Revision: 1.1.1.1 $
## Release           : $Name:  $
##
## Original author   : Peter Dreisiger, MOD, DSTO Stirling
## Original date     : 30-Oct-2008
## Last modified by  : $Author: prd $
## Last modified on  : $Date: 2009/03/12 05:05:54 $
##
## Security Class.   : UNCLASSIFIED
## Copyright         : DSTO
##
## Purpose           : Makefile for the VariableRateExecution library
##
## Revision History  :
##
## Note              : Debug flags include: __DEBUG (generic), 
##                     __DEBUG__INTERACTIVE_TESTS (to enable the TestRunner's
##                     interactive tests)
## =============================================================================
CXX = 		g++
LD = 		g++
TARGET =	VariableRateExecution
FULL_VERSION =	0.1
MAJOR_VERSION =	0.1
TARGET_DEST =	../..

INCLUDES =      -I/usr/local/include
CPPFLAGS = 	$(INCLUDES) -fPIC -Wall 
CPPFLAGS_TEST = $(CPPFLAGS) -I$(TARGET_DEST)/include

RELEASEFLAGS =	-O2
DEBUGFLAGS =	-g -D__DEBUG # -D__DEBUG__INTERACTIVE_TESTS
LIBS =          -lpthread
LIBS_TEST =	$(LIBS) -L/usr/local/lib/cppunit/lib -L$(TARGET_DEST)/lib/debug -ldl -lcppunit -l$(TARGET)
ifeq ($(strip $(OSTYPE)),linux-gnu)
	TARGET_NAME = lib$(TARGET).so
	LDFLAGS = -shared -Wl,-soname,$(TARGET_DEST)/lib/$(TARGET_NAME).$(MAJOR_VERSION)
	DLDFLAGS = -shared -Wl,-soname,$(TARGET_DEST)/lib/debug/$(TARGET_NAME).$(MAJOR_VERSION)
else
	TARGET_NAME = lib$(TARGET).dylib
	LDFLAGS = -dynamiclib -Wl,-install_name,$(TARGET_DEST)/lib/$(TARGET_NAME).$(MAJOR_VERSION)
	DLDFLAGS = -dynamiclib -Wl,-install_name,$(TARGET_DEST)/lib/debug/$(TARGET_NAME).$(MAJOR_VERSION)
endif


OBJS = 		\
		Agent.o \
		Manager.o \
		ManagerConsole.o \
		Message.o


DOBJS =		$(OBJS:.o=.do)


DOBJS_TEST =	\
		AgentTest.do \
		ManagerTest.do \
		TestAgent.do \
		test-$(TARGET).do


all:		debug test

release:	$(OBJS)
		mkdir -p $(TARGET_DEST)/lib $(TARGET_DEST)/include/$(TARGET)
		$(CXX) -shared -fPIC -Wl,$(LDFLAGS) \
			-o $(TARGET_DEST)/lib/$(TARGET_NAME).$(FULL_VERSION) $(OBJS) $(LIBS)
		strip --strip-unneeded $(TARGET_DEST)/$(TARGET_NAME).$(FULL_VERSION)
		chmod ugo+x $(TARGET_DEST)/lib/$(TARGET_NAME).$(FULL_VERSION)
		ln -sf $(TARGET_NAME).$(MAJOR_VERSION) $(TARGET_DEST)/lib/$(TARGET_NAME) && true
		cp *.hpp $(TARGET_DEST)/include/$(TARGET)

debug:		$(DOBJS)
		mkdir -p $(TARGET_DEST)/lib/debug $(TARGET_DEST)/include/$(TARGET)
		$(CXX) -fPIC $(DLDFLAGS) \
			-o $(TARGET_DEST)/lib/debug/$(TARGET_NAME).$(FULL_VERSION) $(DOBJS) $(LIBS)
		chmod ugo+x $(TARGET_DEST)/lib/debug/$(TARGET_NAME).$(FULL_VERSION)
		ln -sf $(TARGET_NAME).$(MAJOR_VERSION) $(TARGET_DEST)/lib/debug/$(TARGET_NAME) && true
		cp *.hpp $(TARGET_DEST)/include/$(TARGET)

test:		$(DOBJS_TEST)
		$(CXX) $(DOBJS_TEST) $(LIBS_TEST) -o test-$(TARGET)

docs:		
		doxygen

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
		rm -f *.o *.do *.*~ test-$(TARGET)
		rm -f ../../lib/lib$(TARGET)* ../../lib/debug/lib$(TARGET)*
