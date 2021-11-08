# Some common definitions...
RM=rm -f
CC=gcc

# Compiler flags and include and library paths
# Change these variables as necessary for your system
CFLAGS= -g -Wall
INCLUDES= -I. -I/usr/X11/include
LPATHS= -L/usr/X11/lib
LIBS= -lXaw -lXmu -lXt -lXext -lX11 -lm

# Rule to create .o  files from .c files
.c.o:
	$(RM) $@
	$(CC) -c  $(CFLAGS) $(INCLUDES) $*.c

# Targets...
all:: discern
main.o qa.o hfm.o trace.o lex.o graph.o: globals.c xglobals.c discerndefs.h

discern: main.o qa.o hfm.o trace.o lex.o graph.o gwin.o
	$(RM) $@
	$(CC) -o $@ $(CFLAGS) main.o qa.o hfm.o trace.o lex.o graph.o gwin.o $(LPATHS) $(LIBS)
