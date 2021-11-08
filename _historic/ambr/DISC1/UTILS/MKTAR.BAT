@Echo off

Rem  MKTAR.BAT -- utility for porting DUAL source files from DOS to UNIX.
Rem
Rem  Usage:  mktar dual         or
Rem          mktar ambr         or
Rem          mktar kb
Rem
Rem  MKTAR creates an archive named DUAL.TAR, AMBR.TAR, or KB.TAR in
Rem  the %TEMP% directory


Rem  MKTAR.BAT is a MS-DOS batch file that depends on the following .EXE files:
Rem    TAR.EXE      --  MS-DOS implementation of the UNIX 'tar' command  and
Rem    DOS2UNIX.EXE --  CR/LF--)LF convertor  (see DOS2UNIX.C for details).
Rem    These two files should be on the path.
Rem  It also uses the environment variable %TEMP% which should point to a
Rem    valid directory name on drive C:.
Rem
Rem  MKTAR converts all source files via DOS2UNIX in order to eliminate the
Rem    redundant CR characters in them.  It then collects the (converted) files
Rem    into a single .tar archive.  This archive is suitable for porting to
Rem    UNIX (e.g. by ftp).  The only thing that remains to be done in the UNIX
Rem    shell is to change the names of the files (e.g. DEFS.LSP --) defs.lisp).
Rem
Rem
Rem  The current version of MKTAR is very rigid -- all path names are wired
Rem    directly into the program.  This is due to the limitations of MS-DOS
Rem    batch language.  (It is difficult, for example, to descend recursively
Rem    to all subdirectories of a given directory.)
Rem
Rem  At present, MKTAR supports the following arguments:
Rem    + dual  -- handle directory D:\DUAL\DUAL and its subdirectories
Rem    + ambr  -- handle directory D:\DUAL\AMBR and its subdirs (except KB)
Rem    + kb    -- handle directory D:\DUAL\AMBR\KB and its subdirectories
Rem  It should be easy to add new options (e.g. directories with experiments).


:start
Rem  Check parameters
if "%1" ==""     goto :usage
if "%TEMP% ==""  goto :TEMP_missing

if "%1" == "dual" goto :DUAL
if "%1" == "DUAL" goto :DUAL
if "%1" == "ambr" goto :AMBR
if "%1" == "AMBR" goto :AMBR
if "%1" == "kb"   goto :KB
if "%1" == "KB"   goto :KB
goto  :no_such_arg

:usage
Echo  Usage:   %0 dual   or
Echo           %0 ambr   or
Echo           %0 kb
goto :end

:TEMP_missing
Echo  The environment variable TEMP has no value.
Echo  Set it to a valid path name on drive C: and re-run %0
goto :end

:no_such_arg
Echo  Unrecognized argument -- %1
goto :end


:DUAL
Rem  Porting  D:\DUAL\DUAL, D:\DUAL\DUAL\ARCHIT, and D:\DUAL\DUAL\INTRFACE.
C:
deltree %TEMP%\dual.$$$
mkdir   %TEMP%\dual.$$$
mkdir   %TEMP%\dual.$$$\archit
mkdir   %TEMP%\dual.$$$\intrface

D:
cd  \dual\dual
cd
for %%f in (*.*) do  dos2unix %%f %TEMP%\dual.$$$\%%f
cd  \dual\dual\archit
cd
for %%f in (*.*) do  dos2unix %%f %TEMP%\dual.$$$\archit\%%f
cd  \dual\dual\intrface
cd
for %%f in (*.*) do  dos2unix %%f %TEMP%\dual.$$$\intrface\%%f

C:
cd %TEMP%\dual.$$$
tar cvf ..\dual.tar *.*  > nul

cd %TEMP%
Echo.
if     exist dual.tar  Echo %TEMP%\dual.tar  created
if not exist dual.tar  Echo Could not create %TEMP%\dual.tar
Echo.
deltree %TEMP%\dual.$$$

D:
cd \dual
goto end


:AMBR
Rem  Porting  D:\DUAL\AMBR and =\INTRFACE only.
Rem  Other directories (and in particular  D:\DUAL\AMBR\KB) not ported.
C:
deltree %TEMP%\ambr.$$$
mkdir   %TEMP%\ambr.$$$
mkdir   %TEMP%\ambr.$$$\intrface

D:
cd  \dual\ambr
cd
for %%f in (*.*) do  dos2unix %%f %TEMP%\ambr.$$$\%%f
cd  \dual\ambr\intrface
cd
for %%f in (*.*) do  dos2unix %%f %TEMP%\ambr.$$$\intrface\%%f

C:
cd %TEMP%\ambr.$$$
tar cvf ..\ambr.tar *.*  > nul

cd %TEMP%
Echo.
if     exist ambr.tar  Echo %TEMP%\ambr.tar  created
if not exist ambr.tar  Echo Could not create %TEMP%\dual.tar
Echo.
deltree %TEMP%\ambr.$$$

D:
cd \dual
goto end


:KB
Rem  Porting  D:\DUAL\AMBR\KB and its subdirs SEMANTIC and EPISODIC.
Rem  Other directories (and in particular  D:\DUAL\AMBR\KB\GENKB) not ported.
C:
deltree %TEMP%\kb.$$$
mkdir   %TEMP%\kb.$$$
mkdir   %TEMP%\kb.$$$\semantic
mkdir   %TEMP%\kb.$$$\episodic

D:
cd  \dual\ambr\kb
cd
for %%f in (*.*) do  dos2unix %%f %TEMP%\kb.$$$\%%f
cd  \dual\ambr\kb\semantic
cd
for %%f in (*.*) do  dos2unix %%f %TEMP%\kb.$$$\semantic\%%f
cd  \dual\ambr\kb\episodic
cd
for %%f in (*.*) do  dos2unix %%f %TEMP%\kb.$$$\episodic\%%f

C:
cd %TEMP%\kb.$$$
tar cvf ..\kb.tar *.*  > nul

cd %TEMP%
Echo.
if     exist kb.tar  Echo %TEMP%\kb.tar  created
if not exist kb.tar  Echo Could not create %TEMP%\kb.tar
Echo.
deltree %TEMP%\kb.$$$

D:
cd \dual
goto end


:end
