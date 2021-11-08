;;; -*- Mode: ASCII Text -*-

;;; FILE:       DUAL/archit/tests/readme.txt
;;; VERSION:    1.1.2      ; see DUAL/VERSION.LSP
;;; PURPOSE:    Test cases for functions defined in DUAL/ARCHIT/BASIC.LSP
;;; DEPENDS-ON: dual/utils.lsp,  dual/archit/basic.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...


The directory DUAL/ARCHIT/TEST contains some obsolete files.
At first, I had the good intention to maintain a test bank that checks all
programs systematically. The files in this directory are an effort in this
direction. They consist mostly of ASSERT clauses and should load peacefully
if everything is OK.

Making a test bank turned out enormously time consuming, however.  I estimated
that making and maintaining a TEST file takes roughly as much time as making
the main source file (in the ARCHIT directory).  Therefore, I abandoned the
whole idea.  The few files that I did produce, however, are kept in this
directory. They are not loaded by the DEFSYSTEM definition (see DUAL/DEFSYS.LSP)

The test files gradually grew out of date. Thus, their only use now could be
to serve as templates.  Some day I might return to them if I do not have more
important things to do (which I doubt).


;;; End of file DUAL/ARCHIT/TEST/README.TXT
