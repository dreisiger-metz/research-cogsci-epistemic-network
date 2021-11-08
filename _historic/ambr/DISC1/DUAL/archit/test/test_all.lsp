;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-

;;; FILE:       DUAL/archit/tests/test_all.lsp
;;; VERSION:    1.1
;;; PURPOSE:    Test cases for all programs in the ARCHIT directory
;;; DEPENDS-ON: DUAL/archit/tests/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov
;;; CREATED:    20-2-97 [1.0]
;;; UPDATED:    8-11-97 [1.1]  The 'official release'
;;; UPDATED:    ...


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;   T E S T   C A S E S   FOR ALL PROGRAMS  ;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;  The directory ARCHIT/TESTS contains test banks for each module of
;;;  the architecture. In general, each file in ARCHIT has a corresponding
;;;  file in ARCHIT/TESTS with the same name. For instance, LISP functions
;;;  defined in ARCHIT/BASIC.LSP are tested by ARCHIT/TESTS/BASIC.LSP .
;;;
;;;  Each test file consists mainly of ASSERT clauses that check whether the
;;;  documented external protocol of one or more functions is observed.
;;;  No checks are made about undocumented or internal features.
;;;
;;;  To run all tests, simply load this file. You may also load individual
;;;  test files; each one of them is self-contained.


(format t "~%;; If everything is OK, this file should load without errors.")
(format t "~%;; (There may be compilation warnings; after all, this is a trial by fire.)~%")

(let ((*load-verbose* nil))
  (load "C:\\dual\\dual\\archit\\tests\\basic.lsp")
  (load "C:\\dual\\dual\\archit\\tests\\symref.lsp")
  (load "C:\\dual\\dual\\archit\\tests\\conref.lsp")
  (load "C:\\dual\\dual\\archit\\tests\\act_fun.lsp")
  (load "C:\\dual\\dual\\archit\\tests\\connect.lsp")
  ;; more LOADs
)


(format t "~%;; ****  All tests completed.   ****~%")

