 ;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/semantic/spat_rel.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Semantic memory -- spatial relations such as ABOVE and LONG.
;;; DEPENDS-ON: ambr/packages.lsp, ambr/intrface/defagent.lsp, =/kb_util.lsp ;
;;;             ambr/kb/abstract.lsp
;;; XREFS:      ambr/kb/temp_rel.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    14-04-98 [3.0]  Used parts of old LTM.LSP.
;;; UPDATED:    ...


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;     S P A T I A L   R E L A T I O N S     ;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package  "AMBR")

;;;; This file defines some spatial relations for the semantic memory of AMBR.
;;;; See file AMBR/KB/PHYS_REL.LSP for the so-called 'physical/spatial rela-
;;;; tions' such as IN-TOUCH-WITH.
;;;; See also file AMBR/KB/TEMP_REL.LSP which deals with temporal relations.


;;; WARNING!!!   The Eliza effect  can be dangerous  to your   WARNING!!!
;;; WARNING!!!   comprehension of cognitive modeling. Please   WARNING!!!
;;; WARNING!!!   examine  all symbols carefully  before use.   WARNING!!!
;;;
;;; With full awareness that AMBR agents are nothing but ungrounded symbols,
;;; we follow the common AI terminology and say that they stand for 'things
;;; in the world'.  We also use mnemonic agent names like FIRE, CAUSE, etc.
;;; Those names are irrelevant for the model itself; the program would work
;;; just as well (or as bad) had the agents been named AG001, AG002, etc.
;;; Indeed, the first version of AMBR (Kokinov, 1994) used such void names.
;;; It was very instructive from a philosophical point of view as it laid
;;; bare how little 'knowledge' the program actually had.  It was not very
;;; practical, however, because it hindered enourmously the process of
;;; developing, tuning, testing, and documenting the model.     [DR#1, p.67]


;; The agents defined in this file depend on the abstract concept-agents
;; provided by AMBR/KB/ABSTRACT.LSP.  Some of the most important of these are:

(require-agents '(relation
                  binary-relation
                  mirror-rel
                  polar-rel
                  object
                  *other*
))


;;;;   *****   GENERAL SPATIAL RELATIONS  *****
;;
;; The detailed lattice below prevents the marker
;; passing mechanism from generating too many
;; 'ACME-like' hypotheses.

(defagent   spatial-relation    concept-agent
  :type    (:concept  :relation)
  :subc    relation
  :superc  (unary-spat-rel
            bin-spat-rel
            (*other* 2.0) )
  :slot1   "operand(s)"
    :type     :aspect
    :subc     (relation . :slot1)
)
(with-agent  relation
  :superc  (spatial-relation 0.5)
)

(defagent   unary-spat-rel    concept-agent
  "Unary spatial relation (i.e. attribute)."
  :type     (:concept  :relation)
  :subc     ((spatial-relation 1.0)
             (unary-relation    0.1) )
  :superc   ((long     0.5)
             (short    0.5)
             (*other*  2.0) )
  :slot1   "operand"
    :type     :aspect
    :subc     (((spatial-relation . :slot1) 1.0)
               ((unary-relation   . :slot1) 0.1) )
)

(defagent   bin-spat-rel    concept-agent
  "Binary spatial relation"
  :type     (:concept  :relation)
  :subc     ((spatial-relation 1.0)
             (binary-relation  0.1) )
  :a-link   (*other* 3.0)
  :slot1   "operand 1"
    :type     :aspect
    :subc     (((spatial-relation . :slot1) 1.0)
               ((binary-relation  . :slot1) 0.1) )
  :slot2   "operand 2"
    :type     :aspect
    :subc     (((spatial-relation . :slot1) 1.0)
               ((binary-relation  . :slot2) 0.1) )
)

(defagent   ter-spat-rel    concept-agent
  "Ternary spatial relation"
  :type     (:concept  :relation)
  :subc     ((spatial-relation 1.0)
             (ternary-relation 0.1) )
  :superc   between
  :a-link   (*other* 1.0)
  :slot1   "operand 1"
    :type     :aspect
    :subc     (((spatial-relation . :slot1) 1.0)
               ((ternary-relation . :slot1) 0.1) )
  :slot2   "operand 2"
    :type     :aspect
    :subc     (((spatial-relation . :slot1) 1.0)
               ((ternary-relation . :slot2) 0.1) )
  :slot3   "operand 3"
    :type     :aspect
    :subc     (((spatial-relation . :slot1) 1.0)
               ((ternary-relation . :slot3) 0.1) )
)


;;;;   *****   SPATIAL DIMENSIONS  *****
;;

(reference-agents 'from-KB/TEMP_REL
  '(brief                   ; temporal analogs, see
    prolonged               ; AMBR/KB/TEMP_REL.LSP
    duration-rel
))

(defagent  dimension-rel  concept-agent
  "The superclass of LENGTH-REL, WIDTH-REL, etc."
  :type     (:concept  :relation)
  :subc     unary-spat-rel
  :superc   ((length-rel 0.5)
             (width-rel  0.5)
             (height-rel 0.5)
             (depth-rel  0.5) )
  :slot1
    :type     :aspect
    :subc     (unary-spat-rel . :slot1)
)

;; Handle the first dimension -- length
(defagent  length-rel  concept-agent
  "The superclass of LONG, SHORT, etc."
  :type     (:concept  :relation)
  :subc     dimension-rel
  :superc   (long
             short )
  :a-link   ((duration-rel 0.5)  ; temporal
             (*other*      1.0)) ;     analog
  :slot1
    :type     :aspect
    :subc     (dimension-rel . :slot1)
)

(defagent  long  concept-agent
  :type     (:concept  :relation)
  :subc     length-rel
  :c-coref  (polar-LONG/SHORT . :slot1)
;;:instance #<genKB>
  :a-link   ((prolonged 0.5)  ; temporal analog
             (short     0.5) )
  :slot1
    :type     :aspect
    :subc     (length-rel . :slot1)
)

(defagent  short    concept-agent
  :type     (:concept  :relation)
  :subc     length-rel
  :c-coref  (polar-LONG/SHORT . :slot2)
;;:instance #<genKB>
  :a-link   ((brief 0.5)    ; temporal analog
             (long  0.5) )
  :slot1
    :type     :aspect
    :subc     (length-rel . :slot1)
)

(defagent   polar-LONG/SHORT  instance-agent
  "(polar-rel long short)"
  :type     (:instance :relation)
  :inst-of  polar-rel
  :slot1
    :type     :aspect
    :inst-of  (polar-rel . :slot1)
    :c-coref  long
  :slot2
    :type     :aspect
    :inst-of  (polar-rel . :slot2)
    :c-coref  short
)
(with-agent  polar-rel
  :instance  (polar-LONG/SHORT 1.0)
)  ; POLAR-LONG/SHORT is a rather prototypical
   ; instance of POLAR-REL (see KB/ABSTRACT.LSP)


;; Handle the second dimension -- width
(defagent  width-rel  concept-agent
  "The superclass of WIDE and NARROW."
  :type     (:concept  :relation)
  :subc     dimension-rel
  :superc   (wide
             narrow )
  :a-link   ((length-rel 1.0)
             (*other*    1.0))
  :slot1
    :type     :aspect
    :subc     (dimension-rel . :slot1)
)

(defagent  wide  concept-agent
  :type     (:concept  :relation)
  :subc     width-rel
  :c-coref  (polar-WIDE/NARROW . :slot1)
;;:instance #<genKB>
  :a-link   ((long   0.5)
             (narrow 0.5) )
  :slot1
    :type     :aspect
    :subc     (width-rel . :slot1)
)

(defagent  narrow   concept-agent
  :type     (:concept  :relation)
  :subc     width-rel
  :c-coref  (polar-WIDE/NARROW . :slot2)
;;:instance #<genKB>
  :a-link   ((short 0.5)
             (wide  0.5) )
  :slot1
    :type     :aspect
    :subc     (width-rel . :slot1)
)

(defagent   polar-WIDE/NARROW  instance-agent
  "(polar-rel wide narrow)"
  :type     (:instance :relation)
  :inst-of  polar-rel
  :slot1
    :type     :aspect
    :inst-of  (polar-rel . :slot1)
    :c-coref  wide
  :slot2   "relational operand 2"
    :type     :aspect
    :inst-of  (polar-rel . :slot2)
    :c-coref  narrow
)

;; Handle the third dimension -- height
(defagent  height-rel  concept-agent
  "The superclass of HIGH, LOW, etc."
  :type     (:concept  :relation)
  :subc     dimension-rel
  :superc   (high
             low  )
  :a-link   ((depth-rel 1.0)
             (*other*   1.0))
  :slot1
    :type     :aspect
    :subc     (dimension-rel . :slot1)
)

(defagent  high  concept-agent
  :type     (:concept  :relation)
  :subc     height-rel
  :c-coref  (polar-HIGH/LOW . :slot1)
;;:instance #<genKB>
  :a-link   ((deep 0.5)
             (low  0.5) )
  :slot1
    :type     :aspect
    :subc     (height-rel . :slot1)
)

(defagent  low      concept-agent
  :type     (:concept  :relation)
  :subc     height-rel
  :c-coref  (polar-HIGH/LOW . :slot2)
;;:instance #<genKB>
  :a-link   ((shallow 0.5)
             (high    0.5) )
  :slot1
    :type     :aspect
    :subc     (height-rel . :slot1)
)

(defagent   polar-HIGH/LOW  instance-agent
  "(polar-rel high low)"
  :type     (:instance :relation)
  :inst-of  polar-rel
  :slot1
    :type     :aspect
    :inst-of  (polar-rel . :slot1)
    :c-coref  high
  :slot2
    :type     :aspect
    :inst-of  (polar-rel . :slot2)
    :c-coref  low
)
(with-agent  polar-rel    ; see KB/ABSTRACT.LSP
  :instance  (polar-HIGH/LOW 0.8)
)

;; More on the third dimension -- depth.
(defagent  depth-rel  concept-agent
  "The superclass of DEEP and SHALLOW."
  :type     (:concept  :relation)
  :subc     dimension-rel
  :superc   (deep
             shallow )
  :a-link   ((height-rel 1.0)
             (*other*    1.0))
  :slot1
    :type     :aspect
    :subc     (dimension-rel . :slot1)
)

(defagent  deep  concept-agent
  :type     (:concept  :relation)
  :subc     depth-rel
  :c-coref  (polar-DEEP/SHLW . :slot1)
;;:instance #<genKB>
  :a-link   ((high    0.5)
             (shallow 0.5) )
  :slot1
    :type     :aspect
    :subc     (depth-rel . :slot1)
)

(defagent  shallow  concept-agent
  :type     (:concept  :relation)
  :subc     depth-rel
  :c-coref  (polar-DEEP/SHLW . :slot2)
;;:instance #<genKB>
  :a-link   ((low  0.5)
             (deep 0.5) )
  :slot1
    :type     :aspect
    :subc     (depth-rel . :slot1)
)

(defagent   polar-DEEP/SHLW  instance-agent
  "(polar-rel deep shallow)"
  :type     (:instance :relation)
  :inst-of  polar-rel
  :slot1
    :type     :aspect
    :inst-of  (polar-rel . :slot1)
    :c-coref  deep
  :slot2
    :type     :aspect
    :inst-of  (polar-rel . :slot2)
    :c-coref  shallow
)


;;;;   *****   RELATIVE POSITION   *******
;;

(defagent  relative-pos  concept-agent
  "The superclass of LEFT-OF, ABOVE, etc."
  :type     (:concept  :relation)
  :subc     bin-spat-rel
  :superc   ((left/right   0.5)
             (above/below  0.5)
             (front/behind 0.5)
             ; phys/spat-rel -- KB/PHYS_REL.LSP
             (between 0.5) )  ; though it's ternary
  :a-link   (*other* 1.0)
  :slot1  "this object"
    :type     :aspect
    :subc     (bin-spat-rel . :slot1)
  :slot2  "that object"
    :type     :aspect
    :subc     (bin-spat-rel . :slot2)
)

;; LEFT-OF and RIGHT-OF
(defagent  left/right   concept-agent
  :type     (:concept  :relation)
  :subc     relative-pos
  :superc   (left-of
             right-of )
  :slot1  "this object"
    :type     :aspect
    :subc     (relative-pos . :slot1)
  :slot2  "that object"
    :type     :aspect
    :subc     (relative-pos . :slot2)
)

(defagent  left-of  concept-agent
  "Obj-1 is to the left of obj-2."
  :type     (:concept  :relation)
  :subc     left/right
  :c-coref  (mirror-LEFT/RIGHT . :slot1)
;;:instance #<genKB>
  :a-link   (right-of 1.0)
  :slot1  "this object"
    :type     :aspect
    :subc     (left/right . :slot1)
  :slot2  "that object"
    :type     :aspect
    :subc     (left/right . :slot2)
)

(defagent  right-of  concept-agent
  "Obj-1 is to the right of obj-2."
  :type     (:concept  :relation)
  :subc     left/right
  :c-coref  (mirror-LEFT/RIGHT . :slot2)
;;:instance #<genKB>
  :a-link   (left-of 1.0)
  :slot1  "this object"
    :type     :aspect
    :subc     (left/right . :slot1)
  :slot2  "that object"
    :type     :aspect
    :subc     (left/right . :slot2)
)

(defagent   mirror-LEFT/RIGHT  instance-agent
  "(mirror-rel left-of right-of)"
  :type     (:instance :relation)
  :inst-of  mirror-rel
  :slot1
    :type     :aspect
    :inst-of  (mirror-rel . :slot1)
    :c-coref  left-of
  :slot2
    :type     :aspect
    :inst-of  (mirror-rel . :slot2)
    :c-coref  right-of
)
(with-agent  mirror-rel
  :instance  (mirror-LEFT/RIGHT 1.0)
)  ; MIRROR-LEFT/RIGHT is a rather prototypical
   ; instance of MIRROR-REL (see KB/ABSTRACT.LSP)


;; ABOVE and BELOW
(defagent  above/below   concept-agent
  :type     (:concept  :relation)
  :subc     relative-pos
  :superc   (above
             below )
  :slot1  "this object"
    :type     :aspect
    :subc     (relative-pos . :slot1)
  :slot2  "that object"
    :type     :aspect
    :subc     (relative-pos . :slot2)
)

(defagent  above  concept-agent
  "Obj-1 is above obj-2."
  :type     (:concept  :relation)
  :subc     above/below
  :c-coref  (mirror-ABOVE/BELOW . :slot1)
;;:instance #<genKB>
  :a-link   ((below 1.0)
            );; --> ON in KB/PHYS_REL.LSP
  :slot1  "this object"
    :type     :aspect
    :subc     (above/below . :slot1)
  :slot2  "that object"
    :type     :aspect
    :subc     (above/below . :slot2)
)

(defagent  below   concept-agent
  "Obj-1 is below obj-2."
  :type     (:concept  :relation)
  :subc     above/below
  :c-coref  (mirror-ABOVE/BELOW . :slot2)
;;:instance #<genKB>
  :a-link   ((above 1.0)
            );; --> UNDER in PHYS_REL.LSP
  :slot1  "this object"
    :type     :aspect
    :subc     (above/below . :slot1)
  :slot2  "that object"
    :type     :aspect
    :subc     (above/below . :slot2)
)

(defagent   mirror-ABOVE/BELOW  instance-agent
  "(mirror-rel above below)"
  :type     (:instance :relation)
  :inst-of  mirror-rel
  :slot1
    :type     :aspect
    :inst-of  (mirror-rel . :slot1)
    :c-coref  above
  :slot2
    :type     :aspect
    :inst-of  (mirror-rel . :slot2)
    :c-coref  below
)
(with-agent  mirror-rel
  :instance  (mirror-ABOVE/BELOW 0.5)
)


;; IN-FRONT-OF and BEHIND
(defagent  front/behind  concept-agent
  :type     (:concept  :relation)
  :subc     relative-pos
  :superc   (in-front-of
             behind  )
  :slot1  "this object"
    :type     :aspect
    :subc     (relative-pos . :slot1)
  :slot2  "that object"
    :type     :aspect
    :subc     (relative-pos . :slot2)
)

(defagent  in-front-of  concept-agent
  "Obj-1 is in front of obj-2."
  :type     (:concept  :relation)
  :subc     front/behind
  :c-coref  (mirror-FRONT/BEHIND . :slot1)
;;:instance #<genKB>
  :a-link   (behind 1.0)
  :slot1  "this object"
    :type     :aspect
    :subc     (front/behind . :slot1)
  :slot2  "that object"
    :type     :aspect
    :subc     (front/behind . :slot2)
)

(defagent  behind  concept-agent
  "Obj-1 is behind obj-2."
  :type     (:concept  :relation)
  :subc     front/behind
  :c-coref  (mirror-FRONT/BEHIND . :slot2)
;;:instance #<genKB>
  :a-link   (in-front-of 1.0)
  :slot1  "this object"
    :type     :aspect
    :subc     (front/behind . :slot1)
  :slot2  "that object"
    :type     :aspect
    :subc     (front/behind . :slot2)
)

(defagent   mirror-FRONT/BEHIND  instance-agent
  "(mirror-rel in-front-of behind)"
  :type     (:instance :relation)
  :inst-of  mirror-rel
  :slot1
    :type     :aspect
    :inst-of  (mirror-rel . :slot1)
    :c-coref  in-front-of
  :slot2
    :type     :aspect
    :inst-of  (mirror-rel . :slot2)
    :c-coref  behind
)


;; BETWEEN is a very interesting case.
;; Syntactically, it is a ternary relation:
;;   'X is between A and B'
;; A and B, however, are interchangeable
;; and, taken together, play the role of
;; the 'other object' in RELATIVE-POS.
;;
;; The representation adopted here has the
;; following peculiarities:
;;   + BETWEEN is a subclass of TER-SPAT-REL ;
;;   + BETWEEN is a subclass of RELATIVE-POS
;;       regardless of the different arity ;
;;   + between.slot2 and between.slot3 both
;;       go to relative-pos.slot2 ;
;;   + slots 2 and 3 are peer slots .

(defagent  between  concept-agent
  "Obj-1 is between obj-2 and obj-3."
  :type     (:concept  :relation)
  :subc     ((relative-pos 1.0)
             (ter-spat-rel 0.1) )
;;:instance #<genKB>
  :slot1  "middle object"
    :type     :aspect
    :subc     (relative-pos . :slot1)
  :slot2  "outer object 1"
    :type     :aspect
    :peer     :slot3
    :subc     (relative-pos . :slot2)
  :slot3  "outer object 2"
    :type     :aspect
    :peer     :slot2
    :subc     (relative-pos . :slot2)
)                             ; --^^^--


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references
  :exemption-id 'from-KB/TEMP_REL )


;;;;;;  ---- Prepare for GENKB ---- ;;;;;;
;;

(GENKB-template
  :herald  "Concepts from KB/SEMANTIC/SPAT_REL.LSP, ver. 3.0.0."
  :templates '(
    (above        (:instance (*other* 10)) )
    (behind       (:instance (*other* 10)) )
    (below        (:instance (*other* 10)) )
    (between      (:instance (*other* 10)) )
    (deep         (:instance (*other* 10)) )
    (high         (:instance (*other* 10)) )
    (in-front-of  (:instance (*other* 10)) )
    (left-of      (:instance (*other* 10)) )
    (long         (:instance (*other* 10)) )
    (low          (:instance (*other* 10)) )
    (narrow       (:instance (*other* 10)) )
    (right-of     (:instance (*other* 10)) )
    (shallow      (:instance (*other* 10)) )
    (short        (:instance (*other* 10)) )
    (wide         (:instance (*other* 10)) )
))

#|
;;;;;;;;;;;;   Appendix   ;;;;;;;;;;
;;
;; This file defines the following 35 agents
;; (ordered by name):

 above
 above/below
 behind
 below
 between
 bin-spat-rel
 deep
 depth-rel
 dimension-rel
 front/behind
 height-rel
 high
 in-front-of
 left-of
 left/right
 length-rel
 long
 low
 mirror-ABOVE/BELOW
 mirror-FRONT/BEHIND
 mirror-LEFT/RIGHT
 narrow
 polar-DEEP/SHLW
 polar-HIGH/LOW
 polar-LONG/SHORT
 polar-WIDE/NARROW
 relative-pos
 right-of
 shallow
 short
 spatial-relation
 ter-spat-rel
 unary-spat-rel
 wide
 width-rel

|#

;;;;;;  End of file  AMBR/KB/SEMANTIC/SPAT_REL.LSP
