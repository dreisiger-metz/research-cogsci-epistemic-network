;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-interface -*-

;;; FILE:       AMBR/intrface/mapping.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Functions monitoring the mapping process
;;; DEPENDS-ON: DUAL; ambr/defs.lsp, ambr/corresp.lsp, ambr/csnet.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    29-06-97
;;; UPDATED:    15-03-98 [2.2.1] Thorough revision. Use explicit MAPPING objects
;;;                              Introduce quadratic mapping indices.
;;; UPDATED:    14-08-98 [2.2.2] The 'official release'
;;; UPDATED:    ...


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;     MAPPINGS  AND  MAPPING INDICES      ;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package  "AMBR-INTERFACE")


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: mapping, make-mapping,
;;          mapping-coalition, mapping-role, mapping-other, mapping-time,
;;          mapping-table, mapping-cm, mapping-mi1, mapping-mi2 ;
;;
;;          coalition-mapping, make-coalition-mapping, fill-in-mapping ;
;;          count-mapped-members, cm ;
;;          mapping-index, mi, *mapping-index-mode*,
;;          raw->index-activation
;;
;; The calculation of mapping indices depends on the variables *MAX-MAP-INDEX*,
;; *MIN-MAP-INDEX*, and *TARGET* defined in AMBR/INTRFACE/DEFS.LSP.

;; ...

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass mapping (DUAL-interface-object)
    ((coalition   :reader      mapping-coalition
                  :type        coalition
                  :initarg     :coalition
                  :initform    (required-argument) )
     (role        :reader      mapping-role
                  :type        (member :DRIVER :RECIPIENT)
                  :initarg     :role
                  :initform    (required-argument) )
     (other       :reader      mapping-other
                  :type        (or coalition boolean)
                  :initarg     :other
                  :initform    (required-argument) )
     (time        :reader      mapping-time
                  :type        float
                  :initarg     :time
                  :initform    (required-argument) )
     (table       :accessor    mapping-table
                  :type        (or list                    ; of quadruples
                                   (member :not-computed))
                  :initform    :not-computed  )
     (cm          :accessor    mapping-cm
                  :type        list    ; (cw cm+ cm- )
                  :initform    nil  )
     (mi1         :accessor    mapping-mi1
                  :type        list    ; (mi1 mi1+ mi1- )
                  :initform    nil  )
     (mi2         :accessor    mapping-mi2
                  :type        list    ; (mi2 mi2+ mi2- )
                  :initform    nil  )
    )
    (:documentation  "Data structure used for computing mapping indices." ))
) ; eval-when


;;;;;;  Constructor

(defun  make-mapping (coalition role other)
  "Construct a data structure used for computing mapping indices."
  (declare (type coalition coalition)
           (type (member :driver :recipient) role)
           (type (or coalition boolean) other)
           (values mapping) )
  (make-instance 'mapping :coalition coalition
                          :role      role
                          :other     other
                          :time      *time* ))

  ;; See also MAKE-COALITION-MAPPING below.

;;;;;;  Printing methods

(defmethod print-object  ((map mapping) stream)
  (if (slot-boundp map 'coalition)
      (if (slot-boundp (mapping-coalition map) 'dual-interface::name)
          (format stream  "#<map ~S>" (coalition-name (mapping-coalition map)))
          (format stream  "#<map ???>") )
      (format stream  "#<malformed mapping>") ))

(defmethod DUAL-describe  ((map mapping)
                           &optional (stream *standard-output*))
  (format stream  "~&~S is an object of type ~S."
          map (type-of map))
  (format stream "~&  Its coalition is: ~S, playing role ~S.~%"
          (if (slot-boundp map 'coalition)
              (mapping-coalition map)
              "(unbound)")
          (if (slot-boundp map 'role)
              (mapping-role map)
              "(unbound)"))
  (format stream "~&  Its mapping counterpart is: ~S~%"
          (if (slot-boundp map 'other)
              (mapping-other map)
              "(unbound)"))
  (format stream "~&  Its has been constructed at time: ~6,3F~%"
          (if (slot-boundp map 'time)
              (mapping-time map)
              "(unbound)"))
  (format stream "~&  Its table is: ~S~%"
          (if (slot-boundp map 'table)
              (mapping-table map)
              "(unbound)"))
  (if (slot-boundp map 'cm)
      (let ((cnt (mapping-cm map)))
        (if (null cnt)
            (format stream "~&  Its counters are not computed.~%")
            (format stream "~&  Its counters are: Ttl=~D CW=~D CM+=~D CM-=~D~%"
                    (apply #'+ cnt) (first cnt) (second cnt) (third cnt)) ))
      (format stream "~&  Its counters are unbound.~%"))
  (if (slot-boundp map 'mi1)
      (let ((mi1 (mapping-mi1 map)))
        (if (null mi1)
            (format stream "~&  Its linear index is not computed.~%")
            (format stream "~&  Its linear index is: ~6,3F  (~6,3F  ~6,3F)~%"
                    (first mi1) (second mi1) (third mi1)) ))
      (format stream "~&  Its linear index is unbound.~%"))
  (if (slot-boundp map 'mi2)
      (let ((mi2 (mapping-mi2 map)))
        (if (null mi2)
            (format stream "~&  Its quadratic index is not computed.~%")
            (format stream "~&  Its quadratic index is: ~6,3F  (~6,3F  ~6,3F)~%"
                    (first mi2) (second mi2) (third mi2)) ))
      (format stream "~&  Its linear index is unbound.~%"))
  (values))


;;;;;;;  Type-checking methods for the accessors.
;;
;;  Not defined.  The system-supplied "no applicable method" is deemed enough.



;;;;;;;  *************   BUILDING THE MAPPING TABLE   *****************
;;
;;  The mapping of a given coalition is not explicitly given in the model.
;;  It is distributed over a number of hypothesis-agents.  Therefore, the
;;  first step toward computing a mapping index is to collect the relevant
;;  hypotheses. The second step is to build a 'mapping table' on the basis
;;  of these hypotheses.


;; FILTER-HYPOTHESES goes through the WM and collects a list of hypotheses
;; such as:
;;  1. only winner and mature hypotheses are included  and
;;  2. the list is sorted so that:
;;   2a. winner hypotheses come before mature hypotheses  and
;;   2b. more active hypotheses come before less active hyps of the same type.
;;
;; The value computed by FILTER-HYPOTHESIS at some time is memoized to avoid
;; re-collecting the hypothesis list for each mapping table.  This option
;; is disabled when MEMOIZE-P is NIL.

(let ((memo nil))    ; to be set to a cons  (*time* . hypoth-list)  later.
  (defun filter-hypotheses (&optional (memoize-p T))
    "Produces a sorted list of all non-embryo hypotheses at the moment."
    (flet ((collect-from-scratch (&aux winners matures)
             (do-all-wm (agent *wm*)
               (cond ((agent-type agent '(:winner :hypoth))
                         (push agent winners))
                     ((agent-type agent '(:mature :hypoth))
                         (push agent matures))) )
             (nconc (sort winners #'>= :key #'agent-activation)
                    (sort matures #'>= :key #'agent-activation)) ))
      (if (and memoize-p                         ; if memoization is allowed
               memo                              ; and have memo
               (= *time* (car memo)) )           ; that is up to date
          (cdr memo)
          (let ((table (collect-from-scratch)))
            (setq memo (cons *time* table))      ; memoize for later use
            table) )))
) ; let memo


;; COLLECT-MAPPING-TABLE produces a 'mapping table' for a given COALITION.
;; The mapping table is derived from a (filtered and sorted) list of hypothesis
;; agents (see FILTER-HYPOTHESES) and contains one item for each member of
;; COALITION.  Each item is a quadruple containing the following fields:
;;   1. ELT       -- an element for the given coalition
;;   2. OTHER-ELT -- the 'other' element from the correspondence (or NIL)
;;   3. RAW-ACT   -- the raw activation level of the respective hypoth  (or 0.0)
;;   4. TAG       -- the type of the hypothesis -- :MATURE, :WINNER, or NIL
;;
;; The parameters to COLLECT-MAPPING-TABLE have the following meaning:
;;   COALITION -- the coalition for which the table is computed
;;   ROLE      -- whether this coalition is driver or recipient
;;   OTHER     -- the counterpart in the mapping
;;     When ROLE is :DRIVER, OTHER could be either a specific coalition or T.
;;     When ROLE is :RECIPIENT, OTHER must be a (driver) coalition.
;;   MEMOIZE-P -- passed to FILTER-HYPOTHESES; optional, defaults to T
;;
;; The quadruples in the table depend on the ROLE parameter (see below).
;; The one-to-one constraint is always observed for the _driver_ coalition.
;; The order of the quadruples in the table is the same as the order of members
;; of COALITION.  Some of the quadruples may be void (i.e. with null OTHER-ELT).

(defun collect-mapping-table (coalition role other &optional (memoize-p T))
  (declare (type coalition coalition)
           (type (or coalition (member T)) other)
           (values list) )
  (ecase role
    (:driver
       (collect-MT-aux coalition
                       *driver-elt-label*  *recipient-elt-label*
                       (filter-1-1 (filter-hypotheses memoize-p)
                                   coalition  other)) )
    (:recipient
       (unless (coalition-p other)
         (error "COLLECT-MAPPING-TABLE: ~S is not a coalition." other ))
       (collect-MT-aux coalition
                       *recipient-elt-label*  *driver-elt-label*
                       (filter-1-1 (filter-hypotheses memoize-p)
                                   other  coalition))) ))

(defun collect-MT-aux (coalition label1 label2 hypotheses)
  (mapcar #'(lambda (elt)
              (let ((hyp (find elt hypotheses
                               :key #'(lambda (h) (corresp-elt h label1))) ))
                (if (null hyp)
                    (list elt nil 0.0 nil)
                    (list elt
                          (corresp-elt hyp label2)
                          (agent-activation hyp)
                          (if (agent-type hyp :winner) :winner :mature)) )))
          (coalition-members coalition) ))


;; FILTER-1-1 enforces the one-to-one restriction for the _driver_ coalition.
;; In other words, a hypothesis H relating a driver element DE to a recipient
;; element RE is preserved if and only if it meets the following criteria:
;;  0. Both hypothesis-elements of H are nonNIL
;;  1. DE belongs to the DRIVER.
;;  2. RE belongs to the RECIPIENT. (When RECIPIENT is T, all REs are allowed.)
;;  3. DE is not mapped already, i.e. there isn't any alternative hypothesis
;;       AH that:
;;        -- AH relates the same DE to some element satisfying criterion 2. and
;;        -- AH precedes H in the hypothesis list produced by FILTER-HYPOTHESES.
;;
;; Note that FILTER-1-1 allows several different DEs map to the same RE.
;; For example  MADE-OF-drv<-->MADE-OF-rec  and  COLOR-OF-drv<-->MADE-OF-rec.
;; This is especially likely when the driver coalition has some elements that
;; do not match any element in the base coalition.  These 'solitary' driver
;; elements then try to 'steal the mates' of their fellows.

(defun filter-1-1 (raw-hypotheses driver recipient)
  (declare (type list raw-hypotheses)    ; sorted by activation levels
           (type coalition driver)
           (type (or coalition (member T)) recipient)
           (values list) )
  (let ((unmapped-driver-members (copy-list              ; to be DELETE-d below
                                       (coalition-members driver)))
        (recipient-members (if (eq T recipient)
                               T
                               (coalition-members recipient)))
        (result nil) )
    (dolist (hyp raw-hypotheses)
      (let ((driver-elt    (corresp-elt hyp *driver-elt-label*   ))
            (recipient-elt (corresp-elt hyp *recipient-elt-label*)) )
        (when (and driver-elt  recipient-elt                      ; crit 0.
                   (member driver-elt unmapped-driver-members)    ; crit 1+3.
                   (or (eq recipient-members T)
                       (member recipient-elt recipient-members))) ; crit 2.
          (setq unmapped-driver-members
                (delete driver-elt unmapped-driver-members))      ; for crit 3.
          (push hyp result) )))
    (nreverse result) ))



;;;;;;;  *************   MAPPING INDICES AND COUNTS  *****************
;;
;;  Mapping tables serve as basis for several numerical indices of the
;;  strength of the mapping involving a given coalition (or situation).
;;  (Compare with the retrieval indices in DUAL/INTRFACE/COALITN.LSP.)
;;
;;  The current implementation uses three such measures:
;;   -- MAPPING-COUNT -- based on the number of 'successful' hypotheses;
;;   -- LINEAR-MAPPING-INDEX -- based on the activation levels of hypotheses;
;;   -- QUADRATIC-MAPPING-INDEX -- uses squaring to sharpen outlyers.
;;  Each of these measures has several sub-components and is kept (as a list)
;;  in a separate slot of the MAPPING structure.


;; MAPPING-COUNT gives the number of mapped elements based on a mapping table.
;; There are three kinds of counts:
;;  -- winner (CW)           -- the number of winner hypotheses
;;  -- positive mature (CM+) -- the number of mature hypotheses whose
;;              _decoded_ activation levels are positive (see AMBR/CSNET.LSP).
;;  -- negative mature (CM-) -- ditto but with negative decoded act.levels.
;; These three kinds constitute an exhaustive partition of the hypothesis set.
;; The total mapping count is the sum of the three sub-counts above.

(defun calculate-mapping-count (mapping-table)
  (declare (type list mapping-table)    ; list of quadruples
           (values list) )              ; (cw cm+ cm- )
  (let ((cw 0) (cm+ 0) (cm- 0))
    (dolist (quadruple mapping-table)
      (unless (null (second quadruple))
        (ecase (fourth quadruple)
          (:winner  (incf cw))
          (:mature  (if (minusp (decode-hypoth-activation (third quadruple)))
                        (incf cm- )
                        (incf cm+ ))) )))
    (list cw cm+ cm- )))



;; MAPPING INDICES are computed on the basis of the activation levels of the
;; hypotheses.  The 'raw' activation levels are taken from the mapping table.
;; These raw levels are transformed as follows (see AMBR/DEFS and =/CSNET.LSP):
;;       raw value            transf. value
;;   ------------------------------------------
;;    *hypoth-max-act*   -->  *max-map-index*      (e.g.  2.0  -->  4.0)
;;    *hypoth-zero-act*  -->  0.0                  (e.g.  0.8  -->  0.0)
;;    *WM-threshold*     -->  *min-map-index*      (e.g.  0.09 --> -1.0)
;;    0.0                -->  *min-map-index*
;; This is achieved by two linear transformations -- for the positive and
;; negative branch, respectively.  Note that the parameter *HYPOTH-MIN-ACT*
;; does not play any role; it is subsumed by *WM-THRESHOLD*.

(defun raw->index-activation  (raw-act)
  "For the purposes of the mapping indices, transform hypothesis' activation."
  ;; Cf. ENCODE- and DECODE-HYPOTH-ACTIVATION in AMBR/CSNET.LSP.
  (declare (type float raw-act)
           (values float) )      ; [-1,+1]
  (cond ((> raw-act *hypoth-zero-act*)
            (* *max-map-index*
               (/ (- raw-act *hypoth-zero-act*)                 ; (0, max]
                  (- *hypoth-max-act* *hypoth-zero-act*))) )
        ((> raw-act *WM-threshold*)
            (* *min-map-index*
               (/ (- raw-act *hypoth-zero-act*)                 ; (min, 0]
                  (- *WM-threshold* *hypoth-zero-act*))) )
        (t  *min-map-index*) ))


;; These transformed activation values are then summed together.  The summation
;; is done separately for the positive and negative branches.  There are linear
;; sums and quadratic sums, yielding a total of four sub-indices:
;;   -- MI1+ := sum of positive transformed activation levels
;;   -- MI1- := sum of negative trasformed activation levels
;;   -- MI2+ := sum of squared positive transformed activation levels
;;   -- MI2- := -1 * sum of squared negative transformed activation levels.

(defun calculate-mapping-sub-indices (mapping-table)
  (declare (type list mapping-table)    ; list of quadruples
           (values list) )              ; (mi1+ mi1- mi2+ mi2- )
  (let ((mi1+ 0.0)  (mi1- 0.0)
        (mi2+ 0.0)  (mi2- 0.0) )
    (dolist (quadruple mapping-table)
      (let ((act (raw->index-activation (third quadruple))))
        (cond ((plusp act)  (incf mi1+ act)
                            (incf mi2+ (* act act)))
              (t            (incf mi1- act)
                            (decf mi2- (* act act))) )))
    (list mi1+ mi1- mi2+ mi2- )))


;; These sub-indices are combined to produce the final mapping indices
;; according to the formulae:
;;      MI1 := (mi1+ + mi1- + correction1) / #(driver)
;;      MI2 := (mi2+ + mi2- + correction2) / #(driver)
;;      correction1 := *min-map-index* * [#(driver) - #(coalition)]
;;      correction2 := -(square *min-map-index*) * [#(driver) - #(coalition)]
;; where  #(...)  denotes the number of agents in a coalition.
;;
;; The CORRECTION term accounts for the different number of driver and recipient
;; elements.  In this way 'big' and 'small' coalitions compete on equal basis.
;; For instance, suppose that DRIVER has 10 elements and RECIPIENT -- 15.
;; Suppose further that all 10 driver elts are successfully mapped to recipient
;; elements.  Still, there will be 5 recipient elements that do not have any
;; correspondence, which will accumulate -5.0 points in negative sub-indices.
;; In reverse, suppose DRIVER has 10 elt and RECIPIENT -- only 5.  Even if all
;; REs map to DEs this still is a bad mapping.  The CORRECTION term compensate
;; for this undue distortion of the total mapping indices.
;;
;; The theoretical extremal values for the two kinds of mapping indices are:
;;   Kind of mapping             linear MI         quadratic MI
;;  ------------------------------------------------------------------------
;;   perfect (all hyps at max)   *max-map-index*   (square *max-map-index)
;;   void (no hypoth's at all)   *min-map-index*   -(square *min-map-index)

(defun calculate-linear-mapping-index (sub-indices my-length driver-length)
  (let ((mi1+ (first sub-indices))
        (mi1- (second sub-indices))
        (correction (* *min-map-index*
                       (- driver-length my-length))) )
    (/ (+ mi1+ mi1- correction)
       driver-length) ))

(defun calculate-quadratic-mapping-index (sub-indices my-length driver-length)
  (let ((mi2+ (third sub-indices))
        (mi2- (fourth sub-indices))
        (correction (* *min-map-index* *min-map-index*
                       (- my-length driver-length))) )
    (/ (+ mi2+ mi2- correction)
       driver-length) ))



;;;;;;;  ********   ATTACHING EVERYTHING TO COALITIONS  ********
;;
;;  Mapping tables are expensive to compute. Moreover, they may be re-used.
;;  To avoid re-computing them each time they are memoized into the first
;;  user-field of COALITION objects (see DUAL/INTRFACE/COALITN.LSP).

(defun make-coalition-mapping  (coalition role other &optional (memoize-p T))
  "Makes (or retrieves memoized) MAPPING structure associated with COALITION."
  (declare (values mapping))
  (let ((memo (coalition-user1 coalition)))
    (if (and memoize-p                              ; if memoization is allowed
             memo                                   ; and have memo
             (= *time* (mapping-time memo))         ; which is up-to-date
             (eq role (mapping-role memo))          ; and is equivalent
             (equal other (mapping-other memo)) )
         memo
         (setf (coalition-user1 coalition)
               (make-mapping coalition role other)) )))


(defun fill-in-mapping (mapping &optional (memoize-p T))
  "Calculates and stores (destructively) all derivate fields of MAPPING."
  (declare (type mapping mapping))
  (with-slots (coalition role other table cm mi1 mi2)
              mapping
    (when (eq :not-computed table)           ; Handle TABLE
      (setf table
            (collect-mapping-table coalition role other memoize-p))
      (fill-in-mapping-indices mapping) )
    (when (null cm)                          ; Handle the counters
      (setf cm
            (calculate-mapping-count table)) )
    (when (or (null mi1) (null mi2))         ; Handle the mapping indices
      (fill-in-mapping-indices mapping) ))
  mapping )

(defun fill-in-mapping-indices (mapping)
  (declare (type mapping mapping))
  (let* ((sub-indices (calculate-mapping-sub-indices (mapping-table mapping)))
         (my-length   (length (coalition-members (mapping-coalition mapping))))
         (driver-length (if (eq (mapping-role mapping) :driver)
                            my-length
                            (length (coalition-members
                                               (mapping-other mapping))))) )
    (setf (mapping-mi1 mapping)
          (list (calculate-linear-mapping-index                     ; mi1
                               sub-indices my-length driver-length)
                    (first  sub-indices)                            ; mi1+
                    (second sub-indices)))                          ; mi1-
    (setf (mapping-mi2 mapping)
          (list (calculate-quadratic-mapping-index                  ; mi2
                               sub-indices my-length driver-length)
                    (third  sub-indices)                            ; mi2+
                    (fourth sub-indices)))  )                       ; mi2-
  mapping )  ; return the filled-in structure


;; The function COALITION-MAPPNG puts everything together.  It shold be the
;; function of choice.

(defun coalition-mapping  (coalition &key role other (memoize-p T))
  "Ensures a filled-in MAPPING structure for COALITION."
  (declare (type coalition coalition)
           (type (member nil :driver :recipient) role)
           (type (or coalition boolean) other)
           (values mapping))
  (when (null role)           ; when not supplied
    (setq role (if (target-p coalition)
                   :driver
                   :recipient)) )
  (when (and (eq role :recipient)
             (eq other T) )
    (cerror "Take value for OTHER from the variable *TARGET* and continue."
            "COALITION-MAPPING: The OTHER parameter cannot be T ~
             when ROLE for ~S is :RECIPIENT."  coalition )
    (setq other *target*) )
  (when (null other)          ; when not supplied
    (setq other (ecase role
                  (:driver    T )
                  (:recipient *target*))) )
  ;; Do the real work
  (fill-in-mapping (make-coalition-mapping coalition role other memoize-p)
                   memoize-p) )


;;;;;;;  ********   USER-INTERFACE FUNCTIONS   *********
;;
;; See variable *MAPPING-INDEX-MODE* in AMBR/INTRFACE/DEFS.LSP

(defun count-mapped-members  (coalition &key (mode :total)
                                             role other (memoize-p T) )
  "Counts how many elts from COALITION are mapped to elts from OTHER."
  ;; See function CALCULATE-MAPPING-COUNT above.
  (declare (values (or integer list)))
  (let* ((cm (mapping-cm (coalition-mapping coalition  :role role
                                    :other other  :memoize-p memoize-p)))
         (total (apply #'+ cm)) )   ; CM = (winner mature+ mature- )
    (ecase  mode
      (:total    total)
      (:winner   (first cm))
      (:positive (+ (first cm) (second cm)))
      (:mature+  (second cm))
      (:mature-  (third cm))
      (:mature   (+ (second cm) (third cm)))
      (:list     (cons total cm)) )))  ; (total winner mature+ mature- )


(defun  mapping-index  (coalition &key (mode *mapping-index-mode*)
                                       role  other  (memoize-p T) )
  "Numerical measure for the strength of mapping b/n COALITION and OTHER."
  ;; See CALCULATE-MAPPIG-SUB-INDICES and CALCULATE-xxx-MAPPING-INDEX above.
  (declare (values (or float list)))
  (let* ((mapping (coalition-mapping coalition  :role role
                             :other other :memoize-p memoize-p))
         (mi1 (mapping-mi1 mapping))       ; (linear linear+ linear- )
         (mi2 (mapping-mi2 mapping)) )     ; (quadratic quadratic+ quadratic- )
    (ecase mode
      (:linear         (first  mi1))
      (:linear+        (second mi1))
      (:linear-        (third  mi1))
      (:linear-list            mi1 )       ; (linear linear+ linear- )
      (:quadratic      (first  mi2))
      (:quadratic+     (second mi2))
      (:quadratic-     (third  mi2))
      (:quadratic-list         mi2 ) )))   ; (quadratic quadratic+ quadratic- )


;;;;; Shorthands

(declaim (inline cm mi))
(defun  cm  (coalition &key (mode :total) role other (memoize-p T) )
  "Counts how many elts from COALITION are mapped to elts from OTHER."
  (count-mapped-members coalition :mode mode
                        :role role  :other other  :memoize-p memoize-p ))

(defun  mi (coalition &key (mode *mapping-index-mode*)
                           role  other  (memoize-p T) )
  "Numerical measure for the strength of mapping b/n COALITION and OTHER."
  (mapping-index coalition :mode mode
                 :role role  :other other  :memoize-p memoize-p ))

;; See also MT in AMBR/INTRFACE/REPORT.LSP.

;;;;;;;;  End of file  AMBR/INTRFACE/MAPPING.LSP
