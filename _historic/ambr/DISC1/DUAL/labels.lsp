;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/labels.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Proclamations of labels, links, and tags.
;;; DEPENDS-ON: DUAL/packages.lsp, DUAL/proclaim.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    03-02-98 [1.1.1]  Was part of DUAL/DEFS.LSP
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;                                              ;;;;;;;;;;
   ;;;;;;;;;;   PROCLAMATIONS OF LABELS, LINKS, AND TAGS   ;;;;;;;;;;
   ;;;;;;;;;;                                              ;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl:in-package  "DUAL-CORE")

;; SYMBOLS: <none>
;;

;;;;;;  Slot labels  (see DR#1, section 3.2.3.4)
;;
;;  See DUAL/PROCLAIM.LSP for documentation on PROCLAIM-LABEL.

(proclaim-label :type      :G-slot :tag )

(proclaim-label :subc      :G-slot :reference )
(proclaim-label :superc    :G-slot :reference )
(proclaim-label :inst-of   :G-slot :reference )
(proclaim-label :instance  :G-slot :reference )
(proclaim-label :c-coref   :G-slot :reference )
(proclaim-label :m-coref   :G-slot :reference )
(proclaim-label :a-link    :G-slot :reference )
(proclaim-label :t-link    :G-slot :reference )
(proclaim-label :procedure :G-slot :reference )

(proclaim-label :slot1     :S-slot )
(proclaim-label :slot2     :S-slot )
(proclaim-label :slot3     :S-slot )
(proclaim-label :slot4     :S-slot )
(proclaim-label :slot5     :S-slot )
(proclaim-label :slot6     :S-slot )
(proclaim-label :slot7     :S-slot )
(proclaim-label :slot8     :S-slot )
(proclaim-label :slot9     :S-slot )
(proclaim-label :slot10    :S-slot )


;;;;;;  Link labels
;;
;;  Link labels are a subset of the G-slot labels (see DR#1, section 3.2.3.5).
;;  See DUAL/PROCLAIM.LSP for documentation on PROCLAIM-LINK.

(proclaim-link :subc      :default-weight 1.0  :marker-passing-p t )
(proclaim-link :superc    :default-weight 1.0 )
(proclaim-link :inst-of   :default-weight 1.0  :marker-passing-p t )
(proclaim-link :instance  :default-weight 1.0 )
(proclaim-link :c-coref   :default-weight 1.0  :marker-passing-p t )
(proclaim-link :m-coref   :default-weight 1.0 )
(proclaim-link :procedure :default-weight 1.0 )
(proclaim-link :a-link    :default-weight nil )   ; defaulting forbidden
(proclaim-link :t-link    :default-weight nil
                          :temporary-p t )


;;;;;;  Tags  (see DR#1, section 3.2.3.3)
;;
;;  See DUAL/PROCLAIM.LSP for documentation on PROCLAIM-TAG.

(proclaim-tag nil         t )    ; just in case
(proclaim-tag :special    t )
(proclaim-tag :temporary  t )

(proclaim-tag :concept      '((:G-slot :type)) )
(proclaim-tag :instance     '((:G-slot :type)) )
(proclaim-tag :word         '((:G-slot :type)) )
(proclaim-tag :procedure    '((:G-slot :type)) )
(proclaim-tag :visual-image '((:G-slot :type)) )
(proclaim-tag :situation    '((:G-slot :type)) )
(proclaim-tag :object       '((:G-slot :type)) )
(proclaim-tag :relation     '((:G-slot :type)
                              (:facet  :type)) )
(proclaim-tag :aspect       '((:facet  :type)) )
(proclaim-tag :action       '((:facet  :type)) )


;;;;;;;  End of file DUAL/LABELS.LSP
