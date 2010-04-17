;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package:CL-WHO; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-who/who.lisp,v 1.42 2009/01/26 11:10:49 edi Exp $

;;; Copyright (c) 2003-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-who)

(defun html-mode ()
  "Returns the current HTML mode. :SGML for \(SGML-)HTML, :XML for
XHTML and :HTML5 for HTML5 (HTML syntax)."
  *html-mode*)

(defun (setf html-mode) (mode)
  "Sets the output mode to XHTML or \(SGML-)HTML.  MODE can be
:SGML for HTML, :XML for XHTML or :HTML5 for HTML5 (HTML syntax)."
  (ecase mode
    ((:sgml)
     (setf *html-mode* :sgml
           *empty-tag-end* ">"
           *prologue* "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"))
    ((:xml)
     (setf *html-mode* :xml
           *empty-tag-end* " />"
           *prologue* "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))
    ((:html5)
     (setf *html-mode* :html5
           *empty-tag-end* ">"
           *prologue* "<!DOCTYPE html>"))))

(defun namespace-tag (cons)
  "Formats a cons cell with a namespace prefix and a tag into a string."
  (format NIL "~A:~A" (car cons) (cdr cons)))

(defun namespace-tag-p (cons)
  (and (listp cons)
       (keywordp (first cons))
       (atom (cdr cons))))

(defun process-tag (sexp body-fn)
  (declare (optimize speed space))
  "Returns a string list corresponding to the `HTML' \(in CL-WHO
syntax) in SEXP.  Uses the generic function CONVERT-TO-STRING-LIST
internally.  Utility function used by TREE-TO-TEMPLATE."
  (let (tag attr-list body)
    (if (keywordp sexp)
	(setq tag sexp)
	(let ((first (first sexp)))
	  (cond
	    ((or (atom first) (namespace-tag-p first))
	     (setq tag (if (atom first)
			   first
			   (namespace-tag first)))
	     ;; collect attribute/value pairs into ATTR-LIST and tag body (if
	     ;; any) into BODY
	     (loop for rest on (cdr sexp) by #'cddr
		as first = (first rest)
		if (keywordp first)
		collect (cons first (second rest)) into attr
		else if (namespace-tag-p first)
		collect (cons (namespace-tag first) (second rest)) into attr
		else
		do (progn (setq attr-list attr)
			  (setq body rest)
			  (return))
		finally (setq attr-list attr)))
	    ((listp first)
	     (let ((maybe-tag (first first)))
	       (setq tag (if (namespace-tag-p maybe-tag)
			     (namespace-tag maybe-tag)
			     maybe-tag)))
	     (loop for rest on (cdr first) by #'cddr
		as first = (first rest)
		if (keywordp first)
		collect (cons first (second rest)) into attr
		else if (namespace-tag-p first)
		collect (cons (namespace-tag first) (second rest)) into attr
		finally (setq attr-list attr))
	     (setq body (cdr sexp))))))
    (convert-tag-to-string-list tag attr-list body body-fn)))

(defun convert-attributes (attr-list)
  "Helper function for CONVERT-TAG-TO-STRING-LIST which converts the
alist ATTR-LIST of attributes into a list of strings and/or Lisp
forms."
  (declare (optimize speed space))
  (loop with =var= = (gensym)
        for (orig-attr . val) in attr-list
        for attr = (if *downcase-tokens-p*
                     (string-downcase orig-attr)
                     (string orig-attr))
        unless (null val) ;; no attribute at all if VAL is NIL
          if (constantp val)
            if (and (eq *html-mode* :sgml) (eq val t)) ; special case for SGML
              nconc (list " " attr)
            else
              nconc (list " "
                          ;; name of attribute
                          attr
                          (format nil "=~C" *attribute-quote-char*)
                          ;; value of attribute
                          (cond ((eq val t)
                                 ;; VAL is T, use attribute's name
                                 attr)
                                (t
                                 ;; constant form, PRINC it -
                                 ;; EVAL is OK here because of CONSTANTP
                                 (format nil "~A" (eval val))))
                          (string *attribute-quote-char*))
            end
          else
            ;; do the same things as above but at runtime
            nconc (list `(let ((,=var= ,val))
                          (cond ((null ,=var=))
                                ((eq ,=var= t)
                                 ,(case *html-mode*
                                    (:sgml
                                     `(fmt " ~A" ,attr))
                                    ;; otherwise default to :xml mode
                                    (t
                                     `(fmt " ~A=~C~A~C"
                                           ,attr
                                           *attribute-quote-char*
                                           ,attr
                                           *attribute-quote-char*))))
                                (t
                                 (fmt " ~A=~C~A~C"
                                      ,attr
                                      *attribute-quote-char*
                                      ,=var=
                                      *attribute-quote-char*)))))))

(defgeneric convert-tag-to-string-list (tag attr-list body body-fn)
  (:documentation "Used by PROCESS-TAG to convert `HTML' into a list
of strings.  TAG is a keyword symbol naming the outer tag, ATTR-LIST
is an alist of its attributes \(the car is the attribute's name as a
keyword, the cdr is its value), BODY is the tag's body, and BODY-FN is
a function which should be applied to BODY.  The function must return
a list of strings or Lisp forms."))

(defmethod convert-tag-to-string-list (tag attr-list body body-fn)
  "The standard method which is not specialized.  The idea is that you
can use EQL specializers on the first argument."
  (declare (optimize speed space))
  (let ((tag (if *downcase-tokens-p* (string-downcase tag) (string tag)))
        (body-indent
          ;; increase *INDENT* by 2 for body -- or disable it
          (when (and *indent* (not (member tag *html-no-indent-tags* :test #'string-equal)))
            (+ 2 *indent*))))
    (nconc
     (if *indent*
       ;; indent by *INDENT* spaces
       (list +newline+ (n-spaces *indent*)))
     ;; tag name
     (list "<" tag)
     ;; attributes
     (convert-attributes attr-list)
     ;; body
     (if body
       (append
        (list ">")
        ;; now hand over the tag's body to TREE-TO-TEMPLATE
        (let ((*indent* body-indent))
          (funcall body-fn body))
        (when body-indent
          ;; indentation
          (list +newline+ (n-spaces *indent*)))
        ;; closing tag
        (list "</" tag ">"))
       ;; no body, so no closing tag unless defined in *HTML-EMPTY-TAGS*
       (if (or (not *html-empty-tag-aware-p*)
               (member tag *html-empty-tags* :test #'string-equal))
         (list *empty-tag-end*)
         (list ">" "</" tag ">"))))))

(defun string-list-to-string (string-list)
  (declare (optimize speed space))
  "Concatenates a list of strings to one string."
  ;; note that we can't use APPLY with CONCATENATE here because of
  ;; CALL-ARGUMENTS-LIMIT
  (let ((total-size 0))
    (dolist (string string-list)
      (incf total-size (length string)))
    (let ((result-string (make-string total-size
                                      #+:lispworks #+:lispworks
                                      :element-type 'lw:simple-char))
          (curr-pos 0))
      (dolist (string string-list)
        (replace result-string string :start1 curr-pos)
        (incf curr-pos (length string)))
      result-string)))

(defun conc (&rest string-list)
  "Concatenates all arguments which should be string into one string."
  (funcall #'string-list-to-string string-list))


;; tree processing

(defun apply-to-tree (function test tree)
  (declare (optimize speed space))
  (declare (type function function test))
  "Apply FUNCTION recursively to all elements of the tree TREE \(not
only leaves) which pass TEST."
  (cond
    ((funcall test tree)
      (funcall function tree))
    ((consp tree)
      (cons
       (apply-to-tree function test (car tree))
       (apply-to-tree function test (cdr tree))))
    (t tree)))

(defun tree-to-template (tree)
  "Transforms an HTML tree into an intermediate format - mainly a
flattened list of strings. Utility function used by TREE-TO-COMMANDS."
  (loop for element in tree
        nconc (cond ((or (keywordp element)
                         (and (listp element)
                              (keywordp (first element)))
                         (and (listp element)
                              (listp (first element))
			      (or (keywordp (first (first element)))
				  (namespace-tag-p (first (first element))))))
                     ;; normal tag
                     (process-tag element #'tree-to-template))
                    (t
                     (if *indent*
                       (list +newline+ (n-spaces *indent*) element)
                       (list element))))))

(defun tree-to-commands-aux (tree stream)
  (declare (optimize speed space))
  "Transforms the intermediate representation of an HTML tree into
Lisp code to print the HTML to STREAM. Utility function used by
TREE-TO-COMMANDS."
  (let ((in-string t)
        collector
        string-collector)
    (flet ((emit-string-collector ()
             "Generate a WRITE-STRING statement for what is currently
in STRING-COLLECTOR."
             (list 'write-string
                   (string-list-to-string (nreverse string-collector))
                   stream)))
      (unless (listp tree)
        (return-from tree-to-commands-aux tree))
      (loop for element in tree
            do (cond ((and in-string (stringp element))
                       ;; this element is a string and the last one
                       ;; also was (or this is the first element) -
                       ;; collect into STRING-COLLECTOR
                       (push element string-collector))
                     ((stringp element)
                       ;; the last one wasn't a string so we start
                       ;; with an empty STRING-COLLECTOR
                       (setq string-collector (list element)
                             in-string t))
                     (string-collector
                       ;; not a string but STRING-COLLECTOR isn't
                       ;; empty so we have to emit the collected
                       ;; strings first
                       (push (emit-string-collector) collector)
                       (setq in-string nil
                             string-collector '())
                       ;; collect this element but walk down the
                       ;; subtree first
                       (push element collector))
                     (t
                       ;; not a string and empty STRING-COLLECTOR
                       (push element collector)))
            finally (return (if string-collector
                              ;; finally empty STRING-COLLECTOR if
                              ;; there's something in it
                              (nreverse (cons (emit-string-collector)
                                              collector))
                              (nreverse collector)))))))

(defun tree-to-commands (tree stream &optional prologue)
  (declare (optimize speed space))
  "Transforms an HTML tree into code to print the HTML to STREAM."
  ;; use TREE-TO-TEMPLATE, then TREE-TO-COMMANDS-AUX, and finally
  ;; replace the special symbols ESC, STR, FMT, and HTM
  (tree-to-commands-aux
   (if prologue
       (list* 'progn prologue +newline+
              (tree-to-template (macroexpand-tree tree)))
       (cons 'progn (tree-to-template (macroexpand-tree tree))))
   stream))


;; who

(defparameter *who-stream* nil
  "Dynamic variable that is bound to the html stream inside who-macro.")

(defmacro with-html-output ((var &optional stream
                                 &key prologue
                                      ((:indent *indent*) *indent*))
                            &body body)
  "Transform the enclosed BODY consisting of HTML as s-expressions
into Lisp code to write the corresponding HTML as strings to VAR -
which should either hold a stream or which'll be bound to STREAM if
supplied."
  (when (and *indent*
             (not (integerp *indent*))
             (not (plusp *indent*)))
    (setq *indent* 0))
  (when (eql prologue t)
    (setq prologue *prologue*))
  `(let ((*who-stream* (or ,stream ,var)))
    ,(tree-to-commands body '*who-stream* prologue)))

(defmacro with-html-output-to-string ((var &optional string-form
                                           &key (element-type ''character)
                                                prologue
                                                indent)
                                      &body body)
  "Transform the enclosed BODY consisting of HTML as s-expressions
into Lisp code which creates the corresponding HTML as a string."
  (declare (ignorable element-type))
  `(with-output-to-string (,var ,string-form
                                #-(or :ecl :cmu :sbcl) :element-type
                                #-(or :ecl :cmu :sbcl) ,element-type)
    (with-html-output (,var nil :prologue ,prologue :indent ,indent)
      ,@body)))

(defmacro show-html-expansion ((var &optional stream
                                    &key prologue
                                         ((:indent *indent*) *indent*))
                               &body body)
  "Show the macro expansion of WITH-HTML-OUTPUT."
  (when (and *indent*
             (not (integerp *indent*)))
    (setq *indent* 0))
  (when (eq prologue t)
    (setq prologue *prologue*))
  `(pprint '(let ((,var ,(or stream var)))
             ,(tree-to-commands body var prologue))))


;; macros embedding (by Victor Kryukov victor.kryukov@gmail.com)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *who-macros* (make-array 0 :adjustable t :fill-pointer t)
    "Vector of macros, that MACROEXPAND-TREE should expand."))

(defun macroexpand-tree (tree)
  "Recursively expand all macro from *WHO-MACROS* list in TREE."
  (apply-to-tree
   (lambda (subtree)
     (macroexpand-tree (macroexpand-1 subtree)))
   (lambda (subtree)
     (and (consp subtree)
          (find (first subtree) *who-macros*)))
   tree))

(defmacro def-internal-macro (name attrs &body body)
  "Define internal macro, that will be added to *WHO-MACROS*
and macroexpanded during W-H-O processing.
Other macros can be defined with DEF-INTERNAL-MACRO, but a better way
would be to add additional wrapper, that will handle such issues, as
multiple evaluation of macro arguments (frequently encountered) etc."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (prog1 (defmacro ,name ,attrs
              ,@body)
       (unless (find ',name *who-macros*)
         ;; the earlier the macro is defined, the faster it will be found
         ;; (optimized for frequently used macros, like the inernal ones,
         ;; defined first)
         (vector-push-extend ',name *who-macros*)))))

;; basic who-macros

(def-internal-macro htm (&rest rest)
  "Defines macroexpasion for HTM special form."
  (tree-to-commands rest '*who-stream*))

(def-internal-macro str (form &rest rest)
  "Defines macroexpansion for STR special form."
  (declare (ignore rest))
  (let ((result (gensym)))
    `(let ((,result ,form))
       (when ,result (princ ,result *who-stream*)))))

(def-internal-macro esc (form &rest rest)
  "Defines macroexpansion for ESC special form."
  (declare (ignore rest))
  (let ((result (gensym)))
    `(let ((,result ,form))
       (when ,form (write-string (escape-string ,result)
                                 *who-stream*)))))

(def-internal-macro fmt (form &rest rest)
  "Defines macroexpansion for FMT special form."
  `(format *who-stream* ,form ,@rest))


;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/cl-who/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :cl-who
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
