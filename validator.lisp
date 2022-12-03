;;;; Proof validator for propositional logic - Honors Option for PHL 130
;       Authored by Kenneth Sun, MIT license.
;
;This program takes in .txt file(s) as input, where each line is a line in a proof,
;formatted like so:
;   input.txt
;   2;B
;   1. A
;   2. (A -> B)
;   3. B; MP 1,2
;
;The first line must have an integer indicating how many lines are in the initial premise,
;followed by what conclusion the proof should reach.
;Every line thereafter must contain a statement and the logical step used to obtain it, separated by a semicolon (;).
;
;;; Use of notation:
;   =>      Implies
;   <=>     Biconditional
;   ^       And
;   v       Or
;   [A-Z]*  Sequence of capital letters represent atomic statements. Space delimited (note that lowercase "v" is processed as the logical Or)
;   -       Negation
;
;
;;; Glossary of accepted logical operations, as well as their inputs:
;   MP      Modus Ponnens   Requires 2 line arguments
;   MT      Modus Tonens    Requires 2 line arguments
;   MTP     Modus tollendo ponnens  Requires 2 line arguments
;   DN      Double Negative Requires 1 line argument
;   Cond    Assume for conditional proof; if a line argument is given, indicates end of conditional proof
;   Ind     Assume for indirect proof; if a line argument is given, indicates end of indirect proof
;   Simp    Simplification  Requires 1 line argument
;   Add     Addition    Requires 1 line argument
;
;

(defpackage "PROOF-VALIDATOR"
  (:use "COMMON-LISP"))

(in-package proof-validator)

(asdf:load-system :uiop)
(ql:quickload 'cl-ppcre)

(defparameter *lines* nil)
(defun trim-space (str) (string-trim '(#\ ) str))

(defun main-op (str)
  (if (char-equal #\- (char str 0))
      '-
      (let ((s (read-from-string str)))
        (if (consp s)
            (nth (floor (/ (length s) 2)) s)
            nil))))

(defun main-states (str op)
  (if (eql op '-)
      (list (read-from-string (subseq str 1)))
      (let ((sep (read-from-string str)))
        ;; Skipping the operator
        (list (first sep) (third sep)))))

(defun match-next-balanced-pair (string left right &optional (start 0) (end (length string)))
  (let* ((count 1) (before (position left string :start start)) (after (1+ before)))
    (do ((pos after (incf after)))
        ((or (eql count 0) (eql pos end)))
      (cond
        ((eql (char string pos) left) (incf count))
        ((eql (char string pos) right) (decf count)))
      )
    (values (subseq string before after) before after)))

(defun replace-negs (line)
  (setf line (cl-ppcre:regex-replace-all "-(\\w+)" line "(+ \\1)"))
  (do ((posneg (position #\- line) (position #\- line)))
      ((null posneg))
    (multiple-value-bind (match before after) (match-next-balanced-pair line #\( #\) posneg)
      (setf line (format nil "~A(+ ~A)~A" (subseq line 0 (1- before)) match (subseq line after)))))
   (cl-ppcre:regex-replace-all "\\+" line "-"))

(defun parse-state (str)
  (read-from-string (replace-negs str)))
  ;; (let ((s (read-from-string
  ;;           (replace-negs str))))
  ;;   (if (atom s)
  ;;       (pars-state (read-from-string (format nil "(~A)" str)))
  ;;       (pars-state s))))

;; ;;   (let ((op (main-op (replace-negs str))))
;;     (if op
;;         (cons op (main-states str op))
;;         ;; cons 'nil so that state has the form (op . xyz)
;;         ; (cons nil (read-from-string str)))))
;;         (read-from-string str))))
;; ;
(defun parse-logic (str)
  (let ((sep (uiop:split-string str :separator '(#\  #\,))))
    (cons
     (intern (string-upcase (first sep)))
     (mapcar #'parse-integer (cdr sep)))))

(defun parse-line (line)

  ;;; Examples of lines for reference
  ; 1. (A ^ B); MP 1,2
  ; 17. -(A -> B); MT 1,2
  ; 2. -B; DN
  (let ((sep (mapcar #'trim-space (uiop:split-string line :separator '(#\. #\;)))))
    (let ((L (intern (n-ln (parse-integer (first sep))))))
      (setf (get L 'state) (parse-state (second sep)))
      (if (third sep) (setf (get L 'logic) (parse-logic (third sep))))
      (setf *lines* (cons L *lines*))
      L)))

(defun n-ln (n) (format nil "L~A" n))
(defun ln-sym (ln) (find-symbol (n-ln ln)))
(defun neg (sym) (list '- sym))

(defun op-in-ln (largs op)
  (let ((l1 (ln-sym (first largs)))
        (l2 (ln-sym (second largs))))
    (cond
      ((or (null l1) (null l2)) nil)
      ((and (consp (get l1 'state))
                  (eql op (second (get l1 'state))))
         (values l1 l2))
      ((and (consp (get l2 'state))
                  (eql op (second (get l2 'state))))
         (values l2 l1))
      (t nil)
      )))

(defun mp (claim largs)
  (multiple-value-bind (lnop lnod) (op-in-ln largs '=>)
    (cond
      ((not (and lnop lnod))
       (format nil "Invalid reference lines: ~A must exist and one must have \"=>\" as main operator" largs))
      ((not (and
             (equal (first (get lnop 'state)) (get lnod 'state))
             (equal (third (get lnop 'state)) claim)))
            (format nil "Incorrect usage: ~A is not (~A => ~A)"
                    lnop lnod claim))
      (t (string "Valid")))))

(defun mt (claim largs)
  (multiple-value-bind (lnop lnod) (op-in-ln largs '=>)
    (cond
      ((not (and lnop lnod))
       (format nil "Invalid reference lines: ~A must exist and one must have \"=>\" as main operator" largs))
      ((not (equal (neg (third (get lnop 'state))) (get lnod 'state)))
       (format nil "Incorrect usage: ~A is not the negation of ~A's consequent" lnod lnop))
      ((not (equal (neg (first (get lnop 'state))) claim))
       (format nil "Incorrect usage: claim is not the negation of ~A's antecedent" lnop ))
      (t (string "Valid")))))

(defun mtp (claim largs)
  (multiple-value-bind (lnop lnod) (op-in-ln largs 'v)
    (cond
      ((not (and lnop lnod))
       (format nil "Invalid reference lines: lines ~A must exist and one must have \"v\" as main operator" largs))
      ((not (member (get lnod 'state) (get lnop 'state) :key #'neg :test #'equal))
       (format nil "Incorrect usage: ~A is not the negation of either argument in ~A" lnod lnop))
      ((or (equal claim (get lnod 'state))
           (not (member claim (get lnop 'state) :test #'equal)))
       (format nil "Incorrect usage: ~A is not a junction in ~A" claim lnop))
      (t (string "Valid")))))

(defun add (claim larg)
  (let ((l (ln-sym (car larg))))
    (cond
      ((not (and l (member (get l 'state) claim :test #'equal))) (format nil "Incorrect usage: L~A must exist and be a junction in claim" (car larg)))
      ((not (eql (second claim) 'v)) (string "Incorrect usage: main operator in claim must be \"v\""))
      (t (string "Valid")))))

(defun conj (claim largs)
  (let ((l1 (ln-sym (first largs)))
        (l2 (ln-sym (second largs))))
    (cond
      ((or (null l1) (null l2)) (format nil "Invalid reference lines: need two lines, both must exist"))
      ((and (not (equal claim (list (get l1 'state) '^ (get l2 'state))))
            (not (equal claim (list (get l2 'state) '^ (get l1 'state)))))
       (format nil "Incorrect usage: claim must be (~A ^ ~A)" l1 l2))
      (t (string "Valid")))))

(defun simp (claim larg)
  (let ((l (ln-sym (car larg))))
    (cond
      ((null l) (format nil "Invalid reference line: ~A must exist" l))
      ((or (not (eql (second (get l 'state)) '^)) (not (member claim (get l 'state))))
       (format nil "Incorrect usage: ~A must be a conjunction and claim must be one of its junctions" l))
      (t (string "Valid")))))

(defun dn (claim larg)
  (let ((l (ln-sym (car larg))))
    (cond
      ((null l) (format nil "Invalid reference line: ~A must exist" l))
      ((and (not (equal claim (neg (neg (get l 'state)))))
            (not (equal (neg (neg claim)) (get l 'state))))
       (format nil "Incorrect usage: claim and ~A are not double negations" l))
      (t (string "Valid")))))

(defun rep (claim larg)
  (if (not (equal (get (ln-sym (car larg)) 'state) claim))
      (string "Invalid usage")
      (string "Valid")))

(defun bi (claim largs)
  (let ((l1 (ln-sym (first largs)))
        (l2 (ln-sym (second largs))))
    (cond
      ((not (eql (second (get l1 'state)) '=>)) (format nil "Invalid reference line: ~A must exist and have \"=>\" as main operator" l1))
      ((not (eql (second (get l2 'state)) '=>)) (format nil "Invalid reference line: ~A must exist and have \"=>\" as main operator" l2))
      ((not (equal (get l1 'state) (reverse (get l2 'state))))
       (format nil "Incorrect usage: ~A and ~A are not reverse implications" l1 l2))
      ((and (not (equal claim (list (first (get l1 'state)) '<=> (third (get l1 'state)))))
            (not (equal claim (list (first (get l2 'state)) '<=> (third (get l2 'state))))))
       (format nil "Incorrect usage: claim must be (~A <=> ~A) or its reverse" (first (get l1 'state)) (first (get l2 'state))))
      (t (string "Valid")))))

(defun equi (claim largs)
  (multiple-value-bind (lnop lnod) (op-in-ln largs '<=>)
    (cond
      ((not (and lnop lnod))
       (format nil "Invalid reference lines: needs two lines and one must have \"<=>\" as main operator"))
      ((not (member (get lnod 'state) (cons (mapcar #'neg (get lnop 'state)) (get lnop 'state)) :test #'equal))
       (format nil "Incorrect usage: ~A is not an argument in ~A" lnod lnop))
      ((not (member claim (cons (mapcar #'neg (get lnop 'state)) (get lnop 'state)) :test #'equal))
       (format nil "Incorrect usage: claim is not an argument in ~A" lnop))
      (t (string "Valid")))))

(defun validate-line (line)
  (let ((logic (get line 'logic))
        (claim (get line 'state)))
    (format t "~A- ~A~%" line
            (cond ((null logic) (string "No justification, taken as premise"))
                  ((null (fboundp (car logic))) (format nil "Unknown operation: ~A" (car logic))) ; check if provided justification has a corresponding defun
                  ; ((null (val-ln (cadr l))) (format nil "Invalid reference line: ~A" (cadr l)))
                  (t (funcall (car logic) claim (cdr logic)))) ; calls corresponding rule of inference with claim and whatever line args were provided
            )))

(defun validate-file (filename)
  (setf *lines* nil)
  (with-open-file (str (make-pathname :name filename))
    (defparameter *claim* (read-from-string (read-line str)))
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof) *lines*)
      (validate-line (parse-line line)))))
