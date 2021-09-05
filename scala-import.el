;;; scala-import.el -- Scala import commands -*- lexical-binding: t -*-

;; Copyright (C) 2015 ENSIME authors
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;; Extracted from ensime-editor.el
;;
;;; Code:


(require 'dash)

(defun scala-import-scala-new-import (qualified-name)
  (format "import %s\n" qualified-name))

(defun scala-import-scala-new-import-grouped-package (base-package grouped-classes)
  (format "import %s.{ %s }" base-package grouped-classes))

(defun scala-import-no-imports-in-buffer-p ()
  (looking-at "^\\s-*package\\s-"))

(defun scala-import-import-block-in-buffer-p ()
  (looking-at "^\\s-*import\\s-"))

(defun scala-import-same-base-package-p (current-import qualified-name)
  "Compare CURRENT-IMPORT's package to QUALIFIED-NAME's package, returning true if they are equal."
  (equal (->> current-import (s-split "\\.") -butlast)
         (->> qualified-name (s-split "\\.") -butlast)))

(defun scala-import-past-starting-point (starting-point)
  "Past STARTING-POINT of excursion.
Should not insert past STARTING-POINT - move to beginning of line that STARTING-POINT is on.
STARTING-POINT is the point where the `scala-import-insert-import' was invoked from."
  (when (>= (point) starting-point)
    (goto-char starting-point)
    (goto-char (point-at-bol))))

(defun scala-import-indent-line ()
  (indent-region (point-at-bol) (point-at-eol)))

(defun scala-import-insert-new-import-no-imports-in-buffer (starting-point java-scala-new-import qualified-name)
  "Insert new import when there are no current import statements in the buffer.
STARTING-POINT is the point where the `scala-import-insert-import' was invoked from.
JAVA-SCALA-NEW-IMPORT is a function to format the import statement for either java or scala.
QUALIFIED-NAME is the name to import.
Returns a function/closure to invoke the necessary buffer operations to perform the insertion."
  (lambda ()
    (goto-char (point-at-eol))
    (newline)
    (newline)
    (scala-import-past-starting-point starting-point)
    (save-excursion (insert (funcall java-scala-new-import qualified-name)))
    (scala-import-indent-line)))

(defun scala-import-insert-new-import-no-imports-or-package-in-buffer (starting-point java-scala-new-import qualified-name)
  "Insert new import when there are no current import statements or package statement in the buffer.
STARTING-POINT is the point where the `scala-import-insert-import' was invoked from.
JAVA-SCALA-NEW-IMPORT is a function to format the import statement for either java or scala.
QUALIFIED-NAME is the name to import.
Returns a function/closure to invoke the necessary buffer operations to perform the insertion."
  (lambda ()
    (unless (looking-at "^\s*$")
     (newline)
     (backward-char 1))
    (scala-import-past-starting-point starting-point)
    (save-excursion (insert (funcall java-scala-new-import qualified-name)))
    (scala-import-indent-line)))

(defun scala-import-insert-new-import-next-line (starting-point java-scala-new-import qualified-name)
  "Insert new import on the next line in the buffer.
STARTING-POINT is the point where the `scala-import-insert-import' was invoked from.
JAVA-SCALA-NEW-IMPORT is a function to format the import statement for either java or scala.
QUALIFIED-NAME is the name to import.
Returns a function/closure to invoke the necessary buffer operations to perform the insertion."
  (lambda ()
    (goto-char (point-at-eol))
    (if (equal (point) (point-max)) (newline) (forward-char 1))
    (scala-import-past-starting-point starting-point)
    (save-excursion (insert (funcall java-scala-new-import qualified-name)))
    (scala-import-indent-line)))

(defun scala-import-insert-new-scala-import-grouped-package-next-line (current-import qualified-name)
  "Insert new grouped import on the next line in the buffer, overriding the whole line.
CURRENT-IMPORT is qualified name of the import line where the base package matches that of QUALIFIED-NAME.
QUALIFIED-NAME is the name to import.
Returns a function/closure to invoke the necessary buffer operations to perform the insertion."
  (let* ((current-import-components (->> current-import (s-split "\\.")))
         (base-package (->> current-import-components -butlast (s-join ".")))
         (current-imports (->> current-import-components -last-item))
         (qualified-class-name (->> qualified-name (s-split "\\.") -last-item))
         (new-imports (->> current-imports (s-chop-prefix "{") (s-chop-suffix "}")
                           (s-split ",") (-map 's-trim)
                           (cons qualified-class-name) (-sort 's-less?) (s-join ", "))))
    (lambda ()
      (cond
        ((equal (point) (point-max)) (newline))
        ; if the import statement is at point-min we can't be above it and are actually at point-at-bol
        ((equal (point) (point-at-eol)) (forward-char 1)))
      (kill-line)
      (->> (scala-import-scala-new-import-grouped-package base-package new-imports)
           insert save-excursion)
      (scala-import-indent-line))))

(defun scala-import-move-to-end-of-multiline-import ()
  "Move point to the end of a multiline import."
  (when (not (string-match (rx (zero-or-more any) ?} (zero-or-more whitespace) eol) (thing-at-point 'line)))
      (goto-char (point-at-eol))
      (when (not (equal (point) (point-max)))
        (forward-line 1)
        (scala-import-move-to-end-of-multiline-import)
      )))

(defun scala-import-at-start-of-multiline-block ()
  "Returns t when at start of multiline block."
      (string-match (rx (zero-or-more anything) ?{ (zero-or-more (not (any ?})))) (thing-at-point 'line)))

(defun scala-import-scala-new-import-insertion-decisioning-in-import-block (insertion-range starting-point qualified-name)
  "Search through import statements in buffer above INSERTION-RANGE and STARTING-POINT.
Decide what line to insert QUALIFIED-NAME."
  (let ((looking-at-import? (looking-at "[\n\t ]*import\\s-\\(.+\\)\n"))
        (matching-import (match-string 1)))
    (cond
     ;; insert at the end of the import block
     ((not looking-at-import?)
      (when (scala-import-at-start-of-multiline-block) (scala-import-move-to-end-of-multiline-import))
      (scala-import-insert-new-import-next-line starting-point 'scala-import-scala-new-import qualified-name))
     ;; same base package, insert on next line, overriding the entire line
     ((scala-import-same-base-package-p matching-import qualified-name)
      (scala-import-insert-new-scala-import-grouped-package-next-line matching-import qualified-name))
     ;; insert next line
     ((not (s-less? matching-import qualified-name))
      (scala-import-insert-new-import-next-line starting-point 'scala-import-scala-new-import qualified-name))
     ;; continue looking for insertion point
     (t
      (search-forward-regexp "^\\s-*import\\s-" insertion-range t)
      (goto-char (point-at-eol))
      (scala-import-scala-new-import-insertion-decisioning-in-import-block insertion-range starting-point qualified-name)))))


(defun scala-import-insert-scala-import (insertion-range starting-point qualified-name)
  "A simple scala import insertion in buffer above INSERTION-RANGE and STARTING-POINT.
Decide what line to insert QUALIFIED-NAME."
  (cond
   ((scala-import-no-imports-in-buffer-p)
    (scala-import-insert-new-import-no-imports-in-buffer starting-point
                                                   'scala-import-scala-new-import
                                                   qualified-name))
   ((scala-import-import-block-in-buffer-p)
    (unless (equal (point) (point-min)) (backward-char))
    (scala-import-scala-new-import-insertion-decisioning-in-import-block insertion-range
                                                                   starting-point
                                                                   qualified-name))
   ;; Neither import nor package: stay at beginning of buffer
   (t
    (scala-import-insert-new-import-no-imports-or-package-in-buffer starting-point
                                                              'scala-import-scala-new-import
                                                              qualified-name))))

(defun scala-import-insert-import (qualified-name)
  "A simple import insertion in buffer of QUALIFIED-NAME."
  (save-excursion
    (let ((insertion-range (point))
          (starting-point (point))
          (insert-import-fn 'scala-import-insert-scala-import))
      (goto-char (point-min))
      (let ((finished? nil))
        (while (not finished?)
          (let ((prev (point)))
            (cond ((not (search-forward-regexp
                         "^\\s-*package\\s-+\\(.+?\\)\\(?:\\s-\\|$\\)"
                         nil t))
                   ;; No more package statements
                   (setq finished? t))
                  ((string= (match-string 1) "object")
                   ;; Found a package object - reverting
                   (goto-char prev)
                   (setq finished? t))))))
      (search-forward-regexp "^\\s-*import\\s-" insertion-range t)
      (goto-char (point-at-bol))
      (funcall (funcall insert-import-fn insertion-range starting-point qualified-name)))))

(provide 'scala-import)
