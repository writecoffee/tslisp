;; util.el -- Miscellaneous and common utilities for elisp.

;; Copyright (C) 2014-2015 Silao Xu

;; Author: Silao Xu
;; URL: https://github.com/writecoffee/tslisp
;; Version: 0.1.0

;;; Commentary:

;; Miscellaneous and common, frequently used elisp utilities.

;;; Code:

(defun util::get-hash-keys (hashtable)
  "Return all keys in hashtable."
  (lexical-let (allkeys)
    (maphash (lambda (key _)
               (setq allkeys (cons key allkeys)))
             hashtable)
    allkeys))

(defun util::princ-list (list-elements &optional delim)
  "Princ a list of elements with DELIM. The default DELIM is space."
  (lexical-let ((delim (if delim delim " ")))
    (pcase list-elements
      (`(,head ,mid . ,remaining) (progn (princ head)
                                         (princ delim)
                                         (util::princ-list (cons mid remaining) delim)))

      (`(,head . nil) (progn (princ head)
                             (princ "\n")))
      (`nil (princ "\n")))))

(defun util::concat-with-delim (list-elements &optional delim)
  "Form a string by a list of elements with DELIM in the middle. The default DELIM is space."
  (letrec ((delim (if delim delim " "))
                 (aux (lambda (list-elements delim)
                        (pcase list-elements
                          (`(,head ,mid . ,remaining) (concat head
                                                              delim
                                                              (funcall aux (cons mid remaining) delim)))
                          (`(,head . nil) (concat head "\n"))
                          (`nil "\n")))))
    (funcall aux list-elements delim)))

(defun util::split-string-twice (str ignore-empty delim1 delim2)
  "Split a string by DELIM1 and then DELIM2 into a list."
  (reduce (lambda (acc inter-column)
            (append acc (split-string inter-column delim2)))
          (split-string str delim1 ignore-empty)
          :initial-value nil))

(defun util::read-lines (file-path &optional with-line-number)
  "Return a list of lines of a file at FILE-PATH by reading the file into temporary buffer firstly."
  (with-temp-buffer
    (insert-file-contents file-path)

    (lexical-let ((lines-read (split-string (buffer-string) "\n" t)))
      (if (not with-line-number)
          lines-read
        (reduce (lambda (acc line)
                  (append acc (list (list line (1+ (length acc))))))
                lines-read
                :initial-value nil)))))

(defun util::split-string-into-line-column-pair (line delimeter &optional ignore-empty)
  "Split a string into a list of line-column pair."
  (reduce (lambda (acc line-cur)
            (append acc
                    (list (list line-cur (1+ (length acc))))))

          (split-string line delimeter ignore-empty)
          :initial-value nil))

(defun util::base64-encode-hex-string (hex-string)
  "Base64-encode HEX-STRING and return the result. For example:
e9f5713dec55d727bb35392cec6190ce will be turned into 6fVxPexV1ye7NTks7GGQzg=="

  (lexical-let* ((length-of-hex-string (length hex-string))
                 (bytes (let ((acc nil))
                          (cl-loop for i from 0 below length-of-hex-string by 2
                                   do (setq acc
                                            (cons (string-to-number (substring hex-string
                                                                               i
                                                                               (+ 2 i))
                                                                    16)
                                                  acc))
                                   finally return (reverse acc))))

                 (result nil)
                 (padding (pcase (mod (length bytes) 3)
                            (`0 nil)
                            (`1 "==")
                            (`2 "=")))
                 (padding-0 (pcase (mod (length bytes) 3)
                              (`0 nil)
                              (`1 "\0\0")
                              (`2 "\0")))
                 (pad-len (length padding))
                 (padded-bytes (vconcat (concat bytes (vconcat padding-0))))
                 (len (+ (length bytes) pad-len)))

    (cl-loop for i from 0 below len by 3
             do (lexical-let* ((n (+ (lsh (aref padded-bytes i) 16)
                                     (lsh (aref padded-bytes (1+ i)) 8)
                                     (aref padded-bytes (1+ (1+ i)))))
                               (n1 (logand (lsh n -18) 63))
                               (n2 (logand (lsh n -12) 63))
                               (n3 (logand (lsh n -6) 63))
                               (n4 (logand n 63)))

                  (setq result (concat result
                                       (char-to-string (aref base64-chars n1))
                                       (char-to-string (aref base64-chars n2))
                                       (char-to-string (aref base64-chars n3))
                                       (char-to-string (aref base64-chars n4)))))

             finally return (concat (substring result 0 (- pad-len)) padding))))

(provide 'util)
