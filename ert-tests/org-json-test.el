(require 'org-json)
(require 'cl-lib)
(require 'subr-x)


(defun cmpfailed (path msg &rest rem)
    "Make the error message for json-compare."
    (format
        "Comparison failed at %s: %s"
        (if path
            (format "path %S" (reverse path))
            "top level")
        (apply 'format msg rem)))

(defun json-cmp-types (type1 type2 path)
  (equal type1 type2))

(put
    'json-cmp-types
    'ert-explainer
    (lambda (type1 type2 path)
        (cmpfailed path "Values have different type (%s, %s)" type1 type2)))

(defun json-cmp-scalars (value1 value2 path)
    (equal value1 value2))

(put
    'json-cmp-scalars
    'ert-explainer
    (lambda (value1 value2 path)
        (cmpfailed path "Scalar values differ (%S, %S)" value1 value2)))

(defun json-cmp-has-key (name key keylist path)
    (member key keylist))

(put
    'json-cmp-has-key
    'ert-explainer
    (lambda (name key keylist path)
        (cmpfailed path "%s missing property \"%s\"" name key)))

(defun json-cmp-lengths (len1 len2 path)
    (= len1 len2))

(put
    'json-cmp-lengths
    'ert-explainer
    (lambda (len1 len2 path)
        (cmpfailed path "Arrays have different lengths (%d, %d)" len1 len2)))

(defun json-cmp-objects (table1 table2 path)
  "Compare JSON objects decoded as hash tables."
  (let ((keys1 (hash-table-keys table1))
        (keys2 (hash-table-keys table2)))
      ; All keys in first
      (dolist (key1 keys1)
        ; Key missing from table 2
        (should (json-cmp-has-key "Object 2" key1 keys2 path))
        ; Recursively compare values
        (json-compare
          (gethash key1 table1)
          (gethash key1 table2)
          (cons key1 path)))
      ; Check keys in table 2 missing in 1
      (dolist (key2 keys2)
        (should (json-cmp-has-key "Object 1" key2 keys1 path)))))

(defun json-cmp-arrays (vec1 vec2 path)
  "Compare JSON arrays decoded as vectors."
  (let ((len1 (length vec1))
        (len2 (length vec2)))
    ; Compare lengths
    (should (json-cmp-lengths len1 len2 path))
    ; Compare values
    (cl-loop
      for i from 0 to (- len1 1)
      with val1 = (aref vec1 i)
      with val2 = (aref vec2 i)
      do (json-compare val1 val2 (cons i path)))))

(defun json-compare (data1 data2 &optional path)
  "Recursively compare two decoded JSON values."
  (let ((type1 (type-of data1))
        (type2 (type-of data2)))
    ; Compare types
    (should (json-cmp-types type1 type2 path))
    ; Comparison based on  type
    (cl-case type1
      ((hash-table) (json-cmp-objects data1 data2 path))
      ((vector) (json-cmp-arrays data1 data2 path))
      ; Scalars, use direct equality
      (t (should (json-cmp-scalars data1 data2 path))))))


(ert-deftest test-encode-buffer ()
  "Check against expected output"
  (let ((json-object-type 'hash-table)
        (json-array-type 'vector)
        (expected nil)
        (encoded  nil)
        (parsed nil))
    (setq expected (json-read-file "expected.json"))
    (setq encoded
      (with-current-buffer (find-file-noselect "test.org")
        (org-json-encode-buffer)))
    (setq parsed (json-read-from-string encoded))
      (json-compare expected parsed)))
