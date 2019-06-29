;;; org-export-json.el --- Export org-mode files as JSON          -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jared Lumpe

;; Author: Jared Lumpe <mjlumpe@gmail.com>
;; Version: 0.1
;; Keywords: outlines
;; Homepage: https://github.com/jlumpe/org-export-json

;; Package-Requires: ((emacs "25") (org "9"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is for encoding all or part of an Org mode file's Abstract Syntax
;; Tree (AST) into JSON format so that it can be read by other tools. The primary
;; interactive entry point is `org-json-export-buffer', which prompts for a file
;; name and exports the entire buffer's AST. The equivalent API function is
;; `org-json-encode-buffer', which returns a string. Additionally, you can use
;; `org-json-encode-node' to encode an AST node (org element or object) from the
;; data structure returned by one of the org-element functions
;; (`org-element-parse-buffer', `org-element-at-point', etc.).

;; The output looks like:

;; {
;;   "$$data_type": "org-node",
;;   "type": "headline",
;;   "properties": { ... },
;;   "keywords": { ... },
;;   "contents": [ ... ]
;; }

;; "type" is the node type returned by `org-element-type'. "properties" are
;; property names and values (leading colons in the key names are omitted)
;; obtained from `org-element-property'. "contents" is the encoded return
;; value of `org-element-contents', the items of which are either more org
;; nodes or strings. Any elements of type "keyword" are omitted from the
;; contents list and instead included as key-value pairs in the "keywords"
;; property, which is not included otherwise.

;; The "$$data-type" property is added to differentiate encoded org nodes
;; and other data types from generic sets of key/value pairs that occur in
;; alists or plists (the latter of which has "$$data-type": "mapping").

;; Additional data types are:

;; {
;;   "$$data-type": "error",
;;   "message": "Describes an error in automatically encoding this data structure."
;; }

;; The resulting JSON *should* include the correct choice of empty object ("{}"),
;; empty list ("[]"), null, or false for the given context, even though these are
;; given a value of nil in elisp (don't get me started).

;; See URL `https://orgmode.org/worg/dev/org-element-api.html' for the complete list
;; of element/object types and their properties.

;;; Code:


(require 'org)
(require 'json)



;;; Variables
(defcustom org-json-data-type-property "$$data_type"
  "This property is added to all objects in the exported JSON to indicate the data type of the object.

Set to nil to disable."
  :type '(string))

(defcustom org-json-property-formatters-plist
  '(
    bool             org-json-format-bool
    string           org-json-format-string
    number           org-json-format-number
    timestamp        org-json-format-timestamp
    strlist          org-json-format-array
    secondary-string org-json-format-secondary-string
    plist            org-json-format-plist
    alist            org-json-format-alist
    t                org-json-format-generic
    src-block-parameters org-json--format-src-block-parameters
    )
  "Controls how different parameter data types are converted to JSON.

plist which maps names of property data types to names of functions which
format the data to a value that can be passed to the `json-encode' function."
  :type '(plist :value-type symbol))

(defcustom org-json-node-property-types-plist
  '(
    all (
      ; Never include parent, leads to infinite recursion
      :parent nil
      ; These properties have to do with absolute buffer positions and thus probably aren't useful to export
      :begin nil
      :end nil
      :contents-begin nil
      :contents-end nil
      ; These can be useful when converting from JSON to another format
      :post-affiliated number
      :pre-blank number
      :post-blank number)
    babel (
      :result strlist)
    entity (
      :latex-math-p bool
      :use-brackets-p bool)
    example-block (
      :preserve-indent bool
      :retain-labels bool
      :use-labels bool)
    headline (
      :archivedp bool
      :commentedp bool
      :deadline timestamp
      :footnote-section-p bool
      :quotedp bool
      :scheduled timestamp
      :tags strlist
      :title secondary-string)
    inlinetask (
      :closed timestamp
      :deadline timestamp
      :scheduled timestamp
      :title secondary-string)
    item (
      :structure nil
      :tag secondary-string)
    macro (
      :args strlist)
    plain-list (
      :structure nil)
    planning (
      :closed timestamp
      :deadline timestamp
      :scheduled timestamp)
    property-drawer (
      :properties nil) ; TODO
    src-block (
        :parameters src-block-parameters)
     )
  "Data types of node properties by node type.

plist of plists. Outer plist keys are node types (see `org-element-type')
with the \"all\" key matching all types. Inner plists map
property names (see `org-element-properties') to data types
in the `org-json-property-formatters-plist' variable. A value of nil
means the property will be skipped."
  :type '(plist :value-type (plist :value-type symbol)))

(defcustom org-json-agenda-property-types-plist
  '(
    agenda-day                 string
    breadcrumbs                string
    date                       string
    day                        string
    ;; done-face                  string
    dotime                     string
    duration                   string
    extra                      string
    ;; face                       string
    ;; format
    ;; help-echo                  string
    level                      string
    ;; mouse-face                 string
    org-agenda-type            string
    org-category               string
    ;; org-complex-heading-regexp string
    org-day-cnt t
    ;; org-hd-marker
    org-highest-priority       number
    ;; org-last-args
    org-lowest-priority        number
    ;; org-marker
    ;; org-not-done-regexp        string
    ;; org-redo-cmd
    org-series-cmd             string
    ;; org-todo-regexp            string
    priority                   number
    priority-letter            string
    tags                       string
    time                       string
    time-of-day                string
    todo                       string
    todo-state                 string
    ts-date                    number
    txt                        string
    type                       string
    ;; undone-face                string
    warntime                   t
    )
  "Data types of agenda properties

plist mapping agenda item property names to data types in the
`org-json-property-formatters-plist' variable. A value of nil means the property
will be skipped."
  :type '(plist :value-type symbol))


;;; Utility code

(defun org-json--plist-get-default (plist key default)
  "Get value from plist or default if key is not present."
  (if (plist-member plist key) (plist-get plist key) default))

(defun org-json--make-object-hash (&optional data-type)
  "Create a hash table to store data for a JSON object.

  DATA-TYPE is a string indicating how the object data should be interpreted,
  e.g. \"mapping\" for a standard mapping with string keys. The hash table
  will be initialized with a property having this value and name according
  to the value of the org-json-data-type-property variable. If either is nil
  this step is skipped.

  This function should be used for all data to be converted into a JSON object
  because json-encode will always handle it correctly even if empty."
  (let ((objhash (make-hash-table :test 'equal)))
    (when (and org-json-data-type-property data-type)
      (puthash org-json-data-type-property data-type objhash))
    objhash))


;;; Formatting generic values for JSON encoding

(defun org-json-format-array (value)
  "Convert a list or sequence into a value to be passed to json-encode.

  Needs to convert nil into something that will be encoded as an empty
  array, not null.
  "
  (seq-into value 'vector))

(defun org-json-format-bool (value)
  "Convert a t/nil value into a value to be passed to json-encode.

  Needs to convert nil into something that will be encoded as false
  instead of null.
  "
  (if value t json-false))

(defun org-json-format-string (value)
  "Convert a string value into a value to be passed to json-encode.

  If value is a string, strip properties (don't know if that matters for JSON
  encoding, but definitely for making printed value legible) and return. If symbol
  return its name. If nil return nil. Otherwise throw an error.
  "
  (cond
    ((not value)
      nil)
    ((stringp value)
      (substring-no-properties value))
    ((symbolp value)
      (symbol-name value))
    (t
      (error "Expected string value or nil, got %s" (type-of value)))))

(defun org-json-format-number (value)
  (if (or (not value) (numberp value))
    value
    (error "Expected numeric value or nil, got %s" (type-of value))))

(defun org-json-format-timestamp (value)
  "Convert a timestamp into a value to be passed to json-encode."
  (if value (org-json-format-node value) nil))

(defun org-json-format-plist (value)
  "Convert a property list into a value to be passed to json-encode."
  (let ((objhash (org-json--make-object-hash "mapping")))
    (dolist (property (plist-get-keys value))
      (puthash property (org-json-format-generic (plist-get property value)) objhash))
    objhash))

(defun org-json-format-alist (value)
  "Convert an alist into a value to be passed to json-encode."
  (let ((objhash (org-json--make-object-hash "mapping")))
    (dolist (pair value)
      ; Should be a cons cell, skip otherwise
      (when (consp pair)
        (puthash (car pair) (org-json-format-generic (cdr pair)) objhash)))
    objhash))

(defun org-json-format-generic (value &optional strict)
  "Format a generic value for JSON output."
  (cond
    ;; Pass "booleans" (t + nil), numbers, symbols through
    ((booleanp value) value)
    ((numberp value) value)
    ((symbolp value) value)
    ((stringp value)
      (org-json-format-string value))
    ; An org AST node
    ((org-element-type value)
      (org-json-format-node value))
    ; Unknown
    (t (org-json--maybe-error strict "Couldn't automatically encode value of type %s" (type-of value)))))

(defun org-json-format-list-generic (value)
  "Map org-json-format-generic over a list and return a vector."
  (org-json-format-array (mapcar 'org-json-format-generic value)))

(defun org-json-format-secondary-string (value)
  "Format a \"secondary string\" property, which as far as I can tell is either a single string or list
    of strings or other types.

    Always return an array for this.
    "
  (org-json-format-list-generic (if (stringp value) (list value) value)))


(defun org-json--format-src-block-parameters (parameters)
  "Special case formatter for :parameters property of src-block element.

    This is an alist with some information that would be useful to exporters trying
    to convert the data to another format (specifically the :export key).
    "
  (let ((parsed (org-babel-parse-header-arguments parameters))
        (objhash (org-json--make-object-hash "mapping"))
        (key nil)
        (val nil)
        (varlist nil))
    (dolist (pair parsed)
      (setq key (car pair) val (cdr pair))
      (case key
        ; This key can appear multiple times, add to list
        ((:var) (push val varlist))
        ; Strings
        ((:results :file :export)
          (puthash key val objhash))
        ((:colnames)
          ; Single string
          (when (stringp val)
            (puthash key val objhash))
          ; List of strings
          (when (listp val)
            (puthash key (org-json-format-array val) objhash)))
        ))
    (puthash :var (org-json-format-array varlist) objhash)
  objhash))


(defun org-json--make-error (message &rest objects)
  "Make a JSON object with an error message"
  (let ((errobj (org-json--make-object-hash "error")))
    (puthash 'message (apply `format message objects) errobj)
    errobj))

(defun org-json--maybe-error (strict message &rest objects)
  "Throw an actual error if strict is non-nil, else return a JSON error object."
  (if strict
    (apply 'error message objects)
    (apply 'org-json--make-error message objects)))


(defun org-json--format-property-values (properties property-types &rest options)
  "Format property values based on their types.

  PROPERTIES is a property plist. PROPERTY-TYPES is a plist mapping property keys to type symbols.
  OPTIONS is an additional plist of options.

  Keys of OPTIONS are:

  :keys - property keys to format. Defaults to all keys of PROPERTIES.
  :formatters - plist mapping formatter names to function names. Defaults to org-json-property-formatters-plist.
  :default-type - Default type symbol for keys not in PROPERTY-TYPES. Note that properties with a type
    of nil will always be skipped.
  :default-formatter - Default formatter function name for keys not in PROPERTY-TYPES.

  Returns a hash table."
  (let ((output (org-json--make-object-hash "mapping"))
        (keys (org-json--plist-get-default options :keys (plist-get-keys properties)))
        (formatters (org-json--plist-get-default options :formatters org-json-property-formatters-plist))
        (default-type (plist-get options :default-type))
        (default-formatter (plist-get options :default-formatter)))
    (dolist (key keys)
      (let* ((value (plist-get properties key))
             (proptype (org-json--plist-get-default property-types key default-type))
             (formatter (org-json--plist-get-default formatters proptype default-formatter)))
        (catch 'skipprop
          ;; Key not present in property plist, skip
          (unless (plist-member properties key)
            (throw 'skipprop nil))
          ;; Type explicitly set to nil in property-types, skip
          (when (and (plist-member property-types key) (not proptype))
            (throw 'skipprop nil))
          ;; Have property type but no formatter for it
          (when (and proptype (not formatter))
            (error "No formatter for property type %s" proptype))
          ;; Valid formatter
          (when formatter
            (puthash key (funcall formatter value) output))
          )))
    output))


;;; Encode org AST nodes


(defun org-json--get-node-properties-plist (node)
  "Get a plist of all properties for an AST node."
  (nth 1 node))


;; (defun org-json--get-property-type (eltype property)
;;   "Get the type of a property from org-json-node-property-types-plist by node type and property name."
;;   (catch 'proptype
;;     (dolist (proptypes (list
;;                  (plist-get org-json-node-property-types-plist eltype)
;;                  (plist-get org-json-node-property-types-plist 'all)))
;;       (if (plist-member proptypes property)
;;         (throw 'proptype (plist-get proptypes property))))
;;     nil))

(defun org-json--get-node-property-types (eltype)
  "Get plist of property types for a given org node type."
  (org-combine-plists
    (plist-get org-json-node-property-types-plist 'all)
    (plist-get org-json-node-property-types-plist eltype)))


(defun org-json--format-node-properties (node)
  (org-json--format-property-values
    (org-json--get-node-properties-plist node)
    (org-json--get-node-property-types (org-element-type node))
    ;; :keys (org-json--list-node-properties node)
    :default-formatter 'org-json-format-generic))


(defun org-json-format-node (node)
  "Transform an org mode AST node into a format that can be passed to json-encode."
  (let ((node-type (org-element-type node))
        (keywords (org-json--make-object-hash "mapping"))
        (contents nil)
        (formatted (org-json--make-object-hash "org-node")))
    ;; Iterate over contents
    (dolist (item (org-element-contents node))
      (if (equal (org-element-type item) 'keyword)
        ;; Intercept keyword nodes and add to hash
        (puthash
          (org-element-property :key item)
          (org-json-format-generic (org-element-property :value item))
          keywords)
        ;; Otherwise add to contents list
        (push item contents)))
    (puthash 'type node-type formatted)
    (puthash 'properties (org-json--format-node-properties node) formatted)
    (puthash 'contents (org-json-format-list-generic (reverse contents)) formatted)
    (unless (hash-table-empty-p keywords)
      (puthash 'keywords keywords formatted))
    formatted))


(defun org-json-encode-node (node)
  "Encode an org mode AST node into a JSON string."
  (json-encode (org-json-format-node node)))


(defun org-json-encode-buffer ()
  "Encode the current org mode buffer into a JSON string."
  (org-json-encode-node (org-element-parse-buffer)))


(defun org-json-export-buffer ()
  "Export current org mode buffer to JSON file."
  (interactive)
  (let* ((default-filename (concat (file-name-nondirectory (buffer-file-name)) ".json"))
       (filename (read-file-name "Export org file to JSON: " nil nil nil default-filename 'identity)))
    (write-region (org-json-encode-buffer) nil filename)))


;;; Agenda


(defun org-json--get-agenda-lines ()
  "Get the lines of org-agenda buffer (must be current buffer) which correspond to agenda items.

  Code is derived from the org-batch-agenda-csv function."
  (seq-filter
    (lambda (line) (get-text-property 0 'org-category line))
    (org-split-string (buffer-string) "\n")))


(defun org-json--agenda-info-from-line (line)
  "Get plist of org agenda info from line of agenda buffer (returned by org-json--get-agenda-lines)

  Code is derived from the org-batch-agenda-csv function."
  (org-fix-agenda-info (text-properties-at 0 line)))


(defun org-json--headline-at-point ()
  "Like org-element-at-point but parse objects in headline's :title property."
  (let* ((elem (org-element-at-point))
       (eltype (org-element-type elem))
       (title (org-element-property :title elem)))
    (unless (eq eltype 'headline)
      (error "Expected headline element at point, got %s" eltype))
    (org-element-put-property elem :title
      (org-element-parse-secondary-string title (alist-get 'headline org-element-object-restrictions)))
    elem))


(defun org-json-format-agenda-info (info)
  "Transform agenda item info into a format that can be passed to json-encode"
  (let* ((formatted
            (org-json--format-property-values
              info
              org-json-agenda-property-types-plist
              :keys (plist-get-keys org-json-agenda-property-types-plist)))
         (marker (plist-get info 'org-hd-marker))
         (info-file (buffer-file-name (marker-buffer marker)))
         (headline nil)
         (deadline nil))
    (puthash 'file info-file formatted)
    (puthash 'file-relative (file-relative-name info-file org-directory) formatted)
    (org-with-point-at marker
      (org-with-wide-buffer
        (setq headline (org-json--headline-at-point))
        (setq deadline (org-element-property :deadline headline))
        (puthash 'node (org-json-format-node headline) formatted)
        (puthash 'deadline (if deadline (org-json-format-node deadline)) formatted)
        (puthash 'path (org-json-format-array (org-get-outline-path t)) formatted)
        ))
    formatted))


(defun org-json-encode-agenda-buffer ()
  "Encode the current agenda buffer as JSON"
  (json-encode
    (org-json-format-array
      (mapcar
        (lambda (line) (org-json-format-agenda-info (org-json--agenda-info-from-line line)))
        (org-json--get-agenda-lines)))))


(defmacro org-json-with-agenda-buffer (options &rest body)
  "Create a temporary agenda buffer and evaluate forms within it.

  OPTIONS must be either a CMD-KEY string or a list of (CMD-KEY[, PARAMETERS]),
  where CMD-KEY and PARAMETERS are the arguments to `org-batch-agenda'."
  `(let ((cmd-key)
         (parameters nil))
    ; Get cmd-key, params from options argument
    (cond
      ; Just cmd-key
      ((stringp ,options)
        (setq cmd-key ,options))
      ; List of (cmd-key[, params])
      ((listp ,options)
        (setq cmd-key (nth 0 ,options))
        (if (>= (length ,options) 2) (setq parameters (nth 1 ,options))))
      ; Not a list either, error
      (t
        (error "options argument should be string or list, got %s" (type-of ,options))))
     ;; (list cmd-key parameters)))
    (with-temp-buffer
       (let ((org-agenda-buffer-name (buffer-name)))  ; Dear god, dynamic scoping...
        ; The following is taken from org-batch-agenda:
        (org-eval-in-environment (org-make-parameter-alist parameters)
          (if (> (length cmd-key) 2)
            (org-tags-view nil cmd-key)
            (org-agenda nil cmd-key)))
         ; Replace the following from org-batch agenda with body:
         ; (set-buffer org-agenda-buffer-name)
         ; (princ (buffer-string)))
         ,@body))))


(provide 'org-export-json)

;;; org-export-json.el ends here
