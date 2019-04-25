;;; org-export-json.el --- export org-mode files as JSON          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jared Lumpe

;; Author: Jared Lumpe
;; Keywords: outlines

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

;;

;;; Code:


(require 'org)
(require 'json)


;;; Utility code

(defun org-json--plist-get-default (plist key default)
	"Get value from plist or default if key is not present."
	(if (plist-member plist key) (plist-get plist key) default))


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
	(if value t :json-false))

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
	"Convert a property list into a value to be passed to json-encode.

	Will be converted to a hash map, because json-encode will handle this
	as an empty object instead of null.
	"
	(let ((myhash make-hash-table :test 'equal))
		(dolist (property (plist-get-keys value))
			(puthash property (plist-get property value) hash))
		myhash))

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


(defun org-json--make-error (message &rest objects)
	"Make a JSON object with an error message"
	(let ((formatted (apply `format message objects)))
		`((_error . ,formatted))))

(defun org-json--maybe-error (strict message &rest objects)
	"Throw an actual error if strict is non-nil, else return a JSON error object."
	(if strict
		(apply 'error message objects)
		(apply 'org-json--make-error message objects)))


(setq org-json-property-formatters-plist
	'(
		 bool org-json-format-bool
		 string org-json-format-string
		 number org-json-format-number
		 timestamp org-json-format-timestamp
		 strlist org-json-format-array
		 secondary-string org-json-format-secondary-string
		 t org-format-generic))


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
	(let ((output (make-hash-table :test 'equal))
	      (keys (org-json--plist-get-default options :keys (plist-get-keys properties)))
	      (formatters (org-json--plist-get-default options :formatters org-json-property-formatters-plist))
	      (default-type (plist-get options :default-type))
	      (default-formatter (plist-get options :default-formatter)))
		(dolist (key keys)
			(let* ((value (plist-get properties key))
			       (proptype (org-json--plist-get-default property-types key default-type))
			       (formatter (org-json--plist-get-default formatters proptype default-formatter)))
				;; (princ "KEY: ")
				;; (princ key)
				;; (princ "\n")
				(catch 'skipprop
					;; Key not present in property plist, skip
					(unless (plist-member properties key)
						(throw 'skipprop nil))
					;; (princ "  in properties!\n")
					;; Type explicitly set to nil in property-types, skip
					(when (and (plist-member property-types key) (not proptype))
						;; (princ "  nil type\n")
						(throw 'skipprop nil))
					;; Have property type but no formatter for it
					(when (and proptype (not formatter))
						(error "No formatter for property type %s" proptype))
					;; Valid formatter
					(when formatter
						;; (princ "  formatting!\n")
						(puthash key (funcall formatter value) output))
					)))
		output))


;;; Encode org AST nodes

(setq org-json-node-property-types-plist
	'(
		all (
			:parent nil
			;; :begin nil
			;; :end nil
			:contents-begin nil
			:contents-end nil
			:post-affiliated nil
			:pre-blank nil
			:post-blank nil)
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
		))


(defun org-json--get-node-properties-plist (node)
	"Get a plist of all properties for an AST node."
	(nth 1 node))


;; (defun org-json--get-property-type (eltype property)
;; 	"Get the type of a property from org-json-node-property-types-plist by node type and property name."
;; 	(catch 'proptype
;; 		(dolist (proptypes (list
;; 							   (plist-get org-json-node-property-types-plist eltype)
;; 							   (plist-get org-json-node-property-types-plist 'all)))
;; 			(if (plist-member proptypes property)
;; 				(throw 'proptype (plist-get proptypes property))))
;; 		nil))

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
			 (keywords (make-hash-table :test 'equal))
			 (contents nil))
		;; Iterate over contents
		(dolist (item (org-element-contents node))
			(if (equal (org-element-type item) 'keyword)
				;; Intercept keyword nodes and add to hash
				(puthash
					(org-element-property :key item)
					(org-json-format-generic (org-element-property :value item))
					keywords)
				;; Otherwise add to contents list
				(add-to-list 'contents item t)))
		(list
			(cons 'org_node_type node-type)
			(cons 'properties (org-json--format-node-properties node))
			(cons 'keywords keywords)
			(cons 'contents (org-json-format-list-generic contents)))))


(defun org-json-encode-node (node)
	"Encode an org mode AST node into a JSON string."
	(json-encode (org-json-format-node node)))


(defun org-json-export-file ()
	(interactive)
	(write-region
		(org-json-encode-node (org-element-parse-buffer))
		nil
		(concat (buffer-file-name) ".json")))


;;; Agenda

(setq org-json-agenda-property-types-plist
	'(
		breadcrumbs string
		done-face string
		dotime string
		duration string
		extra string
		face string
		;; format string
		help-echo string
		level string
		mouse-face string
		org-agenda-type string
		org-category string
		org-complex-heading-regexp string
		;; org-hd-marker string
		org-highest-priority number
		org-last-args string
		org-lowest-priority number
		;; org-marker string
		org-not-done-regexp string
		;; org-redo-cmd string
		org-series-cmd string
		org-todo-regexp string
		priority number
		priority-letter string
		tags string
		time string
		time-of-day string
		todo string
		todo-state string
		ts-date string
		txt string
		type string
		))


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
		(when (not (eq eltype 'headline))
			(error "Expected headline element at point, got %s" eltype))
		(org-element-put-property elem :title
			(org-element-parse-secondary-string title (alist-get 'headline org-element-object-restrictions)))
		elem))


(defun org-json-format-agenda-info (info)
	"Transform agenda item info into a format that can be passed to json-encode"
	(let* ((formatted (org-json--format-property-values info org-json-agenda-property-types-plist
				:keys (plist-get-keys org-json-agenda-property-types-plist)))
			  (marker (plist-get info 'org-marker))
			  (info-file (buffer-file-name (marker-buffer marker))))
		(puthash 'file info-file formatted)
		(puthash 'file-relative (file-relative-name info-file org-directory) formatted)
		(org-with-point-at marker
			(org-with-wide-buffer
				;; (puthash 'node (org-json-format-node (org-element-at-point)) formatted)
				(puthash 'node (org-json-format-node (org-json--headline-at-point)) formatted)
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
	where CMD-KEY and PARAMETERS are the arguments to org-batch-agenda."
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
