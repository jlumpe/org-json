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
	encoding, but definitely for making printed value legible) and return. If nil
	return nil. Otherwise throw an error.
	"
	(cond
		((not value)
			nil)
		((stringp value)
			(substring-no-properties value))
		(t
			(error "Expected string value or nil, got %s" (type-of value)))))

(defun org-json-format-timestamp (value)
	"Convert a timestamp into a value to be passed to json-encode."
	value) ; TODO

(defun org-json-format-plist (value)
	"Convert a property list into a value to be passed to json-encode.

	Will be converted to a hash map, because json-encode will handle this
	as an empty object instead of null.
	"
	(let ((myhash make-hash-table :test 'equal))
		(dolist (property (plist-get-keys value))
			(puthash property (plist-get property value) hash))
		myhash))


(setq org-json-property-formatters-alist
	'(
		 (bool . org-json-format-bool)
		 (string . org-json-format-string)
		 (timestamp . org-json-format-timestamp)
		 (strlist . org-json-format-array)
		 (secondary-string . org-json-format-list-generic)))


(setq org-json-property-types-plist
	`(
		all (
			:parent skip
			:begin skip
			:end skip
			:contents-begin skip
			:contents-end skip
			:post-affiliated skip
			:pre-blank skip
			:post-blank skip)
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
			:structure skip
			:tag secondary-string)
		macro (
			:args strlist)
		plain-list (
			:structure skip)
		planning (
			:closed timestamp
			:deadline timestamp
			:scheduled timestamp)
		property-drawer (
			:properties skip) ; TODO
		))


(defun org-json--make-error (message &rest objects)
	"Make a JSON object with an error message"
	(let ((formatted (eval `(format ,message ,@objects))))
		`((_error . ,formatted))))

(defun org-json--maybe-error (strict message &rest objects)
	"Throw an actual error if strict is non-nil, else return a JSON error object."
	(if strict
		(eval `(error ,message ,@objects))
		(eval `(org-json--make-error ,message ,@objects))))


(defun org-json--list-elem-properties (elem)
	(plist-get-keys (nth 1 elem)))


(defun org-json--get-property-type (eltype property)
	(catch 'proptype
		(dolist (proptypes (list
							   (plist-get org-json-property-types-plist eltype)
							   (plist-get org-json-property-types-plist 'all)))
			(if (plist-member proptypes property)
				(throw 'proptype (plist-get proptypes property))))
		nil))


(defun org-json--format-elem-properties (element)
	(let* ((eltype (org-element-type element))
		   (propvals (make-hash-table :test 'equal))
		   (propval nil)
		   (proptype nil)
		   (formatter nil)
		   (formatted nil))
		(dolist (propname (org-json--list-elem-properties element))
			(setq propval (org-element-property propname element))
			(setq proptype (org-json--get-property-type eltype propname))
			(setq formatter (alist-get proptype org-json-property-formatters-alist 'org-json-format-generic))
			(catch 'skipprop
				(setq formatted
					(cond
						; Proptype nil, use default
						;; ((not proptype) propval)
						; Skip
						((eq proptype 'skip) (throw 'skipprop nil))
						; Formatter exists
						(formatter (funcall formatter propval))
						; Formatter not found
						(t
							(error "No formatter for property type %s" proptype)
							(throw 'skipprop))))
				(puthash propname formatted propvals)))
		propvals))


(defun org-json-format-generic (value &optional strict)
	"Format a generic value for JSON output."
	(cond
		;; Pass "booleans" (t + nil), numbers, symbols through
		((booleanp value) value)
		((numberp value) value)
		((symbolp value) value)
		((stringp value)
			(org-json-format-string value))
		; An org element
		((org-element-type value)
			(org-json-format-element value))
		; Unknown
		(t (org-json--maybe-error strict "Couldn't automatically encode value of type %s" (type-of value)))))


(defun org-json-format-list-generic (value)
	"Map org-json-format-generic over a list and return a vector."
	(org-json-format-array (mapcar 'org-json-format-generic value)))


(defun org-json-format-element (element)
	"Transform an org mode element/object into a format that can be passed to json-encode."
	(list
		(cons 'org_element_type (org-element-type element))
		(cons 'properties (org-json--format-elem-properties element))
		(cons 'contents (org-json-format-list-generic (org-element-contents element)))))


(defun org-json-encode-element (element)
	"Encode an org mode element into a JSON string."
	(json-encode (org-json-format-element element)))


(provide 'org-export-json)

;;; org-export-json.el ends here
