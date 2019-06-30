# org-json

[![Build Status](https://travis-ci.org/jlumpe/org-json.svg?branch=master)](https://travis-ci.org/jlumpe/org-json)

This package is for encoding all or part of an Org mode file`s Abstract Syntax
Tree (AST) into JSON format so that it can be read by other tools.


## Usage

The primary interactive entry point is `org-json-export-buffer`, which prompts for a file name and
exports the entire buffer's AST. The equivalent API function is `org-json-encode-buffer`, which
returns a string. Additionally, you can use `org-json-encode-node` to encode an AST node (org
element or object) from the data structure returned by one of the org-element functions
(`org-element-parse-buffer`, `org-element-at-point`, etc.).

## Output

The output looks like:

```
{
  "$$data_type": "org-node",
  "type": "headline",
  "properties": { ... },
  "keywords": { ... },
  "contents": [ ... ]
}
```

`"type"` is the node type returned by `org-element-type` and `"properties"` are property names and
values obtained from `org-element-property` (see documentation for the
[Org element API](https://orgmode.org/worg/dev/org-element-api.html) for a complete list of all node
types and properties). Leading colons in the property keys are omitted. `"contents"` is the encoded
return value of `org-element-contents`, the items of which are either more org nodes or strings. Any
elements of type "keyword" are omitted from the contents list and instead included as key-value
pairs in the `"keywords"` property, which is not included otherwise.


### Nested data objects

The `"$$data-type"` property is added to differentiate encoded org nodes and other data types from
generic sets of key/value pairs that occur in alists or plists (the latter of which has
`"$$data-type": "mapping"`).

Additional data types are:

```
{
  "$$data-type": "error",
  "message": "Describes an error in automatically encoding this data structure."
}
```


## Agenda export

Agenda export is a work in progress. Use `org-json-encode-agenda-buffer` to encode all items in an
active agenda buffer. Use the macro `org-json-with-agenda-buffer` to run an agenda command in a
temporary buffer, like so:

```elisp
(org-json-with-agenda-buffer "t"
  (setq my-json-string (org-json-encode-agenda-buffer))
```

There is no interactive command yet.


## Notes

The resulting JSON *should* include the correct choice of empty object ("{}"),
empty list ("[]"), null, or false for the given context, even though these are
given a value of nil in elisp (don`t get me started).
