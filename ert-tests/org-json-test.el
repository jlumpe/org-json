(require 'org-json)


(ert-deftest test-encode-buffer ()
	"Check against expected output"
	(setq expected (json-read-file "expected.json"))
	(setq encoded (with-current-buffer (find-file-noselect "test.org")
						(org-json-encode-buffer)))
	(setq parsed (json-read-from-string encoded))
	(should (equal parsed expected)))
