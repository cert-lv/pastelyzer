(in-package #:pastelyzer.tests)

(suite 'util)

(test util.alphabet.empty ()
  (is (equal "" (pastelyzer::alphabet "")))
  (is (equal "0" (pastelyzer::alphabet "0")))
  (is (equal "0" (pastelyzer::alphabet "00")))
  (is (equal "01" (pastelyzer::alphabet "01")))
  (is (equal "01" (pastelyzer::alphabet "10")))
  (is (equal "01" (pastelyzer::alphabet "101")))
  (is (equal "01" (pastelyzer::alphabet "010")))  )

(test util.sub-alphabet ()
  (is (pastelyzer::sub-alphabet-p "" ""))
  (is (pastelyzer::sub-alphabet-p "" "ab"))
  (is (pastelyzer::sub-alphabet-p "a" "ab"))
  (is (pastelyzer::sub-alphabet-p "b" "ab"))
  (is (not (pastelyzer::sub-alphabet-p "c" "ab"))))

(test util.string-context ()
  (let ((string (format nil "aaa~C~Cwhatever NEEDLE blah blah ~C~C~Czzz"
                        ;;   ^  ^ ^ ^        ^     ^    ^     ^   ^ ^
                        ;;   0  3 4 5        14    20   25    31 33 34
                        #\linefeed #\tab #\tab #\return #\linefeed)))
    (macrolet ((check (expected result)
                 `(let ((.expected. ,expected)
                        (.result. ,result))
                    (is (string= .expected. .result.)
                        "Expected ~S, got ~S" .expected. .result.))))
      (check
       ""
       (pastelyzer::string-context-before string 0))
      (check
       "aaa..whatever "
       (pastelyzer::string-context-before string 14 :bol nil))
      (check
       "a..whatever "
       (pastelyzer::string-context-before string 14 :after 2 :trim-space t))
      (check
       "whatever "
       (pastelyzer::string-context-before string 14 :after 3 :trim-space t))
      (check
       ".whatever "
       (pastelyzer::string-context-before string 14 :bol t :trim-space nil))
      (check
       "whatever "
       (pastelyzer::string-context-before string 14 :bol t :trim-space t))
      (check
       "ever "
       (pastelyzer::string-context-before string 14 :limit 5))
      (check
       ""
       (pastelyzer::string-context-after string (length string)))
      (check
       " blah blah ...zzz"
       (pastelyzer::string-context-after string 20 :eol nil))
      (check
       " blah blah ...z"
       (pastelyzer::string-context-after string 20 :before 35 :trim-space t))
      (check
       " blah blah"
       (pastelyzer::string-context-after string 20 :before 34 :trim-space t))
      (check
       ;; XXX: Do we want eol to be until #\return?
       " blah blah .."
       (pastelyzer::string-context-after string 20 :eol t :trim-space nil))
      (check
       " blah blah"
       (pastelyzer::string-context-after string 20 :eol t :trim-space t))
      (check
       " blah"
       (pastelyzer::string-context-after string 20 :limit 5)))))
