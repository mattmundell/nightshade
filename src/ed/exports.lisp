;;; Set up the editor packages.

(if (find-package "EDI")
    (rename-package "EDI" "EDI" 'nil)
    (make-package "EDI" :nicknames () :use nil))
(if (find-package "ED")
    (rename-package "ED" "ED" 'nil)
    (make-package "ED" :nicknames () :use nil))

(use-package '("LISP" "EXTENSIONS" "SYSTEM") "EDI")
(use-package '("LISP" "EDI" "EXTENSIONS" "SYSTEM" "DEFTEST") "ED")

(in-package "SYSTEM")
;; FIX also in rompsite.lisp
(export '(%sp-byte-blt %sp-find-character %sp-find-character-with-attribute
	  %sp-reverse-find-character-with-attribute))

;; FIX for?
(export 'c::compile-from-stream (find-package "C"))

(defvar edi-exports
  '("*BUFFER-LIST*"
    "*BUFFER-NAMES*"
    "*CHARACTER-ATTRIBUTE-NAMES*"
    "*COMMAND-NAMES*"
    "*CREATE-INITIAL-WINDOWS-HOOK*"
    "*CREATE-WINDOW-HOOK*"
    "*DELETE-WINDOW-HOOK*"
    "*ECHO-AREA-BUFFER*"
    "*ECHO-AREA-HISTORY*"
    "*ECHO-AREA-HISTORY-POINTER*"
    "*ECHO-AREA-STREAM*"
    "*ECHO-AREA-WINDOW*"
    "*EDITOR-INPUT*"
    "*GLOBAL-VARIABLE-NAMES*"
    "*INPUT-TRANSCRIPT*"
    "*INVOKE-HOOK*"
    "*KEY-EVENT-HISTORY*"
    "*LAST-KEY-EVENT-TYPED*"
    "*LAST-PARSE-INPUT-STRING*"
    "*LOGICAL-KEY-EVENT-NAMES*"
    "*MODE-NAMES*"
    "*PARSE-DEFAULT*"
    "*PARSE-DEFAULT-STRING*"
    "*PARSE-HELP*"
    "*PARSE-HISTORY*"
    "*PARSE-HISTORY-POINTER*"
    "*PARSE-INITIAL-STRING*"
    "*PARSE-INPUT-REGION*"
    "*PARSE-PROMPT*"
    "*PARSE-STARTING-MARK*"
    "*PARSE-STRING-TABLES*"
    "*PARSE-TYPE*"
    "*PARSE-VALUE-MUST-EXIST*"
    "*PARSE-VERIFICATION-FUNCTION*"
    "*PRINT-REGION*"
    "*RANDOM-TYPEOUT-BUFFERS*"
    "*RANDOM-TYPEOUT-HOOK*"
    "*REAL-EDITOR-INPUT*"
    "*TERMCAP-FILE*"
    "*WINDOW-LIST*"
    "ABORT-RECURSIVE-EDIT"
    "ADD-HOOK"
    "AFTER-EDITOR-INITIALIZATIONS"
    "AT"
    "AT*"
    "BIND-KEY"
    "BLANK-AFTER-P"
    "BLANK-BEFORE-P"
    "BLANK-LINE-P"
    "BUFFER"
    "BUFFER-DEEP-REGION"
    "BUFFER-DEFAULT-PATHNAME"
    "BUFFER-DELETE-HOOK"
    "BUFFER-END"
    "BUFFER-END-MARK"
    "BUFFER-LINE-COUNT"
    "BUFFER-MAJOR-MODE"
    "BUFFER-MINOR-MODE"
    "BUFFER-MODELINE-FIELD-P"
    "BUFFER-MODELINE-FIELDS"
    "BUFFER-MODES"
    "BUFFER-MODIFIED"
    "BUFFER-NAME"
    "BUFFER-PATHNAME"
    "BUFFER-POINT"
    "BUFFER-REGION"
    "BUFFER-SIGNATURE"
    "BUFFER-START"
    "BUFFER-START-MARK"
    "BUFFER-VARIABLES"
    "BUFFER-WINDOWS"
    "BUFFER-WRITABLE"
    "BUFFER-WRITE-DATE"
    "BUFFERP"
    "CENTER-WINDOW"
    "CHARACTER-ATTRIBUTE"
    "CHARACTER-ATTRIBUTE-DOCUMENTATION"
    "CHARACTER-ATTRIBUTE-HOOKS"
    "CHARACTER-ATTRIBUTE-NAME"
    "CHARACTER-ATTRIBUTE-NAMES"
    "CHARACTER-ATTRIBUTE-P"
    "CHARACTER-OFFSET"
    "CLEAR-ECHO-AREA"
    "CLEAR-EDITOR-INPUT"
    "COLOR"
    "COLOR-MARK"
    "COMMAND"
    "COMMAND-BINDINGS"
    "COMMAND-CASE"
    "COMMAND-DOCUMENTATION"
    "COMMAND-FUNCTION"
    "COMMAND-NAME"
    "COMMANDP"
    "COMPLF"
    "COPY-BUFFER"
    "COPY-MARK"
    "COPY-REGION"
    "COUNT-CHARACTERS"
    "COUNT-LINES"
    "CURRENT-BUFFER"
    "CURRENT-LINE"
    "CURRENT-POINT"
    "CURRENT-VARIABLE-TABLES"
    "CURRENT-WINDOW"
    "CURSORPOS-TO-MARK"
    "DEFATTRIBUTE"
    "DEFAULT-FONT"
    "DEFCOMMAND"
    "DEFEVAR"
    "DEFHISTORY"
    "DEFINE-LOGICAL-KEY-EVENT"
    "DEFINE-TTY-FONT"
    "DEFMODE"
    "DEFUN-REGION"
    "DELETE-AND-SAVE-REGION"
    "DELETE-BUFFER"
    "DELETE-CHARACTERS"
    "DELETE-FONT-MARK"
    "DELETE-KEY-BINDING"
    "DELETE-LINE-FONT-MARKS"
    "DELETE-MARK"
    "DELETE-REGION"
    "DELETE-VARIABLE"
    "DELETE-WINDOW"
    "DIRECTORY-NAME-P"
    "DIRECTORYP"
    "DISPLAY-PAGE-DIRECTORY"
    "DISPLAYED-P"
    "DO-ALPHA-CHARS"
    "DO-BUFFER-LINES"
    "DO-LINES-FROM-MARK"
    "DO-PROCESSES"
    "DO-REGION-LINES"
    "DO-STRINGS"
    "ED"
    "EDITOR-BEEP"
    "EDITOR-BOUND-P"
    "EDITOR-DESCRIBE-FUNCTION"
    "EDITOR-ERROR"
    "EDITOR-ERROR-FORMAT-ARGUMENTS"
    "EDITOR-ERROR-FORMAT-STRING"
    "EDITOR-FINISH-OUTPUT"
    "EDITOR-SLEEP"
    "EMPTY-LINE-P"
    "END-LINE-P"
    "ENTER-WINDOW-AUTORAISE"
    "EXIT"
    "EXIT-RECURSIVE-EDIT"
    #+clx "FETCH-CLIPBOARD"
    #+clx "FETCH-CUT-STRING"
    #+clx "FETCH-SELECTION"
    "FILTER-REGION"
    "FIND-ATTRIBUTE"
    "FIND-CHARACTER"
    "FIND-PATTERN"
    "FIND-STRING"
    "FIRST-LINE-P"
    "FONT-MARK"
    "FONT-MARK-BACK-COLOR"
    "FONT-MARK-FORE-COLOR"
    "FORM-OFFSET"
    "FREE-COMMAND"
    "FUN-DEFINED-FROM-PATHNAME"
    "GET-COMMAND"
    "GET-KEY-EVENT"
    "GOTO-PAGE"
    "HANDLE-LISP-ERRORS"
    "HELP"
    "EDITOR-OUTPUT-STREAM"
    "EDITOR-OUTPUT-STREAM-P"
    "EDITOR-REGION-STREAM"
    "EDITOR-REGION-STREAM-P"
    "ELET"
    "IN-DIRECTORY"
    "IN-LISP"
    "IN-POP-UP-WINDOW"
    "IN-RECURSIVE-EDIT"
    "INPUT-WAITING"
    "INSERT-CHARACTER"
    "INSERT-REGION"
    "INSERT-STRING"
    "INTERACTIVE"
    "INVOKE-HOOK"
    "KEY-TRANSLATION"
    "LAST-COMMAND-TYPE"
    "LAST-KEY-EVENT-CURSORPOS"
    "LAST-LINE-P"
    "LINE"
    "LINE-BUFFER"
    "LINE-CHARACTER"
    "LINE-END"
    "LINE-LENGTH"
    "LINE-NEXT"
    "LINE-OFFSET"
    "LINE-PLIST"
    "LINE-PREVIOUS"
    "LINE-SIGNATURE"
    "LINE-START"
    "LINE-STRING"
    "LINE-TO-REGION"
    "LINE<"
    "LINE<="
    "LINE>"
    "LINE>="
    "LINEP"
    "LINES-RELATED"
    "LISTEN-EDITOR-INPUT"
    "LOAD-HISTORIES"
    "LOGICAL-KEY-EVENT-DOCUMENTATION"
    "LOGICAL-KEY-EVENT-KEY-EVENTS"
    "LOGICAL-KEY-EVENT-NAME"
    "LOGICAL-KEY-EVENT-P"
    "LOUD-MESSAGE"
    "MAKE-BUFFER"
    "MAKE-COMMAND"
    "MAKE-EMPTY-REGION"
    "MAKE-EDITOR-OUTPUT-STREAM"
    "MAKE-EDITOR-REGION-STREAM"
    "MAKE-KBDMAC-STREAM"
    "MAKE-MODELINE-FIELD"
    "MAKE-REGION-UNDO"
    "MAKE-RING"
    "MAKE-UNIQUE-BUFFER"
    "MAKE-WINDOW"
    #+clx "MAKE-XWINDOW-LIKE-WINDOW"
    "MANUAL-NAME-AT-POINT"
    "MAP-BINDINGS"
    "MARK"
    "MARK-AFTER"
    "MARK-BEFORE"
    "MARK-BUFFER"
    "MARK-CHARPOS"
    "MARK-COLUMN"
    "MARK-KIND"
    "MARK-LINE"
    "MARK-TO-CURSORPOS"
    "MARK-TOP-LEVEL-FORM"
    "MARK<"
    "MARK<="
    "MARK="
    "MARK>"
    "MARK>="
    "MARKP"
    "MAYBE-DELETE-HORIZONTAL-SPACE"
    "MAYBE-LOAD-HISTORIES"
    "MAYBE-SAVE-HISTORIES"
    "MESSAGE"
    "MODE-DOCUMENTATION"
    "MODE-MAJOR-P"
    "MODE-VARIABLES"
    "MODELINE-FIELD"
    "MODELINE-FIELD-FUNCTION"
    "MODELINE-FIELD-NAME"
    "MODELINE-FIELD-P"
    "MODELINE-FIELD-WIDTH"
    "MODIFY-KBDMAC-STREAM"
    "MOVE-FONT-MARK"
    "MOVE-MARK"
    "MOVE-TO-COLUMN"
    "MOVE-TO-POSITION"
    "MSG"
    "NEW-SEARCH-PATTERN"
    "NEXT-CHARACTER"
    "NEXT-WINDOW"
    "NINSERT-REGION"
    "NUMBER-AT-POINT"
    "PATHNAME-AT-POINT"
    "PAUSE"
    "PARSE-UNIQUE-NAME"
    "PREFIX-ARGUMENT"
    "PREVIOUS-CHARACTER"
    "PREVIOUS-WINDOW"
    "PROMPT-FOR-BUFFER"
    "PROMPT-FOR-DATE"
    "PROMPT-FOR-EXPRESSION"
    "PROMPT-FOR-FILE"
    "PROMPT-FOR-INTEGER"
    "PROMPT-FOR-KEY"
    "PROMPT-FOR-KEY-EVENT"
    "PROMPT-FOR-KEYWORD"
    "PROMPT-FOR-MODE"
    "PROMPT-FOR-STRING"
    "PROMPT-FOR-VARIABLE"
    "PROMPT-FOR-Y-OR-N"
    "PROMPT-FOR-YES-OR-NO"
    "PROMPT-IN-BUFFER"
    "READ-FILE"
    "RECURSIVE-EDIT"
    "REDISPLAY"
    "REDISPLAY-ALL"
    "REGION"
    "REGION-BOUNDS"
    "REGION-END"
    "REGION-START"
    "REGION-TO-STRING"
    "REGIONP"
    "REMOVE-HOOK"
    "REMOVE-SCHEDULED-EVENT"
    "REMOVE-SCHEDULED-FUNCTION"
    "REPLACE-PATTERN"
    "REPROMPT"
    "REVERSE-FIND-ATTRIBUTE"
    "REVERSE-FIND-CHARACTER"
    "RING"
    "RING-LENGTH"
    "RING-POP"
    "RING-PUSH"
    "RING-REF"
    "RINGP"
    "ROTATE-RING"
    "SAME-LINE-P"
    "SAVE-HISTORIES"
    "SCHEDULE-EVENT"
    "SCROLL-WINDOW"
    "SEARCH-CHAR-CODE-LIMIT"
    "SEARCH-PATTERN"
    "SEARCH-PATTERN-P"
    "SET-REGION-BOUNDS"
    "SET-WINDOW-BACKGROUND-COLOR"
    "SET-WINDOW-FOREGROUND-COLOR"
    "SETV"
    "SHADOW-ATTRIBUTE"
    "SHOW-MARK"
    "START-LINE-P"
    #+clx "STORE-CLIPBOARD"
    #+clx "STORE-CUT-REGION"
    #+clx "STORE-CUT-STRING"
    #+clx "STORE-SELECTION-REGION"
    #+clx "STORE-SELECTION-STRING"
    "STRING-TABLE"
    "STRING-TO-REGION"
    "STRING-TO-VARIABLE"
    "SYMBOL-AT-POINT"
    "SYMLINKP"
    "SYNTAX-CHAR-CODE-LIMIT"
    "UNGET-KEY-EVENT"
    "UNIQUE-BUFFER-NAME"
    "UNSHADOW-ATTRIBUTE"
    "UPDATE-ALL-MODELINE-BUFFER-STATE-FIELDS"
    "UPDATE-ALL-MODELINE-FIELDS"
    "UPDATE-LINE-NUMBER"
    "UPDATE-LINE-NUMBERS"
    "UPDATE-MODELINE-BUFFER-STATE-FIELD"
    "UPDATE-MODELINE-COLUMN-FIELD"
    "UPDATE-MODELINE-FIELD"
    "UPDATE-MODELINE-FIELDS"
    "UPDATE-MODELINE-POSITION-FIELD"
    "URL-AT-POINT"
    "USE-BUFFER"
    "VALUE"
    "VARIABLE-DOCUMENTATION"
    "VARIABLE-HOOKS"
    "VARIABLE-NAME"
    "VARIABLE-SOURCE"
    "VARIABLE-VALUE"
    "VISIBLE-COMMAND-BINDINGS"
    "WELCOME"
    "WINDOW"
    "WINDOW-BUFFER"
    "WINDOW-DISPLAY-END"
    "WINDOW-DISPLAY-RECENTERING"
    "WINDOW-DISPLAY-START"
    "WINDOW-FONT"
    "WINDOW-HEIGHT"
    "WINDOW-LINE-NUMBER"
    "WINDOW-POINT"
    "WINDOW-WIDTH"
    "WINDOWED-MONITOR-P"
    "WINDOWP"
    "WITH-INPUT-FROM-REGION"
    "WITH-MARK"
    "WITH-OUTPUT-TO-MARK"
    "WITH-OUTPUT-TO-WINDOW"
    "WITH-POP-UP-DISPLAY"
    "WITH-POP-UP-WINDOW"
    "WITH-TEMP-BUFFER"
    "WITH-WRITABLE-BUFFER"
    "WORD-AT-POINT"
    "WORD-END"
    "WORD-START"
    "WRITE-FILE"
    "WRITE-REGION"))

(eval `(defpackage "EDI"
	 (:export ,@edi-exports)
	 (:documentation "Editor internals.")))

(eval `(defpackage "ED"
	 (:import-from "EDI" ,@edi-exports)
	 (:import-from "PARSE"
		       "CHAR-NODE-CONTENT" "CHAR-NODE-NEXT" "CHAR-NODE-PREVIOUS"
		       "CHAR-NODE-PARENT"
		       "NODE-CONTENT" "NODE-NEXT" "NODE-PREVIOUS" "NODE-PARENT"
		       "REGION-NODE-CONTENT" "REGION-NODE-NEXT" "REGION-NODE-NEXT"
		       "REGION-NODE-PREVIOUS" "REGION-NODE-PARENT")
	 (:documentation "The editor.")
	 (:export
	  "*ACTIVE-FILE-GROUP*"
	  "*BUFFER-HISTORY*"
	  "*EPHEMERALLY-ACTIVE-COMMAND-TYPES*"
	  "*KILL-RING*"
	  "*LAST-SEARCH-STRING*"
	  "*LAST-SEARCH-PATTERN*"
	  ;;
	  "ACTIVE-FILE-GROUP"
	  "ACTIVATE-REGION"
	  "ADD-DEFINITION-DIR-TRANSLATION"
	  "BACKWARD-UP-LIST"
	  "BUFFER-MARK"
	  "CHANGE-TO-BUFFER"
	  "CHECK-REGION-ACTIVE"
	  "CHECK-REGION-QUERY-SIZE"
	  "CREATE-SLAVE"
	  "CURRENT-MARK"     ;; FIX should current* be in edi?
	  "CURRENT-REGION"
	  "CSV-PARSE-BUFFER"
	  "DEFINDENT"
	  "DEFINE-FILE-OPTION"
	  "DEFINE-FILE-TYPE-HOOK"
	  "DELETE-BUFFER-IF-POSSIBLE"
	  "DELETE-BUFFER-SAFELY"
	  "DELETE-DEFINITION-DIR-TRANSLATION"
	  "DELETE-HORIZONTAL-SPACE"
	  "DO-ACTIVE-GROUP"
	  "END-OF-PARSE-BLOCK"
	  "EVAL-FORM-IN-SERVER"
	  "EVAL-FORM-IN-SERVER-1"
	  "FILE-COMPILE"
	  "FILL-REGION"
	  "FILL-REGION-BY-PARAGRAPHS"
	  "FIND-FILE-BUFFER"
	  "FROB-[-KEYS"
	  "FROB-HELP-KEY"
	  "FROB-HYPER-KEY"
	  "FORWARD-UP-LIST"
	  "GET-CURRENT-COMPILE-SERVER"
	  "GET-CURRENT-EVAL-SERVER"
	  "GET-SEARCH-PATTERN"
	  "INDENT-REGION"
	  "INDENT-REGION-FOR-COMMANDS"
	  "INSIDE-DEFUN-P"
	  "KILL-CHARACTERS"
	  "KILL-REGION"
	  "MARK-PARAGRAPH"
	  "PACIFY-REGION"
	  "PAGE-DIRECTORY"
	  "PAGE-OFFSET"
	  "PARAGRAPH-OFFSET"
	  "PATHNAME-TO-BUFFER-NAME"
	  "POP-BUFFER-MARK"
	  "PRE-COMMAND-PARSE-CHECK"
	  "PREVIOUS-BUFFER"
	  "PROCESS-FILE-OPTIONS"
	  "PUSH-BUFFER-MARK"
	  "PUSH-KILL"
	  "READ-BUFFER-FILE"
	  "READ-DICTIONARY"
	  "REGION-ACTIVE-P"
	  "REGION-COMPILE"
	  "REGION-EVAL"
	  "SAVE-FOR-UNDO"
	  "SENTENCE-OFFSET"
	  "START-DEFUN-P"
	  "START-OF-PARSE-BLOCK"
	  "STRING-EVAL"
	  "SUPPLY-GENERIC-POINTER-UP-FUNCTION"
	  "TOP-LEVEL-OFFSET"
	  "VALID-SPOT"
	  "WRITE-BUFFER-FILE"
	  "WORD-OFFSET"
	  ,@edi-exports)))  ; FIX export all of edi?
