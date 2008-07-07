;;;  -*- coding: utf-8; mode: emacs-lisp; -*-
;;; perl-completion.el

;; Author: Kenji.Imakado <ken.imakaado@gmail.com>
;; Version: 0.3
;; Keywords: perl

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Tested on Emacs 22

;; to customize
;; M-x customize-group RET perl-completion RET

;;;code:

(require 'cl)
(require 'cperl-mode)
(require 'dabbrev)
(require 'rx)

;;; provide
(provide 'perl-completion)

;;; group
(defgroup perl-completion nil
  ""
  :group 'perl-completion)

;;; customizable variables
(defcustom plcmp-anything-candidate-number-limit 1000
  "補完候補の最大表示数
`anything-candidate-number-limit'と同じ"
  :type 'number
  :group 'perl-completion)

(defcustom plcmp-buffer-dabbrev-expansions-number 2
  "バッファ内のdabbrevを候補に入れる文字数
initial-inputのlengthがこの数値より小さいと補完を行わない
注意、0(常に補完)に設定する事も可能ですが動作が遅くなる可能性があります"
  :type 'number
  :group 'perl-completion) ; TODO name and doc

(defcustom plcmp-get-words-other-perl-buf-limit-number 30
  "補完対象にする他のperlバッファの最大数"
  :type 'number
  :group 'perl-completion)

(defcustom plcmp-config-modules-filter-list
  '("strict" "warning")
  "補完対象に含めないモジュール名のリスト
このリストに含まれているモジュールのメソッドは補完対象にならない"
  :type '(repeat (string :tag "Module name"))
  :group 'perl-completion)

(defcustom plcmp-dabbrev-abbrev-char-regexp "\\sw\\|\\s_\\|[:_]"
  "dabbrevの候補になるcharを決めるregexp
`dabbrev-abbrev-char-regexp'にダイナミックにバインドされて使用される"
  :type 'regexp
  :group 'perl-completion)

(defcustom plcmp-match-only-real-candidate nil
  "この変数がnon-nilだとパターンが実際の補完候補のみにマッチするようになる
例： nil(デフォルト)の場合、パターン\"agent\"は以下の両方の補完候補にマッチする
なぜなら\"UserAgent\"の部分に\"agent\"がマッチするからである
[LWP::UserAgent] | agent
[LWP::UserAgent] | clone
non-nilの場合は\"|\"以降の文字列のみにマッチする"
  :type 'boolean
  :group 'perl-completion)

;;; const
(defconst plcmp-version 0.3)
(defconst plcmp-lang (cond ((string-match "japanese" (format "%s" locale-coding-system)) 'ja)
                           (t 'english)))
(defconst plcmp-perlvar-output-buf-name "*perlvar output*")
(defconst plcmp-perlfunc-output-buf-name "*perlfunc output*")
(defconst plcmp-perldoc-output-buf-name "*perldoc output*")
(defconst plcmp-perl-ident-re "[a-zA-Z_][a-zA-Z_0-9]*")
(defconst plcmp-installed-modules-buf-name "*perl installed modules*")
(defconst plcmp-display-format-variables "buffer variable")
(defconst plcmp-display-format-functions "buffer function")
(defconst plcmp-display-format-dabbrev-expansions "buffer dabbrev")
(defconst plcmp-display-format-builtin-variables "builtin variable")
(defconst plcmp-display-format-builtin-functions "builtin function")
(defconst plcmp-display-format-using-modules "using module")
(defconst plcmp-display-format-installed-modules "installed module")
(defconst plcmp-get-installed-modules-command "find `perl -e 'pop @INC; print join(q{ }, @INC);'` -name '*.pm' -type f | xargs egrep -h -o 'package [a-zA-Z0-9:]+;' | perl -nle 's/package\s+(.+);/$1/; print' | sort | uniq ") ;
(defconst plcmp-get-installed-modules-async-command
  (concat plcmp-get-installed-modules-command " &"))
(defconst plcmp-builtin-functions
  '("abs" "exec" "glob" "order" "seek" "symlink" "accept" "exists" "gmtime"
    "our" "seekdir" "syscall" "alarm" "exit" "goto" "pack" "select" "sysopen"
    "atan" "exp" "grep" "package" "semctl" "sysread" "bind" "fcntl" "hex"
    "pipe" "semget" "sysseek" "binmode" "fileno" "import" "pop" "semop"
    "system" "bless" "flags" "index" "pos" "send" "syswrite" "caller" "flock"
    "int" "precision" "setgrent" "tell" "chdir" "fork" "ioctl" "print" "sethostent"
    "telldir" "chmod" "format" "join" "printf" "setnetent" "tie" "chomp" "formline"
    "keys" "prototype" "setpgrp" "tied" "chop" "getc" "kill" "push" "setpriority"
    "time" "chown" "getgrent" "last" "q" "setprotoent" "times" "chr" "getgrgid"
    "lc" "qq" "setpwent" "tr" "chroot" "getgrnam" "lcfirst" "qr" "setservent"
    "truncate" "close" "gethostbyaddr" "length" "quotemeta" "setsockopt" "uc"
    "closedir" "gethostbyname" "link" "qw" "shift" "ucfirst" "connect" "gethostent"
    "listen" "qx" "shmctl" "umask" "continue" "getlogin" "local" "rand" "shmget"
    "undef" "cos" "getnetbyaddr" "localtime" "read" "shmread" "unlink" "crypt"
    "getnetbyname" "lock" "readdir" "shmwrite" "unpack" "dbmclose" "getnetent"
    "log" "readline" "shutdown" "unshift" "dbmopen" "getpeername" "lstat" "readlink"
    "sin" "untie" "defined" "getpgrp" "m" "readpipe" "size" "use" "delete" "getppid"
    "map" "recv" "sleep" "utime" "die" "getpriority" "mkdir" "redo" "socket" "values"
    "do" "getprotobyname" "msgctl" "ref" "socketpair" "vec" "dump" "getprotobynumber"
    "msgget" "rename" "sort" "vector" "each" "getprotoent" "msgrcv" "require" "splice"
    "wait" "endgrent" "getpwent" "msgsnd" "reset" "split" "waitpid" "endhostent"
    "getpwnam" "my" "return" "sprintf" "wantarray" "endnetent" "getpwuid" "next"
    "reverse" "sqrt" "warn" "endprotoent" "getservbyname" "no" "rewinddir" "srand"
    "write" "endpwent" "getservbyport" "oct" "rindex" "stat" "y" "endservent" "getservent"
    "open" "rmdir" "study" "eof" "getsockname" "opendir" "s" "sub" "eval" "getsockopt"
    "ord" "scalar" "substr"))

(defconst plcmp-builtin-variables
  '("$SIG{expr}" "%SIG" "$ENV{expr}" "%ENV" "%INC" "@_" "@INC" "@F" "ARGVOUT"
    "@ARGV" "$ARGV" "ARGV" "$^X" "$EXECUTABLE_NAME" "${^WARNING_BITS}" "$^W"
    "$WARNING" "$^V" "$PERL_VERSION" "${^UTF8LOCALE}" "${^UNICODE}" "${^TAINT}"
    "$^T" "$BASETIME" "$^S" "$EXCEPTIONS_BEING_CAUGHT" "$^R"
    "$LAST_REGEXP_CODE_RESULT" "$^P" "$PERLDB" "${^OPEN}" "$^O" "$OSNAME" "$^M" "$^I" "$INPLACE_EDIT"
    "%^H" "$^H" "$^F" "$SYSTEM_FD_MAX" "$^D" "$DEBUGGING" "$^C" "$COMPILING" "$]"
    "$[" "$0" "$PROGRAM_NAME" "$)" "$EGID" "$EFFECTIVE_GROUP_ID" "$(" "$GID" "$REAL_GROUP_ID"
    "$>" "$EUID" "$EFFECTIVE_USER_ID" "$<" "$UID" "$REAL_USER_ID" "$$" "$PID" "$PROCESS_ID"
    "$@" "$EVAL_ERROR" "$^E" "$EXTENDED_OS_ERROR" "%!" "$!" "$ERRNO" "$OS_ERROR" "${^ENCODING}"
    "$?" "$CHILD_ERROR" "$^A" "$ACCUMULATOR" "$^L" "$FORMAT_FORMFEED" "IO::Handle->format_formfeed" "$:"
    "$FORMAT_LINE_BREAK_CHARACTERS" "IO::Handle->format_line_break_characters" "$^"
    "$FORMAT_TOP_NAME" "HANDLE->format_top_name(EXPR)" "$~"
    "$FORMAT_NAME" "HANDLE->format_name(EXPR)" "@-" "@LAST_MATCH_START"
    "$-" "$FORMAT_LINES_LEFT" "HANDLE->format_lines_left(EXPR)" "$="
    "$FORMAT_LINES_PER_PAGE" "HANDLE->format_lines_per_page(EXPR)" "$%"
    "$FORMAT_PAGE_NUMBER" "HANDLE->format_page_number(EXPR)" "$#" "$;"
    "$SUBSEP" "$SUBSCRIPT_SEPARATOR" "$\"" "$LIST_SEPARATOR" "$\\" "$ORS"
    "$OUTPUT_RECORD_SEPARATOR" "IO::Handle->output_record_separator" "$," "$OFS"
    "$OUTPUT_FIELD_SEPARATOR" "IO::Handle->output_field_separator" "$|"
    "$OUTPUT_AUTOFLUSH" "HANDLE->autoflush(EXPR)" "$/" "$RS"
    "$INPUT_RECORD_SEPARATOR" "IO::Handle->input_record_separator(EXPR)" "$."
    "$NR" "$INPUT_LINE_NUMBER" "HANDLE->input_line_number(EXPR)" "$*" "@+"
    "@LAST_MATCH_END" "$^N" "$+" "$LAST_PAREN_MATCH" "$'" "$POSTMATCH" "$`"
    "$PREMATCH" "$&" "$MATCH" "$<digits>" "$b" "$a" "$_" "$ARG"))

;;; face
(defface plcmp-search-match
  '((t (:background "grey15" :foreground "magenta" :underline t)))
  ""
  :group 'perl-completion
  :tag "Plcmp Search Match Face")

;;; struct
(defstruct (plcmp-completion-data (:constructor plcmp-make-completion-data))
  (initial-input "")
  state
  default-action-state
  persistent-action-buffer-point
  using-modules
  current-buffer
  current-object
  current-package
  cache-installed-modules
  cache-using-modules
  other-perl-buffers
  obj-instance-of-module-maybe-alist
  installed-modules)

;;; variables
(defvar plcmp-data (plcmp-make-completion-data) "strunct")
(defvar plcmp-search-match-face 'plcmp-search-match)
(defvar plcmp-overlay nil)
(defvar plcmp-metadata-matcher-re (rx bol (* (not (any "|"))) "|" space (*? not-newline)))
(defvar plcmp-metadata-matcher "")

;;; buffer local variables
(defvar plcmp-last-using-modules nil)
(make-variable-buffer-local 'plcmp-last-using-modules)
(defvar plcmp-modules-methods-alist nil)
(make-variable-buffer-local 'plcmp-modules-methods-alist)

;;; anything's variables
(defvar plcmp-anything-sources nil)
(defvar plcmp-anything-enable-digit-shortcuts nil )
(defvar plcmp-anything-candidate-number-limit plcmp-anything-candidate-number-limit )
(defvar plcmp-anything-idle-delay 0.5 )
(defvar plcmp-anything-samewindow nil )
(defvar plcmp-anything-source-filter nil )
(defvar plcmp-anything-isearch-map
  (let ((map (copy-keymap (current-global-map))))
    (define-key map (kbd "<return>") 'plcmp-anything-isearch-default-action)
    (define-key map (kbd "C-i") 'plcmp-anything-isearch-select-action)
    (define-key map (kbd "C-g") 'plcmp-anything-isearch-cancel)
    (define-key map (kbd "M-s") 'plcmp-anything-isearch-again)
    (define-key map (kbd "<backspace>") 'plcmp-anything-isearch-delete)
    (let ((i 32))
      (while (< i 256)
        (define-key map (vector i) 'plcmp-anything-isearch-printing-char)
        (setq i (1+ i))))
    map))
(defgroup plcmp-anything nil
  "Open plcmp-anything." :prefix "plcmp-anything-" :group 'convenience)
(if (facep 'header-line)
    (copy-face 'header-line 'plcmp-anything-header)
  (defface plcmp-anything-header
    '((t (:bold t :underline t)))
    "Face for header lines in the plcmp-anything buffer." :group 'plcmp-anything))
(defvar plcmp-anything-header-face 'plcmp-anything-header )
(defface plcmp-anything-isearch-match '((t (:background "Yellow")))
  "Face for isearch in the plcmp-anything buffer." :group 'plcmp-anything)
(defvar plcmp-anything-isearch-match-face 'plcmp-anything-isearch-match )
(defvar plcmp-anything-iswitchb-idle-delay 1 )
(defvar plcmp-anything-iswitchb-dont-touch-iswithcb-keys nil )
(defconst plcmp-anything-buffer "*perl-completion anything*" )
(defvar plcmp-anything-selection-overlay nil )
(defvar plcmp-anything-isearch-overlay nil )
(defvar plcmp-anything-digit-overlays nil )
(defvar plcmp-anything-candidate-cache nil )
(defvar plcmp-anything-pattern "")
(defvar plcmp-anything-input "")
(defvar plcmp-anything-async-processes nil )
(defvar plcmp-anything-digit-shortcut-count 0 )
(defvar plcmp-anything-update-hook nil )
(defvar plcmp-anything-saved-sources nil )
(defvar plcmp-anything-saved-selection nil )
(defvar plcmp-anything-original-source-filter nil )

;;; hack variables
;; idea: http://www.emacswiki.org/cgi-bin/wiki/RubikitchAnythingConfiguration
(defvar plcmp-anything-saved-action nil
  "Saved value of the currently selected action by key.")

(defvar plcmp-anything-matched-candidate-cache nil
  "(name . ((pattern . (list of string))
            (pattern . (list of string)))) ")

;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; Utilities
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro plcmp-with-slots (struct conc-name slots &rest body)
  `(symbol-macrolet ,(loop for slot in slots
                           collect `(,slot (,(intern (concat (symbol-name conc-name) (symbol-name slot))) ,struct)))
     ,@body))
(def-edebug-spec plcmp-with-slots (symbolp symbolp (&rest symbolp) body)) ;TODO
  
(defmacro plcmp-with-completion-data-slots (struct slots &rest body)
  (declare (indent 2))
  `(plcmp-with-slots ,struct plcmp-completion-data- ,slots ,@body))
(def-edebug-spec plcmp-with-completion-data-slots (symbolp (&rest symbolp) body))
  
(defmacro plcmp-with-gensyms (symbols &rest body)
  (declare (indent 1))
  `(let ,(mapcar (lambda (sym)
                   `(,sym (gensym)))
                 symbols)
     ,@body))

(defmacro plcmp-my (var val &rest body)
  (declare (indent 2))
  `(lexical-let ((,var ,val))
     ,@body))

(put 'plcmp-acond 'lisp-indent-function 'defun) ;TODO
(defmacro plcmp-acond (&rest clauses)
  (unless (null clauses)
    (plcmp-with-gensyms (sym)
      (plcmp-my clause (car clauses)
        `(plcmp-my ,sym ,(car clause)
           (if ,sym
               (plcmp-my it ,sym
                 ,@(cdr clause))        ;expr
             (plcmp-acond ,@(cdr clauses))))))))
(def-edebug-spec plcmp-acond cond)

(defsubst plcmp-trim (s)
  "strip space and newline"
  (replace-regexp-in-string
   "[ \t\n]*$" "" (replace-regexp-in-string "^[ \t\n]*" "" s)))

(defun plcmp-get-preceding-string (&optional count)
  "現在の位置からcount文字前方位置までの文字列を返す
例外を出さない"
  (let ((count (or count 1)))
    (buffer-substring-no-properties
     (point)
     (condition-case nil
         (save-excursion (backward-char count) (point))
       (error (point))))))

(defsubst plcmp-module-p (s)
  (string-match "^[a-zA-Z:_]+$" s))

(defsubst plcmp-perl-identifier-p (s)
  (string-match (concat "^" plcmp-perl-ident-re "$") s))

(defun plcmp-notfound-p (s)
  (string-match "^Can't locate [^ \t]+ in" s))

(defmacro plcmp-ignore-errors (&rest body)
  `(condition-case e (progn ,@body)
     (error (plcmp-log "Error plcmp-ignore-errors :  %s" (error-message-string e)))))

;; idea: anything-dabbrev-expand.el
(lexical-let ((store-times 0))
  (defun plcmp-seq-times (command-name &optional max)
    (let ((max (or max -99)))
      (if (eq last-command command-name)
          (if (= (incf store-times) max)
              (setq store-times 0)
            store-times)
        (setq store-times 0)))))

;;; log
(defvar plcmp-debug nil)
(defvar plcmp-log-buf-name "*plcmp debug*")
(defun plcmp-log-buf ()
  (get-buffer-create plcmp-log-buf-name))
(defun plcmp-log (&rest messages)
  (ignore-errors
    (let* ((str (or (ignore-errors (apply 'format messages))
                    (prin1-to-string messages)))
           (strn (concat str "\n")))
      (when plcmp-debug
        (with-current-buffer (plcmp-log-buf)
          (goto-char (point-max))
          (insert strn)))
      str)))


;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Initialize
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; idea: http://subtech.g.hatena.ne.jp/antipop/20070917/1190009355
(defun plcmp-get-installed-modules-synchronously ()
  (message "fetching installed modules...")
  (let ((modules (split-string (shell-command-to-string plcmp-get-installed-modules-command) "\n")))
    (message "fetching installed modules done")
    (remove-if (lambda (module)
                 (string-match "No such file or directory$" module))
               modules)))

(defun plcmp-get-installed-modules-from-buf (buf)
  (with-current-buffer buf
    (let ((modules (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
      (remove-if (lambda (module)
                   (string-match "No such file or directory$" module))
                 modules))))

(defun plcmp-send-command-get-installed-modules ()
  (message "send command to get installed modules")
  (save-window-excursion
    (shell-command plcmp-get-installed-modules-async-command plcmp-installed-modules-buf-name))
  (with-current-buffer plcmp-installed-modules-buf-name
    (setq buffer-read-only t)))

(defun plcmp-fetch-installed-modules (struct)
  (plcmp-with-completion-data-slots struct
      (cache-installed-modules)
    (let ((buf (get-buffer plcmp-installed-modules-buf-name)))
      (cond
       ((null cache-installed-modules)
        (if (and (buffer-live-p buf)
                 (not (processp (get-buffer-process buf)))) ;finished
            (setf cache-installed-modules (plcmp-get-installed-modules-from-buf buf))
          (unless (buffer-live-p buf)
            (plcmp-send-command-get-installed-modules))
          (plcmp-get-installed-modules-synchronously)))
       ;; return cache
       (t
        cache-installed-modules)))))

(defun plcmp-get-current-package ()
  "nil or string"
  (let ((re (concat "^[ \t]*package\\s *" "\\([a-zA-Z:]+\\)" ".*;$"))
        (limit 500))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward re limit t)
        (match-string-no-properties 1)))))

(defun plcmp-get-using-modules ()
  (let ((re "^[ \t]*use[ \t]+\\([a-zA-Z:_]+\\)\\s *[^;\n]*;") ;todo
        (ret nil))
    (save-excursion
      (goto-char (point-min))
      (loop always (re-search-forward re nil t)
            do (add-to-list 'ret (match-string-no-properties 1))))

    ;; filter plcmp-config-modules-filter-list
    (setq ret (set-difference ret plcmp-config-modules-filter-list :test 'string-equal))
    (plcmp-log "get-using-modules: %S" ret)
    ret))

;;(plcmp-sort-methods '("_asdf" "asdf" "bsd" "_bsd" "ASDF"))
;; => ("ASDF" "asdf" "bsd" "_asdf" "_bsd")
(defun plcmp-sort-methods (los)
  (loop for s in los
        if (string-match (rx bol "_") s)
        collect s into unders
        else
        collect s into methods
        finally return (nconc methods unders)))

(defsubst plcmp-inspect-methods (module)
  "Class::Inspectorを使用してモジュールのメソッド調べる。
モジュール名に使用できる文字以外が含まれていた場合はnilを返す
return los"
  (ignore-errors
    (unless (string-match "^[a-zA-Z:_]+$" module)
      (error "invild modulename"))
    (let ((mods (shell-command-to-string
                 (concat "perl -MClass::Inspector -e'use " module "; print join \"\n\"=>@{Class::Inspector->methods(" module ")} '"))))
      (cond
       ((plcmp-notfound-p mods)
        (error "cant locate %s" module))
       (t
        (plcmp-sort-methods (split-string mods "\n")))))))

(defun plcmp-get-modules-methods (modules)
  "return alist"
  (let ((ret nil))
    (dolist (mod modules ret)
      (message "getting methods of %s ..." mod)
      (push `(,mod . ,(plcmp-inspect-methods mod)) ret)
      (message "getting methods of %s done" mod))))

;; TODO
(defun plcmp-get-modules-methods-alist (struct)
  (plcmp-with-completion-data-slots struct
      (using-modules current-buffer)
    ;;`plcmp-modules-methods-alist' and `plcmp-last-using-modules' are buffer local variables
    (with-current-buffer current-buffer
      (let ((ret nil))
        (cond
         ((and (equal using-modules plcmp-last-using-modules)
               (not (null plcmp-modules-methods-alist)))
          (setq ret plcmp-modules-methods-alist))
         ;; cache not ready
         ((null plcmp-modules-methods-alist)
          (setf ret
                (setf plcmp-modules-methods-alist
                      (plcmp-get-modules-methods using-modules))))
         (t
          (let ((new-mods (delete-dups (set-difference using-modules plcmp-last-using-modules :test 'string-equal)))
                (removed-mods (delete-dups (set-difference plcmp-last-using-modules using-modules :test 'string-equal))))
            (plcmp-log "new-mods: %S\nremoved-mods: %S" new-mods removed-mods)
            ;; add new
            (when new-mods
              (setq plcmp-modules-methods-alist
                    (append plcmp-modules-methods-alist
                            (plcmp-get-modules-methods new-mods))))
            ;; remove
            (when removed-mods
              (setq plcmp-modules-methods-alist
                    (remove-if (lambda (mod) (assoc mod plcmp-modules-methods-alist)) removed-mods)))
            (setq ret plcmp-modules-methods-alist))))
        ;; set last
        (setq plcmp-last-using-modules using-modules)
        ret
        ))))

(defun plcmp-get-obj-instance-of-module-maybe-alist (struct)
  (plcmp-with-completion-data-slots struct
      (using-modules)
    (let* ((re (regexp-opt using-modules t))
           (re (concat "\\(\\$" plcmp-perl-ident-re "\\)\\s *=\\s *" re)) ;perliden + usingmodule
           (ret nil))
      (save-excursion
        (goto-char (point-min))
        (loop always (re-search-forward re nil t)
              do (let ((var (match-string-no-properties 1))
                       (mod (match-string-no-properties 2)))
                   (add-to-list 'ret `(,var . ,mod)))))
      ret)))

(defun plcmp-get-other-perl-buffers (struct)
  (plcmp-with-completion-data-slots struct
      (current-buffer)
    (remove current-buffer
            (remove-if-not (lambda (buf)
                             (string-match "\\.p[lm]$" (buffer-name buf)))
                           (buffer-list)))))

(defun plcmp-initialize (struct)
  (plcmp-with-completion-data-slots struct
      (using-modules installed-modules current-package
                     current-buffer obj-instance-of-module-maybe-alist
                     current-object other-perl-buffers default-action-state)
    ;; initialize slots
    (setf installed-modules (plcmp-fetch-installed-modules struct)
          current-buffer (current-buffer)
          current-package (plcmp-get-current-package)
          using-modules (plcmp-get-using-modules)
          plcmp-modules-methods-alist (plcmp-get-modules-methods-alist struct) ;buffer local variable
          obj-instance-of-module-maybe-alist (plcmp-get-obj-instance-of-module-maybe-alist struct)
          other-perl-buffers (plcmp-get-other-perl-buffers struct)
          current-object ""
          default-action-state nil
          persistent-action-buffer-point nil)

    ;; initialize variable
    (setq plcmp-metadata-matcher
          (if plcmp-match-only-real-candidate
              plcmp-metadata-matcher-re
            ""))

    ;; get context
    (plcmp-get-context struct)))

(defun plcmp-method-p ()
  (let ((s (plcmp-get-preceding-string 2)))
    (string-equal s "->")))

(defun plcmp-get-context (struct)
  (plcmp-with-completion-data-slots struct
      (initial-input state current-object)
    (save-excursion
      (let* ((start (point))
             (start-input (progn (skip-syntax-backward "w_") ;move point
                                 (buffer-substring-no-properties (point) start)))
             (obj-str (buffer-substring-no-properties
                       (or (ignore-errors (save-excursion (forward-char -2) (point)))
                           (point))
                       (save-excursion (or (ignore-errors (backward-sexp)
                                                          (point))
                                           (point))))))
        (cond
         ;; $self->`!!'
         ((and (plcmp-method-p)         ; TODO
               (string-match "^\\(\\$self\\|__PACKAGE__\\)$" obj-str))
          (setf initial-input start-input
                state 'self
                current-object obj-str))
         ;; methods
         ;; Foo->`!!'
         ((plcmp-method-p)
          (setf initial-input start-input
                state 'methods
                current-object obj-str))
         ;; $foo`!!'
         ((string-match "[$@%&]" (plcmp-get-preceding-string 1))
          (save-excursion
            (forward-char -1)
            (setf initial-input (buffer-substring-no-properties start (point))
                  state 'globals)))
         ;; installed-modules
         ;; use Foo::Ba`!!'
         ((string-match "^\\s *use\\s *" (buffer-substring-no-properties (point-at-bol) (point)))
          (setf initial-input start-input
                state 'installed-modules))
         ;; globals
         ((or (bolp)
              (string-match "[ \t]" (plcmp-get-preceding-string 1)))
          (setf initial-input start-input
                state 'globals))
         ;; otherwise
         (t
          (setf initial-input start-input
                state 'globals))
         )))))

;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Candidates
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defsubst plcmp-build-display-candidate (metadata str)
  (concat "[" metadata "]" " | " str))

(defsubst plcmp-get-real-candidate (display-candidate)
  "return string"
  (if (string-match (concat "^\\[[^\]\n]*\\] | "
                            "\\(.*\\)")
                    display-candidate)
      (match-string 1 display-candidate)
    display-candidate))

(defsubst plcmp-get-metadate-candidate (display-candidate)
  "return string"
  (if (string-match "^\\[\\([^\]\n]*\\)\\] | " display-candidate)
      (match-string 1 display-candidate)
    ""))

(defun plcmp-get-modulename-candidate (struct display-candidate)
  (let ((metadata (plcmp-get-metadate-candidate display-candidate)))
    (plcmp-acond
      ((plcmp-get-module-and-method-by-candidate struct display-candidate)
       (multiple-value-bind (module method) it ;when bind, IT must be list of string '(module method)
         module))
      (t
       (plcmp-get-real-candidate display-candidate)))))

(defsubst plcmp-start-initial-input-p (initial-input candidate)
  (let ((re (concat "^" (regexp-quote initial-input))))
    (string-match re candidate)))

(defsubst plcmp-fillter-and-add-metadata (los initial-input metadata-format) ;TODO: funcname
  "return los"
  (loop for str in los
        with ret
        do (when (plcmp-start-initial-input-p initial-input str)
             (push (plcmp-build-display-candidate metadata-format str) ret))
        finally return (nreverse ret)))

;;; buffer dabbrev, functions, variables
(defsubst plcmp-check-face (facename &optional point)
  "POINTの位置のフェイスを調べる(前の文字)"
  (let* ((p (or point (point)))
         (face (get-text-property p 'face)))
    (if (listp face)
        (memq facename face)
      (eq facename face))))

(defsubst plcmp-bit-regexp-p (s)
  (string-match "^[/:$@&%(),.?<>+!|^*';\"\\]+$" s))

(defun plcmp-get-words-by-face (face)
  (ignore-errors
    (save-excursion
      (let ((ret nil))
        (goto-char (point-min))
        ;;最初のfaceへ移動
        (loop always (not (plcmp-check-face face (if (bobp) (point) (- (point) 1))))
              do (unless (forward-word)
                   (error "no variables")))
        (forward-char -1)
        ;;プロパティが変わる部分を走査する
        (loop for next-change = (or (next-property-change (point) (current-buffer))
                                    (point-max))
              always (not (eobp))
              do (progn
                   (when (plcmp-check-face face)
                     (let ((str (or (cperl-word-at-point) "")))
                       ;; fillter
                       (unless (plcmp-bit-regexp-p str)
                         (push str ret)))) ;must be string
                   (goto-char next-change)))
        (delete-dups ret)))))

(defun plcmp-get-words-by-face-internal (struct face &optional buffer)
  (plcmp-with-completion-data-slots struct
      (current-buffer)
    (let ((buffer (or buffer current-buffer)))
      (with-current-buffer buffer
        (plcmp-get-words-by-face face)))))

(defun plcmp-get-buffer-variables (struct)
  (plcmp-with-completion-data-slots struct
      (current-buffer initial-input)
    (with-current-buffer current-buffer
      (plcmp-fillter-and-add-metadata
       (plcmp-get-words-by-face-internal struct 'font-lock-variable-name-face)
       initial-input
       plcmp-display-format-variables))))

(defun plcmp-get-buffer-functions (struct &optional buffer)
  (plcmp-with-completion-data-slots struct
      (current-buffer using-modules initial-input)
    (let ((buffer (or buffer current-buffer)))
      (with-current-buffer buffer
        (let ((los (remove-if (lambda (s) (member s using-modules))
                              (plcmp-get-words-by-face-internal struct 'font-lock-function-name-face))))
          (plcmp-fillter-and-add-metadata los initial-input plcmp-display-format-functions))))))

;; this code Stolen from anything-dabbrev-expand.el
;; written by rubikitch
(defun plcmp-buffer-dabbrev-expansions (initial-input &optional all)
  (let ((dabbrev-check-other-buffers all))
    (dabbrev--reset-global-variables)
    (dabbrev--find-all-expansions initial-input nil)))

(defun plcmp-get-buffer-dabbrevs (struct)
  (plcmp-with-completion-data-slots struct
      (initial-input current-buffer)
    (let ((dabbrevs (with-current-buffer current-buffer
                      (when (>= (length initial-input) plcmp-buffer-dabbrev-expansions-number)
                        (let ((dabbrev-abbrev-char-regexp (if  (and (not (null plcmp-dabbrev-abbrev-char-regexp))
                                                                    (not (string-equal "" plcmp-dabbrev-abbrev-char-regexp)))
                                                              plcmp-dabbrev-abbrev-char-regexp
                                                            dabbrev-abbrev-char-regexp))
                              (initial-input (if (equal "" initial-input) "" (substring initial-input 0 -1)))) ;include initial-input
                          (plcmp-buffer-dabbrev-expansions initial-input))))))
      (plcmp-fillter-and-add-metadata dabbrevs initial-input plcmp-display-format-dabbrev-expansions))))

;;; other buffer's functions, variables
(defun plcmp-get-other-perl-buffer-internal (struct face display-format)
  (plcmp-with-completion-data-slots struct
      (current-buffer other-perl-buffers initial-input)
    (let ((perl-bufs other-perl-buffers )
          (ret nil)
          (count 0))
      (dolist (buffer perl-bufs)
        (if (= count plcmp-get-words-other-perl-buf-limit-number)
            (return)
          (let* ((display (concat display-format " *" (buffer-name buffer) "*"))
                 (los (plcmp-get-words-by-face-internal struct face buffer))
                 (los (plcmp-fillter-and-add-metadata los initial-input display)))
            (setq ret (nconc los ret))
            (incf count))))
      ret)))

(defun plcmp-get-other-perl-buffer-functions (struct)
  (plcmp-get-other-perl-buffer-internal struct 'font-lock-function-name-face plcmp-display-format-functions))

(defun plcmp-get-other-perl-buffer-variables (struct)
  (plcmp-get-other-perl-buffer-internal struct 'font-lock-variable-name-face plcmp-display-format-variables))

;;; module
(defun plcmp-get-installed-modules (struct)
  (plcmp-with-completion-data-slots struct
      (installed-modules initial-input)
    (plcmp-fillter-and-add-metadata installed-modules initial-input plcmp-display-format-installed-modules)))

(defun plcmp-get-methods (struct modulename)
  (plcmp-with-completion-data-slots struct
      (current-buffer initial-input)
    (let* ((modules-methods-alist (plcmp-get-modules-methods-alist struct))
           (methods (assoc-default modulename modules-methods-alist)))
      (plcmp-fillter-and-add-metadata methods initial-input modulename))))

(defun plcmp-get-all-methods (struct)
  (plcmp-with-completion-data-slots struct
      (initial-input)
    (let ((modules-methods-alist (plcmp-get-modules-methods-alist struct)))
      (loop for (module-name . methods) in modules-methods-alist
            nconc (plcmp-fillter-and-add-metadata methods initial-input module-name)))))

;; TODO fname
(defun plcmp-get-cands-using-modules (struct)
  (plcmp-with-completion-data-slots struct
      (using-modules initial-input)
    (plcmp-fillter-and-add-metadata using-modules initial-input plcmp-display-format-using-modules)))

(defun plcmp-get-builtin-functions (struct)
  (plcmp-with-completion-data-slots struct
      (initial-input)
    (plcmp-fillter-and-add-metadata plcmp-builtin-functions initial-input plcmp-display-format-builtin-functions)))

(defun plcmp-get-builtin-variables (struct)
  (plcmp-with-completion-data-slots struct
      (initial-input)
    (plcmp-fillter-and-add-metadata plcmp-builtin-variables initial-input plcmp-display-format-builtin-variables)))

(defun plcmp-build-candidates (struct)
  (plcmp-with-completion-data-slots struct
      (state current-object obj-instance-of-module-maybe-alist
             using-modules)
    (let* ((module-name (or (assoc-default current-object obj-instance-of-module-maybe-alist) ;e.x, $ua = LWP::UserAgent->new(); $ua->`!!'
                            (find current-object using-modules :test 'string-equal)))) ; e.x, LWP::UserAgent->`!!'
      (cond
       ;; methods
       ((and (eq state 'methods)
             module-name)
        ;; match only method name
        (setq plcmp-metadata-matcher plcmp-metadata-matcher-re)
        (plcmp-get-methods struct module-name))
       ;; $self
       ((eq state 'self)
        (nconc
         (plcmp-get-buffer-functions struct) ; dabbrev functions
         (plcmp-get-all-methods struct)      ; methods
         (plcmp-get-buffer-dabbrevs struct)  ;dabbrev-expansions
         ))
       ;; all methods
       ;; モジュールが特定できなかったケース
       ((eq state 'methods)
        (nconc
         (plcmp-get-all-methods struct)       ; all methods
         (plcmp-get-buffer-dabbrevs struct))) ; dabbrev-expansions
       ;; installed-modules
       ((eq state 'installed-modules)
        (plcmp-get-installed-modules struct))
       ;; using-modules ;TODO
       ((eq state 'using-modules)
        (plcmp-get-cands-using-modules struct))
       ;; dabbrev variables
       ((eq state 'dabbrev-variables)
        (plcmp-get-buffer-variables struct))
       ;; dabbrev functions
       ((eq state 'dabbrev-functions)
        (plcmp-get-buffer-functions struct))
       ;; builtin-functions
       ((eq state 'builtin-functions)
        (plcmp-get-builtin-functions struct))
       ;; builtin-variables
       ((eq state 'builtin-variables)
        (plcmp-get-builtin-variables struct))
       ;; globals
       ((eq state 'globals)
        (nconc
         (plcmp-get-buffer-functions struct)    ; dabbrev-functions
         (plcmp-get-buffer-variables struct)    ; dabbrev-variables
         (plcmp-get-buffer-dabbrevs struct)     ; dabbrev-expansions
         (plcmp-get-builtin-functions struct)   ; builtin-functions
         (plcmp-get-builtin-variables struct)   ; builtin-variables
         (plcmp-get-cands-using-modules struct) ; using-modules
         (plcmp-get-other-perl-buffer-functions struct) ; dabbrev-functions other perl buffer
         (plcmp-get-other-perl-buffer-variables struct) ; dabbrev-variables other perl buffer
         ))
       ;; all
       (t
        (nconc
         (plcmp-get-cands-using-modules struct) ; using-modules
         (plcmp-get-all-methods struct)
         (plcmp-get-buffer-functions struct)  ; dabbrev-functions
         (plcmp-get-buffer-variables struct)  ; dabbrev-variables
         (plcmp-get-buffer-dabbrevs struct)   ; dabbrev-expansions
         (plcmp-get-builtin-functions struct) ; builtin-functions
         (plcmp-get-builtin-variables struct) ; builtin-variables
         (plcmp-get-other-perl-buffer-functions struct) ; dabbrev-functions other perl buffer
         (plcmp-get-other-perl-buffer-variables struct) ; dabbrev-variables other perl buffer
         ))
       ))))


;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; anything
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; actions
(defun plcmp-insert (struct candidate)
  (plcmp-with-completion-data-slots plcmp-data
      (initial-input default-action-state persistent-action-buffer-point)
    (cond
     ((eq default-action-state 'perldoc-m)
      (multiple-value-bind (output-buf module method)
                           (plcmp-perldoc-m-create-buffer struct candidate)
        (pop-to-buffer output-buf)
        (goto-char persistent-action-buffer-point)))
     (t
      (delete-backward-char (length initial-input))
      (insert (plcmp-get-real-candidate candidate))))))

(defun plcmp-insert-modulename (candidate)
  (plcmp-with-completion-data-slots plcmp-data
      (initial-input)
    (delete-backward-char (length initial-input))
    (insert (plcmp-get-metadate-candidate candidate))))

(defun plcmp-get-builtin-by-candidate-internal (candidate display-format)
  (let ((metadata (plcmp-get-metadate-candidate candidate)))
    (when (string-equal metadata display-format)
      (plcmp-get-real-candidate candidate))))

(defun plcmp-get-builtin-variable-by-candidate (candidate)
  (plcmp-get-builtin-by-candidate-internal candidate plcmp-display-format-builtin-variables))

(defun plcmp-get-builtin-function-by-candidate (candidate)
  (plcmp-get-builtin-by-candidate-internal candidate plcmp-display-format-builtin-functions))

(defun plcmp-dabbrev-variables-p (candidate)
  (let ((metadata (plcmp-get-metadate-candidate candidate)))
    (string-equal metadata plcmp-display-format-variables)))

(defun plcmp-get-buffer-dabbrev-word-by-candidate (struct candidate)
  (plcmp-with-completion-data-slots struct
      (current-buffer)
    (let ((metadata (plcmp-get-metadate-candidate candidate)))
      (when (string-equal metadata plcmp-display-format-dabbrev-expansions)
        (values current-buffer
                (plcmp-get-real-candidate candidate))))))

(defun plcmp-get-buffer-face-word-internal (candidate display-format)
  (let ((metadata (plcmp-get-metadate-candidate candidate)))
    (when (string-equal metadata display-format)
      (plcmp-get-real-candidate candidate))))

(defun plcmp-get-buffer-function-by-candidate (candidate)
  (plcmp-get-buffer-face-word-internal candidate plcmp-display-format-functions))

(defun plcmp-get-buffer-variable-by-candidate (candidate)
  (plcmp-get-buffer-face-word-internal candidate plcmp-display-format-variables))

(defun plcmp-get-dabbrev-other-perl-buffer-internal (candidate display-format)
  "return values '(buffer word) or nil"
  (let ((metadata (plcmp-get-metadate-candidate candidate))
        (re (rx-to-string `(and ,display-format
                                space
                                "*"
                                (group (0+ not-newline))
                                "*"
                                line-end))))
    (when (string-match re metadata)
      (values (match-string-no-properties 1 metadata) ;buffer
              (plcmp-get-real-candidate candidate)))))

(defun plcmp-get-dabbrev-function-other-perl-buffer-by-candidate (candidate)
  (plcmp-get-dabbrev-other-perl-buffer-internal candidate
                                                plcmp-display-format-functions))

(defun plcmp-get-dabbrev-variable-other-perl-buffer-by-candidate (candidate)
  (plcmp-get-dabbrev-other-perl-buffer-internal candidate
                                                plcmp-display-format-variables))

(defun plcmp-get-module-and-method-by-candidate (struct candidate)
  "return values (list module method) or nil"
  (plcmp-with-completion-data-slots struct
      (using-modules)
    (let ((module (plcmp-get-metadate-candidate candidate))
          (method (plcmp-get-real-candidate candidate)))
      (when (and (plcmp-module-p module)
                 (member module using-modules)
                 (plcmp-perl-identifier-p method))
        (values module method)))))

(defun plcmp-fontify-re-search-forward (regexp)
  (let ((struct plcmp-data)) ;TODO
    (plcmp-with-completion-data-slots struct
        (persistent-action-buffer-point)
      (when (re-search-forward regexp nil t)
        (let ((beg (match-beginning 1))
              (end (match-end 1)))
          ;; remember point
          (setq persistent-action-buffer-point (point))
          
          (when (and beg end)
            (if (overlayp plcmp-overlay)
                (move-overlay plcmp-overlay beg end (current-buffer))
              (setq plcmp-overlay (make-overlay beg end)))
            (overlay-put plcmp-overlay 'face plcmp-search-match-face)))))))

(defun plcmp-visit-and-re-search-forward (regexp buffer-name)
  (pop-to-buffer buffer-name)
  (with-current-buffer (get-buffer buffer-name)
    (if (plcmp-fontify-re-search-forward regexp)
        (recenter 2)
      (goto-char (point-min))
      (plcmp-fontify-re-search-forward regexp)
      (recenter 2))))

(defun plcmp-re-search-forward-as-cperl-mode (regexp buffer-name)
  (let ((saved-major-mode major-mode))
    (unwind-protect
        (progn
          (cperl-mode)
          (plcmp-visit-and-re-search-forward regexp buffer-name))
      (when (functionp saved-major-mode)
        (funcall saved-major-mode)))))

(defun plcmp-open-perldoc (arg type &optional pop-to-buffer)
  "open perldoc.
return buffer"
  (let* ((process-environment (copy-sequence process-environment))
         (coding-system-for-read (if default-enable-multibyte-characters
                                     locale-coding-system
                                   'raw-text-unix))
         (program (case type
                    (var "perldoc perlvar")
                    (func "perldoc -f")
                    (module "perldoc")))
         (command (if (eq type 'var)
                      program
                    (concat program " " (shell-quote-argument arg)))))
    (require 'env)
    (setenv "TERM" "dumb") ; problem when (equal (getenv "TERM") "xterm-color")

    (save-window-excursion
      (unless (eq real-last-command 'plcmp-anything-execute-persistent-action)
        (shell-command command plcmp-perldoc-output-buf-name)))
    (let ((buf (get-buffer plcmp-perldoc-output-buf-name)))
      (if pop-to-buffer
          (pop-to-buffer buf)
        (switch-to-buffer buf))
      ;; return buffer
      buf)))

;; TODO
(defun plcmp-perldoc (struct candidate &optional pop-to-buffer)
  (plcmp-acond
    ;; builtin-variables
    ((plcmp-get-builtin-variable-by-candidate candidate)
     (let ((buffer (plcmp-open-perldoc it 'var))
           (re (rx-to-string `(and bol (= 4 space)
                                   (group (eval ,it))
                                   (syntax whitespace)))))
       (plcmp-visit-and-re-search-forward re buffer)))
    ;; builtin-function
    ((plcmp-get-builtin-function-by-candidate candidate)
     (plcmp-open-perldoc it 'func))
    ;; buffer dabbrev
    ((plcmp-get-buffer-dabbrev-word-by-candidate struct candidate)
     (multiple-value-bind (buffer-name word) it
       (let ((re (rx-to-string `(and symbol-start (group (eval ,word)) symbol-end))))
         (plcmp-visit-and-re-search-forward re buffer-name))))
    ;; dabbrev function current buffer
    ((plcmp-get-buffer-function-by-candidate candidate)
     (plcmp-with-completion-data-slots struct
         (current-buffer)
       (let ((re (rx-to-string `(and symbol-start (group (eval ,it)) symbol-end))))
         (plcmp-visit-and-re-search-forward re current-buffer))))
    ;; dabbrev variable current buffer
    ((plcmp-get-buffer-variable-by-candidate candidate)
     (plcmp-with-completion-data-slots struct
         (current-buffer)
       (let ((re (rx-to-string `(and (group (eval ,it)) symbol-end))))
         (plcmp-visit-and-re-search-forward re current-buffer))))
    ;; dabbrev function other perl buffer
    ((plcmp-get-dabbrev-function-other-perl-buffer-by-candidate candidate)
     (multiple-value-bind (buffer-name func-name) it
       (let ((re (rx-to-string `(and symbol-start (group (eval ,func-name)) symbol-end))))
         (plcmp-visit-and-re-search-forward re buffer-name))))
    ;; dabbrev variable other perl buffer
    ((plcmp-get-dabbrev-variable-other-perl-buffer-by-candidate candidate)
     (multiple-value-bind (buffer-name var-name) it
       (let ((re (rx-to-string `(and (group (eval ,var-name)) symbol-end))))
         (plcmp-visit-and-re-search-forward re buffer-name))))
    ;; method
    ((plcmp-get-module-and-method-by-candidate struct candidate)
     (multiple-value-bind (module method) it ;when bind, IT must be list of string '(module method)
       (let ((buffer (plcmp-open-perldoc module 'module))
             (re (rx-to-string `(and symbol-start (group (eval ,method)) symbol-end))))
         (plcmp-re-search-forward-as-cperl-mode re buffer))))
    ;; otherwise
    (t
     (let* ((modname (if (plcmp-module-p (plcmp-get-metadate-candidate candidate))
                         (plcmp-get-metadate-candidate candidate)
                       (plcmp-get-real-candidate candidate))))
       (plcmp-open-perldoc modname 'module pop-to-buffer)))))

(defun plcmp-perldoc-m-create-buffer (struct candidate)
  (let (module method)
    (plcmp-acond
      ((plcmp-get-module-and-method-by-candidate struct candidate)
       (setq module (first it)
             method (second it)))
      (t
       (setq module (plcmp-get-real-candidate candidate))))
    (let ((cperl-mode-hook nil)
          (output-buf (concat "*Perldoc -m " module "*")))
      (cond
       ((buffer-live-p (get-buffer output-buf))
        (values output-buf module method))
       (t
        (shell-command (concat "perldoc -m " module) output-buf)

        (with-current-buffer output-buf
          (goto-char (point-min))
          (cperl-mode))

        (values output-buf module method))))))

(defun plcmp-perldoc-m (struct candidate)
  (multiple-value-bind (output-buf module method)
                       (plcmp-perldoc-m-create-buffer struct candidate)
    (let* ((re (cond ((null method)
                      "")
                     ((= (plcmp-seq-times 'plcmp-persistent-perldoc-m) 0)
                      (rx-to-string `(and "sub" (1+ space) (group (eval ,method)))))
                     (t
                      (rx-to-string `(and symbol-start (group (eval ,method)) symbol-end))))))
      (plcmp-visit-and-re-search-forward re output-buf))))

(defun plcmp-persistent-perldoc-m ()
  (interactive)
  (let ((struct plcmp-data))
    (plcmp-with-completion-data-slots struct
        (default-action-state)
      (save-selected-window
        (select-window (get-buffer-window plcmp-anything-buffer))
        (select-window (setq minibuffer-scroll-window
                             (if (one-window-p t) (split-window) (next-window (selected-window) 1))))
        (let* ((plcmp-anything-window (get-buffer-window plcmp-anything-buffer))
               (selection (if plcmp-anything-saved-sources
                              ;; the action list is shown
                              plcmp-anything-saved-selection
                            (plcmp-anything-get-selection))))
          (set-window-dedicated-p plcmp-anything-window t)
          (unwind-protect
              (progn
                (setq default-action-state 'perldoc-m)
                (plcmp-perldoc-m struct selection))
            (set-window-dedicated-p plcmp-anything-window nil)))))))

(defun plcmp-open-module-file (struct candidate)
  (condition-case e
      (let ((modulename (plcmp-get-modulename-candidate struct candidate)))
        (unless (plcmp-module-p modulename)
          (error "invild format: %s" modulename))
        (let* ((path (shell-command-to-string (concat "perldoc -l " modulename)))
               (path (plcmp-trim path)))
          (if (file-exists-p path)
              (find-file path)
            (error "can't find module %s" path))))
    (message "%s" (plcmp-log "%s" (error-message-string e)))))

;;; match
(defcustom plcmp-match-any-greedy t
  "non-nilだとパターンをスペースで区切って候補を絞り込めるようになる"
  :type 'boolean
  :group 'perl-completion) ;TODO: varname
(defvar plcmp-match-last-re "")
(defvar plcmp-match-last-anything-pattern "")

(defun plcmp-match (candidate)
  (cond
   ((string-equal plcmp-anything-pattern
                  plcmp-match-last-anything-pattern)
    (string-match plcmp-match-last-re candidate))
   (t
    (let* ((re (replace-regexp-in-string
               "[ \t]+" ".*?"
               (plcmp-trim plcmp-anything-pattern)))
           (re (concat plcmp-metadata-matcher re)))
      (setq plcmp-match-last-re re
            plcmp-match-last-anything-pattern plcmp-anything-pattern)
      (string-match re candidate)))))

;;; keymap
(defvar plcmp-anything-map
  (let ((map (copy-keymap minibuffer-local-map)))
    ;; persistent-action
    (define-key map (kbd "C-z")  'plcmp-anything-execute-persistent-action)

    ;; call action with selection
    (define-key map (kbd "M") 'plcmp-persistent-perldoc-m)
    (define-key map (kbd "D") 'plcmp-action-perldoc)
    (define-key map (kbd "O") 'plcmp-action-open-module-file)

    ;; JKL, for reading document
    (define-key map (kbd "J") 'scroll-other-window)
    (define-key map (kbd "K") 'scroll-other-window-down)
    (define-key map (kbd "L") 'plcmp-anything-execute-persistent-action)

    (define-key map (kbd "M-C-v") 'scroll-other-window)
    (define-key map (kbd "M-C-S-v") 'scroll-other-window-down)
    
    ;; Stolen from anything-config.el
    (define-key map (kbd "<down>")  'plcmp-anything-next-line)
    (define-key map (kbd "<up>")    'plcmp-anything-previous-line)
    (define-key map (kbd "C-n")     'plcmp-anything-next-line)
    (define-key map (kbd "C-p")     'plcmp-anything-previous-line)
    (define-key map (kbd "<prior>") 'plcmp-anything-previous-page)
    (define-key map (kbd "<next>")  'plcmp-anything-next-page)
    (define-key map (kbd "M-v")     'plcmp-anything-previous-page)
    (define-key map (kbd "C-v")     'plcmp-anything-next-page)
    ;;(define-key map (kbd "<right>") 'plcmp-anything-next-source)
    (define-key map "\M-\C-f" 'plcmp-anything-next-source)
    (define-key map "\M-\C-b"  'plcmp-anything-previous-source)
    (define-key map (kbd "<RET>")   'plcmp-anything-exit-minibuffer)
    (define-key map (kbd "C-1")     'plcmp-anything-select-with-digit-shortcut)
    (define-key map (kbd "C-2")     'plcmp-anything-select-with-digit-shortcut)
    (define-key map (kbd "C-3")     'plcmp-anything-select-with-digit-shortcut)
    (define-key map (kbd "C-4")     'plcmp-anything-select-with-digit-shortcut)
    (define-key map (kbd "C-5")     'plcmp-anything-select-with-digit-shortcut)
    (define-key map (kbd "C-6")     'plcmp-anything-select-with-digit-shortcut)
    (define-key map (kbd "C-7")     'plcmp-anything-select-with-digit-shortcut)
    (define-key map (kbd "C-8")     'plcmp-anything-select-with-digit-shortcut)
    (define-key map (kbd "C-9")     'plcmp-anything-select-with-digit-shortcut)
    (define-key map (kbd "<tab>")   'plcmp-anything-select-action)
    (defalias 'plcmp-anything-next-history-element     'next-history-element)
    (defalias 'plcmp-anything-previous-history-element 'previous-history-element)
    (define-key map (kbd "M-p")     'plcmp-anything-previous-history-element)
    (define-key map (kbd "M-n")     'plcmp-anything-next-history-element)
    (define-key map (kbd "C-s")     'plcmp-anything-isearch)
    (define-key map (kbd "C-r")     'undefined)
    map))

(defvar plcmp-anything-isearch-map
  (let ((map (copy-keymap (current-global-map))))
    (define-key map (kbd "<return>")    'plcmp-anything-isearch-default-action)
    (define-key map (kbd "<tab>")       'plcmp-anything-isearch-select-action)
    (define-key map (kbd "C-g")         'plcmp-anything-isearch-cancel)
    (define-key map (kbd "C-s")         'plcmp-anything-isearch-again)
    (define-key map (kbd "C-r")         'undefined)
    (define-key map (kbd "<backspace>") 'plcmp-anything-isearch-delete)
    ;; add printing chars
    (let ((i ?\s))
      (while (< i 256)
        (define-key map (vector i) 'plcmp-anything-isearch-printing-char)
        (setq i (1+ i))))
    map))

;;; sources


(defvar plcmp-anything-type-attributes
  `((plcmp
     (action . (("Insert" . (lambda (candidate)
                              (plcmp-insert plcmp-data candidate)))
                ("Open module file" . (lambda (candidate)
                                        (plcmp-open-module-file plcmp-data candidate)))
                ("Perldoc" . (lambda (candidate)
                               (plcmp-perldoc plcmp-data candidate)))
                ("Perldoc -m" . (lambda (candidate)
                                  (plcmp-perldoc-m plcmp-data candidate)))
                ))
     (persistent-action . (lambda (candidate)
                            (plcmp-perldoc plcmp-data candidate))))))

(defvar plcmp-anything-c-source-smart-complete
  `((name . "perl completion")
    (type . plcmp)
    (init . (lambda ()
              (plcmp-initialize plcmp-data)))
    (candidates . (lambda ()
                    (plcmp-build-candidates plcmp-data)))
    (cache)
    (match . (plcmp-match))))

(defvar plcmp-anything-source-builtin-functions
  `((name . "builtin-functions")
    (type . plcmp)
    (init . (lambda ()
              (plcmp-initialize plcmp-data)
              (plcmp-with-completion-data-slots plcmp-data
                  (initial-input state)
                (setf initial-input ""
                      state 'builtin-functions))))
    (candidates . (lambda ()
                    (plcmp-build-candidates plcmp-data)))
    (match . (plcmp-match))))

(defvar plcmp-anything-source-builtin-variables
  `((name . "builtin-variables")
    (type . plcmp)
    (init . (lambda ()
              (plcmp-initialize plcmp-data)
              (plcmp-with-completion-data-slots plcmp-data
                  (initial-input state)
                (setf initial-input ""
                      state 'builtin-variables))))
    (candidates . (lambda ()
                    (plcmp-build-candidates plcmp-data)))
    (match . (plcmp-match))))

(defvar plcmp-anything-source-using-modules
  `((name . "using-modules")
    (type . plcmp)
    (init . (lambda ()
              (plcmp-initialize plcmp-data)
              (plcmp-with-completion-data-slots plcmp-data
                  (initial-input state)
                (setf initial-input ""
                      state 'using-modules))))
    (candidates . (lambda ()
                    (plcmp-build-candidates plcmp-data)))
    (match . (plcmp-match))))

(defvar plcmp-anything-source-installed-modules
  `((name . "installed-modules")
    (type . plcmp)
    (init . (lambda ()
              (plcmp-initialize plcmp-data)
              (plcmp-with-completion-data-slots plcmp-data
                  (initial-input state)
                (setf state 'installed-modules
                      initial-input ""))))
    (candidates . (lambda ()
                    (plcmp-build-candidates plcmp-data)))
    (match . (plcmp-match))))

(defvar plcmp-anything-source-all
  `((name . "All")
    (type . plcmp)
    (init . (lambda ()
              (plcmp-initialize plcmp-data)
              (plcmp-with-completion-data-slots plcmp-data
                  (initial-input state)
                (setf state 'all
                      initial-input ""))))
    (candidates . (lambda ()
                    (plcmp-build-candidates plcmp-data)))
    (match . (plcmp-match))))

(defun plcmp-smart-complete ()
  (interactive)
  (let ((plcmp-anything-sources (list plcmp-anything-c-source-smart-complete)))
    (condition-case e
        (plcmp-anything)
      (message "%s" (error-message-string e)))))

(defun plcmp-installed-modules-complete ()
  (interactive)
  (let ((plcmp-anything-sources (list plcmp-anything-source-installed-modules)))
    (plcmp-anything)))

(defun plcmp-builtin-function-complete ()
  (interactive)
  (let ((plcmp-anything-sources (list plcmp-anything-source-builtin-functions)))
    (plcmp-anything)))

(defun plcmp-builtin-variables-complete ()
  (interactive)
  (let ((plcmp-anything-sources (list plcmp-anything-source-builtin-variables)))
    (plcmp-anything)))

(defun plcmp-search-word-at-point ()
  (interactive)
  (let ((plcmp-anything-sources (list plcmp-anything-source-all))
        (word (concat (or (cperl-word-at-point) "") " "))
        (plcmp-metadata-matcher-re ""))
    (plcmp-anything word)))

(defun plcmp-using-modules-complete ()
  (interactive)
  (let ((plcmp-anything-sources (list plcmp-anything-source-using-modules)))
    (plcmp-anything)))

(defun plcmp-reset ()
  (interactive)
  (unload-feature 'perl-completion t)
  (require 'perl-completion nil t))

(defun plcmp-clear-all-cache ()
  (interactive)
  (ignore-errors
    (plcmp-with-completion-data-slots plcmp-data
        (cache-installed-modules)
      (setf plcmp-last-using-modules nil
            plcmp-modules-methods-alist nil
            cache-installed-modules nil)
      (kill-buffer plcmp-installed-modules-buf-name)
      (plcmp-send-command-get-installed-modules))))
      

;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Commands
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;; anything
(defun plcmp-call-action-by-action-name (action-name)
  (setq plcmp-anything-saved-selection (plcmp-anything-get-selection))
  (unless plcmp-anything-saved-selection
    (error "Nothing is selected."))
  (let ((action (cdr (assoc action-name (plcmp-anything-get-action)))))
    (if action
        (setq plcmp-anything-saved-action action
              plcmp-anything-saved-sources plcmp-anything-sources)
      (error "no action %s" action-name))
    (plcmp-anything-exit-minibuffer)))

(defun plcmp-action-insert-modulename ()
  (interactive)
  (plcmp-call-action-by-action-name "Insert modulename"))

(defun plcmp-action-open-module-file ()
  (interactive)
  (plcmp-call-action-by-action-name "Open module file"))

(defun plcmp-action-perldoc-m ()
    (interactive)
    (plcmp-call-action-by-action-name "Perldoc -m"))

(defun plcmp-action-perldoc ()
  (interactive)
  (plcmp-call-action-by-action-name "Perldoc"))


;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Mode
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defmacro plcmp-set-key (key binding)
  `(define-key plcmp-mode-map ,key ,binding))

(defvar plcmp-mode-map (make-keymap))
;;; setup
(plcmp-set-key (kbd "M-TAB") 'plcmp-smart-complete)
(plcmp-set-key (kbd "C-RET") 'plcmp-smart-complete)
(plcmp-set-key (kbd "C-<return>") 'plcmp-smart-complete)
(plcmp-set-key (kbd "C-c f") 'plcmp-builtin-function-complete)
(plcmp-set-key (kbd "C-c v") 'plcmp-builtin-variables-complete)
(plcmp-set-key (kbd "C-c i") 'plcmp-installed-modules-complete)
(plcmp-set-key (kbd "C-c u") 'plcmp-using-modules-complete)
(plcmp-set-key (kbd "C-c c") 'plcmp-clear-all-cache)
(plcmp-set-key (kbd "C-c s") 'plcmp-search-word-at-point)


(defun plcmp-mode-init ()
  ;;初回起動時
  (unless (buffer-live-p (get-buffer plcmp-installed-modules-buf-name))
    (plcmp-send-command-get-installed-modules)))

(define-minor-mode perl-completion-mode "" nil " PLCompletion" plcmp-mode-map (plcmp-mode-init))


;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; compatibility anything
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 名前空間がバッティングしないように全てのシンボルにprefixを付加し、ドキュメントと空行を削除したanything.elのソース
;;; perl-completion redefine anything core

(defun plcmp-anything (&optional initial-pattern)
  (interactive)
  (let ((frameconfig (current-frame-configuration)))
    (add-hook 'post-command-hook 'plcmp-anything-check-minibuffer-input)
    (plcmp-anything-initialize)
    (if plcmp-anything-samewindow
        (switch-to-buffer plcmp-anything-buffer)
      (pop-to-buffer plcmp-anything-buffer))
    (unwind-protect
        (progn
          (plcmp-anything-update)
          (select-frame-set-input-focus (window-frame (minibuffer-window)))
          (let ((minibuffer-local-map plcmp-anything-map))
            (if (null initial-pattern)
                (read-string "pattern: ")
              (read-string "pattern: " initial-pattern))
            ))
      (plcmp-anything-cleanup)
      (remove-hook 'post-command-hook 'plcmp-anything-check-minibuffer-input)
      (set-frame-configuration frameconfig)))
  
  (plcmp-anything-execute-selection-action))

;; マッチ部分の高速化(実験段階)


(defadvice plcmp-anything-initialize (after initialize-matched-candidate-cache activate)
  (setq plcmp-anything-matched-candidate-cache nil))

(defadvice plcmp-anything (around cleanup-overlay activate)
  (unwind-protect
      ad-do-it
    ;; overlay
    (when (overlayp plcmp-overlay)
      (delete-overlay plcmp-overlay))))

(defun plcmp-anything-get-cached-matched-candidates (source)
  (let ((cache (assq 'cache source))
        (name (assoc-default 'name source))
        (alist (assoc-default name plcmp-anything-matched-candidate-cache))
        (pattern (or (ignore-errors (substring plcmp-anything-pattern
                                               0 (1- (length plcmp-anything-pattern))))
                     ""))
        (ret nil))
    (plcmp-log "pattern: %s" pattern)
    (cond
     ((string-match "^\\w+\\s *\\w+" pattern)
      (assoc-default pattern alist))
     (t
      nil))))

(defun plcmp-anything-process-source (source)
  (let (matches)
    (if (equal plcmp-anything-pattern "")
        (progn
          (setq matches (plcmp-anything-get-cached-candidates source))
          (if (> (length matches) plcmp-anything-candidate-number-limit)
              (setq matches
                    (subseq matches 0 plcmp-anything-candidate-number-limit))))
      (condition-case nil
          (let ((item-count 0)
                (functions (assoc-default 'match source))
                exit
                ;; cache option
                (cache (assq 'cache source))
                (name (assoc-default 'name source))
                )
            (unless functions
              (setq functions
                    (list (lambda (candidate)
                            (string-match plcmp-anything-pattern candidate)))))
            (dolist (function functions)
              (let (newmatches)
                ;; get cache
                (dolist (candidate (or (plcmp-anything-get-cached-matched-candidates source) ;match cache
                                       (plcmp-anything-get-cached-candidates source)))
                  (when (and (not (member candidate matches))
                             (funcall function (if (listp candidate)
                                                   (car candidate)
                                                 candidate)))
                    (push candidate newmatches)
                    
                    (when (and plcmp-anything-candidate-number-limit
                               (not cache))
                      (incf item-count)
                      (when (= item-count plcmp-anything-candidate-number-limit)
                        (setq exit t)
                        (return)))))

                (setq matches (append matches (reverse newmatches)))
                (if exit
                    (return))))
            ;; return cached
            (cond ((assoc name plcmp-anything-matched-candidate-cache)
                   (let* ((lst (assoc name plcmp-anything-matched-candidate-cache))
                          (alist (rest lst)))
                     (setcdr lst
                             (add-to-list 'alist `(,plcmp-anything-pattern . ,matches)))))
                  (t
                   (push `(,name . ((,plcmp-anything-pattern . ,matches)))
                         plcmp-anything-matched-candidate-cache)))
                
            (plcmp-log "input %S \nmatches: %S\n" plcmp-anything-pattern matches))
        (invalid-regexp (setq matches nil))))
    
    (let* ((transformer (assoc-default 'filtered-candidate-transformer source)))
      (if transformer
          (setq matches (funcall transformer matches source))))
    (when matches
      (plcmp-anything-insert-header (assoc-default 'name source))
      (dolist (match matches)
        (when (and plcmp-anything-enable-digit-shortcuts
                   (not (eq plcmp-anything-digit-shortcut-count 9)))
          (move-overlay (nth plcmp-anything-digit-shortcut-count
                             plcmp-anything-digit-overlays)
                        (line-beginning-position)
                        (line-beginning-position))
          (incf plcmp-anything-digit-shortcut-count))
        (plcmp-anything-insert-match match 'insert)))))

(defun plcmp-anything-execute-selection-action () 
  (let* ((selection (if plcmp-anything-saved-sources
                        plcmp-anything-saved-selection
                      (plcmp-anything-get-selection)))
         (action (or plcmp-anything-saved-action
                     (if plcmp-anything-saved-sources
                         (plcmp-anything-get-selection)
                       (plcmp-anything-get-action)))))
    (if (and (listp action)
             (not (functionp action)))
        (setq action (cdar action)))
    (setq plcmp-anything-saved-action nil)
    (if (and selection action)
        (funcall action selection))))

;;; aything core
(defvar plcmp-anything-sources nil)
(defvar plcmp-anything-enable-digit-shortcuts nil )
(defvar plcmp-anything-candidate-number-limit plcmp-anything-candidate-number-limit )
(defvar plcmp-anything-idle-delay 0.5 )
(defvar plcmp-anything-samewindow nil )
(defvar plcmp-anything-source-filter nil )
(defvar plcmp-anything-isearch-map
  (let ((map (copy-keymap (current-global-map))))
    (define-key map (kbd "<return>") 'plcmp-anything-isearch-default-action)
    (define-key map (kbd "C-i") 'plcmp-anything-isearch-select-action)
    (define-key map (kbd "C-g") 'plcmp-anything-isearch-cancel)
    (define-key map (kbd "M-s") 'plcmp-anything-isearch-again)
    (define-key map (kbd "<backspace>") 'plcmp-anything-isearch-delete)
    (let ((i 32))
      (while (< i 256)
        (define-key map (vector i) 'plcmp-anything-isearch-printing-char)
        (setq i (1+ i))))
    map))
(defgroup plcmp-anything nil
  "Open plcmp-anything." :prefix "plcmp-anything-" :group 'convenience)
(if (facep 'header-line)
    (copy-face 'header-line 'plcmp-anything-header)
  (defface plcmp-anything-header
    '((t (:bold t :underline t)))
    "Face for header lines in the plcmp-anything buffer." :group 'plcmp-anything))
(defvar plcmp-anything-header-face 'plcmp-anything-header )
(defface plcmp-anything-isearch-match '((t (:background "Yellow")))
  "Face for isearch in the plcmp-anything buffer." :group 'plcmp-anything)
(defvar plcmp-anything-isearch-match-face 'plcmp-anything-isearch-match )
(defvar plcmp-anything-iswitchb-idle-delay 1 )
(defvar plcmp-anything-iswitchb-dont-touch-iswithcb-keys nil )
(defconst plcmp-anything-buffer "*perl-completion anything*" )
(defvar plcmp-anything-selection-overlay nil )
(defvar plcmp-anything-isearch-overlay nil )
(defvar plcmp-anything-digit-overlays nil )
(defvar plcmp-anything-candidate-cache nil )
(defvar plcmp-anything-pattern "")
(defvar plcmp-anything-input "")
(defvar plcmp-anything-async-processes nil )
(defvar plcmp-anything-digit-shortcut-count 0 )
(defvar plcmp-anything-update-hook nil )
(defvar plcmp-anything-saved-sources nil )
(defvar plcmp-anything-saved-selection nil )
(defvar plcmp-anything-original-source-filter nil )
(put 'plcmp-anything 'timid-completion 'disabled)
(defun plcmp-anything-check-minibuffer-input () 
  (with-selected-window (minibuffer-window)
    (plcmp-anything-check-new-input (minibuffer-contents))))
(defun plcmp-anything-check-new-input (input) 
  (unless (equal input plcmp-anything-pattern)
    (setq plcmp-anything-pattern input)
    (unless plcmp-anything-saved-sources
      (setq plcmp-anything-input plcmp-anything-pattern))
    (plcmp-anything-update)))
(defun plcmp-anything-update () 
  (setq plcmp-anything-digit-shortcut-count 0)
  (plcmp-anything-kill-async-processes)
  (with-current-buffer plcmp-anything-buffer
    (erase-buffer)
    (if plcmp-anything-enable-digit-shortcuts
        (dolist (overlay plcmp-anything-digit-overlays)
          (delete-overlay overlay)))
    (let (delayed-sources)
      (dolist (source (plcmp-anything-get-sources))
        (if (or (not plcmp-anything-source-filter)
                (member (assoc-default 'name source) plcmp-anything-source-filter))
            (if (equal plcmp-anything-pattern "")
                (unless (assoc 'requires-pattern source)
                  (if (assoc 'delayed source)
                      (push source delayed-sources)
                    (plcmp-anything-process-source source)))
              (let ((min-pattern-length (assoc-default 'requires-pattern source)))
                (unless (and min-pattern-length
                             (< (length plcmp-anything-pattern) min-pattern-length))
                  (if (assoc 'delayed source)
                      (push source delayed-sources)
                    (plcmp-anything-process-source source)))))))
      (goto-char (point-min))
      (run-hooks 'plcmp-anything-update-hook)
      (plcmp-anything-next-line)
      (plcmp-anything-maybe-fit-frame)
      (run-with-idle-timer (if (featurep 'xemacs)
                               0.1
                             0)
                           nil
                           'plcmp-anything-process-delayed-sources
                           delayed-sources))))
(defun plcmp-anything-get-sources () 
  (mapcar (lambda (source)
            (let ((type (assoc-default 'type source)))
              (if type
                  (append source (assoc-default type plcmp-anything-type-attributes) nil)
                source)))
          plcmp-anything-sources))
;; (defun plcmp-anything-process-source (source) 
;;   (let (matches)
;;     (if (equal plcmp-anything-pattern "")
;;         (progn
;;           (setq matches (plcmp-anything-get-cached-candidates source))
;;           (if (> (length matches) plcmp-anything-candidate-number-limit)
;;               (setq matches
;;                     (subseq matches 0 plcmp-anything-candidate-number-limit))))
;;       (condition-case nil
;;           (let ((item-count 0)
;;                 (functions (assoc-default 'match source))
;;                 exit)
;;             (unless functions
;;               (setq functions
;;                     (list (lambda (candidate)
;;                             (string-match plcmp-anything-pattern candidate)))))
;;             (dolist (function functions)
;;               (let (newmatches)
;;                 (dolist (candidate (plcmp-anything-get-cached-candidates source))
;;                   (when (and (not (member candidate matches))
;;                              (funcall function (if (listp candidate)
;;                                                    (car candidate)
;;                                                  candidate)))
;;                     (push candidate newmatches)
;;                     (when plcmp-anything-candidate-number-limit
;;                       (incf item-count)
;;                       (when (= item-count plcmp-anything-candidate-number-limit)
;;                         (setq exit t)
;;                         (return)))))
;;                 (setq matches (append matches (reverse newmatches)))
;;                 (if exit
;;                     (return)))))
;;         (invalid-regexp (setq matches nil))))
;;     (let* ((transformer (assoc-default 'filtered-candidate-transformer source)))
;;       (if transformer
;;           (setq matches (funcall transformer matches source))))
;;     (when matches
;;       (plcmp-anything-insert-header (assoc-default 'name source))
;;       (dolist (match matches)
;;         (when (and plcmp-anything-enable-digit-shortcuts
;;                    (not (eq plcmp-anything-digit-shortcut-count 9)))
;;           (move-overlay (nth plcmp-anything-digit-shortcut-count
;;                              plcmp-anything-digit-overlays)
;;                         (line-beginning-position)
;;                         (line-beginning-position))
;;           (incf plcmp-anything-digit-shortcut-count))
;;         (plcmp-anything-insert-match match 'insert)))))
(defun plcmp-anything-insert-match (match insert-function) 
  (if (not (listp match))
      (funcall insert-function match)
    (funcall insert-function (car match))
    (put-text-property (line-beginning-position) (line-end-position)
                       'plcmp-anything-realvalue (cdr match)))
  (funcall insert-function "\n"))
(defun plcmp-anything-process-delayed-sources (delayed-sources) 
  (if (sit-for plcmp-anything-idle-delay)
      (with-current-buffer plcmp-anything-buffer
        (save-excursion
          (goto-char (point-max))
          (dolist (source delayed-sources)
            (plcmp-anything-process-source source))
          (when (and (not (equal (buffer-size) 0))
                     (= (overlay-start plcmp-anything-selection-overlay)
                        (overlay-end plcmp-anything-selection-overlay)))
            (goto-char (point-min))
            (run-hooks 'plcmp-anything-update-hook)
            (plcmp-anything-next-line)))
        (plcmp-anything-maybe-fit-frame))))

;;define above

;; (defun plcmp-anything () 
;;   (interactive)
;;   (let ((frameconfig (current-frame-configuration)))
;;     (add-hook 'post-command-hook 'plcmp-anything-check-minibuffer-input)
;;     (plcmp-anything-initialize)
;;     (if plcmp-anything-samewindow
;;         (switch-to-buffer plcmp-anything-buffer)
;;       (pop-to-buffer plcmp-anything-buffer))
;;     (unwind-protect
;;         (progn
;;           (plcmp-anything-update)
;;           (select-frame-set-input-focus (window-frame (minibuffer-window)))
;;           (let ((minibuffer-local-map plcmp-anything-map))
;;             (read-string "pattern: " ) 
;;             ))
;;       (plcmp-anything-cleanup)
;;       (remove-hook 'post-command-hook 'plcmp-anything-check-minibuffer-input)
;;       (set-frame-configuration frameconfig)))
;;   (plcmp-anything-execute-selection-action))
;; Redefined above
;; (defun plcmp-anything-execute-selection-action () 
;;   (let* ((selection (if plcmp-anything-saved-sources
;;                         plcmp-anything-saved-selection
;;                       (plcmp-anything-get-selection)))
;;          (action (if plcmp-anything-saved-sources
;;                      (plcmp-anything-get-selection)
;;                    (plcmp-anything-get-action))))
;;     (if (and (listp action)
;;              (not (functionp action)))  
;;         (setq action (cdar action)))
;;     (if (and selection action)
;;         (funcall action selection))))
(defun plcmp-anything-get-selection () 
  (unless (= (buffer-size (get-buffer plcmp-anything-buffer)) 0)
    (with-current-buffer plcmp-anything-buffer
      (let ((selection
             (or (get-text-property (overlay-start
                                     plcmp-anything-selection-overlay)
                                    'plcmp-anything-realvalue)
                 (buffer-substring-no-properties
                  (overlay-start plcmp-anything-selection-overlay)
                  (1- (overlay-end plcmp-anything-selection-overlay))))))
        (unless (equal selection "")
          selection)))))
(defun plcmp-anything-get-action () 
  (unless (= (buffer-size (get-buffer plcmp-anything-buffer)) 0)
    (let* ((source (plcmp-anything-get-current-source))
           (actions (assoc-default 'action source)))
      (let* ((transformer (assoc-default 'action-transformer source)))
        (if transformer
            (funcall transformer actions (plcmp-anything-get-selection))
          actions)))))
(defun plcmp-anything-select-action () 
  (interactive)
  (if plcmp-anything-saved-sources
      (error "Already showing the action list"))
  (setq plcmp-anything-saved-selection (plcmp-anything-get-selection))
  (unless plcmp-anything-saved-selection
    (error "Nothing is selected."))
  (let ((actions (plcmp-anything-get-action)))
    (setq plcmp-anything-source-filter nil)
    (setq plcmp-anything-saved-sources plcmp-anything-sources)
    (setq plcmp-anything-sources `(((name . "Actions")
                                    (candidates . ,actions))))
    (with-selected-window (minibuffer-window)
      (delete-minibuffer-contents))
    (setq plcmp-anything-pattern 'dummy)      
    (plcmp-anything-check-minibuffer-input)))
(defun plcmp-anything-initialize () 
  (dolist (source (plcmp-anything-get-sources))
    (let ((init (assoc-default 'init source)))
      (if init
          (funcall init))))
  (setq plcmp-anything-pattern "")
  (setq plcmp-anything-input "")
  (setq plcmp-anything-candidate-cache nil)
  (setq plcmp-anything-saved-sources nil)
  (setq plcmp-anything-original-source-filter plcmp-anything-source-filter)
  (with-current-buffer (get-buffer-create plcmp-anything-buffer)
    (setq cursor-type nil)
    (setq mode-name "plcmp Anything"))
  (if plcmp-anything-selection-overlay
      (move-overlay plcmp-anything-selection-overlay (point-min) (point-min)
                    (get-buffer plcmp-anything-buffer))
    (setq plcmp-anything-selection-overlay
          (make-overlay (point-min) (point-min) (get-buffer plcmp-anything-buffer)))
    (overlay-put plcmp-anything-selection-overlay 'face 'highlight))
  (if plcmp-anything-enable-digit-shortcuts
      (unless plcmp-anything-digit-overlays
        (dotimes (i 9)
          (push (make-overlay (point-min) (point-min)
                              (get-buffer plcmp-anything-buffer))
                plcmp-anything-digit-overlays)
          (overlay-put (car plcmp-anything-digit-overlays)
                       'before-string (concat (int-to-string (1+ i)) " - ")))
        (setq plcmp-anything-digit-overlays (nreverse plcmp-anything-digit-overlays)))
    (when plcmp-anything-digit-overlays
      (dolist (overlay plcmp-anything-digit-overlays)
        (delete-overlay overlay))
      (setq plcmp-anything-digit-overlays nil))))
(defun plcmp-anything-cleanup () 
  (setq plcmp-anything-source-filter plcmp-anything-original-source-filter)
  (if plcmp-anything-saved-sources
      (setq plcmp-anything-sources plcmp-anything-saved-sources))
  (with-current-buffer plcmp-anything-buffer
    (setq cursor-type t))
  (bury-buffer plcmp-anything-buffer)
  (plcmp-anything-kill-async-processes))
(defun plcmp-anything-previous-line () 
  (interactive)
  (plcmp-anything-move-selection 'line 'previous))
(defun plcmp-anything-next-line () 
  (interactive)
  (plcmp-anything-move-selection 'line 'next))
(defun plcmp-anything-previous-page () 
  (interactive)
  (plcmp-anything-move-selection 'page 'previous))
(defun plcmp-anything-next-page () 
  (interactive)
  (plcmp-anything-move-selection 'page 'next))
(defun plcmp-anything-previous-source () 
  (interactive)
  (plcmp-anything-move-selection 'source 'previous))
(defun plcmp-anything-next-source () 
  (interactive)
  (plcmp-anything-move-selection 'source 'next))
(defun plcmp-anything-move-selection (unit direction) 
  (unless (or (= (buffer-size (get-buffer plcmp-anything-buffer)) 0)
              (not (get-buffer-window plcmp-anything-buffer 'visible)))
    (save-selected-window
      (select-window (get-buffer-window plcmp-anything-buffer 'visible))
      (case unit
        (line (forward-line (case direction
                              (next 1)
                              (previous -1)
                              (t (error "Invalid direction.")))))
        (page (case direction
                (next (condition-case nil
                          (scroll-up)
                        (end-of-buffer (goto-char (point-max)))))
                (previous (condition-case nil
                              (scroll-down)
                            (beginning-of-buffer (goto-char (point-min)))))
                (t (error "Invalid direction."))))
        (source (case direction
                  (next (goto-char (or (plcmp-anything-get-next-header-pos)
                                       (point-min))))
                  (previous (progn
                              (forward-line -1)
                              (if (bobp)
                                  (goto-char (point-max))
                                (if (plcmp-anything-pos-header-line-p)
                                    (forward-line -1)
                                  (forward-line 1)))
                              (goto-char (plcmp-anything-get-previous-header-pos))
                              (forward-line 1)))
                  (t (error "Invalid direction."))))
        (t (error "Invalid unit.")))
      (while (plcmp-anything-pos-header-line-p)
        (forward-line (if (and (eq direction 'previous)
                               (not (eq (line-beginning-position)
                                        (point-min))))
                          -1
                        1)))
      (if (eobp)
          (forward-line -1))
      (plcmp-anything-mark-current-line))))
(defun plcmp-anything-mark-current-line () 
  (move-overlay plcmp-anything-selection-overlay
                (line-beginning-position)
                (1+ (line-end-position))))
(defun plcmp-anything-select-with-digit-shortcut ()
  (interactive)
  (if plcmp-anything-enable-digit-shortcuts
      (let* ((index (- (event-basic-type (elt (this-command-keys-vector) 0)) ?1))
             (overlay (nth index plcmp-anything-digit-overlays)))
        (if (overlay-buffer overlay)
            (save-selected-window
              (select-window (get-buffer-window plcmp-anything-buffer 'visible))
              (goto-char (overlay-start overlay))
              (plcmp-anything-mark-current-line)
              (plcmp-anything-exit-minibuffer))))))
(defun plcmp-anything-exit-minibuffer () 
  (interactive)
  (setq plcmp-anything-iswitchb-candidate-selected (plcmp-anything-get-selection))
  (exit-minibuffer))
(defun plcmp-anything-get-current-source () 
  (with-current-buffer plcmp-anything-buffer
    (goto-char (overlay-start plcmp-anything-selection-overlay))
    (let* ((header-pos (plcmp-anything-get-previous-header-pos))
           (source-name
            (save-excursion
              (assert header-pos)
              (goto-char header-pos)
              (buffer-substring-no-properties
               (line-beginning-position) (line-end-position)))))
      (some (lambda (source)
              (if (equal (assoc-default 'name source)
                         source-name)
                  source))
            (plcmp-anything-get-sources)))))
(defun plcmp-anything-get-next-header-pos () 
  (next-single-property-change (point) 'plcmp-anything-header))
(defun plcmp-anything-get-previous-header-pos () 
  (previous-single-property-change (point) 'plcmp-anything-header))
(defun plcmp-anything-pos-header-line-p () 
  (or (get-text-property (line-beginning-position) 'plcmp-anything-header)
      (get-text-property (line-beginning-position) 'plcmp-anything-header-separator)))
(defun plcmp-anything-get-candidates (source) 
  (let* ((candidate-source (assoc-default 'candidates source))
         (candidates
          (if (functionp candidate-source)
              (funcall candidate-source)
            (if (listp candidate-source)
                candidate-source
              (if (and (symbolp candidate-source)
                       (boundp candidate-source))
                  (symbol-value candidate-source)
                (error (concat "Candidates must either be a function, "
                               " a variable or a list: %s")
                       candidate-source))))))
    (if (processp candidates)
        candidates
      (plcmp-anything-transform-candidates candidates source))))
(defun plcmp-anything-transform-candidates (candidates source) 
  (let* ((transformer (assoc-default 'candidate-transformer source)))
    (if transformer
        (funcall transformer candidates)
      candidates)))
(defun plcmp-anything-get-cached-candidates (source) 
  (let* ((name (assoc-default 'name source))
         (candidate-cache (assoc name plcmp-anything-candidate-cache))
         candidates)
    (if candidate-cache
        (setq candidates (cdr candidate-cache))
      (setq candidates (plcmp-anything-get-candidates source))
      (if (processp candidates)
          (progn
            (push (cons candidates
                        (append source
                                (list (cons 'item-count 0)
                                      (cons 'incomplete-line ""))))
                  plcmp-anything-async-processes)
            (set-process-filter candidates 'plcmp-anything-output-filter)
            (setq candidates nil))
        (unless (assoc 'volatile source)
          (setq candidate-cache (cons name candidates))
          (push candidate-cache plcmp-anything-candidate-cache))))
    candidates))
(defun plcmp-anything-output-filter (process string) 
  (let* ((process-assoc (assoc process plcmp-anything-async-processes))
         (process-info (cdr process-assoc))
         (insertion-marker (assoc-default 'insertion-marker process-info))
         (incomplete-line-info (assoc 'incomplete-line process-info))
         (item-count-info (assoc 'item-count process-info)))
    (with-current-buffer plcmp-anything-buffer
      (save-excursion
        (if insertion-marker
            (goto-char insertion-marker)
          (goto-char (point-max))
          (plcmp-anything-insert-header (assoc-default 'name process-info))
          (setcdr process-assoc
                  (append process-info `((insertion-marker . ,(point-marker))))))
        (let ((lines (split-string string "\n"))
              candidates)
          (while lines
            (if (not (cdr lines))
                (setcdr incomplete-line-info (car lines))
              (if (cdr incomplete-line-info)
                  (progn
                    (push (concat (cdr incomplete-line-info) (car lines))
                          candidates)
                    (setcdr incomplete-line-info nil))
                (push (car lines) candidates)))
            (pop lines))
          (setq candidates (reverse candidates))
          (dolist (candidate (plcmp-anything-transform-candidates candidates process-info))
            (plcmp-anything-insert-match candidate 'insert-before-markers)
            (incf (cdr item-count-info))
            (when (>= (cdr item-count-info) plcmp-anything-candidate-number-limit)
              (plcmp-anything-kill-async-process process)
              (return)))))
      (plcmp-anything-maybe-fit-frame)
      (run-hooks 'plcmp-anything-update-hook)
      (if (bobp)
          (plcmp-anything-next-line)
        (save-selected-window
          (select-window (get-buffer-window plcmp-anything-buffer 'visible))
          (plcmp-anything-mark-current-line))))))
(defun plcmp-anything-kill-async-processes ()
  (dolist (process-info plcmp-anything-async-processes)
    (plcmp-anything-kill-async-process (car process-info)))
  (setq plcmp-anything-async-processes nil))
(defun plcmp-anything-kill-async-process (process) 
  (set-process-filter process nil)
  (delete-process process))
(defun plcmp-anything-insert-header (name) 
  (unless (bobp)
    (let ((start (point)))
      (insert "\n")
      (put-text-property start (point) 'plcmp-anything-header-separator t)))
  (let ((start (point)))
    (insert name)
    (put-text-property (line-beginning-position)
                       (line-end-position) 'plcmp-anything-header t)
    (insert "\n")
    (put-text-property start (point) 'face plcmp-anything-header-face)))
(defun plcmp-anything-set-source-filter (sources) 
  (setq plcmp-anything-source-filter sources)
  (plcmp-anything-update))
(defun plcmp-anything-maybe-fit-frame ()
  (when (and (require 'fit-frame nil t)
             (boundp 'fit-frame-inhibit-fitting-flag)
             (not fit-frame-inhibit-fitting-flag)
             (get-buffer-window plcmp-anything-buffer 'visible))
    (with-selected-window (get-buffer-window plcmp-anything-buffer 'visible)
      (fit-frame nil nil nil t)
      (modify-frame-parameters
       (selected-frame)
       `((left . ,(- (x-display-pixel-width) (+ (frame-pixel-width) 7)))
         (top . 0)))))) 
(defvar plcmp-anything-isearch-original-global-map nil )
(defvar plcmp-anything-isearch-original-message-timeout nil )
(defvar plcmp-anything-isearch-pattern nil )
(defvar plcmp-anything-isearch-message-suffix "" )
(defvar plcmp-anything-isearch-original-point nil )
(defvar plcmp-anything-isearch-original-window nil )
(defvar plcmp-anything-isearch-original-cursor-in-non-selected-windows nil )
(defvar plcmp-anything-isearch-original-post-command-hook nil )
(defvar plcmp-anything-isearch-match-positions nil )
(defvar plcmp-anything-isearch-match-start nil )
(defun plcmp-anything-isearch () 
  (interactive)
  (if (eq (buffer-size (get-buffer plcmp-anything-buffer)) 0)
      (message "There are no results.")
    (setq plcmp-anything-isearch-original-message-timeout minibuffer-message-timeout)
    (setq minibuffer-message-timeout nil)
    (setq plcmp-anything-isearch-original-global-map global-map)
    (condition-case nil
        (progn
          (setq plcmp-anything-isearch-original-window (selected-window))
          (select-window (get-buffer-window plcmp-anything-buffer 'visible))
          (setq cursor-type t)
          (setq plcmp-anything-isearch-original-post-command-hook
                (default-value 'post-command-hook))
          (setq-default post-command-hook nil)
          (add-hook 'post-command-hook 'plcmp-anything-isearch-post-command)
          (use-global-map plcmp-anything-isearch-map)
          (setq overriding-terminal-local-map plcmp-anything-isearch-map)
          (setq plcmp-anything-isearch-pattern "")
          (setq plcmp-anything-isearch-original-cursor-in-non-selected-windows
                cursor-in-non-selected-windows)
          (setq cursor-in-non-selected-windows nil)
          (setq plcmp-anything-isearch-original-point (point-marker))
          (goto-char (point-min))
          (forward-line)
          (plcmp-anything-mark-current-line)
          (setq plcmp-anything-isearch-match-positions nil)
          (setq plcmp-anything-isearch-match-start (point-marker))
          (if plcmp-anything-isearch-overlay
              (move-overlay plcmp-anything-isearch-overlay (point-min) (point-min)
                            (get-buffer plcmp-anything-buffer))
            (setq plcmp-anything-isearch-overlay (make-overlay (point-min) (point-min)))
            (overlay-put plcmp-anything-isearch-overlay 'face plcmp-anything-isearch-match-face))
          (setq plcmp-anything-isearch-message-suffix
                (substitute-command-keys "cancel with \\[plcmp-anything-isearch-cancel]")))
      (error (plcmp-anything-isearch-cleanup)))))
(defun plcmp-anything-isearch-post-command () 
  (plcmp-anything-isearch-message)
  (when (get-buffer-window plcmp-anything-buffer 'visible)
    (with-selected-window (get-buffer-window plcmp-anything-buffer 'visible)
      (move-overlay plcmp-anything-isearch-overlay plcmp-anything-isearch-match-start (point)
                    (get-buffer plcmp-anything-buffer)))))
(defun plcmp-anything-isearch-printing-char () 
  (interactive)
  (let ((char (char-to-string last-command-char)))
    (setq plcmp-anything-isearch-pattern (concat plcmp-anything-isearch-pattern char))
    (with-selected-window (get-buffer-window plcmp-anything-buffer 'visible)
      (if (looking-at char)
          (progn
            (push (list 'event 'char
                        'start plcmp-anything-isearch-match-start
                        'pos (point-marker))
                  plcmp-anything-isearch-match-positions)
            (forward-char))
        (let ((start (point)))
          (while (and (re-search-forward plcmp-anything-isearch-pattern nil t)
                      (plcmp-anything-pos-header-line-p)))
          (if (or (plcmp-anything-pos-header-line-p)
                  (eq start (point)))
              (progn
                (goto-char start)
                (push (list 'event 'error
                            'start plcmp-anything-isearch-match-start
                            'pos (point-marker))
                      plcmp-anything-isearch-match-positions))
            (push (list 'event 'search
                        'start plcmp-anything-isearch-match-start
                        'pos (copy-marker start))
                  plcmp-anything-isearch-match-positions)
            (setq plcmp-anything-isearch-match-start (copy-marker (match-beginning 0))))))
      (plcmp-anything-mark-current-line))))
(defun plcmp-anything-isearch-again () 
  (interactive)
  (if (equal plcmp-anything-isearch-pattern "")
      (setq plcmp-anything-isearch-message-suffix "no pattern yet")
    (with-selected-window (get-buffer-window plcmp-anything-buffer 'visible)
      (let ((start (point)))
        (while (and (re-search-forward plcmp-anything-isearch-pattern nil t)
                    (plcmp-anything-pos-header-line-p)))
        (if (or (plcmp-anything-pos-header-line-p)
                (eq start (point)))
            (progn
              (goto-char start)
              (unless (eq 'error (plist-get (car plcmp-anything-isearch-match-positions)
                                            'event))
                (setq plcmp-anything-isearch-message-suffix "no more matches")))
          (push (list 'event 'search-again
                      'start plcmp-anything-isearch-match-start
                      'pos (copy-marker start))
                plcmp-anything-isearch-match-positions)
          (setq plcmp-anything-isearch-match-start (copy-marker (match-beginning 0)))
          (plcmp-anything-mark-current-line))))))
(defun plcmp-anything-isearch-delete () 
  (interactive)
  (unless (equal plcmp-anything-isearch-pattern "")
    (let ((last (pop plcmp-anything-isearch-match-positions)))
      (unless (eq 'search-again (plist-get last 'event))
        (setq plcmp-anything-isearch-pattern
              (substring plcmp-anything-isearch-pattern 0 -1)))
      (with-selected-window (get-buffer-window plcmp-anything-buffer 'visible)
        (goto-char (plist-get last 'pos))
        (setq plcmp-anything-isearch-match-start (plist-get last 'start))
        (plcmp-anything-mark-current-line)))))
(defun plcmp-anything-isearch-default-action () 
  (interactive)
  (plcmp-anything-isearch-cleanup)
  (with-current-buffer plcmp-anything-buffer (plcmp-anything-exit-minibuffer)))
(defun plcmp-anything-isearch-select-action () 
  (interactive)
  (plcmp-anything-isearch-cleanup)
  (with-selected-window (get-buffer-window plcmp-anything-buffer 'visible)
    (plcmp-anything-select-action)))
(defun plcmp-anything-isearch-cancel () 
  (interactive)
  (plcmp-anything-isearch-cleanup)
  (when (get-buffer-window plcmp-anything-buffer 'visible)
    (with-selected-window (get-buffer-window plcmp-anything-buffer 'visible)
      (goto-char plcmp-anything-isearch-original-point)
      (plcmp-anything-mark-current-line))))
(defun plcmp-anything-isearch-cleanup () 
  (setq minibuffer-message-timeout plcmp-anything-isearch-original-message-timeout)
  (with-current-buffer plcmp-anything-buffer
    (setq overriding-terminal-local-map nil)
    (setq cursor-type nil)
    (setq cursor-in-non-selected-windows
          plcmp-anything-isearch-original-cursor-in-non-selected-windows))
  (when plcmp-anything-isearch-original-window
    (select-window plcmp-anything-isearch-original-window))
  (use-global-map plcmp-anything-isearch-original-global-map)
  (setq-default post-command-hook plcmp-anything-isearch-original-post-command-hook)
  (when (overlayp plcmp-anything-isearch-overlay)
    (delete-overlay plcmp-anything-isearch-overlay)))
(defun plcmp-anything-isearch-message () 
  (if (and (equal plcmp-anything-isearch-message-suffix "")
           (eq (plist-get (car plcmp-anything-isearch-match-positions) 'event)
               'error))
      (setq plcmp-anything-isearch-message-suffix "failing"))
  (unless (equal plcmp-anything-isearch-message-suffix "")
    (setq plcmp-anything-isearch-message-suffix
          (concat " [" plcmp-anything-isearch-message-suffix "]")))
  (message (concat "Search within results: "
                   plcmp-anything-isearch-pattern
                   plcmp-anything-isearch-message-suffix))
  (setq plcmp-anything-isearch-message-suffix ""))
(defvar plcmp-anything-iswitchb-candidate-selected nil )
(defvar plcmp-anything-iswitchb-frame-configuration nil )
(defvar plcmp-anything-iswitchb-saved-keys nil )
(defun plcmp-anything-iswitchb-setup () 
  (interactive)
  (require 'iswitchb)
  (put 'iswitchb-buffer 'timid-completion 'disabled)
  (add-hook 'minibuffer-setup-hook  'plcmp-anything-iswitchb-minibuffer-setup)
  (defadvice iswitchb-visit-buffer
    (around plcmp-anything-iswitchb-visit-buffer activate)
    (if plcmp-anything-iswitchb-candidate-selected
        (plcmp-anything-execute-selection-action)
      ad-do-it))
  (defadvice iswitchb-possible-new-buffer
    (around plcmp-anything-iswitchb-possible-new-buffer activate)
    (if plcmp-anything-iswitchb-candidate-selected
        (plcmp-anything-execute-selection-action)
      ad-do-it))
  (message "Iswitchb integration is activated."))
(defun plcmp-anything-iswitchb-minibuffer-setup ()
  (when (eq this-command 'iswitchb-buffer)
    (add-hook 'minibuffer-exit-hook  'plcmp-anything-iswitchb-minibuffer-exit)
    (setq plcmp-anything-iswitchb-frame-configuration nil)
    (setq plcmp-anything-iswitchb-candidate-selected nil)
    (add-hook 'plcmp-anything-update-hook 'plcmp-anything-iswitchb-handle-update)
    (plcmp-anything-initialize)
    (add-hook 'post-command-hook 'plcmp-anything-iswitchb-check-input)))
(defun plcmp-anything-iswitchb-minibuffer-exit ()
  (remove-hook 'minibuffer-exit-hook  'plcmp-anything-iswitchb-minibuffer-exit)
  (remove-hook 'post-command-hook 'plcmp-anything-iswitchb-check-input)
  (remove-hook 'plcmp-anything-update-hook 'plcmp-anything-iswitchb-handle-update)
  (plcmp-anything-cleanup)
  (when plcmp-anything-iswitchb-frame-configuration
    (set-frame-configuration plcmp-anything-iswitchb-frame-configuration)
    (setq plcmp-anything-iswitchb-frame-configuration nil)))
(defun plcmp-anything-iswitchb-check-input () 
  (if (or plcmp-anything-iswitchb-frame-configuration
          (sit-for plcmp-anything-iswitchb-idle-delay))
      (plcmp-anything-check-new-input iswitchb-text)))
(defun plcmp-anything-iswitchb-handle-update () 
  (unless (or (equal (buffer-size (get-buffer plcmp-anything-buffer)) 0)
              plcmp-anything-iswitchb-frame-configuration)
    (setq plcmp-anything-iswitchb-frame-configuration (current-frame-configuration))
    (save-selected-window
      (if (not plcmp-anything-samewindow)
          (pop-to-buffer plcmp-anything-buffer)
        (select-window (get-lru-fwindow))
        (switch-to-buffer plcmp-anything-buffer)))
    (with-current-buffer (window-buffer (active-minibuffer-window))
      (let* ((plcmp-anything-prefix "plcmp-anything-")
             (prefix-length (length plcmp-anything-prefix))
             (commands
              (delete-dups
               (remove-if 'null
                          (mapcar
                           (lambda (binding)
                             (let ((command (cdr binding)))
                               (when (and (symbolp command)
                                          (eq (compare-strings
                                               plcmp-anything-prefix
                                               0 prefix-length
                                               (symbol-name command)
                                               0 prefix-length)
                                              t))
                                 command)))
                           (cdr plcmp-anything-map)))))
             (bindings (mapcar (lambda (command)
                                 (cons command
                                       (where-is-internal command plcmp-anything-map)))
                               commands)))
        (push (list 'plcmp-anything-iswitchb-cancel-anything (kbd "<ESC>"))
              bindings)
        (setq plcmp-anything-iswitchb-saved-keys nil)
        (let* ((iswitchb-prefix "iswitchb-")
               (prefix-length (length iswitchb-prefix)))
          (dolist (binding bindings)
            (dolist (key (cdr binding))
              (let ((old-command (lookup-key (current-local-map) key)))
                (unless (and plcmp-anything-iswitchb-dont-touch-iswithcb-keys
                             (symbolp old-command)
                             (eq (compare-strings iswitchb-prefix
                                                  0 prefix-length
                                                  (symbol-name old-command)
                                                  0 prefix-length)
                                 t))
                  (push (cons key old-command)
                        plcmp-anything-iswitchb-saved-keys)
                  (define-key (current-local-map) key (car binding)))))))))))
(defun plcmp-anything-iswitchb-cancel-anything () 
  (interactive)
  (save-excursion
    (dolist (binding plcmp-anything-iswitchb-saved-keys)
      (define-key (current-local-map) (car binding) (cdr binding)))
    (plcmp-anything-iswitchb-minibuffer-exit)))
(unless (fboundp 'assoc-default)
  (defun assoc-default (key alist &optional test default) 
    (let (found (tail alist) value)
      (while (and tail (not found))
        (let ((elt (car tail)))
          (when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
            (setq found t value (if (consp elt) (cdr elt) default))))
        (setq tail (cdr tail)))
      value)))
(unless (fboundp 'minibuffer-contents)
  (defun minibuffer-contents () 
    (field-string (point-max)))
  (defun delete-minibuffer-contents  () 
    (delete-field (point-max))))

;;; compatibility anything-execute-persistent-action
;;; written by rubikitch see http://d.hatena.ne.jp/rubikitch/20071228/anythingpersistent (japanese)
(defun plcmp-anything-execute-persistent-action ()
  "If a candidate was selected then perform the associated action without quitting anything."
  (interactive)
  (save-selected-window
    (select-window (get-buffer-window plcmp-anything-buffer))
    (select-window (setq minibuffer-scroll-window
                         (if (one-window-p t) (split-window) (next-window (selected-window) 1))))
    (let* ((plcmp-anything-window (get-buffer-window plcmp-anything-buffer))
           (selection (if plcmp-anything-saved-sources
                          ;; the action list is shown
                          plcmp-anything-saved-selection
                        (plcmp-anything-get-selection)))
           (default-action (plcmp-anything-get-action))
           (action (assoc-default 'persistent-action (plcmp-anything-get-current-source))))
      (setq action (or action default-action))
      (if (and (listp action)
               (not (functionp action))) ; lambda
          ;; select the default action
          (setq action (cdar action)))
      (set-window-dedicated-p plcmp-anything-window t)
      (unwind-protect
          (and action selection (funcall action selection))
        (set-window-dedicated-p plcmp-anything-window nil)))))

;;; perl-completion.el ends here