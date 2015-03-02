;;; xah-clojure-mode.el --- Major mode for editing clojure. -*- coding: utf-8 -*-

;; Copyright © 2014 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2014-10-31
;; Keywords: languages, convenience

;; LICENSE:
;; paypal me $5
;; Buy Xah Emacs Tutorial
;; Xah Emacs Tutorial
;; http://ergoemacs.org/emacs/emacs.html
;; Xah Emacs Lisp Tutorial
;; http://ergoemacs.org/emacs/elisp.html

;;; Commentary:
;; Major mode for editing clojure.
;; See: http://ergoemacs.org/emacs/xah-clojure-mode.html
   
;;; History:
;; version 0.1, 2014-10-31 first version

;;; Code:

(require 'lisp-mode)

(defvar xah-clojure-mode-hook nil "Standard hook for `xah-clojure-mode'")

(defvar xcj-clojure-basic-words nil "list of clojure words.")
(setq xcj-clojure-basic-words
      '(

"="
"<"
">"
"+"

 "do" "if" "let" "var" "fn" "loop" "recur" "throw" "try" "catch"
 "finally" "set!" "new" "monitor-enter" "monitor-exit" "quote"

"defstruct" "deftype" "defprotocol" "defrecord"

"letfn" "case" "cond" "cond->" "cond->>" "condp" "for" "when"
"when-not" "when-let" "when-first" "when-some" "if-let" "if-not"
"if-some" ".." "->" "->>" "doto" "and" "or" "dosync" "doseq" "dotimes"
"dorun" "doall" "load" "import" "unimport" "ns" "in-ns" "refer"
"with-open" "with-local-vars" "binding" "gen-class"
"gen-and-load-class" "gen-and-save-class" "handler-case" "handle"
"declare"

"*1" "*2" "*3" "*agent*" "*allow-unresolved-vars*" "*assert*"
"*clojure-version*" "*command-line-args*" "*compile-files*"
"*compile-path*" "*e" "*err*" "*file*" "*flush-on-newline*" "*in*"
"*macro-meta*" "*math-context*" "*ns*" "*out*" "*print-dup*"
"*print-length*" "*print-level*" "*print-meta*" "*print-readably*"
"*read-eval*" "*source-path*" "*use-context-classloader*"
"*warn-on-reflection*"

"true" "false" "nil"

        ))

(defvar xcj-clojure.core-words nil "list of clojure.core words.")
(setq xcj-clojure.core-words
      '(

"&" "*" "*'" "*1" "*2" "*3" "*agent*" "*allow-unresolved-vars*"
"*assert*" "*clojure-version*" "*command-line-args*" "*compile-files*"
"*compile-path*" "*compiler-options*" "*data-readers*"
"*default-data-reader-fn*" "*e" "*err*" "*file*" "*flush-on-newline*"
"*fn-loader*" "*in*" "*math-context*" "*ns*" "*out*" "*print-dup*"
"*print-length*" "*print-level*" "*print-meta*" "*print-readably*"
"*read-eval*" "*source-path*" "*unchecked-math*"
"*use-context-classloader*" "*verbose-defrecords*"
"*warn-on-reflection*" "+" "+'" "-" "-'" "->" "->>" "->ArrayChunk"
"->Vec" "->VecNode" "->VecSeq" "-cache-protocol-fn" "-reset-methods"
"." ".." "/" "<" "<=" "=" "==" ">" ">=" "ArrayChunk" "EMPTY-NODE"
"Vec" "VecNode" "VecSeq" "accessor" "aclone" "add-classpath"
"add-watch" "agent" "agent-error" "agent-errors" "aget" "alength"
"alias" "all-ns" "alter" "alter-meta!" "alter-var-root" "amap"
"ancestors" "and" "apply" "areduce" "array-map" "as->" "aset"
"aset-boolean" "aset-byte" "aset-char" "aset-double" "aset-float"
"aset-int" "aset-long" "aset-short" "assert" "assoc!" "assoc"
"assoc-in" "associative?" "atom" "await" "await-for" "await1" "bases"
"bean" "bigdec" "bigint" "biginteger" "binding" "bit-and"
"bit-and-not" "bit-clear" "bit-flip" "bit-not" "bit-or" "bit-set"
"bit-shift-left" "bit-shift-right" "bit-test" "bit-xor" "boolean"
"boolean-array" "booleans" "bound-fn" "bound-fn*" "bound?" "butlast"
"byte" "byte-array" "bytes" "case" "cast" "catch" "char" "char-array"
"char-escape-string" "char-name-string" "char?" "chars" "chunk"
"chunk-append" "chunk-buffer" "chunk-cons" "chunk-first" "chunk-next"
"chunk-rest" "chunked-seq?" "class" "class?" "clear-agent-errors"
"clojure-version" "coll?" "comment" "commute" "comp" "comparator"
"compare" "compare-and-set!" "compile" "complement" "concat" "cond"
"cond->" "cond->>" "condp" "conj!" "conj" "cons" "constantly"
"construct-proxy" "contains?" "count" "counted?" "create-ns"
"create-struct" "cycle" "dec" "dec'" "decimal?" "declare" "def"
"default-data-readers" "definline" "definterface" "defmacro"
"defmethod" "defmulti" "defn" "defn-" "defonce" "defprotocol"
"defrecord" "defstruct" "deftype" "delay" "delay?" "deliver"
"denominator" "deref" "derive" "descendants" "destructure" "disj!"
"disj" "dissoc!" "dissoc" "distinct" "distinct?" "do" "doall" "dorun"
"doseq" "dosync" "dotimes" "doto" "double" "double-array" "doubles"
"drop" "drop-last" "drop-while" "empty" "empty?" "ensure"
"enumeration-seq" "error-handler" "error-mode" "eval" "even?"
"every-pred" "every?" "ex-data" "ex-info" "extend" "extend-protocol"
"extend-type" "extenders" "extends?" "false?" "ffirst" "file-seq"
"filter" "filterv" "finally" "find" "find-keyword" "find-ns"
"find-protocol-impl" "find-protocol-method" "find-var" "first"
"flatten" "float" "float-array" "float?" "floats" "flush" "fn" "fn?"
"fnext" "fnil" "for" "force" "format" "frequencies" "future"
"future-call" "future-cancel" "future-cancelled?" "future-done?"
"future?" "gen-class" "gen-interface" "gensym" "get" "get-in"
"get-method" "get-proxy-class" "get-thread-bindings" "get-validator"
"group-by" "hash" "hash-combine" "hash-map" "hash-ordered-coll"
"hash-set" "hash-unordered-coll" "identical?" "identity" "if" "if-let"
"if-not" "if-some" "ifn?" "import" "in-ns" "inc" "inc'" "init-proxy"
"instance?" "int" "int-array" "integer?" "interleave" "intern"
"interpose" "into" "into-array" "ints" "io!" "isa?" "iterate"
"iterator-seq" "juxt" "keep" "keep-indexed" "key" "keys" "keyword"
"keyword?" "last" "lazy-cat" "lazy-seq" "let" "letfn" "line-seq"
"list" "list*" "list?" "load" "load-file" "load-reader" "load-string"
"loaded-libs" "locking" "long" "long-array" "longs" "loop"
"macroexpand" "macroexpand-1" "make-array" "make-hierarchy" "map"
"map-indexed" "map?" "mapcat" "mapv" "max" "max-key" "memfn" "memoize"
"merge" "merge-with" "meta" "method-sig" "methods" "min" "min-key"
"mix-collection-hash" "mod" "monitor-enter" "monitor-exit" "munge"
"name" "namespace" "namespace-munge" "neg?" "new" "newline" "next"
"nfirst" "nil?" "nnext" "not" "not-any?" "not-empty" "not-every?"
"not=" "ns" "ns-aliases" "ns-imports" "ns-interns" "ns-map" "ns-name"
"ns-publics" "ns-refers" "ns-resolve" "ns-unalias" "ns-unmap" "nth"
"nthnext" "nthrest" "num" "number?" "numerator" "object-array" "odd?"
"or" "parents" "partial" "partition" "partition-all" "partition-by"
"pcalls" "peek" "persistent!" "pmap" "pop!" "pop"
"pop-thread-bindings" "pos?" "pr" "pr-str" "prefer-method" "prefers"
"primitives-classnames" "print" "print-ctor" "print-dup"
"print-method" "print-simple" "print-str" "printf" "println"
"println-str" "prn" "prn-str" "promise" "proxy"
"proxy-call-with-super" "proxy-mappings" "proxy-name" "proxy-super"
"push-thread-bindings" "pvalues" "quot" "quote" "rand" "rand-int"
"rand-nth" "range" "ratio?" "rational?" "rationalize" "re-find"
"re-groups" "re-matcher" "re-matches" "re-pattern" "re-seq" "read"
"read-line" "read-string" "realized?" "record?" "recur" "reduce"
"reduce-kv" "reduced" "reduced?" "reductions" "ref"
"ref-history-count" "ref-max-history" "ref-min-history" "ref-set"
"refer" "refer-clojure" "reify" "release-pending-sends" "rem" "remove"
"remove-all-methods" "remove-method" "remove-ns" "remove-watch"
"repeat" "repeatedly" "replace" "replicate" "require" "reset!"
"reset-meta!" "resolve" "rest" "restart-agent" "resultset-seq"
"reverse" "reversible?" "rseq" "rsubseq" "satisfies?" "second"
"select-keys" "send" "send-off" "send-via" "seq" "seq?" "seque"
"sequence" "sequential?" "set!" "set" "set-agent-send-executor!"
"set-agent-send-off-executor!" "set-error-handler!" "set-error-mode!"
"set-validator!" "set?" "short" "short-array" "shorts" "shuffle"
"shutdown-agents" "slurp" "some" "some->" "some->>" "some-fn" "some?"
"sort" "sort-by" "sorted-map" "sorted-map-by" "sorted-set"
"sorted-set-by" "sorted?" "special-symbol?" "spit" "split-at"
"split-with" "str" "string?" "struct" "struct-map" "subs" "subseq"
"subvec" "supers" "swap!" "symbol" "symbol?" "sync" "take" "take-last"
"take-nth" "take-while" "test" "the-ns" "thread-bound?" "throw" "time"
"to-array" "to-array-2d" "trampoline" "transient" "tree-seq" "true?"
"try" "type" "unchecked-add" "unchecked-add-int" "unchecked-byte"
"unchecked-char" "unchecked-dec" "unchecked-dec-int"
"unchecked-divide-int" "unchecked-double" "unchecked-float"
"unchecked-inc" "unchecked-inc-int" "unchecked-int" "unchecked-long"
"unchecked-multiply" "unchecked-multiply-int" "unchecked-negate"
"unchecked-negate-int" "unchecked-remainder-int" "unchecked-short"
"unchecked-subtract" "unchecked-subtract-int" "underive" "unquote"
"unquote-splicing" "unsigned-bit-shift-right" "update-in"
"update-proxy" "use" "val" "vals" "var" "var-get" "var-set" "var?"
"vary-meta" "vec" "vector" "vector-of" "vector?" "when" "when-first"
"when-let" "when-not" "when-some" "while" "with-bindings"
"with-bindings*" "with-in-str" "with-loading-context"
"with-local-vars" "with-meta" "with-open" "with-out-str"
"with-precision" "with-redefs" "with-redefs-fn" "xml-seq" "zero?"
"zipmap"

        ))

(defvar xcj-clojure.data-words nil "list of clojure.core words.")
(setq xcj-clojure.data-words
      '(

"diff"
"Diff"
"diff-similar"
"equality-partition"
"EqualityPartition"

        ))

(defvar xcj-clojure.edn-words nil "list of clojure.core words.")
(setq xcj-clojure.edn-words
      '(
"read"
"read-string"

        ))

(defvar xcj-clojure.inspector-words nil "list of clojure.core words.")
(setq xcj-clojure.inspector-words
      '(

"inspect"
"inspect-table"
"inspect-tree"

        ))

(defvar xcj-clojure.instant-words nil "list of clojure.core words.")
(setq xcj-clojure.instant-words
      '(
"parse-timestamp"
"read-instant-calendar"
"read-instant-date"
"read-instant-timestamp"
"validated"

        ))

(defvar xcj-clojure.java.browse-words nil "list of clojure.core words.")
(setq xcj-clojure.java.browse-words
      '(
"browse-url"
        ))

(defvar xcj-clojure.java.io-words nil "list of clojure.core words.")
(setq xcj-clojure.java.io-words
      '(
"as-file"
"as-relative-path"
"as-url"
"Coercions"
"copy"
"delete-file"
"file"
"input-stream"
"IOFactory"
"make-input-stream"
"make-output-stream"
"make-parents"
"make-reader"
"make-writer"
"output-stream"
"reader"
"resource"
"writer"

        ))

(defvar xcj-clojure.java.javadoc-words nil "list of clojure.core words.")
(setq xcj-clojure.java.javadoc-words
      '(
"add-local-javadoc"
"add-remote-javadoc"
"javadoc"

        ))

(defvar xcj-clojure.java.shell-words nil "list of clojure.core words.")
(setq xcj-clojure.java.shell-words
      '(
  "sh"
"with-sh-dir"
"with-sh-env"

        ))

(defvar xcj-clojure.main-words nil "list of clojure.core words.")
(setq xcj-clojure.main-words
      '(
"demunge"
"load-script"
"main"
"repl"
"repl-caught"
"repl-exception"
"repl-prompt"
"repl-read"
"repl-requires"
"root-cause"
"skip-if-eol"
"skip-whitespace"
"stack-element-str"
"with-bindings"
"with-read-known"

        ))

(defvar xcj-clojure.pprint-words nil "list of clojure.core words.")
(setq xcj-clojure.pprint-words
      '(

"*print-base*"
"*print-miser-width*"
"*print-pprint-dispatch*"
"*print-pretty*"
"*print-radix*"
"*print-right-margin*"
"*print-suppress-namespaces*"
"cl-format"
"code-dispatch"
"formatter"
"formatter-out"
"fresh-line"
"get-pretty-writer"
"pp"
"pprint"
"pprint-indent"
"pprint-logical-block"
"pprint-newline"
"pprint-tab"
"print-length-loop"
"print-table"
"set-pprint-dispatch"
"simple-dispatch"
"with-pprint-dispatch"
"write"
"write-out"

        ))

(defvar xcj-clojure.reflect-words nil "list of clojure.core words.")
(setq xcj-clojure.reflect-words
      '(
"->AsmReflector"
"->Constructor"
"->Field"
"->JavaReflector"
"->Method"
"AsmReflector"
"ClassResolver"
"Constructor"
"do-reflect"
"Field"
"flag-descriptors"
"JavaReflector"
"map->Constructor"
"map->Field"
"map->Method"
"Method"
"reflect"
"Reflector"
"resolve-class"
"type-reflect"
"typename"
"TypeReference"

        ))

(defvar xcj-clojure.repl-words nil "list of clojure.core words.")
(setq xcj-clojure.repl-words
      '(
"apropos"
"demunge"
"dir"
"dir-fn"
"doc"
"find-doc"
"pst"
"root-cause"
"set-break-handler!"
"source"
"source-fn"
"stack-element-str"
"thread-stopper"

        ))

(defvar xcj-clojure.set-words nil "list of clojure.core words.")
(setq xcj-clojure.set-words
      '(
"difference"
"index"
"intersection"
"join"
"map-invert"
"project"
"rename"
"rename-keys"
"select"
"subset?"
"superset?"
"union"

        ))

(defvar xcj-clojure.stacktrace-words nil "list of clojure.core words.")
(setq xcj-clojure.stacktrace-words
      '(
"e"
"print-cause-trace"
"print-stack-trace"
"print-throwable"
"print-trace-element"
"root-cause"
        ))

(defvar xcj-clojure.string-words nil "list of clojure.core words.")
(setq xcj-clojure.string-words
      '(
"blank?"
"capitalize"
"escape"
"join"
"lower-case"
"re-quote-replacement"
"replace"
"replace-first"
"reverse"
"split"
"split-lines"
"trim"
"trim-newline"
"triml"
"trimr"
"upper-case"
        ))

(defvar xcj-clojure.template-words nil "list of clojure.core words.")
(setq xcj-clojure.template-words
      '(
"apply-template"
"do-template"
        ))

(defvar xcj-clojure.test-words nil "list of clojure.core words.")
(setq xcj-clojure.test-words
      '(
"*load-tests*"
"*stack-trace-depth*"
"are"
"assert-any"
"assert-predicate"
"compose-fixtures"
"deftest"
"deftest-"
"do-report"
"file-position"
"function?"
"get-possibly-unbound-var"
"inc-report-counter"
"is"
"join-fixtures"
"report"
"run-all-tests"
"run-tests"
"set-test"
"successful?"
"test-all-vars"
"test-ns"
"test-var"
"test-vars"
"testing"
"testing-contexts-str"
"testing-vars-str"
"try-expr"
"use-fixtures"
"with-test"
"with-test-out"
        ))

(defvar xcj-clojure.walk-words nil "list of clojure.core words.")
(setq xcj-clojure.walk-words
      '(
"keywordize-keys"
"macroexpand-all"
"postwalk"
"postwalk-demo"
"postwalk-replace"
"prewalk"
"prewalk-demo"
"prewalk-replace"
"stringify-keys"
"walk"
        ))

(defvar xcj-clojure.xml-words nil "list of clojure.core words.")
(setq xcj-clojure.xml-words
      '(
"parse"
        ))

(defvar xcj-clojure.zip-words nil "list of clojure.core words.")
(setq xcj-clojure.zip-words
      '(
"append-child"
"branch?"
"children"
"down"
"edit"
"end?"
"insert-child"
"insert-left"
"insert-right"
"left"
"leftmost"
"lefts"
"make-node"
"next"
"node"
"path"
"prev"
"remove"
"replace"
"right"
"rightmost"
"rights"
"root"
"seq-zip"
"up"
"vector-zip"
"xml-zip"
"zipper"
        ))

(defvar xcj-clojure-fun-words nil "list of clojure.core words.")
(setq xcj-clojure-fun-words
      '(

;; clojure.inspector
        "atom?"
        "collection-tag"
        "get-child"
        "get-child-count"
        "inspect"
        "inspect-table"
        "inspect-tree"
        "is-leaf"
        "list-model"
        "list-provider"

;; clojure.main
        "load-script"
        "main"
        "repl"
        "repl-caught"
        "repl-exception"
        "repl-prompt"
        "repl-read"
        "skip-if-eol"
        "skip-whitespace"
        "with-bindings"

;; clojure.set
        "difference"
        "index"
        "intersection"
        "join"
        "map-invert"
        "project"
        "rename"
        "rename-keys"
        "select"
        "union"

;; clojure.stacktrace
        "e"
        "print-cause-trace"
        "print-stack-trace"
        "print-throwable"
        "print-trace-element"

;; clojure.template
        "do-template"
        "apply-template"

;; clojure.test
        "are"
        "assert-any"
        "assert-expr"
        "assert-predicate"
        "compose-fixtures"
        "deftest"
        "deftest-"
        "file-position"
        "function?"
        "get-possibly-unbound-var"
        "inc-report-counter"
        "is"
        "join-fixtures"
        "report"
        "run-all-tests"
        "run-tests"
        "set-test"
        "successful?"
        "test-all-vars"
        "test-ns"
        "test-var"
        "test-vars"
        "testing"
        "testing-contexts-str"
        "testing-vars-str"
        "try-expr"
        "use-fixtures"
        "with-test"
        "with-test-out"

;; clojure.walk
        "keywordize-keys"
        "macroexpand-all"
        "postwalk"
        "postwalk-demo"
        "postwalk-replace"
        "prewalk"
        "prewalk-demo"
        "prewalk-replace"
        "stringify-keys"
        "walk"

;; clojure.xml
        "attrs"
        "content"
        "content-handler"
        "element"
        "emit"
        "emit-element"

;; clojure.zip
        "append-child"
        "branch?"
        "children"
        "down"
        "edit"
        "end?"
        "insert-child"
        "insert-left"
        "insert-right"
        "left"
        "leftmost"
        "lefts"
        "make-node"
        "next"
        "node"
        "path"
        "prev"
        "remove"
        "replace"
        "right"
        "rightmost"
        "rights"
        "root"
        "seq-zip"
        "up"
        ))

(defvar xcj-clojure-all-keywords nil "list of all clojure keywords")
(setq xcj-clojure-all-keywords
(append
xcj-clojure-fun-words
xcj-clojure-basic-words
xcj-clojure-basic-words
xcj-clojure.core-words
xcj-clojure.data-words
xcj-clojure.edn-words
xcj-clojure.inspector-words
xcj-clojure.instant-words
xcj-clojure.java.browse-words
xcj-clojure.java.io-words
xcj-clojure.java.javadoc-words
xcj-clojure.java.shell-words
xcj-clojure.main-words
xcj-clojure.pprint-words
xcj-clojure.reflect-words
xcj-clojure.repl-words
xcj-clojure.set-words
xcj-clojure.stacktrace-words
xcj-clojure.string-words
xcj-clojure.template-words
xcj-clojure.test-words
xcj-clojure.walk-words
xcj-clojure.xml-words
xcj-clojure.zip-words
xcj-clojure-fun-words

))



;; emacs 24.4 or 24.3 change fix

(defun xcj-up-list (arg1 &optional arg2 arg3)
  "Backward compatibility fix for emacs 24.4's up-list.
emacs 24.4 changed up-list to take up to 3 args. Before, only 1.
See
 `backward-up-list',
 `up-list'"
  (interactive)
  (if (>= emacs-major-version 25)
      (up-list arg1 arg2 arg3)
    (up-list arg1)))


;; completion

(defun xcj-complete-symbol ()
  "Perform keyword completion on current word.
This uses `ido-mode' user interface for completion."
  (interactive)
  (let* (
         (ξbds (bounds-of-thing-at-point 'symbol))
         (ξp1 (car ξbds))
         (ξp2 (cdr ξbds))
         (ξcurrent-sym
          (if  (or (null ξp1) (null ξp2) (equal ξp1 ξp2))
              ""
            (buffer-substring-no-properties ξp1 ξp2)))
         ξresult-sym)
    (when (not ξcurrent-sym) (setq ξcurrent-sym ""))
    (setq ξresult-sym
          (ido-completing-read "" xcj-clojure-all-keywords nil nil ξcurrent-sym ))
    (delete-region ξp1 ξp2)
    (insert ξresult-sym)

    ;; use case of completion

    (when (not (xcj-start-with-left-paren-p))
      (let ( (ξabbrev-expanded-p (xcj-expand-abbrev)))
        ;; (when (not (xcj-start-with-left-paren-p)) (xcj-add-paren-around-symbol))
))))

(defun xcj-start-with-left-paren-p ()
  "true or false"
  (interactive)
  (save-excursion
    (forward-symbol -1) (backward-char 1)
    (if (looking-at "(")
      t
      nil)))

(defun xcj-add-paren-around-symbol ()
  "add paren around symbol before cursor and add a space before closing paren, place cursor ther.
 ⁖
 do-something▮
becomes
 (do-something ▮)
"
  (interactive)
  (forward-symbol -1) (insert "(") (forward-symbol 1) (insert " )")
  (backward-char 1))

(defun xcj-remove-paren-pair ()
  "Remove closest outer paren around cursor or remove string quote and activate the region.
Cursor is moved to the left deleted paren spot, mark is set to the right deleted paren spot.
Call `exchange-point-and-mark' to highlight them.
“closest outer paren” is based on left side of cursor.
"
  (interactive)
  (let ((pos (point))
        p1 p2
        )
    (atomic-change-group
      (xcj-up-list -1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING")
      (while (not (char-equal (char-after) ?\( ))
        (xcj-up-list -1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING"))
      (setq p1 (point))
      (forward-sexp)
      (setq p2 (point))
      (delete-char -1)
      (push-mark (point) t t)
      (goto-char p1)
      (delete-char 1))))

(defun xcj-expand-abbrev-maybe (&optional φexpand-func)
  "Expand clojure function name before cursor into template.
Right now, always expand.
Used to be: Don't expand when in string or comment.
Returns true if there's a expansion, else false."
  (interactive)
  (let (
        ξp1 ξp2
        ξab-str
        (ξsyntax-state (syntax-ppss)))
    ;; (if (or (nth 3 ξsyntax-state) (nth 4 ξsyntax-state))
    ;;     nil
    ;;   (xcj-expand-abbrev))
    (xcj-expand-abbrev)
    ))

(put 'xcj-expand-abbrev-maybe 'no-self-insert t)

(defun xcj-expand-abbrev ()
  "Expand the symbol before cursor.
Returns true if there's a expansion, else false."
  (interactive)
  (let (
        ξp1 ξp2
        ξab-str
        )
    (save-excursion
      (forward-symbol -1)
      (setq ξp1 (point))
      (forward-symbol 1)
      (setq ξp2 (point)))
    (setq ξab-str (buffer-substring-no-properties ξp1 ξp2))
    (if (abbrev-symbol ξab-str)
        (progn
          (abbrev-insert (abbrev-symbol ξab-str) ξab-str ξp1 ξp2 )
          (xcj--abbrev-position-cursor ξp1)
          t)
      nil)))

(defun xcj-abbrev-enable-function ()
  "Determine whether to expand abbrev.
This is called by emacs abbrev system."
  (let ((ξsyntax-state (syntax-ppss)))
    (if (or (nth 3 ξsyntax-state) (nth 4 ξsyntax-state))
        (progn nil)
      t)))

(defun xcj--abbrev-position-cursor (&optional φpos)
  "Move cursor back to ▮.
but limit backward search to at φpos or at beginning of line.
return true if found, else false."
  (interactive)
  (search-backward "▮" (if φpos φpos (line-beginning-position)) t ))


;; indent/reformat related

(defun xcj-complete-or-indent ()
  "Do keyword completion or indent/prettify-format.

If char before point is letters and char after point is whitespace or punctuation, then do completion, except when in string or comment. In these cases, do `xcj-prettify-root-sexp'."
  (interactive)
  ;; consider the char to the left or right of cursor. Each side is either empty or char.
  ;; there are 4 cases:
  ;; space▮space → do indent
  ;; space▮char → do indent
  ;; char▮space → do completion
  ;; char ▮char → do indent
  (let ( (ξsyntax-state (syntax-ppss)))
    (if (or (nth 3 ξsyntax-state) (nth 4 ξsyntax-state))
        (progn
          (xcj-prettify-root-sexp))
      (progn (if
                 (and (looking-back "[-_a-zA-Z]")
                      (or (eobp) (looking-at "[\n[:blank:][:punct:]]")))
                 (xcj-complete-symbol)
               (xcj-prettify-root-sexp))))))

(defun xcj-prettify-root-sexp ()
  "Prettify format current root sexp group.
Root sexp group is the outmost sexp unit."
  (interactive)
  (save-excursion
    (let (ξp1 ξp2)
      (xcj-goto-outmost-bracket)
      (setq ξp1 (point))
      (setq ξp2 (scan-sexps (point) 1))
      (progn
        (goto-char ξp1)
        (indent-sexp)
        (xcj-compact-parens-region ξp1 ξp2)))))

(defun xcj-goto-outmost-bracket (&optional φpos)
  "Move cursor to the beginning of outer-most bracket, with respect to φpos.
Returns true if point is moved, else false."
  (interactive)
  (let ((ξi 0)
        (ξp0 (if (number-or-marker-p φpos)
                 φpos
               (point))))
    (goto-char ξp0)
    (while
        (and (< (setq ξi (1+ ξi)) 20)
             (not (eq (nth 0 (syntax-ppss (point))) 0)))
      (xcj-up-list -1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING"))
    (if (equal ξp0 (point))
        nil
      t
      )))

(defun xcj-compact-parens (&optional φp1 φp2)
  "Remove whitespaces in ending repetition of parenthesises.
If there's a text selection, act on the region, else, on defun block."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (save-excursion
       (xcj-goto-outmost-bracket)
       (list (point) (scan-sexps (point) 1)))))
  (let ((ξp1 φp1) (ξp2 φp2))
    (when (null φp1)
      (save-excursion
        (xcj-goto-outmost-bracket)
        (setq ξp1 (point))
        (setq ξp2 (scan-sexps (point) 1))))
    (xcj-compact-parens-region ξp1 ξp2)))

(defun xcj-compact-parens-region (φp1 φp2)
  "Remove whitespaces in ending repetition of parenthesises in region."
  (interactive "r")
  (let (ξsyntax-state)
    (save-restriction
      (narrow-to-region φp1 φp2)
      (goto-char (point-min))
      (while (search-forward-regexp ")[ \t\n]+)" nil t)
        (setq ξsyntax-state (syntax-ppss (match-beginning 0)))
        (if (or (nth 3 ξsyntax-state ) (nth 4 ξsyntax-state))
            (progn (search-forward ")"))
          (progn (replace-match "))")
                 (search-backward ")")))))))


;; abbrev

(setq xcj-abbrev-table nil)

(define-abbrev-table 'xcj-abbrev-table
  '(

    ("let" "(let [a▮ 3] 4)" nil :system t )
    ("and" "(and ▮)" nil :system t )
    ("fn" "(fn [▮] ▮)" nil :system t )
    ("defn" "(defn ▮ \"\" [x] x)" nil :system t )
    ("if" " (if ▮
    (do )
  (do )
)" nil :system t )

("def" "(def NAME▮ VALUE)" nil :system t )
("when" "(when ▮ )" nil :system t )
("do" "(do ▮)" nil :system t )
("recur" "(recur ▮ )" nil :system t )
("str" "(str x &▮ )" nil :system t )
("pr" "(pr x &▮ )" nil :system t )
("sort" "(sort comp▮❓ coll )" nil :system t )
("sort-by" "(sort keyfu▮❓ comp❓ coll )" nil :system t )

)

  "abbrev table for `xah-clojure-mode'"
;; :regexp "\\_<\\([_-0-9A-Za-z]+\\)"
  :regexp "\\([_-0-9A-Za-z]+\\)"
  :case-fixed t
  ;; :enable-function 'xcj-abbrev-enable-function
  )


;; syntax coloring related

(defface xcj-function-param
  '(
    (t :foreground "black" :background "LightYellow"))
  "face for function parameters."
  :group 'xah-clojure-mode )

(defface xcj-user-variable
  '(
    (t :foreground "magenta"))
  "face for user variables."
  :group 'xah-clojure-mode )

(setq xcj-font-lock-keywords
      (let (
            (clojureLangWords (regexp-opt xcj-clojure-basic-words 'symbols))
            (clojureCoreWords (regexp-opt xcj-clojure.core-words 'symbols))
            (clojureFunWords (regexp-opt xcj-clojure-fun-words 'symbols))
            (clojureKeyWords ":[-_?0-9A-Za-z]+" )
            (clojureConstantWords "\\*[-_?0-9A-Za-z]+\\*" )
            (functionParameters "φ[-_?0-9A-Za-z]+" )
            (userVars "ξ[-_?0-9A-Za-z]+" ))
        `(
          (,clojureLangWords . font-lock-function-name-face)
          (,clojureCoreWords . font-lock-type-face)
          (,clojureFunWords . font-lock-function-name-face)
          (,clojureKeyWords . font-lock-keyword-face)
          (,clojureConstantWords . font-lock-constant-face)

          (,functionParameters . 'xcj-function-param)
          (,userVars . 'xcj-user-variable))))

;; font-lock-builtin-face
;; font-lock-comment-delimiter-face
;; font-lock-comment-face
;; font-lock-constant-face
;; font-lock-doc-face
;; font-lock-function-name-face
;; font-lock-keyword-face
;; font-lock-negation-char-face
;; font-lock-preprocessor-face
;; font-lock-reference-face
;; font-lock-string-face
;; font-lock-type-face
;; font-lock-variable-name-face
;; font-lock-warning-face


;; syntax table
(defvar xcj-syntax-table nil "Syntax table for `xah-clojure-mode'.")
(setq xcj-syntax-table
      (let ((synTable (copy-syntax-table emacs-lisp-mode-syntax-table)))

        (modify-syntax-entry ?~ "'   " synTable)
        (modify-syntax-entry ?\{ "(}" synTable)
        (modify-syntax-entry ?\} "){" synTable)
        (modify-syntax-entry ?\[ "(]" synTable)
        (modify-syntax-entry ?\] ")[" synTable)
        (modify-syntax-entry ?^ "'" synTable)
        ;; Make hash a usual word character
        (modify-syntax-entry ?# "_ p" synTable)

        ;; (modify-syntax-entry ?@ "'   " synTable)

        ;; (modify-syntax-entry ?\; "<" synTable)
        ;; (modify-syntax-entry ?\n ">" synTable)
        ;; (modify-syntax-entry ?` "'   " synTable)
        ;; (modify-syntax-entry ?' "'   " synTable)
        ;; (modify-syntax-entry ?, "'   " synTable)
        ;; (modify-syntax-entry ?@ "'   " synTable)

        synTable))


;; keybinding

(when (string-equal system-type "windows-nt")
  (define-key key-translation-map (kbd "<apps>") (kbd "<menu>")))

(defvar xcj-keymap nil "Keybinding for `xah-clojure-mode'")
(progn
  (setq xcj-keymap (make-sparse-keymap))
  (define-key xcj-keymap (kbd "TAB") 'xcj-complete-or-indent)

  (define-prefix-command 'xcj-single-keys-keymap)
  (define-key xcj-keymap (kbd "<menu> e") xcj-single-keys-keymap)

  (define-key xcj-single-keys-keymap (kbd "u") 'xcj-add-paren-around-symbol)

  (define-key xcj-single-keys-keymap (kbd "t") 'xcj-prettify-root-sexp)
  (define-key xcj-single-keys-keymap (kbd "h") 'xcj-remove-paren-pair)

  (define-key xcj-single-keys-keymap (kbd "p") 'xcj-compact-parens)
  (define-key xcj-single-keys-keymap (kbd "c") 'xcj-complete-symbol)

  (define-key xcj-single-keys-keymap (kbd "e") 'xcj-expand-abbrev-maybe)

  (define-key xcj-single-keys-keymap (kbd "w .") 'cider-eval-buffer)
  (define-key xcj-single-keys-keymap (kbd "w e") 'cider-eval-defun-at-point)
  (define-key xcj-single-keys-keymap (kbd "w m") 'cider-eval-last-sexp)
  (define-key xcj-single-keys-keymap (kbd "w u") 'cider-eval-region)

)



;; define the mode
(defun xah-clojure-mode ()
  "A major mode for clojure.

Most useful command is `xcj-complete-or-indent'.

Press TAB before word to pretty format (indent).

Press TAB after word to complete.

Press SPACE to expand name to template.

i also recommend you use these commands:
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
or
URL `http://ergoemacs.github.io/ergoemacs-mode/'

\\{xcj-keymap}"
  (interactive)

  (kill-all-local-variables)

  (setq mode-name "∑clojure")
  (setq major-mode 'xah-clojure-mode)
  (setq font-lock-defaults '((xcj-font-lock-keywords)))

  (set-syntax-table xcj-syntax-table)
  (use-local-map xcj-keymap)
  (setq local-abbrev-table xcj-abbrev-table)

  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1) ;default to `;;' in comment-region
  (setq-local comment-column 2)

  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local tab-always-indent 'complete)

  (add-hook 'completion-at-point-functions 'xcj-complete-symbol nil 'local)

  (abbrev-mode 1)

  (progn
    ;; setup auto-complete-mode
    (when (fboundp 'auto-complete-mode)
      (add-to-list 'ac-modes 'xah-clojure-mode)))
  ;; (add-hook 'xah-clojure-mode-hook 'ac-emacs-lisp-mode-setup)

  (make-local-variable abbrev-expand-function)
  (if (or
       (and (>= emacs-major-version 24)
            (>= emacs-minor-version 4))
       (>= emacs-major-version 25))
      (progn
        (setq abbrev-expand-function 'xcj-expand-abbrev-maybe))
    (progn (add-hook 'abbrev-expand-functions 'xcj-expand-abbrev-maybe nil t)))

  (run-mode-hooks 'xah-clojure-mode-hook))

(provide 'xah-clojure-mode)
