;;; gud-lldb.el --- Grand Unified Debugger mode for running LLDB

;; Copyright (C) 1992-1996, 1998, 2000-2014 Free Software Foundation, Inc;
;;               2019 Duzy Chan, ExtBit Limited (http://extbit.com).

;; Authors:
;;     Duzy Chan <code@duzy.info> (since 2019)
;;     Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: unix, tools, osx

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file can be found from here:
;; `https://github.com/extbit/gud/gud-lldb.el'.
;;
;; It's implemented based on this one (not working properlly):
;; https://raw.githubusercontent.com/ptrv/emacs.d/master/site-lisp/gud-lldb.el.

;;; Code:

(require 'gud)

(defvar gud-lldb-args-history nil
  "History of argument lists passed to lldb.")
(defvar gud-lldb-thread-id nil
  "Parsed thread id.")
(defvar gud-lldb-current-frame nil
  "Parsed current frame id.")

;; Keeps track of breakpoint created.  In the following case, the id is "1".
;; It is used to implement temporary breakpoint.
;; (lldb) b main.c:39
;; breakpoint set --file 'main.c' --line 39
;; Breakpoint created: 1: file ='main.c', line = 39, locations = 1
(defvar gud-lldb-breakpoint-id nil)

(defun lldb-extract-breakpoint-id (string)
  ;; Search for "Breakpoint created: \\([^:\n]*\\):" pattern.
  ;(message "gud-marker-acc string is: |%s|" string)
  (when (string-match "Breakpoint created: \\([^:\n]*\\):" string)
    (setq gud-lldb-breakpoint-id (match-string 1 string))
    (message "breakpoint id: %s" gud-lldb-breakpoint-id)))

;; see gud-sdb-marker-filter in `gud.el`
(defun gud-lldb-marker-filter (string)
  (unless gud-marker-acc (setq gud-marker-acc ""))
  (setq gud-marker-acc (concat gud-marker-acc string) string "")
  ;;(lldb-extract-breakpoint-id gud-marker-acc)
  ;;(message "%s" gud-marker-acc)
  ;;(message "(%s %s)" gud-lldb-thread-id gud-lldb-current-frame)
  (let (start)
    ;; Process all complete markers in this chunk
    (while
        (cond
         ;; (lldb) bt
         ;; * thread #1, queue = 'com.apple.main-thread', stop reason = signal SIGABRT
         ;;     frame #0: 0x00007fff70b402c6 libsystem_kernel.dylib`__pthread_kill + 10
         ;;   * frame #1: 0x00007fff70bfbbf1 libsystem_pthread.dylib`pthread_kill + 284
         ;;     frame #2: 0x00007fff70aaa6a6 libsystem_c.dylib`abort + 127
         ;;     ...
         ;;     frame #5: 0x00007fff70bfbbf1 a.out`main + 2 at main.cpp:123
         ((string-match "^[*] thread #\\([0-9]+\\), queue = \\('[^']*'\\), stop reason = \\([^\n]+\\)\n"
                        gud-marker-acc start)
          (setq gud-lldb-thread-id (match-string 1 gud-marker-acc)
                gud-lldb-current-frame nil
                gud-last-frame nil
                string (concat string (match-string 0 gud-marker-acc))
                start (match-end 0))
          ;;(message "thread %s (%s %s)" (match-string 1 gud-marker-acc) gud-lldb-thread-id gud-lldb-current-frame)
          t)
         ((and gud-lldb-thread-id
               (string-match (concat "^[ ]+\\([*] \\)?"
                                     "frame #\\([0-9]+\\): 0x[0-9a-fA-F]+ "
                                     "\\([^`]+\\)`\\([^+]+?\\) [+] [0-9]+"
                                     "\\( at \\([^:\n]+\\):\\([0-9]+\\)\\)?"
                                     "[ ]*\n")
                             gud-marker-acc start))
          (if (and (not gud-lldb-current-frame)
                   (match-string 1 gud-marker-acc))
              (setq gud-lldb-current-frame (match-string 2 gud-marker-acc)
                    gud-last-frame (if (match-string 5 gud-marker-acc)
                                       (cons (match-string 6 gud-marker-acc)
                                             (string-to-number (match-string 7 gud-marker-acc))))))
          (setq string (concat string (match-string 0 gud-marker-acc))
                start (match-end 0))
          ;;(message "frame %s (%s %s) (%s:%s)" (match-string 2 gud-marker-acc) gud-lldb-thread-id gud-lldb-current-frame (match-string 6 gud-marker-acc) (match-string 7 gud-marker-acc))
          t)

         ;; (lldb) up
         ;; frame #1: 0x00007fff70bfbbf1 libsystem_pthread.dylib`pthread_kill + 284
         ;; libsystem_pthread.dylib`pthread_kill:
         ;; ->  0x7fff70bfbbf1 <+284>: movl   %eax, %r15d
         ;;     0x7fff70bfbbf4 <+287>: cmpl   $-0x1, %eax
         ;;     ...
         ;; (lldb) frame select -r 1
         ;; frame #1: 0x0000000100000e09 a.out`main + 25 at main.c:44
         ;; ...
         ((and (not gud-lldb-thread-id) (not gud-lldb-current-frame)
               (string-match (concat "^frame #\\([0-9]+\\): \\(0x[0-9a-fA-F]+\\) "
                                     "\\([^`]+\\)`\\([^+]+?\\) [+] [0-9]+"
                                     "\\( at \\([^:\n]+\\):\\([0-9]+\\)\\)?"
                                     "[ ]*\n")
                             gud-marker-acc start))
          (message (match-string 0 gud-marker-acc))
          (setq gud-lldb-current-frame (match-string 1 gud-marker-acc)
                gud-last-frame (if (match-string 5 gud-marker-acc)
                                   (cons (match-string 6 gud-marker-acc)
                                         (string-to-number (match-string 7 gud-marker-acc))))
                ;;string (concat string (match-string 0 gud-marker-acc))
                string (concat string (format
                                       "frame #%s: %s (%s)\n-> %s\n  at %s:%s\n"
                                       (match-string 1 gud-marker-acc)
                                       (match-string 2 gud-marker-acc)
                                       (match-string 3 gud-marker-acc)
                                       (match-string 4 gud-marker-acc)
                                       (match-string 6 gud-marker-acc)
                                       (match-string 7 gud-marker-acc)))
                start (match-end 0))
          ;;(message "frame %s (%s %s) (%s:%s)" (match-string 2 gud-marker-acc) gud-lldb-thread-id gud-lldb-current-frame (match-string 5 gud-marker-acc) (match-string 6 gud-marker-acc))
          t)

         ;; (lldb) r
         ;; Process 15408 launched: '/Volumes/data/lldb/svn/trunk/test/conditional_break/a.out' (x86_64)
         ;; (lldb) Process 15408 stopped
         ;; * thread #1: tid = 0x2e03, 0x0000000100000de8 a.out`c + 7 at main.c:39, stop reason = breakpoint 1.1, queue = com.apple.main-thread
         ;; (nil (string-match " at \\([^:\n]*\\):\\([0-9]*\\), stop reason = .*\n"
         ;;                    gud-marker-acc start)
         ;;      (setq gud-last-frame (cons (match-string 1 gud-marker-acc)
         ;;                                 (string-to-number (match-string 2 gud-marker-acc)))
         ;;            start (match-end 0)))

         ;; frame #4: 0x00007fff59cdee38 libsystem_malloc.dylib`malloc_report + 151
         ;; libsystem_malloc.dylib`malloc_report:
         ;; ->  0x7fff59cdee38 <+151>: movq   0x366ed1d1(%rip), %rax    ; (void *)0x00007fff903b9070: __stack_chk_guard
         ;;     0x7fff59cdee3f <+158>: movq   (%rax), %rax
         ;;     0x7fff59cdee42 <+161>: cmpq   -0x8(%rbp), %rax
         ;;     0x7fff59cdee46 <+165>: jne    0x7fff59cdee51            ; <+176>
         ((string-match "^\\([^\n]+\\)\n" gud-marker-acc start)
          ;;(message "%s (%s %s)" (match-string 1 gud-marker-acc) gud-lldb-thread-id gud-lldb-current-frame)
          (if (string-match-p "^(lldb) $" (match-string 1 gud-marker-acc))
              (setq gud-lldb-thread-id nil
                    gud-lldb-current-frame nil))
          ;; Output text after a frame declaration without source location.
          (if (not gud-last-frame);(and gud-lldb-current-frame (not gud-last-frame))
              (setq string (concat string (match-string 0 gud-marker-acc))))
          (setq start (match-end 0))
          t)

         ;; the end
         ((string-match "^(lldb) $" gud-marker-acc start)
          ;;(message "<lldb> (%s %s)" gud-lldb-thread-id gud-lldb-current-frame)
          (setq gud-lldb-thread-id nil
                gud-lldb-current-frame nil
                string (concat string (match-string 0 gud-marker-acc))
                start (match-end 0))
          t)))

    (when nil;gud-lldb-current-frame
      ;; Search for lines to discard in this chunk
      (while (string-match "\n" gud-marker-acc start)
        (setq start (match-end 0))))

    ;; If we have an incomplete line, store it in gud-marker-acc.
    (setq gud-marker-acc (substring gud-marker-acc (or start 0))))

  ;;(message "(%s %s)" gud-lldb-thread-id gud-lldb-current-frame)

  (if (and (string= gud-marker-acc "")
           (not (string-match-p "(lldb)[ ]+$" string)))
      (concat string "(lldb) ")
    string))

;; Keeps track of whether the Python lldb_oneshot_break function definition has
;; been exec'ed.
(defvar lldb-oneshot-break-defined nil)

;;;###autoload
(defun lldb (command-line)
  "Run lldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive (list (gud-query-cmdline 'lldb)))

  (gud-common-init command-line nil 'gud-lldb-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'lldb)
  (setq lldb-oneshot-break-defined nil)

  ;; Make lldb dump fullpath instead of basename for a file.
  ;; See also gud-lldb-marker-filter where gud-last-frame is grokked from lldb output.
  (progn
    (gud-call "settings set frame-format frame #${frame.index}: ${frame.pc}{ ${module.file.basename}{`${function.name}${function.pc-offset}}}{ at ${line.file.fullpath}:${line.number}}\\n")
    (sit-for 1)
    (gud-call "settings set thread-format thread #${thread.index}: tid = ${thread.id}{, ${frame.pc}}{ ${module.file.basename}{`${function.name}${function.pc-offset}}}{ at ${line.file.fullpath}:${line.number}}{, stop reason = ${thread.stop-reason}}\\n")
    (sit-for 1))

  (gud-def gud-listb  "breakpoint list"
                      "l"    "List all breakpoints.")
  (gud-def gud-bt     "thread backtrace"
                      "b"    "Show stack for the current thread.")
  (gud-def gud-bt-all "thread backtrace all"
                      "B"    "Show stacks for all the threads.")

  (gud-def gud-break  "breakpoint set -f %f -l %l"
                      "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak
       (progn (gud-call "breakpoint set -f %f -l %l")
                  (sit-for 1)
                  (if (not lldb-oneshot-break-defined)
                      (progn
                        ;; The "\\n"'s are required to escape the newline chars
                        ;; passed to the lldb process.
                        (gud-call (concat "script exec \"def lldb_oneshot_break(frame, bp_loc):\\n"
                                                        "    target=frame.GetThread().GetProcess().GetTarget()\\n"
                                                        "    bp=bp_loc.GetBreakpoint()\\n"
                                                        "    print 'Deleting oneshot breakpoint:', bp\\n"
                                                        "    target.BreakpointDelete(bp.GetID())\""))
                        (sit-for 1)
                        ;; Set the flag since Python knows about the function def now.
                        (setq lldb-oneshot-break-defined t)))
                  (gud-call "breakpoint command add -p %b -o 'lldb_oneshot_break(frame, bp_loc)'"))
                  "\C-t" "Set temporary breakpoint at current line.")
  (gud-def gud-remove "breakpoint clear -f %f -l %l"
                      "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "thread step-in"
                      "\C-s" "Step one source line with display.")
  (gud-def gud-stepi  "thread step-inst"
                      "\C-i" "Step one instruction with display.")
  (gud-def gud-next   "thread step-over"
                      "\C-n" "Step one line (skip functions).")
  (gud-def gud-nexti  "thread step-inst-over"
                      nil    "Step one instruction (skip functions).")
  (gud-def gud-cont   "process continue"
                      "\C-r" "Continue with display.")
  (gud-def gud-finish "thread step-out"
                      "\C-f" "Finish executing current function.")
  (gud-def gud-up
           (progn (gud-call "frame select -r 1")
                  (sit-for 1))
                      "<"    "Up 1 stack frame.")
  (gud-def gud-down
           (progn (gud-call "frame select -r -1")
                  (sit-for 1))
                      ">"    "Down 1 stack frame.")
  (gud-def gud-print  "expression -- %e"
                      "\C-p" "Evaluate C expression at point.")
  (gud-def gud-pstar  "expression -- *%e"
                      nil    "Evaluate C dereferenced pointer expression at point.")
  (gud-def gud-run    "run"
                      "r"    "Run the program.")
  (gud-def gud-stop-subjob    "process kill"
                      "s"    "Stop the program.")

  (setq comint-prompt-regexp  "\\(^\\|\n\\)\\*")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'lldb-mode-hook)
  )

;; ;; tooltip
;; (defun gud-lldb-tooltip-print-command (expr)
;;   "Return a suitable command to print the expression EXPR."
;;   (pcase gud-minor-mode
;;     ;; '-o' to print the objc object description if available
;;     (`lldb (concat "expression -o -- " expr))
;;     (`gdbmi (concat "-data-evaluate-expression \"" expr "\""))
;;     (`guiler expr)
;;     (`dbx (concat "print " expr))
;;     ((or `xdb `pdb) (concat "p " expr))
;;     (`sdb (concat expr "/"))))

;; (advice-add 'gud-tooltip-print-command :override #'gud-lldb-tooltip-print-command)

;; menu
(setcdr (nth 2 (nth 7 (assoc 'nexti gud-menu-map))) '((lldb gdbmi gdb dbx)))
(setcdr (nth 2 (nth 7 (assoc 'stepi gud-menu-map))) '((lldb gdbmi gdb dbx)))
(setcdr (nth 2 (nth 7 (assoc 'finish gud-menu-map))) '((lldb gdbmi gdb guiler xdb jdb pdb)))
(setcdr (nth 2 (nth 7 (assoc 'print* gud-menu-map))) '((lldb gdbmi gdb jdb)))
(setcdr (nth 2 (nth 7 (assoc 'down gud-menu-map))) '((lldb gdbmi gdb guiler dbx xdb jdb pdb)))
(setcdr (nth 2 (nth 7 (assoc 'up gud-menu-map))) '((lldb gdbmi gdb guiler dbx xdb jdb pdb)))
(setcdr (nth 2 (nth 7 (assoc 'tbreak gud-menu-map))) '((lldb gdbmi gdb sdb xdb)))
(setcdr (nth 2 (nth 7 (assoc 'run gud-menu-map))) '((lldb gdbmi gdb dbx jdb)))
;; (setcdr (nth 2 (nth 7 (assoc 'tooltips gud-menu-map))) '((lldb gdbmi guiler dbx sdb xdb pdb)))


(provide 'gud-lldb)

;;; gud-lldb.el ends here
