;;; multistate-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "multistate" "multistate.el" (0 0 0 0))
;;; Generated autoloads from multistate.el

(autoload 'multistate-manage-variables "multistate" "\
Manage multistate internal data structure for current state.

Return current hash table VARIABLE when WRITE is nil,
assign VALUE to hash table VARIABLE when WRITE is t.
Internal variables are: name, lighter, cursor,
keymap, parent, control, enter-hook, exit-hook.
Arbitrary variable may be used to store user data.

\(fn VARIABLE &optional WRITE VALUE)" nil nil)

(autoload 'multistate-define-state "multistate" "\
Define new NAME state.

LIGHTER will be passed to `multistate-lighter-format' to indicate state.
CURSOR will be applied when switched to this state.
PARENT keymap will be setup for state keymap.
Use `multistate-suppress-map' to suppress global keymap bindings.
Mark state to be DEFAULT if t.

\(fn NAME &key LIGHTER (CURSOR t) PARENT DEFAULT)" nil nil)

(autoload 'multistate-mode "multistate" "\
Toggle `multistate-mode' minor mode.

With a prefix argument ARG, enable `multistate-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'.

This minor mode provides modal editing features by creating
multiple keymaps and swapping them on demand.

\(fn &optional ARG)" t nil)

(put 'multistate-global-mode 'globalized-minor-mode t)

(defvar multistate-global-mode nil "\
Non-nil if Multistate-Global mode is enabled.
See the `multistate-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `multistate-global-mode'.")

(custom-autoload 'multistate-global-mode "multistate" nil)

(autoload 'multistate-global-mode "multistate" "\
Toggle Multistate mode in all buffers.
With prefix ARG, enable Multistate-Global mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Multistate mode is enabled in all buffers where
`multistate--maybe-activate' would do it.

See `multistate-mode' for more information on Multistate mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "multistate" '("multistate-"))

;;;***

;;;### (autoloads nil nil ("multistate-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; multistate-autoloads.el ends here
