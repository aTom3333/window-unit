;;; window-unit-tests.el --- Tests for window-unit  -*- lexical-binding: t; -*-

;; Run with:
;;   emacs -Q --batch -L . -l window-unit.el -l window-unit-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tab-bar)
(require 'window-unit)

;;; ---------------------------------------------------------------------------
;;; Fixture

(defconst window-unit-test--buffer-names
  '("random" "chat" "input" "x" "y" "other" "ctl" "a" "b" " *hint*")
  "Buffers the tests create, killed between them so names stay predictable.")

(defun window-unit-test--reset ()
  "Return the frame to one plain window and drop every test buffer."
  (setq window-unit--dissolving nil
        window-unit--registry nil
        window-unit--folding nil)
  (window-unit-mode -1)
  ;; Clear atomicity before collapsing: an atomic group whose root is the
  ;; frame's root makes `delete-other-windows' signal.
  (walk-window-tree (lambda (window)
                      (set-window-parameter window 'window-atom nil)
                      (set-window-parameter window 'window-unit nil)
                      (when (window-live-p window)
                        (set-window-dedicated-p window nil)))
                    nil nil t)
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (dolist (name window-unit-test--buffer-names)
    (let ((buffer (get-buffer name)))
      (when buffer
        (with-current-buffer buffer
          (kill-local-variable 'window-unit--of-buffer)
          (remove-hook 'kill-buffer-hook #'window-unit--member-killed t))
        (kill-buffer buffer)))))

(defmacro window-unit-test--with-clean-frame (&rest body)
  "Run BODY on a frame holding one plain window, and clean up after.
`window-unit-mode' is on for the duration: `window-unit-adopt' refuses to build
a unit without it."
  (declare (indent 0) (debug t))
  `(progn
     (window-unit-test--reset)
     (window-unit-mode 1)
     (unwind-protect (progn ,@body)
       (window-unit-test--reset))))

(defun window-unit-test--switch (buffer)
  "Switch to BUFFER as a command would, settling at the command's end.
Batch mode runs no command loop, so the settling pass has to be called here."
  (switch-to-buffer buffer)
  (window-unit--settle-all))

(defun window-unit-test--preview (&rest buffers)
  "Show each of BUFFERS the way a completion framework previews a candidate.
Each keystroke is a command inside the session, so the settling pass runs and
must decline to act while the session is open."
  (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1)))
    (dolist (buffer buffers)
      (switch-to-buffer buffer)
      (window-unit--settle-all))))

(defun window-unit-test--shape (&optional window)
  "WINDOW's tree as nested lists, so a layout can be compared literally.
A live window becomes its buffer's name, suffixed `@' when it is a member of an
atomic group and `!' when it is dedicated.  An internal window becomes (h ...)
when its children sit side by side and (v ...) when they are stacked."
  (let ((window (or window (frame-root-window))))
    (if (window-live-p window)
        (intern (concat (buffer-name (window-buffer window))
                        (if (window-parameter window 'window-atom) "@" "")
                        (if (window-dedicated-p window) "!" "")))
      (cons (if (window-combined-p (window-child window) t) 'h 'v)
            (let (children (child (window-child window)))
              (while child
                (push (window-unit-test--shape child) children)
                (setq child (window-next-sibling child)))
              (nreverse children))))))

(defun window-unit-test--build-pi ()
  "Lay out random | (chat over input) and make the right pair a unit."
  (set-window-buffer (selected-window) (get-buffer-create "random"))
  (let* ((left (selected-window))
         (top (split-window left nil 'right))
         (bottom (split-window top nil 'below)))
    (set-window-buffer top (get-buffer-create "chat"))
    (set-window-buffer bottom (get-buffer-create "input"))
    (window-unit-adopt (list top bottom))))

(defun window-unit-test--rotate ()
  "Rotate window states the way `evil-window-rotate-downwards' does.
It moves each window's whole state into the next window, so a group's members
end up scattered across windows that are not a group."
  (let ((windows (window-list nil 'no-mini))
        (states (mapcar #'window-state-get (window-list nil 'no-mini))))
    (setq states (append (last states) states))
    (while (and windows states)
      (window-state-put (car states) (car windows))
      (setq windows (cdr windows)
            states (cdr states)))))

(defun window-unit-test--window (name)
  "The window showing the buffer called NAME."
  (get-buffer-window (get-buffer name)))

;;; ---------------------------------------------------------------------------
;;; Adopting

(ert-deftest window-unit-adopt-makes-the-group-atomic-and-dedicated ()
  "Adopting marks each pane an atom member and dedicates it."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (should (equal (window-unit-test--shape)
                   '(h random (v chat@! input@!))))))

(ert-deftest window-unit-adopt-refuses-a-scattered-window-set ()
  "Windows that are not one region of the tree cannot become a unit."
  (window-unit-test--with-clean-frame
    (set-window-buffer (selected-window) (get-buffer-create "random"))
    (let* ((left (selected-window))
           (top (split-window left nil 'right))
           (bottom (split-window top nil 'below)))
      (set-window-buffer top (get-buffer-create "chat"))
      (set-window-buffer bottom (get-buffer-create "input"))
      ;; The smallest subtree holding left and bottom holds top as well.
      (should-error (window-unit-adopt (list left bottom))))))

(ert-deftest window-unit-adopt-refuses-a-window-already-in-a-unit ()
  "A pane cannot be adopted into a second unit."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (should-error (window-unit-adopt (list (window-unit-test--window "chat")
                                           (window-unit-test--window "input"))))))

(ert-deftest window-unit-adopt-refuses-a-single-window ()
  "One buffer in one window already behaves as itself, so it is not a unit.
Adopting it also broke that behaviour: the rule that a group is never shown
twice made switching to the buffer from elsewhere move point instead of showing
it in both windows, which is what Emacs does for any buffer."
  (window-unit-test--with-clean-frame
    (set-window-buffer (selected-window) (get-buffer-create "chat"))
    (should-error (window-unit-adopt (list (selected-window))))
    (should-not window-unit--registry)
    (should-not (window-unit-of (get-buffer "chat")))
    (should-not (window-parameter (selected-window) 'window-unit))
    (should-not (window-dedicated-p (selected-window)))))

;;; ---------------------------------------------------------------------------
;;; The rules

(ert-deftest window-unit-switching-away-hands-the-group-place-over ()
  "Switching to a non-member from inside a unit gives it the unit's place."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "chat"))
      (window-unit-test--switch (get-buffer-create "x"))
      (should (equal (window-unit-test--shape) '(h random x)))
      (should-not (window-unit-footprint unit))
      (should (buffer-live-p (get-buffer "chat")))
      (should (buffer-live-p (get-buffer "input"))))))

(ert-deftest window-unit-switching-to-a-member-brings-the-group-here ()
  "Switching to a member from outside puts the whole group in that window."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (select-window (window-unit-test--window "chat"))
    (window-unit-test--switch (get-buffer-create "x"))
    (select-window (window-unit-test--window "random"))
    (window-unit-test--switch (get-buffer "input"))
    (should (equal (window-unit-test--shape)
                   '(h (v chat@! input@!) x)))
    (should (eq (current-buffer) (get-buffer "input")))))

(ert-deftest window-unit-switching-to-a-shown-member-just-selects-it ()
  "A unit already on screen is selected, never shown a second time."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (select-window (window-unit-test--window "random"))
    (window-unit-test--switch (get-buffer "input"))
    (should (equal (window-unit-test--shape)
                   '(h random (v chat@! input@!))))
    (should (eq (current-buffer) (get-buffer "input")))
    (should (= 1 (length (get-buffer-window-list (get-buffer "chat") nil t))))))

(ert-deftest window-unit-a-unit-can-take-another-unit-place ()
  "Summoning a dormant unit from inside another displaces that one first."
  (window-unit-test--with-clean-frame
    ;; (a over b) | (chat over input), both units
    (set-window-buffer (selected-window) (get-buffer-create "a"))
    (let* ((left (selected-window))
           (right (split-window left nil 'right))
           (left-bottom (split-window left nil 'below))
           (right-bottom (split-window right nil 'below)))
      (set-window-buffer left-bottom (get-buffer-create "b"))
      (set-window-buffer right (get-buffer-create "chat"))
      (set-window-buffer right-bottom (get-buffer-create "input"))
      (window-unit-adopt (list left left-bottom))
      (window-unit-adopt (list right right-bottom)))
    ;; send the left unit away, so it is dormant
    (select-window (window-unit-test--window "a"))
    (window-unit-test--switch (get-buffer-create "other"))
    ;; now summon it from inside the right unit
    (select-window (window-unit-test--window "chat"))
    (window-unit-test--switch (get-buffer "a"))
    (should (equal (window-unit-test--shape)
                   '(h other (v a@! b@!))))
    (should (eq (current-buffer) (get-buffer "a")))))

(ert-deftest window-unit-an-ephemeral-buffer-does-not-displace ()
  "Switching to an internal buffer leaves the unit where it is."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "chat"))
      (window-unit-test--switch (get-buffer-create " *hint*"))
      (should (window-unit-footprint unit)))))

(ert-deftest window-unit-a-pop-up-lands-outside-the-group ()
  "A buffer shown with `display-buffer' opens beside the unit, not in it."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "chat"))
      (let ((window (display-buffer (get-buffer-create "x"))))
        (window-unit--settle-all)
        (should-not (window-parameter window 'window-atom))
        (should (window-unit-footprint unit))))))

(ert-deftest window-unit-same-window-display-cannot-take-a-pane ()
  "`display-buffer-same-window' is turned away by a pane's dedication.
It is the one display action the atomic group alone does not deflect, so
without dedication it would land in a pane and read as a displacement."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "chat"))
      (let ((window (display-buffer (get-buffer-create "x")
                                    '(display-buffer-same-window))))
        (window-unit--settle-all)
        (should-not (eq window (window-unit-test--window "chat")))
        (should (window-unit-footprint unit))))))

(ert-deftest window-unit-quitting-a-pane-dismisses-the-group ()
  "`quit-window' in a pane takes the whole group off screen, not one pane."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "chat"))
      (quit-window)
      (window-unit--settle-all)
      (should-not (window-unit-footprint unit))
      (should (buffer-live-p (get-buffer "chat")))
      (should (buffer-live-p (get-buffer "input"))))))

;;; ---------------------------------------------------------------------------
;;; Previews

(ert-deftest window-unit-a-preview-does-not-disturb-the-unit ()
  "Candidates shown during a completion session leave the group untouched."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "input"))
      ;; consult shows one candidate per keystroke with `switch-to-buffer',
      ;; then puts the original buffer back when the session ends.
      (window-unit-test--preview (get-buffer-create "x")
                                 (get-buffer-create "y")
                                 (get-buffer "input"))
      (window-unit--settle-all)
      (should (window-unit-footprint unit))
      (should (equal (window-unit-test--shape)
                     '(h random (v chat@! input@!)))))))

(ert-deftest window-unit-a-preview-shows-the-final-arrangement ()
  "A previewed candidate takes the whole of the group's place, as it would."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (select-window (window-unit-test--window "input"))
    (window-unit-test--preview (get-buffer-create "x"))
    (should (equal (window-unit-test--shape) '(h random x)))
    (should (eq (current-buffer) (get-buffer "x")))))

(ert-deftest window-unit-a-preview-of-a-member-shows-the-group ()
  "Previewing a member of a dormant unit shows the group, not the buffer alone."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (select-window (window-unit-test--window "input"))
    (window-unit-test--switch (get-buffer-create "x"))
    (select-window (window-unit-test--window "random"))
    (window-unit-test--preview (get-buffer "chat"))
    (should (equal (window-unit-test--shape)
                   '(h (v chat@! input@!) x)))))

(ert-deftest window-unit-an-abandoned-preview-puts-the-group-back ()
  "The buffer restored when a session ends brings the whole group back."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "input"))
      ;; two candidates, then the original buffer, which is consult's reset
      (window-unit-test--preview (get-buffer-create "x")
                                 (get-buffer-create "y")
                                 (get-buffer "input"))
      (window-unit--settle-all)
      (should (window-unit-footprint unit))
      (should (equal (window-unit-test--shape)
                     '(h random (v chat@! input@!)))))))

(ert-deftest window-unit-a-choice-confirmed-after-a-preview-takes-the-place ()
  "After the reset, the confirmed buffer still ends up with the group's place."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "input"))
      (window-unit-test--preview (get-buffer-create "x")
                                 (get-buffer "input"))
      (window-unit-test--switch (get-buffer-create "y"))
      (should (equal (window-unit-test--shape) '(h random y)))
      (should-not (window-unit-footprint unit)))))

(ert-deftest window-unit-a-committed-preview-becomes-a-displacement ()
  "A candidate left in the pane when the session ends takes the group's place."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "input"))
      (window-unit-test--preview (get-buffer-create "x"))
      ;; the session ends with x still showing, so it was the choice
      (window-unit--settle-all)
      (should (equal (window-unit-test--shape) '(h random x)))
      (should-not (window-unit-footprint unit)))))

;;; ---------------------------------------------------------------------------
;;; What the group remembers

(ert-deftest window-unit-comes-back-at-the-position-it-left ()
  "A summoned unit restores each pane's point and scroll position."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (with-current-buffer (get-buffer "chat")
      (insert (mapconcat #'number-to-string (number-sequence 1 400) "\n")))
    (let ((chat-window (window-unit-test--window "chat")))
      (set-window-start chat-window 300)
      (set-window-point chat-window 400)
      (select-window chat-window)
      (window-unit-test--switch (get-buffer-create "x")))
    (select-window (window-unit-test--window "random"))
    (window-unit-test--switch (get-buffer "chat"))
    (let ((chat-window (window-unit-test--window "chat")))
      (should (= 300 (window-start chat-window)))
      (should (= 400 (window-point chat-window))))))

(ert-deftest window-unit-restores-a-nested-group ()
  "A control buffer over two side-by-side panes survives a round trip."
  (window-unit-test--with-clean-frame
    (set-window-buffer (selected-window) (get-buffer-create "random"))
    (let* ((left (selected-window))
           (top (split-window left nil 'right))
           (bottom (split-window top nil 'below))
           (bottom-right (split-window bottom nil 'right)))
      (set-window-buffer top (get-buffer-create "ctl"))
      (set-window-buffer bottom (get-buffer-create "a"))
      (set-window-buffer bottom-right (get-buffer-create "b"))
      (window-unit-adopt (list top bottom bottom-right)))
    (should (equal (window-unit-test--shape)
                   '(h random (v ctl@! (h a@! b@!)))))
    (select-window (window-unit-test--window "ctl"))
    (window-unit-test--switch (get-buffer-create "x"))
    (should (equal (window-unit-test--shape) '(h random x)))
    (select-window (window-unit-test--window "random"))
    (window-unit-test--switch (get-buffer "a"))
    (should (equal (window-unit-test--shape)
                   '(h (v ctl@! (h a@! b@!)) x)))))

;;; ---------------------------------------------------------------------------
;;; Guards

(ert-deftest window-unit-killing-a-member-dissolves-without-signalling ()
  "Killing a member tears the unit down instead of erroring in Emacs."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (kill-buffer (get-buffer "chat"))
      (should-not (window-unit-footprint unit))
      (should (buffer-live-p (get-buffer "input")))
      (should-not (window-unit-of (get-buffer "input"))))))

(ert-deftest window-unit-filling-the-frame-twice-does-not-signal ()
  "Once the unit fills the frame, asking for that again changes nothing."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "chat"))
      (delete-other-windows)
      (should (equal (window-unit-test--shape) '(v chat@! input@!)))
      (delete-other-windows)
      (should (equal (window-unit-test--shape) '(v chat@! input@!)))
      (should (window-unit-footprint unit)))))

(ert-deftest window-unit-delete-other-windows-from-outside-keeps-working ()
  "From outside a unit, asking for one window still drops the unit as one."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "random"))
      (delete-other-windows)
      (should (equal (window-unit-test--shape) 'random))
      (should-not (window-unit-footprint unit)))))

(ert-deftest window-unit-delete-other-windows-works-as-a-command ()
  "The guard has to accept the arguments a key binding passes.
`delete-other-windows' takes two, and its interactive spec supplies both, so an
advice declaring one signals rather than doing the job."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "random"))
      (call-interactively #'delete-other-windows)
      (should (equal (window-unit-test--shape) 'random))
      (should-not (window-unit-footprint unit)))))

(ert-deftest window-unit-filling-the-frame-as-a-command-does-not-signal ()
  "Called as a command from inside a pane, twice, the guard still holds."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "chat"))
      (call-interactively #'delete-other-windows)
      (should (equal (window-unit-test--shape) '(v chat@! input@!)))
      (call-interactively #'delete-other-windows)
      (should (equal (window-unit-test--shape) '(v chat@! input@!)))
      (should (window-unit-footprint unit)))))

(ert-deftest window-unit-the-advices-accept-their-targets-full-arguments ()
  "Every advised command still works when given all the arguments it takes.
An advice declaring fewer arguments than its target signals for every key bound
to it, which a call with no arguments never reveals."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (select-window (window-unit-test--window "random"))
    (switch-to-buffer (get-buffer-create "x") nil t)
    (should (eq (current-buffer) (get-buffer "x")))
    (previous-buffer 1)
    (next-buffer 1)
    (delete-other-windows nil 1)
    (should (= 1 (length (window-list nil 'no-mini))))))

(defun window-unit-test--rotate-command ()
  "A command that shuffles window states, as a window package's would."
  (interactive)
  (window-unit-test--rotate))

(defmacro window-unit-test--with-folding-command (&rest body)
  "Run BODY with the test rotation listed as a single-window command."
  (declare (indent 0) (debug t))
  `(let ((window-unit-single-window-commands
          '(window-unit-test--rotate-command)))
     (window-unit-refresh-single-window-commands)
     (unwind-protect (progn ,@body)
       (setq window-unit-single-window-commands nil)
       (window-unit-refresh-single-window-commands))))

(ert-deftest window-unit-a-listed-command-sees-one-window ()
  "A listed command rotates the group's slot, not its panes.
Unlisted, the same command scatters the members and the group has to be put
back together; listed, it never sees the panes at all."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (window-unit-test--with-folding-command
      (call-interactively #'window-unit-test--rotate-command)
      (should (equal (window-unit-test--shape)
                     '(h (v chat@! input@!) random)))
      (call-interactively #'window-unit-test--rotate-command)
      (should (equal (window-unit-test--shape)
                     '(h random (v chat@! input@!)))))))

(ert-deftest window-unit-folding-keeps-the-selected-member ()
  "Point stays in the member it was in when a listed command folds the group."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (window-unit-test--with-folding-command
      (select-window (window-unit-test--window "input"))
      (call-interactively #'window-unit-test--rotate-command)
      (should (eq (current-buffer) (get-buffer "input"))))))

(defvar window-unit-test--folded-during-command nil
  "Whether the group was folded while the failing command ran.")

(defun window-unit-test--failing-command ()
  "A command that records whether it ran folded, then signals."
  (interactive)
  (setq window-unit-test--folded-during-command
        (cl-notany (lambda (window) (window-parameter window 'window-atom))
                   (window-list nil 'no-mini)))
  (error "Deliberate failure"))

(ert-deftest window-unit-folding-unfolds-after-a-command-fails ()
  "A listed command that signals ran folded and still leaves the group whole."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi))
          (window-unit-single-window-commands '(window-unit-test--failing-command)))
      (setq window-unit-test--folded-during-command nil)
      (window-unit-refresh-single-window-commands)
      (unwind-protect
          (progn
            (should-error (window-unit-test--failing-command))
            ;; without this the test would pass with no advice installed at all
            (should window-unit-test--folded-during-command)
            (should (window-unit-footprint unit)))
        (setq window-unit-single-window-commands nil)
        (window-unit-refresh-single-window-commands)))))

(ert-deftest window-unit-a-rotation-does-not-grow-the-frame ()
  "Rotating window states repeatedly leaves the frame with the windows it had.
Putting a scattered group back together must drop the windows its members were
strewn into, or every rotation adds one."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (let ((before (length (window-list nil 'no-mini))))
      (dotimes (_ 4)
        (window-unit-test--rotate)
        (window-unit--settle-all))
      (should (= before (length (window-list nil 'no-mini)))))))

(ert-deftest window-unit-previous-buffer-dismisses-the-whole-unit ()
  "`previous-buffer' in a pane collapses the group and takes its place.
The same rule as any other switch: a member's window changing buffer means the
group is done, so its windows become one."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "chat"))
      (previous-buffer)
      ;; No settling pass: the collapse has to be complete on its own, or a
      ;; command that changes no window leaves a pane un-dedicated for good.
      (should-not (window-unit-footprint unit))
      (should-not (window-unit-test--window "chat"))
      (should-not (window-unit-test--window "input"))
      (should (= 2 (length (window-list nil 'no-mini))))
      (should (cl-notany #'window-dedicated-p (window-list nil 'no-mini)))
      (should (cl-notany (lambda (window) (window-parameter window 'window-atom))
                         (window-list nil 'no-mini)))
      ;; and the members survive, so the unit can be summoned back
      (should (buffer-live-p (get-buffer "chat")))
      (should (buffer-live-p (get-buffer "input"))))))

(ert-deftest window-unit-next-buffer-dismisses-the-whole-unit ()
  "`next-buffer' in a pane behaves the same way as `previous-buffer'."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "input"))
      (next-buffer)
      (should-not (window-unit-footprint unit))
      (should (= 2 (length (window-list nil 'no-mini))))
      (should (cl-notany #'window-dedicated-p (window-list nil 'no-mini))))))

(ert-deftest window-unit-previous-buffer-summons-a-member-it-lands-on ()
  "`previous-buffer' reaching a member brings the whole group back.
Not by way of the settling pass: `switch-to-prev-buffer' is not
`switch-to-buffer', so the policy advice never sees it, and leaving the group
to be reassembled later shows the member alone for a redisplay first."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "chat"))
      (window-unit-test--switch (get-buffer-create "x"))
      (should-not (window-unit-footprint unit))
      ;; the window the unit gave up now has a member in its previous buffers
      (previous-buffer)
      (should (window-unit-footprint unit))
      (should (equal (window-unit-test--shape)
                     '(h random (v chat@! input@!))))
      (should (eq (current-buffer) (get-buffer "chat"))))))

(ert-deftest window-unit-previous-buffer-round-trips-a-unit ()
  "Away and back with the same key leaves the group as it was."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi))
          (before nil))
      (select-window (window-unit-test--window "chat"))
      (setq before (window-unit-test--shape))
      (previous-buffer)
      (should-not (window-unit-footprint unit))
      (previous-buffer)
      (should (window-unit-footprint unit))
      (should (equal before (window-unit-test--shape))))))

(ert-deftest window-unit-previous-buffer-leaves-a-bystander-unit-alone ()
  "Dismissing one unit does not move or disturb another on the same frame.
The buffer that lands can be a member of the other unit, which makes the
folding handle ambiguous - the group was put back in the wrong slot before the
handle remembered its window as well as its buffer."
  (window-unit-test--with-clean-frame
    (set-window-buffer (selected-window) (get-buffer-create "a"))
    (let* ((left (selected-window))
           (right (split-window left nil 'right))
           (left-bottom (split-window left nil 'below))
           (right-bottom (split-window right nil 'below)))
      (set-window-buffer left-bottom (get-buffer-create "b"))
      (set-window-buffer right (get-buffer-create "chat"))
      (set-window-buffer right-bottom (get-buffer-create "input"))
      (window-unit-adopt (list left left-bottom))
      (window-unit-adopt (list right right-bottom)))
    (let ((bystander (window-unit-of (get-buffer "a")))
          (dismissed (window-unit-of (get-buffer "chat"))))
      (with-current-buffer (get-buffer "a")
        (insert (mapconcat #'number-to-string (number-sequence 1 400) "\n")))
      (let ((pane (window-unit-test--window "a")))
        (set-window-start pane 300)
        (set-window-point pane 350))
      (select-window (window-unit-test--window "chat"))
      (previous-buffer)
      ;; the bystander is still whole, still on the left, still where it was
      (should (window-unit-footprint bystander))
      (should-not (window-unit-footprint dismissed))
      (should (equal (window-unit-test--shape) '(h (v a@! b@!) a)))
      (let ((pane (cl-find-if (lambda (window)
                                (window-parameter window 'window-atom))
                              (get-buffer-window-list (get-buffer "a") 'no-mini))))
        (should pane)
        (should (= 300 (window-start pane)))
        (should (= 350 (window-point pane)))))))

;;; ---------------------------------------------------------------------------
;;; Surviving Emacs and other packages

(ert-deftest window-unit-survives-emacs-dissolving-the-group ()
  "A group Emacs has un-atomized is put back together before the next switch."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      ;; `window--atom-check' does exactly this whenever the group looks
      ;; wrong to it, leaving the panes dedicated but no longer one thing.
      (dolist (window (list (window-unit-test--window "chat")
                            (window-unit-test--window "input")))
        (set-window-parameter window 'window-atom nil))
      (should-not (window-unit-footprint unit))
      (select-window (window-unit-test--window "input"))
      (window-unit-test--switch (get-buffer-create "x"))
      (should (equal (window-unit-test--shape) '(h random x)))
      (should (buffer-live-p (get-buffer "chat"))))))

(ert-deftest window-unit-reasserts-itself-when-a-pane-is-taken ()
  "A group whose pane was taken while un-atomized comes back whole."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (dolist (window (list (window-unit-test--window "chat")
                            (window-unit-test--window "input")))
        (set-window-parameter window 'window-atom nil))
      (let ((chat-window (window-unit-test--window "chat")))
        (set-window-dedicated-p chat-window nil)
        (set-window-buffer chat-window (get-buffer-create "other")))
      (window-unit--settle-all)
      (should (window-unit-footprint unit))
      (should (window-unit-test--window "other"))
      (should (window-unit-test--window "chat"))
      (should (window-unit-test--window "input")))))

(ert-deftest window-unit-survives-a-tab-switch ()
  "A unit keeps its atomicity across a tab round trip.
A tab is stored as a window state, which drops any parameter
`window-persistent-parameters' does not list - dedication is kept regardless,
so without the entry a switch gives back dedicated windows that are no longer
one group."
  (window-unit-test--with-clean-frame
    (tab-bar-mode 1)
    (unwind-protect
        (let ((unit (window-unit-test--build-pi)))
          (tab-bar-new-tab)
          (tab-bar-switch-to-prev-tab)
          (should (window-unit-footprint unit))
          (should (equal (window-unit-test--shape)
                         '(h random (v chat@! input@!)))))
      (when (> (length (tab-bar-tabs)) 1) (tab-bar-close-other-tabs))
      (tab-bar-mode -1))))

;;; ---------------------------------------------------------------------------
;;; Repairs the review turned up

(ert-deftest window-unit-recovers-when-a-member-is-shown-twice ()
  "A member landing in two windows is put back, not left wedged.
A window the survey cannot see is a window nothing un-dedicates, and the
`window-state-put' that follows then signals against it."
  (window-unit-test--with-clean-frame
    (set-window-buffer (selected-window) (get-buffer-create "random"))
    (let* ((left (selected-window))
           (top (split-window left nil 'right)))
      ;; Showing both in one window before splitting is how a consumer builds
      ;; this layout, and it is what puts input into chat's previous buffers.
      (set-window-buffer top (get-buffer-create "input"))
      (set-window-buffer top (get-buffer-create "chat"))
      (let ((bottom (split-window top nil 'below)))
        (set-window-buffer bottom (get-buffer "input"))
        (let ((unit (window-unit-adopt (list top bottom))))
          (select-window (window-unit-test--window "chat"))
          (previous-buffer)
          (window-unit--settle-all)
          (should (window-unit-footprint unit))
          (should (equal (window-unit-test--shape)
                         '(h random (v chat@! input@!)))))))))

(ert-deftest window-unit-a-unit-that-cannot-settle-leaves-the-others-alone ()
  "Each unit settles inside its own guard, so one failure is not a blackout."
  (window-unit-test--with-clean-frame
    (set-window-buffer (selected-window) (get-buffer-create "a"))
    (let* ((left (selected-window))
           (right (split-window left nil 'right))
           (left-bottom (split-window left nil 'below))
           (right-bottom (split-window right nil 'below)))
      (set-window-buffer left-bottom (get-buffer-create "b"))
      (set-window-buffer right (get-buffer-create "chat"))
      (set-window-buffer right-bottom (get-buffer-create "input"))
      (window-unit-adopt (list left left-bottom))
      (window-unit-adopt (list right right-bottom)))
    (let ((seen nil)
          (real (symbol-function 'window-unit--settle))
          (doomed (car window-unit--registry)))
      (cl-letf (((symbol-function 'window-unit--settle)
                 (lambda (unit &optional frame)
                   (push unit seen)
                   (if (eq unit doomed)
                       (error "Deliberate failure")
                     (funcall real unit frame)))))
        (window-unit--settle-all))
      (should (= 2 (length (delete-dups seen)))))))

(ert-deftest window-unit-adopt-refuses-a-member-of-a-dormant-unit ()
  "A buffer already in a unit cannot join a second one, unit on screen or not."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (select-window (window-unit-test--window "chat"))
    (window-unit-test--switch (get-buffer-create "x"))
    (should-not (window-unit-test--window "chat"))
    (select-window (window-unit-test--window "random"))
    (set-window-buffer (selected-window) (get-buffer "chat"))
    (should-error (window-unit-adopt (list (selected-window)
                                           (window-unit-test--window "x"))))))

(ert-deftest window-unit-dissolving-leaves-no-atom-flag-behind ()
  "A dissolved group leaves no window claiming to belong to one.
A flag nothing owns would keep the `delete-other-windows' guard defending a
group that no longer exists."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (select-window (window-unit-test--window "chat"))
      (delete-other-windows)
      ;; a package drops a buffer into one pane
      (let ((pane (window-unit-test--window "chat")))
        (set-window-dedicated-p pane nil)
        (set-window-buffer pane (get-buffer-create "other")))
      (window-unit-dissolve unit)
      (should (cl-notany (lambda (window) (window-parameter window 'window-atom))
                         (window-list nil 'no-mini)))
      (should (cl-notany #'window-dedicated-p (window-list nil 'no-mini)))
      (delete-other-windows)
      (should (= 1 (length (window-list nil 'no-mini)))))))

(ert-deftest window-unit-settling-stays-out-while-a-command-holds-a-fold ()
  "A folded frame is half-finished, so the pass declines to repair it."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (window-unit--collapse unit)
      (let ((window-unit--folding t))
        (window-unit--settle-all))
      (should-not (window-unit-footprint unit))
      (window-unit--settle-all)
      (should (window-unit-footprint unit)))))

(ert-deftest window-unit-frame-search-puts-this-frame-first ()
  "An explicit frame is honoured, and otherwise a unit here wins over one away."
  (should (equal (list (selected-frame))
                 (window-unit--frames (selected-frame))))
  (should (eq (selected-frame) (car (window-unit--frames nil)))))

(ert-deftest window-unit-settling-uses-the-frame-it-is-given ()
  "The pass repairs on the frame it was handed."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (window-unit--collapse unit)
      (window-unit--settle-all (selected-frame))
      (should (window-unit-footprint unit)))))

(ert-deftest window-unit-ignores-a-foreign-atomic-group ()
  "Another package's atomic group holding one of our members is not ours.
Anchoring on the `window-atom' parameter read such a group as this unit's, and
the settling pass collapsed the package's layout out from under it."
  (window-unit-test--with-clean-frame
    (set-window-buffer (selected-window) (get-buffer-create "random"))
    (let* ((left (selected-window))
           (top (split-window left nil 'right))
           (bottom (split-window top nil 'below))
           unit)
      (set-window-buffer top (get-buffer-create "chat"))
      (set-window-buffer bottom (get-buffer-create "input"))
      (setq unit (window-unit-adopt (list top bottom)))
      ;; something else builds its own atomic group, and it happens to show
      ;; one of our members
      (select-window left)
      (let ((second (split-window left nil 'below)))
        (set-window-buffer left (get-buffer-create "ctl"))
        (set-window-buffer second (get-buffer "chat"))
        (window-make-atom (window-parent left)))
      (should (window-unit-footprint unit))
      (should (eq 'intact (plist-get (window-unit--survey unit) :state)))
      (window-unit--settle-all)
      ;; the foreign group is left exactly as it was
      (should (window-unit-test--window "ctl"))
      (should (equal (window-unit-test--shape)
                     '(h (v ctl@ chat@) (v chat@! input@!)))))))

(ert-deftest window-unit-will-not-take-a-foreign-dedicated-window ()
  "A unit steps aside rather than seize a window dedicated by someone else.
Taking it stripped the dedication and left that package with nowhere to draw."
  (window-unit-test--with-clean-frame
    (set-window-buffer (selected-window) (get-buffer-create "x"))
    (let* ((left (selected-window))
           (top (split-window left nil 'right))
           (bottom (split-window top nil 'below))
           unit)
      (set-window-buffer top (get-buffer-create "chat"))
      (set-window-buffer bottom (get-buffer-create "input"))
      (setq unit (window-unit-adopt (list top bottom)))
      (select-window top)
      (window-unit-test--switch (get-buffer "x"))
      (should-not (window-unit-footprint unit))
      ;; a foreign package dedicates a window to its own buffer
      (let ((side (split-window (selected-window) nil 'left)))
        (set-window-buffer side (get-buffer-create "other"))
        (set-window-dedicated-p side t)
        (select-window side)
        (window-unit-summon unit (get-buffer "chat"))
        (should (window-live-p side))
        (should (eq (window-buffer side) (get-buffer "other")))
        (should (window-dedicated-p side))
        (should (window-unit-footprint unit))))))



;;; ---------------------------------------------------------------------------
;;; Defects a second review turned up

(ert-deftest window-unit-a-summoned-member-becomes-current ()
  "Switching to a member makes that member current, wherever the group lands.
`window-state-put' selects whichever window the stash recorded as selected, so
a summon that has to step aside from a foreign dedicated window left a
different member current and `switch-to-buffer' lied about what it returned."
  (window-unit-test--with-clean-frame
    (set-window-buffer (selected-window) (get-buffer-create "chat"))
    (let* ((top (selected-window))
           (bottom (split-window top nil 'below)))
      (set-window-buffer bottom (get-buffer-create "input"))
      (window-unit-adopt (list top bottom))
      ;; displaced from `input', so the stash records `input' as selected
      (select-window (window-unit-test--window "input"))
      (window-unit-test--switch (get-buffer-create "x"))
      ;; now stand in a window that is not ours to take
      (let ((side (split-window (selected-window) nil 'left)))
        (set-window-buffer side (get-buffer-create "other"))
        (set-window-dedicated-p side t)
        (select-window side)
        (let ((returned (switch-to-buffer (get-buffer "chat"))))
          (should (eq returned (get-buffer "chat")))
          (should (eq (current-buffer) (get-buffer "chat")))
          (should (eq (window-buffer (selected-window)) (get-buffer "chat"))))))))

(ert-deftest window-unit-at-never-names-a-unit-that-does-not-own-the-pane ()
  "A pane marked for one unit holding another unit's buffer is not that unit's.
The marker says \"a pane of something\" and the buffer's back-pointer says
\"the unit of this buffer\"; with nothing tying them together the answer was
whichever unit the buffer belonged to."
  (window-unit-test--with-clean-frame
    (set-window-buffer (selected-window) (get-buffer-create "a"))
    (let* ((a-top (selected-window))
           (b-top (split-window a-top nil 'right))
           (a-bottom (split-window a-top nil 'below))
           (b-bottom (split-window b-top nil 'below))
           unit-a unit-b)
      (set-window-buffer a-bottom (get-buffer-create "b"))
      (set-window-buffer b-top (get-buffer-create "chat"))
      (set-window-buffer b-bottom (get-buffer-create "input"))
      (setq unit-a (window-unit-adopt (list a-top a-bottom)))
      (setq unit-b (window-unit-adopt (list b-top b-bottom)))
      ;; Emacs un-atomizes A, then a package reuses one of A's panes
      (dolist (window (list a-top a-bottom))
        (set-window-parameter window 'window-atom nil))
      (set-window-dedicated-p a-bottom nil)
      (set-window-buffer a-bottom (get-buffer "chat"))
      (should-not (eq (window-unit-at a-bottom) unit-b))
      (ignore unit-a))))

(ert-deftest window-unit-turning-the-mode-off-releases-live-units ()
  "Disabling the mode leaves no window dedicated with nothing behind it.
The advices and the settling pass go, so a group left marked and dedicated has
nothing to repair it and no gesture recovers it."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (window-unit-mode -1)
    (should-not window-unit--registry)
    (should (cl-notany #'window-dedicated-p (window-list nil 'no-mini)))
    (should (cl-notany (lambda (window) (window-parameter window 'window-unit))
                       (window-list nil 'no-mini)))
    (should (cl-notany (lambda (window) (window-parameter window 'window-atom))
                       (window-list nil 'no-mini)))))

(ert-deftest window-unit-folding-repairs-a-group-emacs-took-apart ()
  "A listed command folds a group even when Emacs has dropped its atomicity.
Folding asked for an intact footprint, so an un-atomized group was not folded
and the command saw the panes."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    ;; `window--atom-check' does this whenever a group looks wrong to it
    (dolist (window (list (window-unit-test--window "chat")
                          (window-unit-test--window "input")))
      (set-window-parameter window 'window-atom nil))
    (window-unit-test--with-folding-command
      (call-interactively #'window-unit-test--rotate-command)
      (should (equal (window-unit-test--shape)
                     '(h (v chat@! input@!) random))))))

(ert-deftest window-unit-folding-works-on-the-frame-it-runs-on ()
  "The folding advice names the frame, so it cannot reach units on another."
  (window-unit-test--with-clean-frame
    (window-unit-test--build-pi)
    (let (seen)
      (cl-letf* ((real (symbol-function 'window-unit--fold-all))
                 ((symbol-function 'window-unit--fold-all)
                  (lambda (&optional frame)
                    (push frame seen)
                    (funcall real frame))))
        (window-unit-test--with-folding-command
          (call-interactively #'window-unit-test--rotate-command)))
      (should seen)
      (should (cl-every #'framep seen)))))

(ert-deftest window-unit-one-unfold-failing-does-not-strand-the-others ()
  "Each unit is unfolded on its own, so one that cannot be put back is alone."
  (window-unit-test--with-clean-frame
    (set-window-buffer (selected-window) (get-buffer-create "a"))
    (let* ((left (selected-window))
           (right (split-window left nil 'right))
           (left-bottom (split-window left nil 'below))
           (right-bottom (split-window right nil 'below))
           first)
      (set-window-buffer left-bottom (get-buffer-create "b"))
      (set-window-buffer right (get-buffer-create "chat"))
      (set-window-buffer right-bottom (get-buffer-create "input"))
      (window-unit-adopt (list left left-bottom))
      (window-unit-adopt (list right right-bottom))
      (setq first (car (last window-unit--registry)))
      (cl-letf* ((real (symbol-function 'window-unit--put))
                 ((symbol-function 'window-unit--put)
                  (lambda (unit window member &optional norecord)
                    (if (eq unit first)
                        (error "Deliberate failure")
                      (funcall real unit window member norecord)))))
        (window-unit-test--with-folding-command
          (call-interactively #'window-unit-test--rotate-command)))
      ;; the unit that could be put back is whole again
      (should (cl-some (lambda (unit) (window-unit-footprint unit))
                       window-unit--registry)))))

(ert-deftest window-unit-dissolving-clears-the-internal-windows-too ()
  "No window of a dissolved group keeps the atom parameter, internal included."
  (window-unit-test--with-clean-frame
    (let ((unit (window-unit-test--build-pi)))
      (window-unit-dissolve unit)
      (let (leftover)
        (walk-window-tree (lambda (window)
                            (when (window-parameter window 'window-atom)
                              (push window leftover)))
                          nil nil t)
        (should-not leftover)))))


(provide 'window-unit-tests)

;;; window-unit-tests.el ends here
