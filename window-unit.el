;;; window-unit.el --- Treat a group of buffers as one buffer  -*- lexical-binding: t; -*-

;; Author: Thomas Ferrand
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, windows

;;; Commentary:

;; A unit is a set of buffers, shown in a set of windows, that the rest of Emacs
;; treats as a single buffer in a single window.  Switching to a member brings
;; the whole group into the selected window's place.  Switching away from inside
;; the group hands its place to the new buffer and leaves the group dormant,
;; remembering its layout, so summoning it again restores what was on screen.
;;
;; The rearrangement happens as the switch does, so what is on screen is always
;; the real arrangement: a completion framework previewing a candidate shows
;; what choosing it would actually give you.  That costs the group nothing,
;; because such a preview ends by switching back to the buffer that was there,
;; which summons the group again.  A settling pass on
;; `window-buffer-change-functions' then repairs whatever Emacs or another
;; package has disturbed.
;;
;; Enable `window-unit-mode', then arrange the windows however you like and call
;; `window-unit-adopt' on them; this library never builds the first layout.
;;
;; DESIGN.org carries the rules, the reasoning, and what the built-ins do.

;;; Code:

(require 'cl-lib)

(defgroup window-unit nil
  "Groups of buffers that behave as one buffer."
  :group 'windows)

(defcustom window-unit-single-window-commands
  '(;; So the group's slot is a reuse candidate; dedication hides it otherwise.
    display-buffer
    previous-buffer
    next-buffer
    balance-windows
    balance-windows-area
    ;; Detaching a window would take one pane of a group into another tab.
    tab-window-detach
    ;; The layout commands arrived in Emacs 31.  Listing a command that does
    ;; not exist yet is harmless: the advice takes hold if it is ever defined.
    rotate-windows
    window-layout-rotate-clockwise
    window-layout-rotate-anticlockwise
    window-layout-flip-leftright
    window-layout-flip-topdown)
  "Functions that see a unit as the one window it stands for.
Each runs with every unit on the frame folded into a single window and the
groups put back afterwards.  A group whose slot the function took is left
dormant.

Functions that only move point belong nowhere near this list: they need to see
the panes.  Nothing about a function says which kind it is, so it has to be
declared.  Only built-ins are listed, and a name need not be defined yet."
  :type '(repeat function)
  ;; The default is installed without calling :set, which is not defined yet.
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (window-unit-refresh-single-window-commands))
  :group 'window-unit)

(defvar window-unit--folding-advised nil
  "Commands currently carrying the folding advice.")

;; Defined by the minor mode at the end of the file.
(defvar window-unit-mode)

(cl-defstruct (window-unit (:constructor window-unit--make) (:copier nil))
  "A group of buffers displayed as one."
  id         ; identifies this unit in a pane's `window-unit' parameter
  members    ; buffers, in adoption order
  stash)     ; window state, replayed to show the group

(defvar-local window-unit--of-buffer nil
  "The unit this buffer belongs to, or nil.")

(defvar window-unit--next-id 0
  "Counter handing each unit an id no other unit has.")

(defvar window-unit--registry nil
  "Every unit that still exists, so the settling pass can find them.")

(defvar window-unit--dissolving nil
  "Non-nil while a unit is being dissolved, so a hook cannot re-enter.")

(defvar window-unit--folding nil
  "Non-nil while units are folded for a command, so settling stays out.")

(defconst window-unit--persistent-parameters
  '((window-atom . writable) (window-unit . writable))
  "Window parameters that have to survive a window state.")

(defvar window-unit--added-parameters nil
  "Entries this mode put into `window-persistent-parameters'.")

;;; ---------------------------------------------------------------------------
;;; Reading the current state

(defun window-unit-of (buffer)
  "The unit BUFFER belongs to, or nil."
  (and (buffer-live-p buffer)
       (buffer-local-value 'window-unit--of-buffer buffer)))

(defun window-unit--members-live-p (unit)
  "Non-nil when every member of UNIT is still alive."
  (and (window-unit-members unit)
       (cl-every #'buffer-live-p (window-unit-members unit))))

(defun window-unit--panes (root)
  "The live windows of ROOT's subtree, in tree order.
A live ROOT yields just itself, which is what the callers that hand one expect."
  (let (panes)
    (walk-window-subtree (lambda (window) (push window panes)) root)
    (nreverse panes)))

(defun window-unit--ephemeral-p (buffer)
  "Non-nil when BUFFER is internal, its name starting with a space."
  (and (bufferp buffer) (string-prefix-p " " (buffer-name buffer))))

(defun window-unit--ephemeral-argument-p (args)
  "Non-nil when the first of ARGS names an internal buffer.
Only `display-buffer' takes a buffer there; the rest take a count or a
window."
  (let ((first (car args)))
    (and (or (bufferp first) (stringp first))
         (let ((buffer (get-buffer first)))
           (and buffer (window-unit--ephemeral-p buffer))))))

(defun window-unit--pane-p (window)
  "Non-nil when WINDOW is a pane this library put a unit in."
  (window-parameter window 'window-unit))

(defun window-unit--frames (frame)
  "The frames to look for a unit on.
FRAME alone when given; otherwise the selected frame first, so a group here
wins over a copy elsewhere, then the rest of the visible ones."
  (if frame
      (list frame)
    (cons (selected-frame) (delq (selected-frame) (visible-frame-list)))))

(defun window-unit--shown (unit frame)
  "Every window in FRAME showing a member of UNIT.
A member can be in more than one window, and all of them have to be accounted
for: a window the survey cannot see is a window nothing un-dedicates, and a
later `window-state-put' then signals against it."
  (let (windows)
    (dolist (buffer (window-unit-members unit))
      (when (buffer-live-p buffer)
        (dolist (window (get-buffer-window-list buffer 'no-mini frame))
          (push window windows))))
    (nreverse windows)))

(defun window-unit-footprint (unit &optional frame)
  "The window UNIT occupies, or nil when UNIT is not intact.
That is the root of the atomic group, whose live windows must show exactly
UNIT's members."
  (let ((survey (window-unit--survey unit frame)))
    (and (eq 'intact (plist-get survey :state)) (plist-get survey :root))))

(defun window-unit--survey (unit &optional frame)
  "How UNIT stands, as a plist, on the first frame showing any of it.
:state is `dormant' when no member is on screen, `intact' when the members are
the atomic group exactly, `clobbered' when a window of the group holds a buffer
that is not a member, and `scattered' when members are on screen without being
a group.  :root is the group's root, :foreign the window holding the intruder,
:shown every window showing a member, :frame where they were found."
  (if (null (window-unit-members unit))
      (list :state 'dormant)
    (catch 'found
      (dolist (candidate (window-unit--frames frame))
        (let ((shown (window-unit--shown unit candidate)))
          (when shown
            (throw 'found (window-unit--classify unit shown candidate)))))
      (list :state 'dormant))))

(defun window-unit--classify (unit shown frame)
  "Say how UNIT stands, given the windows SHOWN on FRAME."
  (let* ((members (window-unit-members unit))
         (anchor (cl-find-if #'window-unit--pane-p shown)))
    (cond
     ;; Members are on screen but none of them in a pane of ours: whatever we
     ;; are looking at, it is not this group.
     ((null anchor) (list :state 'scattered :shown shown :frame frame))
     (t
      (let ((root (window-atom-root anchor)))
        (if (null root)
            (list :state 'scattered :shown shown :frame frame)
          (let* ((panes (window-unit--panes root))
                 (foreign (cl-remove-if
                           (lambda (window)
                             (memq (window-buffer window) members))
                           panes))
                 (missing (cl-set-difference
                           members (mapcar #'window-buffer panes) :test #'eq)))
            (cond
             (foreign
              (list :state 'clobbered :root root :shown shown :frame frame
                    :foreign (if (memq (selected-window) foreign)
                                 (selected-window)
                               (car foreign))))
             (missing
              (list :state 'scattered :root root :shown shown :frame frame))
             (t (list :state 'intact :root root :shown shown :frame frame))))))))))

(defun window-unit--pane-for (buffer frame)
  "A window in FRAME showing BUFFER, preferring a pane of ours."
  (let ((windows (get-buffer-window-list buffer 'no-mini frame)))
    (or (cl-find-if #'window-unit--pane-p windows)
        (car windows))))

(defun window-unit--reatomize (unit &optional frame)
  "Make UNIT's windows one atomic group again, and return its root.
Nil when they can no longer be one group: a member is off screen, or a window
that is not a member now sits among them."
  (let ((windows (mapcar (lambda (buffer)
                           (and (buffer-live-p buffer)
                                (window-unit--pane-for buffer frame)))
                         (window-unit-members unit))))
    (when (and windows (cl-every #'window-live-p windows))
      (let ((root (window-unit--enclosing-root windows)))
        (when root
          (window-make-atom root)
          root)))))

(defun window-unit--ensure-group (unit &optional frame)
  "Return the window UNIT occupies, re-forming the group first if it came apart.
Emacs drops the `window-atom' parameter whenever a group looks wrong to it,
leaving the panes where they are but no longer acting as one.  This puts it back
when the panes are still in place, so a caller sees a whole group rather than
none.  Not a predicate: it changes window parameters."
  (or (window-unit-footprint unit frame)
      (window-unit--reatomize unit frame)))

(defun window-unit-at (window)
  "The unit WINDOW is a pane of, or nil."
  (let* ((id (window-parameter window 'window-unit))
         (unit (and id (window-unit-of (window-buffer window)))))
    (and unit (eql id (window-unit-id unit)) unit)))

(defun window-unit--claim-panes (unit root)
  "Mark ROOT's windows as UNIT's panes and dedicate them."
  (dolist (pane (window-unit--panes root))
    (set-window-parameter pane 'window-unit (window-unit-id unit))
    (set-window-dedicated-p pane t)))

(defun window-unit--release-panes (window-or-root)
  "Make WINDOW-OR-ROOT's group ordinary windows again.
A live window is resolved to its group's root first, so releasing one pane
releases the internal windows above it too."
  (let ((root (or (window-atom-root window-or-root) window-or-root)))
    (walk-window-subtree
     (lambda (window)
       (set-window-parameter window 'window-atom nil)
       (set-window-parameter window 'window-unit nil)
       (when (window-live-p window) (set-window-dedicated-p window nil)))
     root t)))

(defun window-unit--restash (unit &optional frame)
  "Record how UNIT looks right now, so it can be shown again as it is."
  (let ((root (window-unit-footprint unit frame)))
    (when root
      (setf (window-unit-stash unit) (window-state-get root t)))))

;;; ---------------------------------------------------------------------------
;;; Taking a group of windows over

(defun window-unit--member-killed ()
  "Dissolve this buffer's unit before Emacs replaces the buffer in its windows.
Put on `kill-buffer-hook' buffer-locally by `window-unit-adopt'."
  (let ((unit (window-unit-of (current-buffer))))
    (when unit (window-unit-dissolve unit))))

(defun window-unit--enclosing-root (windows)
  "The smallest internal window whose live windows are exactly WINDOWS, or nil.
Nil means no atomic group can hold just WINDOWS: every subtree containing them
contains something else too."
  (let ((root (window-parent (car windows))))
    (while (and root
                (cl-set-difference windows (window-unit--panes root) :test #'eq))
      (setq root (window-parent root)))
    (and root
         (null (cl-set-exclusive-or windows (window-unit--panes root) :test #'eq))
         root)))

(defun window-unit-adopt (windows)
  "Make the buffers shown in WINDOWS one unit, in place.
WINDOWS must be exactly the live windows of one region of the window tree, so
that making them atomic cannot swallow a window that is not a member.
`window-unit-mode' has to be on.  Returns the unit."
  (unless window-unit-mode
    (error "Enable `window-unit-mode' before adopting windows into a unit"))
  (unless (cdr windows)
    (error "A unit needs at least two windows"))
  (unless (cl-every #'window-live-p windows)
    (error "Those windows are not all live"))
  ;; Ask the buffer, not the window: a dormant unit's buffer would otherwise
  ;; slip through and end up claimed by two units at once.
  (dolist (window windows)
    (let ((unit (window-unit-of (window-buffer window))))
      (when unit
        (error "%s already belongs to a unit"
               (buffer-name (window-buffer window))))))
  (let ((buffers (mapcar #'window-buffer windows)))
    (unless (= (length buffers) (length (delete-dups (copy-sequence buffers))))
      (error "The same buffer appears in two of those windows"))
    (let ((unit (window-unit--make :id (cl-incf window-unit--next-id)
                                   :members buffers)))
      (let ((root (window-unit--enclosing-root windows)))
        (unless root
          (error "Those windows are not one region of the window tree"))
        (window-make-atom root)
        (window-unit--claim-panes unit root))
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (setq-local window-unit--of-buffer unit)
          (add-hook 'kill-buffer-hook #'window-unit--member-killed nil t)))
      (push unit window-unit--registry)
      (window-unit--restash unit)
      unit)))

;;; ---------------------------------------------------------------------------
;;; The durable operations

(defun window-unit--collapse-to (root keep)
  "Reduce ROOT's group to KEEP alone and return KEEP.
KEEP's buffer is left as it is, so a window already showing what should have
the group's place simply keeps it.  The stash is not touched here: it has to
have been taken while the group was still whole."
  (let* ((panes (window-unit--panes root))
         (was-inside (memq (selected-window) panes)))
    ;; Clear the atom before deleting anything: while the group is live,
    ;; deleting one member deletes the whole group, KEEP included.
    (window-unit--release-panes root)
    (dolist (pane panes)
      (when (and (window-live-p pane) (not (eq pane keep)))
        (delete-window pane)))
    (when (and was-inside (window-live-p keep)) (select-window keep))
    keep))

(defun window-unit--collapse (unit &optional frame)
  "Reduce UNIT to a single ordinary window and return it.
We stash UNIT first, so summoning it later restores what was on screen.  The
window returned still shows one of the members."
  (let ((root (window-unit--ensure-group unit frame)))
    (when root
      (window-unit--restash unit frame)
      (let* ((panes (window-unit--panes root))
             (keep (if (memq (selected-window) panes)
                       (selected-window)
                     (car panes))))
        (window-unit--collapse-to root keep)))))

(defun window-unit-displace (unit buffer)
  "Give UNIT's place to BUFFER and leave UNIT dormant.
UNIT keeps its buffers and its layout.  Returns the window BUFFER landed in."
  (let ((survivor (window-unit--collapse unit)))
    (when survivor
      (set-window-buffer survivor buffer)
      survivor)))

(defun window-unit--placeable-p (window)
  "Non-nil when a unit may take WINDOW's place.
One of our own panes counts: displacing a unit is how another one gets a place."
  (and (not (window-parameter window 'window-side))
       (or (not (window-dedicated-p window))
           (window-unit--pane-p window))))

(defun window-unit--placeable-window (window)
  "A window fit to hold a unit, starting from WINDOW.
A side window cannot be split, and somebody else's dedicated window is not ours
to take, so we step aside to a window on the frame that is neither."
  (if (window-unit--placeable-p window)
      window
    (or (cl-find-if #'window-unit--placeable-p
                    (window-list (window-frame window) 'no-mini))
        (error "No window on this frame can hold a unit"))))

(defun window-unit--put (unit window member &optional norecord)
  "Replace WINDOW with UNIT's group and return the window MEMBER landed in.
WINDOW is gone afterwards.  Selection ends up where it started, so a caller
that wants point to follow the group moves it afterwards.  NORECORD keeps the
buffer list order as it was."
  (let* ((window (window-unit--placeable-window window))
         (previously (selected-window))
         (was-target (eq window previously)))
    (set-window-dedicated-p window nil)
    (window-state-put (window-unit-stash unit) window)
    (let ((landed (get-buffer-window member)))
      (cond
       ((and was-target landed) (select-window landed norecord))
       ((window-live-p previously) (select-window previously norecord)))
      landed)))

(defun window-unit-summon (unit member &optional window norecord)
  "Show UNIT and select MEMBER, the way switching to a buffer would.
When UNIT is already intact we select MEMBER's window and change nothing else:
a group is one thing, so it is never shown in two places, and a unit on another
visible frame counts as shown.  Otherwise UNIT takes over WINDOW, the selected
one by default, whose contents give way.  NORECORD leaves the buffer list order
alone."
  (cond
   ((not (window-unit--members-live-p unit))
    (window-unit-dissolve unit)
    nil)
   ((window-unit--ensure-group unit)
    (let ((landed (or (get-buffer-window member nil)
                      (get-buffer-window member 'visible))))
      (when landed (select-window landed norecord))
      landed))
   ((null (window-unit-stash unit)) nil)
   (t
    (let* ((target (or window (selected-window)))
           (here (window-unit-at target)))
      ;; Standing inside another unit: that one gives up its place first.
      (when here (setq target (window-unit--collapse here)))
      (window-unit--put unit target member norecord)))))

(defun window-unit-dissolve (unit)
  "Forget UNIT, leaving its buffers ordinary.
The place UNIT held becomes a single window showing one of the members.  Any
window still showing a member is made ordinary too, so a group Emacs has
already dissolved cannot leave a dedicated window behind."
  (unless window-unit--dissolving
    (let ((window-unit--dissolving t)
          ;; Note the group's windows before collapsing.  A pane holding an
          ;; intruder shows no member, so releasing member windows alone would
          ;; leave it marked and dedicated, and `delete-other-windows' would go
          ;; on guarding a group that no longer exists.
          (panes (let ((anchor (cl-find-if #'window-unit--pane-p
                                           (window-unit--shown unit nil))))
                   (and anchor
                        (let ((root (window-atom-root anchor)))
                          (and root (window-unit--panes root)))))))
      (window-unit--collapse unit)
      (dolist (window panes)
        (when (window-live-p window)
          (window-unit--release-panes window)
          (set-window-dedicated-p window nil)))
      (dolist (buffer (window-unit-members unit))
        (when (buffer-live-p buffer)
          (dolist (window (get-buffer-window-list buffer nil t))
            (set-window-parameter window 'window-atom nil)
            (set-window-parameter window 'window-unit nil)
            (set-window-dedicated-p window nil))
          (with-current-buffer buffer
            (kill-local-variable 'window-unit--of-buffer)
            (remove-hook 'kill-buffer-hook #'window-unit--member-killed t))))
      (setq window-unit--registry (delq unit window-unit--registry))
      (setf (window-unit-members unit) nil
            (window-unit-stash unit) nil))))

;;; ---------------------------------------------------------------------------
;;; Policy: what switching to or from a unit means


(defun window-unit--switch-advice (original buffer-or-name &rest args)
  "Route `switch-to-buffer' so a unit behaves as one buffer.
Switching to a member shows its whole group; switching to anything else from
inside a group hands the group's place over.  Both happen at once, so a preview
shows the arrangement that choosing the candidate would give.  A preview ends by
switching back to the buffer that was there, which summons the group again, so
nothing is lost by acting immediately."
  (let* ((target (window-normalize-buffer-to-switch-to buffer-or-name))
         (unit (window-unit-of target))
         (here (window-unit-at (selected-window))))
    (cond
     ((window-unit--ephemeral-p target)
      (apply original buffer-or-name args))
     (unit
      (let ((landed (window-unit-summon unit target nil (car args))))
        (if landed
            (select-window landed (car args))
          (apply original buffer-or-name args)))
      target)
     (here
      (let ((window (window-unit-displace here target)))
        (if window
            (select-window window (car args))
          (apply original buffer-or-name args)))
      target)
     (t (apply original buffer-or-name args)))))

;;; ---------------------------------------------------------------------------
;;; Settling: repairing what others did

(defun window-unit--settle (unit &optional frame)
  "Bring UNIT into line with the rules, now the command has finished."
  (if (not (window-unit--members-live-p unit))
      (window-unit-dissolve unit)
    (let ((survey (window-unit--survey unit frame)))
      (pcase (plist-get survey :state)
        ('dormant nil)
        ('intact
         (window-unit--claim-panes unit (plist-get survey :root))
         (window-unit--restash unit frame))
        ;; The buffer put in a pane is still there, so the switch was meant:
        ;; hand it the group's whole place.
        ('clobbered
         (window-unit--collapse-to (plist-get survey :root)
                                   (plist-get survey :foreign)))
        ('scattered (window-unit--settle-scattered unit survey frame))))))

(defun window-unit--settle-scattered (unit survey frame)
  "Put UNIT back together when its members are on screen but are not a group."
  (let ((root (window-unit--reatomize unit frame)))
    (cond
     ;; every member is there and they still form one region
     (root
      (window-unit--claim-panes unit root)
      (window-unit--restash unit frame))
     ;; the members are on screen but strewn about: the group belongs in one
     ;; window, and the windows the others sit in have to go, or a command that
     ;; scatters them adds a window to the frame every time it runs
     ((window-unit-stash unit)
      (let* ((shown (plist-get survey :shown))
             (target (car shown))
             (strays (cdr shown)))
        ;; A scattering command moves whole window states about, atomicity
        ;; included, so a stray may carry a parameter that would take its
        ;; neighbours down with it.
        (dolist (window strays)
          (when (window-live-p window)
            (set-window-parameter window 'window-atom nil)
            (set-window-parameter window 'window-unit nil)
            (set-window-dedicated-p window nil)))
        (dolist (window strays)
          (when (and (window-live-p window)
                     (cdr (window-list (window-frame window) 'no-mini)))
            (delete-window window)))
        (when (window-live-p target)
          (window-unit--put unit target (window-buffer target)))))
     (t (window-unit-dissolve unit)))))

(defun window-unit--settle-all (&optional frame)
  "Settle every unit on FRAME, once the current interaction has finished.
Runs from `window-buffer-change-functions', so only when a window was actually
added, deleted or given another buffer - not on every command.

While a minibuffer session is in progress we wait: a completion framework
previews a candidate per keystroke by switching buffers, and each preview is
undone before the session ends.  Waiting makes a rearrangement late, not wrong.
We also stay out while units are folded for a command, whose half-finished frame
is nobody's idea of a state to repair.

Each unit is settled inside its own `with-demoted-errors', so one that cannot be
repaired does not stop the others from being looked at."
  (when (and window-unit--registry
             (not window-unit--folding)
             (zerop (minibuffer-depth)))
    (let ((units (copy-sequence window-unit--registry)))
      ;; A unit losing its place goes first, so one claiming a place lands in a
      ;; window that is already ordinary.
      (dolist (unit units)
        (with-demoted-errors "window-unit: %S"
          (when (and (memq unit window-unit--registry)
                     (eq 'clobbered
                         (plist-get (window-unit--survey unit frame) :state)))
            (window-unit--settle unit frame))))
      (dolist (unit units)
        (with-demoted-errors "window-unit: %S"
          (when (memq unit window-unit--registry)
            (window-unit--settle unit frame)))))))

;;; ---------------------------------------------------------------------------
;;; Presenting as one window

(defun window-unit--fold-all (&optional frame)
  "Collapse every unit shown in FRAME and return what is needed to unfold.
FRAME is named by the caller, so a command on one frame cannot fold a unit
living on another - which `window-unit-footprint' would happily find.
Each entry is (UNIT BUFFER WINDOW): the buffer left in the window the unit
folded into, and that window."
  (let (folded)
    (dolist (unit window-unit--registry)
      (when (window-unit--ensure-group unit frame)
        (let ((survivor (window-unit--collapse unit frame)))
          (when survivor
            (push (list unit (window-buffer survivor) survivor) folded)))))
    folded))

(defun window-unit--unfold-all (folded selected)
  "Put back every unit in FOLDED, then select SELECTED's window if it has one."
  (dolist (entry folded)
    (with-demoted-errors "window-unit: %S"
      (let* ((unit (nth 0 entry))
             (handle (nth 1 entry))
             (kept (nth 2 entry))
             (window (cond
                      ;; The window we folded into, when the command left it be.
                      ((and (window-live-p kept)
                            (eq (window-buffer kept) handle))
                       kept)
                      ;; Otherwise follow the buffer: the command moved windows
                      ;; about and the handle went with its slot.
                      ((buffer-live-p handle) (get-buffer-window handle)))))
        (when (and window (window-unit-members unit) (window-unit-stash unit))
          (window-unit--put unit window handle)))))
  (when (buffer-live-p selected)
    (let ((window (get-buffer-window selected)))
      (when window (select-window window)))))

(defun window-unit--single-window-advice (original &rest args)
  "Run ORIGINAL with every unit folded into the one window it stands for.
A unit whose slot ORIGINAL took is left dormant."
  (if (or (null window-unit--registry)
          (window-unit--ephemeral-argument-p args))
      ;; Folding for an internal buffer would delete panes its own code may
      ;; still be holding.
      (apply original args)
    (let ((selected (and (window-unit-at (selected-window))
                         (window-buffer (selected-window))))
          (folded nil))
      (unwind-protect
          (let ((window-unit--folding t))
            (setq folded (window-unit--fold-all (selected-frame)))
            (apply original args))
        ;; Unfold even when the command fails, or a group stays collapsed.
        (let ((window-unit--folding t))
          (window-unit--unfold-all folded selected))
        (window-unit--settle-all (selected-frame))))))

(defun window-unit-refresh-single-window-commands ()
  "Advise exactly the commands in `window-unit-single-window-commands'."
  (interactive)
  (dolist (command window-unit--folding-advised)
    (advice-remove command #'window-unit--single-window-advice))
  (setq window-unit--folding-advised nil)
  (when window-unit-mode
    (dolist (command window-unit-single-window-commands)
      (advice-add command :around #'window-unit--single-window-advice)
      (push command window-unit--folding-advised))))

;;; ---------------------------------------------------------------------------
;;; Guards against Emacs itself

(defun window-unit--delete-other-windows-advice (original &rest args)
  "Keep `delete-other-windows' working around an atomic group.
Emacs signals when an atomic group's root is the frame's root window.  When a
unit fills the frame that is already what the command asks for, so we do
nothing.  When the parameter belongs to no unit it is residue, and we drop it
rather than let the call fail.  ARGS is passed on untouched: the command takes a
second argument its interactive spec supplies, so anything but `apply' breaks
every key bound to it."
  (let* ((window (or (car args) (selected-window)))
         (unit (window-unit-at window))
         (root (and (window-parameter window 'window-atom)
                    (window-atom-root window)))
         (fills-frame (and root (eq root (frame-root-window window)))))
    (cond
     ((and unit fills-frame) nil)
     (t (when fills-frame (window-unit--release-panes window))
        (apply original args)))))

;;;###autoload
(define-minor-mode window-unit-mode
  "Global mode letting units behave as single buffers.
Has to be on before `window-unit-adopt' will build a unit: it carries the
policy, the guards and the window parameters a group needs to survive a tab
switch.  Every advice and the settling pass look for a unit first, so the mode
costs nothing while none exists."
  :global t
  :group 'window-unit
  (if window-unit-mode
      (progn
        (advice-add 'switch-to-buffer :around #'window-unit--switch-advice)
        (advice-add 'delete-other-windows :around
                    #'window-unit--delete-other-windows-advice)
        (add-hook 'window-buffer-change-functions #'window-unit--settle-all)
        ;; Anything that saves a window STATE rather than a window
        ;; configuration - a tab switch, `desktop', `burly' - drops a parameter
        ;; this does not list, and gives back panes that are still dedicated but
        ;; no longer one group.  Only what we add is removed again, so a user
        ;; who set the same entry keeps it.
        (dolist (entry window-unit--persistent-parameters)
          (unless (member entry window-persistent-parameters)
            (push entry window-persistent-parameters)
            (push entry window-unit--added-parameters)))
        (window-unit-refresh-single-window-commands))
    ;; Let every unit go first: the advices and the settling pass are about to
    ;; leave, and a group still marked and dedicated would have nothing behind
    ;; it and no gesture that recovers it.
    (dolist (unit (copy-sequence window-unit--registry))
      (window-unit-dissolve unit))
    (advice-remove 'switch-to-buffer #'window-unit--switch-advice)
    (advice-remove 'delete-other-windows
                   #'window-unit--delete-other-windows-advice)
    (remove-hook 'window-buffer-change-functions #'window-unit--settle-all)
    (dolist (entry window-unit--added-parameters)
      (setq window-persistent-parameters
            (delete entry window-persistent-parameters)))
    (setq window-unit--added-parameters nil)
    (window-unit-refresh-single-window-commands)))

(provide 'window-unit)

;;; window-unit.el ends here
