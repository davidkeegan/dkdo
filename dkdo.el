;; dkdo.el --- Do List major mode based on org-mode.

;; Copyright: (c) David Keegan 2010-2013.
;; Licence: FSF GPLv3.
;; Author: David Keegan <dksw@eircom.net>
;; Version: 1.01
;; Package-Requires: ((dkmisc "0.50") (emacs "24.1"))
;; Keywords: dolist task productivity
;; URL: https://github.com/davidkeegan/dkdo

;;; Commentary:

;; Do List Mode is a major mode supporting a freely editable task
;; list. It is based on org-mode, and org-mode features like
;; outlining, visibility cycling, and lists/checkboxes are used for
;; structuring and managing tasks. However in so far as it maintains
;; the Do List in a separate document and allows for continuous
;; re-ordering of tasks to reflect changing priorities, it provides an
;; alternative to org mode's TODO items feature.

;; It keeps urgent tasks near the top of the file to maximise
;; visibility and supports deferred and recurring tasks.

;;; Code:

(require 'dkmisc)

(defconst dkdo-ModeName "Do List")

;;;###autoload
(defgroup dkdo nil
 (concat dkdo-ModeName " mode.")
 :tag "dkdo"
 :group 'dk)

(defcustom dkdo-AutoFinishCheckedTasks nil
 "If t, auto-finish task with checkboxes from section NOW.
A task is automatically moved from section NOW to section DONE when
all its checkboxes are ticked."
 :tag "dkdo-AutoFinishCheckedTasks"
 :type '(boolean))

(defcustom dkdo-DoneTimestampLength dkmisc-TimeYmdhmLen
 "Length of timestamps prefixed on insertion in DONE section.
The length includes separator characters. Default is 16 for
'YYYY-MM-DD HH:MM'"
 :tag "dkdo-DoneTimestampLength"
 :type '(integer))

(defcustom dkdo-ActionableNoticeHours 9.0
 "Hours before its due date when a task in LATER becomes actionable.
The intent is to provide some prior notice of upcoming tasks to
allow for preparation. As 'working' hours are needed for
preparation, the time from `dkdo-ActionableSkipBeforeMidnight' hours
before midnight to `dkdo-ActionableSkipAfterMidnight' hours after
midnight is not included when calculating the actionable time."
 :tag "dkdo-ActionableNoticeHours"
 :type '(float))

(defcustom dkdo-ActionableSkipAfterMidnight 7.0
 "Hours after midnight to skip when calculating a task's actionable time.
This period is not considered to be working time, and is
therefore skipped when calculating the actionable time of a task in
section LATER."
 :tag "dkdo-ActionableSkipAfterMidnight"
 :type '(float))

(defcustom dkdo-ActionableSkipBeforeMidnight 3.0
 "Hours before midnight to skip when calculating a task's actionable time.
This period is not considered to be working time, and is
therefore skipped when calculating the actionable time of a task in
section LATER."
 :tag "dkdo-ActionableSkipBeforeMidnight"
 :type '(float))

(defcustom dkdo-Filename nil
 "Default dolist filename for `dkdo-Edit'.
Function `dkdo-Edit' visits this file if invoked without a prefix
argument. Otherwise it prompts for a filename."
 :tag "dkdo-Filename"
 :type '(file))

(defcustom dkdo-RefreshSeconds 1800
 (concat "The interval in seconds between automatic refreshes.
The value in effect when " dkdo-ModeName " mode is started
controls automatic refreshes for the buffer. If greater than
zero, `dkdo-BufferRefresh' is called periodically at the
specified interval.")
 :tag "dkdo-RefreshSeconds"
 :type '(integer))

(defcustom dkdo-IcalLocation-function nil
"A function to transform the calendar location field.
Should take one string arg and return a string."
 :tag "dkdo-IcalLocation-function"
 :type '(function))

(defcustom dkdo-mode-hook nil
 (concat "Hooks called on entering " dkdo-ModeName ".")
 :tag "dkdo-mode-hook"
 :type '(hook :options (dkdo-SetCcKeys)))

(defconst dkdo-SectionHeaderPrefix "* ")
(defconst dkdo-SectionHeaderSuffix ":")

(defconst dkdo-SectionHeaderRe (concat "^\\" dkdo-SectionHeaderPrefix)
 "Start of a section header. Use function of same name.")

(defconst dkdo-ReHeadingTextAny "[^ 	\n]"
 "First character of valid section header text.")

(defconst dkdo-ReHeading "^[*]\\{1,3\\} "
 "Only outline levels 1, 2, and 3 are significant.")

(defconst dkdo-PrefixTask "** ")
(defconst dkdo-ReTask "^\\*\\* ")
(defconst dkdo-PrefixSubtask "*** ")
(defconst dkdo-ReSubtask "^\\*\\*\\* ")

(defconst dkdo-PackageDirectory
 (if load-file-name
  (file-name-directory load-file-name)
  nil))

(defconst dkdo-SampleDoFilename
 (if dkdo-PackageDirectory
  (expand-file-name "sample.do" dkdo-PackageDirectory)
  nil))

(defconst dkdo-Sections
 '((dkdo-Now . "NOW")
   (dkdo-Later . "LATER")
   (dkdo-Done . "DONE"))
 (concat dkdo-ModeName " section symbols/name association."))

(defconst dkdo-ReCheckboxesNumericDone "\\[\\([0-9+]\\)/\\1\\]"
 "Org mode checkbox statistics cookie indicating all done.")

(defconst dkdo-ReCheckboxesPercentDone "\\[100%\\]"
 "Org mode percent checkbox statistics cookie indicating all done.")

(defconst dkdo-ReCheckboxesDone
 (concat dkdo-ReCheckboxesNumericDone "\\|" dkdo-ReCheckboxesPercentDone))

; Eliminate warning.
(defvar dkdo-mode-map)

;;;###autoload
(defun dkdo-SetCcKeys()
 "Define C-cx keys for dkdo-mode.
To invoke add this function to dkdo-mode-hook."
 (define-key dkdo-mode-map "\C-cd" 'dkdo-TaskToDone)
 (define-key dkdo-mode-map "\C-cl" 'dkdo-TaskToLater)
 (define-key dkdo-mode-map "\C-cn" 'dkdo-TaskToNow)
 (define-key dkdo-mode-map "\C-cr" 'dkdo-BufferRefresh)
 (define-key dkdo-mode-map "\C-cs" 'dkdo-TaskStart))

;;;###autoload
(defun dkdo-Edit(PrefixArg)
"Prepare to edit a dolist.
With non-nil PREFIXARG, prompt for the filename, and visit it.
Otherwise visit configured `dkdo-Filename' unless already editing
a dolist"
(interactive "P")
(if PrefixArg
 (call-interactively 'find-file)
 (if (not (dkdo-InDolist))
  (find-file dkdo-Filename))))

(defun dkdo-InDolist()
"Return t if current buffer is a dolist."
 (and
  (buffer-file-name)
  (string-match "\\.do$" (buffer-file-name))))

;;;###autoload
(define-derived-mode dkdo-mode org-mode dkdo-ModeName
 "A do file has top-level Sections NOW, LATER, and DONE.
Each second-level header within a section is a task. A
third-level header within a task is a subtask. The task/subtask
content starts immediately after the header prefix and includes
the remainder of the header and any content, including any lower
level sections (which are not significant to this mode).

Dated tasks start life in LATER, other tasks in NOW. Tasks are
generally shifted LATER->NOW to be worked on (this shift can be
manual or automatic) and NOW->DONE when finished. On entering
DONE tasks are prefixed with a timestamp indicating the date of
completion. Tasks can also be shifted DONE->NOW if they are being
re-visited (the DONE timestamp is removed), or NOW-LATER to be
deferred (eg pending some other event). A task shifting to LATER
is prefixed with a new due date (which is prompted for).

The intent of a subtask is to allow related items to be collected
and managed together and perhaps hidden from view util they are
being worked on. The relationship between a subtask and its
parent is not permanent and is broken at some point before the
subtask is completed. To ensure every subtask is processed it is
illegal to shift a task containing subtasks from NOW->DONE.
Instead each subtask must be shifted individually. A consequence
of this is that subtasks do not exist in DONE.

When point is on a task header or on any non-subtask content, an
operation applies to the whole task including the subtasks. When
point is on a subtask, it applies to the subtask only. When a
subtask is shifted it is detached from its parent and promoted to
a task (by adjusting its header).

An alternative approach to task breakdown is where the task
consists of a number of 'components', all of which must be
completed before the task itself can be considered completed.
This situation is handled by means of an org-mode checkboxed list
item in the task body for each component. The task header line
can indicate the number or percentage of components currently
completed, and the task may be automatically shifted to DONE
once all components have been checked (configurable). Unlike
subtasks, the components are tightly integrated into the task."
 (make-local-variable 'org-cycle-separator-lines)
 (setq org-cycle-separator-lines 1)
 (turn-off-auto-fill)
 (add-hook 'kill-buffer-hook 'dkdo-RefreshTimerStop nil t)
 (add-hook 'org-checkbox-statistics-hook 'dkdo-CheckboxStatistics nil t)
 (if (equal (buffer-size) 0)
  (dkdo-BufferPrepare)
  (dkdo-BufferRefresh)
  (if (buffer-modified-p) (basic-save-buffer)))
 (dkdo-RefreshTimerStartIf)
 (goto-char (point-min))

 (define-key dkdo-mode-map "\C-c\C-xd" 'dkdo-TaskToDone)
 (define-key dkdo-mode-map "\C-c\C-xl" 'dkdo-TaskToLater)
 (define-key dkdo-mode-map "\C-c\C-xn" 'dkdo-TaskToNow)
 (define-key dkdo-mode-map "\C-c\C-xr" 'dkdo-BufferRefresh)
 (define-key dkdo-mode-map "\C-c\C-xs" 'dkdo-TaskStart))

(defun dkdo-SectionKeys()
 "Return a list containing the keys of dkdo-Sections."
 (let*
  ((Rv nil))
  (dolist (Elem dkdo-Sections)
   (setq Rv (cons (car Elem) Rv)))
  (reverse Rv)))

(defun dkdo-SectionText(Key)
 "Return the Section Header text for symbol KEY."
 (or
  (cdr (assq Key dkdo-Sections))
  "<none>"))

(defun dkdo-SectionSymbol(SectionText)
 "Return the symbol corresponding to SECTIONTEXT.
Return nil if SECTIONTEXT is unsupported."
 (or
  (car (rassoc SectionText dkdo-Sections))))

(defun dkdo-InSection(Section)
 "Return t if point is in SECTION."
 (eq (dkdo-SectionCurrent) Section))

(defun dkdo-SectionHeader(Key)
 "Return the full section header for KEY."
 (concat dkdo-SectionHeaderPrefix (dkdo-SectionText Key)
  dkdo-SectionHeaderSuffix))

(defun dkdo-SectionHeaderRe(&optional Key)
 "Return a section header regex for KEY.
If Key is nil, matches any non-empty section header."
 (concat dkdo-SectionHeaderRe
  (if Key (dkdo-SectionHeader Key) dkdo-ReHeadingTextAny)))

(defun dkdo-SectionPresent(Key)
 "Return the position of section KEY, or nil if it is not present."
 (save-excursion
  (dkdo-SectionTo Key t)))

(defun dkdo-SectionTo(&optional Key NoError)
 "Move to the start of section KEY.
If KEY is nil, move to the start of the current section. Section
start is the beginning of the header line. Returns the target
buffer position. Error if section not found, or return nil if
NOERROR is non-nil."
 (let*
  ((Re (dkdo-SectionHeaderRe Key))
   (Target (point)))
  (save-excursion
   (if Key
    ; To end of header of section Key.
    (progn
     (goto-char (point-min))
     (setq Target (re-search-forward Re nil t))))

    ; To header of current section.
    (if (not (looking-at Re))
     (progn
      ; In case we are in the middle of the section header regex, which
      ; would cause the search to fail.
      (end-of-line)
      (setq Target (re-search-backward Re nil 1)))))
  (if Target
   (goto-char Target)
   (or NoError
    (if Key
     (error (concat "Section: \"%s\" is missing from this buffer! "
      "Please create the missing section and re-visit the file!")
      (dkdo-SectionText Key))
     (error "No current section!"))))
  Target))

(defun dkdo-SectionToEnd(&optional Key NoError)
 "Move to just after the end of section KEY.
If KEY is nil, moves to the end of the current section. Note this
may be at the start of the next section which is in the next
section. However trailing empty lines are NOT included in the
section body. Return the target buffer position, or nil if
section not found, or error if NOERROR is nil."
 (if Key (dkdo-SectionTo Key NoError))
 (or (dkdo-SectionCurrent) (error "Not in a section!"))

 ; Avoid spurious match at start of current section.
 (if (looking-at dkdo-SectionHeaderRe) (forward-char))
 (if (re-search-forward dkdo-SectionHeaderRe nil t)
  (goto-char (match-beginning 0))
  (goto-char (point-max)))
 (dkdo-SkipEmptyLinesBackward)
 (point))

(defun dkdo-SectionCurrent()
 "Return the symbol for the current section, or nil if none."
 (save-excursion
  (dkdo-SectionTo nil t)
  (if (not (looking-at (dkdo-SectionHeaderRe)))
   nil
   (let*
    ((Start (re-search-forward dkdo-SectionHeaderPrefix))
     (End
      (progn
       (re-search-forward dkdo-SectionHeaderSuffix)
       (re-search-backward dkdo-SectionHeaderSuffix)))
     (Text (buffer-substring Start End)))
    (or (dkdo-SectionSymbol Text) Text)))))

(defun dkdo-SectionSortMaybe(&optional Section)
 "Sort SECTION (default current), but only if appropriate.
NOTE: This should be avoided as it seems to mess up undo."
 (save-excursion
  (and Section (dkdo-SectionTo Section))
  (let*
   ((Section (dkdo-SectionCurrent)))
   (if (and Section (not (eq Section 'dkdo-Now)))
    (progn
     (dkdo-SectionTo)
     (condition-case nil
       (progn
        (org-sort-entries nil ?a))
       (error nil)))))
  (dkdo-BufferFixAppearance)))

(defun dkdo-SectionForeach(Function &optional SkipMissing)
 "Apply FUNCTION to each section symbol.
Skip missing sections if SKIPMISSING."
 (save-excursion
  (dolist (Section (dkdo-SectionKeys))
   (if (or (not SkipMissing) (dkdo-SectionPresent Section))
    (funcall Function Section)))))

(defvar dkdo-RefreshTimer nil
 "Timer for repeatedly refreshing the buffer.")

(defun dkdo-RefreshTimerStartIf()
 "Start the Refresh Timer if appropriately configured.
First unconditionally stop any existing timer and then start a
new one if variable `dkdo-RefreshSeconds' is greater than zero.
Current buffer must be in Do List mode."
 (interactive)
 (if (not (equal major-mode 'dkdo-mode))
  (error "Not in dkdo-mode!"))
 (dkdo-RefreshTimerStop)
 (if (> dkdo-RefreshSeconds 0)
  (progn
   (message "Starting Refresh Timer...")
   (make-local-variable 'dkdo-RefreshTimer)
   (setq dkdo-RefreshTimer (run-at-time t dkdo-RefreshSeconds
    'dkdo-BufferTimerRefresh (current-buffer))))))

(defun dkdo-RefreshTimerStop()
 "Stop the current buffer refresh timer (if any).
Return t if a timer was actually in effect and stopped, nil
otherwise."
 (interactive)
 (if (equal major-mode 'dkdo-mode)
  (progn
   (make-local-variable 'dkdo-RefreshTimer)
   (let* ((Rv nil))
    (if dkdo-RefreshTimer
     (progn
      (message "Cancelling Refresh Timer...")
      (cancel-timer dkdo-RefreshTimer)
      (setq dkdo-RefreshTimer nil)
      (setq Rv t)))))))

(defun dkdo-BufferTimerRefresh(Buffer)
 "Invoke `dkdo-BufferRefresh' on BUFFER.
Call `dkdo-BufferRefresh' on behalf of the Refresh
Timer, with sort and other cosmetic adjustments disabled."
 (save-excursion
  (condition-case nil
   (progn
    (set-buffer Buffer)
    (dkdo-BufferRefresh t t))
   (error (dkdo-RefreshTimerStop)))))

(defun dkdo-BufferRefresh(&optional NoSort NoFixAppearance)
 "Scan the current buffer and perform various adjustments.
Move actionable tasks from section LATER to section NOW. If
NOSORT is nil, sort tasks in the Do List sections. If
NOFIXAPPEARANCE is nil, perform task folding and other cosmetic
adjustments."
 (interactive)

 ; It's a bad idea to move tasks around when there are
 ; merge conflicts to be resolved.
 (dkdo-BufferCheckForConflictMarker)
 (dkdo-BufferCheckStructure)
 (or NoSort (dkdo-SectionForeach 'dkdo-SectionSortMaybe))
 (if (dkdo-SectionPresent 'dkdo-Later)
  (let*
   ((Moved (dkdo-ActionableTasksLaterToNow)))
   (if (> Moved 0)
    (progn
     (dkmisc-Beep) 
     (message "Found %d actionable Task(s) in LATER." Moved)))
   (or NoFixAppearance (dkdo-BufferFixAppearance)))))

(defun dkdo-BufferCheckStructure()
 "Check all sections are present."
 (dkdo-SectionForeach (lambda(Section) "" (dkdo-SectionTo Section))))

(defun dkdo-BufferCheckForConflictMarker()
 "Error if buffer has a merge conflict marker."
 (save-excursion
  (goto-char (point-min))
  (if (re-search-forward dkmisc-ConflictMarkerRe nil t)
   (error
    (concat "File has merge conflicts! "
     "Cannot refresh until conflicts are resolved.")))))

(defun dkdo-BufferPrepare()
 "Prepare the current (empty) buffer.
Inserts empty sections or loads a sample do file if confirmed by
user. Buffer must be empty to begin with."
(unless
 (eq (buffer-size) 0)
 (error "Cannot prepare this buffer! It's not empty!"))
(if (yes-or-no-p "Empty dolist. Insert sections? ")
 (dkdo-BufferInsertSections)
 (if (yes-or-no-p "Empty dolist. Load sample do file? ")
  (dkdo-BufferLoadSample)))
(goto-char (point-min)))

(defun dkdo-BufferInsertSections()
 "Load the dkdo package sample do file into the current buffer.
Buffer must be empty to begin with."
(interactive)
(unless
 (eq (buffer-size) 0)
 (error "Need empty buffer to insert sections!"))
(dkdo-SectionForeach
 (lambda(Section)
  (insert "\n")
  (insert (concat (dkdo-SectionHeader Section) "\n")))))

(defun dkdo-BufferLoadSample()
 "Load the dkdo package sample do file into the current buffer.
Buffer must be empty to begin with."
(interactive)
(unless
 (eq (buffer-size) 0)
 (error "Need empty buffer to load sample!"))
(unless dkdo-SampleDoFilename
 (error "Package Load Directory is unknown!"))
(insert-file-contents dkdo-SampleDoFilename)
(dkdo-BufferRefresh))

(defun dkdo-BufferFixAppearance()
 "Fixe up the current buffer presentation."
 (org-content 2)
 (org-cycle-show-empty-lines t))

(defun dkdo-TaskToLater()
 "Shift the current task to section LATER.
Prompt for a new date/time or period of deferral."
(interactive)
(dkdo-TaskTo 'dkdo-Later))

(defun dkdo-TaskToNow()
 "Shift the current task to section NOW."
(interactive)
(dkdo-TaskTo 'dkdo-Now))

(defun dkdo-TaskToDone()
 "Shift the current task to section DONE."
(interactive)
(dkdo-TaskTo 'dkdo-Done))

(defun dkdo-TaskTo(To)
 "Shift the current task to section TO.
Return the final position."
 (interactive)
 (if (not (dkdo-TaskAtPoint)) (error "Not in a task!"))
 (save-excursion
  (dkdo-PromoteIfSubtask)

  (let*
   ((Seconds nil)
    (From (dkdo-SectionCurrent))
    (Destination nil)
    (Move t)
    (Repeater nil))

   ; Check for implicit subtask completion.
   (if (eq To 'dkdo-Done)
    (progn
     (if (and (not (dkdo-SubtaskAtPoint)) (dkdo-TaskHasSubtask))
      (error "Task has subtask(s). Please shift to DONE individually!"))))

   ; Find the repeater if any.
   (if (and (eq From 'dkdo-Later) (not (eq To 'dkdo-Later)))
    (setq Repeater (dkdo-TaskConvertTimestampRepeater)))

   ; Maybe insert/adjust timestamp.
   (if (eq To 'dkdo-Now)
    ; L->N N->N D->N No timestamp required.
    (progn
     (dkdo-TaskRemoveTimestampIfInDone)
     (setq Seconds (dkdo-TaskConvertTimestamp)))

    ; Marking task as finished.
    (if (and (eq To 'dkdo-Done) (not (eq From 'dkdo-Done)))
     ; L->D N->D Auto-stamp with current time.
     (setq Seconds (dkmisc-TimeParse (dkdo-TaskInsertTimestamp)))

     ; L->L N->L D->L D->D Prompt for timestamp.
     (let*
      ((Ignore nil)
       (Length nil)
       (Ts nil))

      (if (and (eq To 'dkdo-Later) (not (eq From 'dkdo-Later)))
       (progn
        ; N->L D->L Ignore current task timestamp.
        (setq Ignore t)
        (if (eq From 'dkdo-Done) (setq Length dkmisc-TimeYmdLen))))

      (setq Ts (dkdo-TaskPromptforTimestamp Ignore Length))
      (dkdo-TaskRemoveTimestampIfInDone)
      (setq Seconds (dkdo-TaskReplaceTimestamp Ts)))))

   ; Shift the task.
   (setq Destination
    (dkdo-TaskCopyTo (dkdo-SectionFindInsertPoint To Seconds) (not Repeater)))

   (if Repeater
    (progn
     ; Remove the repeater from the copied task to avoid duplication.
     (save-excursion
      ; Back into copied task.
      (goto-char (- Destination 1))
      (dkdo-TaskTimestampRemoveRepeaterMaybe)

      ; Adjust the return position to account for any deleted characters.
      (dkdo-ToTaskEnd)
      (setq Destination (point)))

     ; Remove the inserted timestamp to expose the repeater.
     (if (eq To 'dkdo-Done) (dkdo-TaskRemoveTimestampIfAny))

     ; Adjust the date and task position for the repeat.
     (let*
      ((NewSeconds (dkdo-TaskTimestampApplyRepeater)))
      (if NewSeconds
       (dkdo-TaskMoveTo
        (dkdo-SectionFindInsertPoint 'dkdo-Later NewSeconds))))))
   Destination)))

(defun dkdo-PromoteIfSubtask()
 "Promote a subtask to a task in the same section.
Move the subtask to the beginning of the current section and
change it to a task. Leave point on the promoted task ready for a
subsequent operation. Do not adjust dates etc. Do nothing if not
a subtask."
 (if (dkdo-SubtaskAtPoint)
  (progn
   (dkdo-ErrorIfSubtaskInDone)
   (let* ((Cs (dkdo-SectionCurrent)))
    (dkdo-TaskMoveToSection Cs t)
    (dkdo-ToFirstTaskPosition Cs)))))

(defun dkdo-ErrorIfSubtaskInDone()
 "Error if in subtask in section DONE."
 (and
  (dkdo-SubtaskAtPoint)
  (dkdo-InSection 'dkdo-Done)
  (error "Subtask in DONE is illegal")))

(defun dkdo-BadStartingSection(CurrentSection)
 "Error re CURRENTSECTION."
 (error "Bad starting Section: \"%s\"!" (dkdo-SectionText CurrentSection)))

(defun dkdo-SectionFindInsertPoint(Section Seconds)
 "Find the location in SECTION for a task with the specified time/date.
Location is after existing task with same date.
If SECONDS is nil the location is at the section start."
 (save-excursion
  (let*
   ((Target nil)
    (Done nil)
    (DtSeconds nil))

   ; Default to start of section.
   (dkdo-ToFirstTaskPosition Section)

   ; Already done?
   (if (or (not Seconds) (not (dkdo-TaskAtPoint)))
    (setq Done t)
    (if (and (not (dkdo-TaskGetTimestamp)) (not (dkdo-ToNextTimestampTask)))
     (setq Done t)))

   (while (not Done)
    (setq DtSeconds (dkdo-TaskConvertTimestamp))
    ; Ignore non-date entry. New entry inserts AFTER existing entry
    ; with same date.
    (if (and DtSeconds (> DtSeconds Seconds))
     (setq Done t)
     ; To Next timestamped task.
     (if (not (dkdo-ToNextTimestampTask))
      ; No next task. Target is end of current task.
      (setq Done t Target (dkdo-ToTaskEnd)))))
   (setq Target (point)))))

(defun dkdo-ActionableTasksLaterToNow()
 "Move actionable tasks from LATER to NOW.
Return the number of tasks actually moved."
 ; Avoid unintentional processing of DONE tasks where DONE header is
 ; missing/corrupt.
 (dkdo-BufferCheckStructure)
 (save-excursion
  (let*
   ((Done nil)
    (Rv 0))
   (if (and (dkdo-ToFirstTaskPosition 'dkdo-Later) (dkdo-TaskAtPoint))
    ; Traverse all later entries.
    (while (not Done)
     (if (not (dkdo-TaskActionable))
      (or (dkdo-ToNextTask) (setq Done t))
      (dkdo-TaskToNow)
      (setq Rv (+ Rv 1))
      (unless (dkdo-TaskAtPoint) (setq Done t)))))
   Rv)))

(defun dkdo-ToFirstTaskPosition(Key)
 "Move point to the start of the first task of section KEY.
If the section is empty, move to the point at which a first task
should be inserted. Return the new value of point. Error if no
suitable point found."
 (let*
  ((Target nil))
  (save-excursion
   (dkdo-SectionTo Key)
   (save-excursion
    ; Try for first task in section.
    (if (re-search-forward dkdo-ReTask nil t)
     (setq Target (match-beginning 0)))
    (unless (equal (dkdo-SectionCurrent) Key)
     (setq Target nil)))
   (if (null Target)
   ; No task. Move beyond section header.
    (setq Target (search-forward "\n" nil t))))
  (if (null Target)
   nil
   (goto-char Target))))

(defun dkdo-TaskAtPoint()
 "Return t if point is currently in an task."
 (and
  (not (looking-at dkdo-SectionHeaderRe))
  (or
   (looking-at dkdo-ReTask)
   (let*
    ((Cs (dkdo-SectionCurrent)))
    (save-excursion
     ; Would miss match if already in pattern.
     (end-of-line)
     (and
      (re-search-backward dkdo-ReTask nil t)
      (equal Cs (dkdo-SectionCurrent))))))))

(defun dkdo-ToNextTimestampTask()
 "Like `dkdo-ToNextTask', but go to the next task with a timestamp."
 (let*
  ((Done nil)
   (There nil)
   (Rv nil))
  (save-excursion
   (while (not Done)
    (setq There (dkdo-ToNextTask))
    (if (not There)
     (setq Done t)
     (if (dkdo-TaskGetTimestamp) (setq Done t Rv (point))))))
  (and Rv (goto-char Rv))
  Rv))

(defun dkdo-ToNextTask()
 "Move point to the start of the next task in the current section.
Return the new value of point. If no such task, return nil and do
not move point."
 (or
  (dkdo-TaskAtPoint)
  (error "Not in an task! Call dkdo-ToFirstTaskPosition first!"))
 (let*
  ((Target nil)
   (Cs (dkdo-SectionCurrent)))
  (save-excursion
   (if (looking-at dkdo-ReTask) (forward-char 1))
   (if (re-search-forward dkdo-ReTask nil t)
    (setq Target (match-beginning 0)))
   (unless (equal Cs (dkdo-SectionCurrent))
    (setq Target nil)))
  (if (null Target)
   nil
   (goto-char Target))))

(defun dkdo-ToTaskStart()
 "Move point to the start of the current task.
Return the new position."
 (or (dkdo-TaskAtPoint) (error "Not in a task!"))

 ; Already there?
 (if (looking-at dkdo-ReTask)
  (point)
  (let*
   ((Target nil)
    (Cs (dkdo-SectionCurrent)))
   (save-excursion
    ; Get out of pattern to avoid missing it.
    (end-of-line)
    (if (re-search-backward dkdo-ReTask nil t)
     (setq Target (point)))
    (unless (equal Cs (dkdo-SectionCurrent))
     (setq Target nil)))
   (if (null Target)
    nil
    (goto-char Target)))))

(defun dkdo-ToTaskContent()
 "Move point to the start of the current task's content.
Return the new position."
 (dkdo-ToTaskStart)
 (re-search-forward dkdo-ReTask))

(defun dkdo-ToTaskEnd()
 "Move point to just after the end of the current task body.
Note this is may be at the start of the next task which is in the
next task. However trailing empty lines are NOT included in the
task body. Return the final position."
 (or (dkdo-TaskAtPoint) (error "Not in a task!"))
 (or
  (dkdo-ToNextTask)
  (and
   (re-search-forward dkdo-SectionHeaderRe nil t)
   (goto-char (match-beginning 0)))
  (goto-char (point-max)))
 (dkdo-SkipEmptyLinesBackward)
 (point))

(defun dkdo-TaskHasSubtask()
 "Return t if the current task has at least one subtask."
 (let*
  ((End (dkdo-PointAfter 'dkdo-ToTaskEnd))
   (Rv nil))
  (save-excursion
   (dkdo-ToTaskStart)
   (if (re-search-forward dkdo-ReSubtask End t)
    (setq Rv t)))
  Rv))

(defun dkdo-SubtaskAtPoint()
 "Return t if point is currently in a subtask."
 (or
  (looking-at dkdo-ReSubtask)
  (let*
   ((Cs (dkdo-SectionCurrent)))
   (save-excursion
    ; Would miss match if already in pattern.
    (end-of-line)
    (and
     (re-search-backward dkdo-ReHeading nil t)
     (looking-at dkdo-ReSubtask))))))

(defun dkdo-SubtaskToStart()
 "Move point to the start of the current subtask.
Return the new position."
 (or (dkdo-SubtaskAtPoint) (error "Not in a subtask!"))
 (if (looking-at dkdo-ReSubtask)
  (point)
  (let*
   ((Target nil))
   (save-excursion
    ; Get out of pattern to avoid missing it.
    (end-of-line)
    (setq Target (re-search-backward dkdo-ReSubtask nil t)))
   (if Target (goto-char Target)))))
    
(defun dkdo-SubtaskToEnd()
 "Move to just after the end of the current subtask body.
Note this is may be at the start of the next task/subtask which
is in the next task/subtask. However trailing empty lines are NOT
included in the subtask body. Return the final position."
 (or (dkdo-SubtaskAtPoint) (error "Not in a subtask!"))
 (let*
  ((Target nil))
  (save-excursion
   ; Would miss match if already in pattern.
   (end-of-line)
   (if (re-search-forward dkdo-ReHeading)
    (setq Target (match-beginning 0))))
  (if Target
   (goto-char Target)
   (goto-char (point-max)))
  (dkdo-SkipEmptyLinesBackward)
  (point)))

(defun dkdo-SkipEmptyLinesBackward()
 "Move back over preceding empty lines.
Stop at the beginning of the last empty line encountered. The
line preceding the final position is non-empty or non-existent.
Return the number of lines skipped."
 (let* ((Rv 0))
  (while (dkdo-PreviousLineEmpty)
   (forward-line -1)
   (setq Rv (+ Rv 1)))
  Rv))

(defun dkdo-PreviousLineEmpty()
 "Return t if there is a previous line and it is empty."
 (save-excursion
  (let* ((Rv nil))
   (if (and (equal (forward-line -1) 0) (looking-at "^$"))
    (setq Rv t))
   Rv)))

(defun dkdo-TaskGetTimestamp()
 "Extract a timestamp from the beginning of the current task.
Return the timestamp as a string, or nil if none is present."
 (let*
  ((Rv nil))
  (save-excursion
   (dkdo-ToTaskContent)
   (if (looking-at dkmisc-DateTimeRe)
    (setq Rv (buffer-substring (match-beginning 0) (match-end 0)))))
  Rv))

(defun dkdo-TaskConvertTimestamp()
 "Extract and converts a timestamp from the beginning of the current task.
Returns the timestamp as float seconds, or nil if none is present."
 (let*
  ((Tss (dkdo-TaskGetTimestamp))
   (Rv nil))
  (if Tss
   (condition-case nil
    (setq Rv (dkmisc-TimeParse Tss))
    (error nil)))))

(defun dkdo-TaskGetTimestampRepeater()
 "Extract a timestamp repeater from the beginning of the current task.
Returns the repeater as a string, or nil if none is present."
 (let*
  ((Rv nil)
   (Re dkmisc-DateTimeRepeaterRe))
  (save-excursion
   (dkdo-ToTaskContent)
   (if (looking-at Re)
    (let*
     ((Start (match-beginning 8))
      (End (match-end 8)))
     (if (and Start End)
      (setq Rv (buffer-substring Start End)))))
  Rv)))

(defun dkdo-TaskConvertTimestampRepeater()
 "Extract and converts timestamp repeater from the current task.
Return the repeater as per `dkmisc-TimeParseShift', or nil if none."
 (let*
  ((Trs (dkdo-TaskGetTimestampRepeater))
   (Rv nil))
  (if Trs
   (condition-case nil
    (setq Rv (dkmisc-TimeParseShift Trs))
    (error nil)))))

(defun dkdo-TaskPromptforTimestamp(&optional Ignore Length)
 "Prompt for a timestamp for the current task.
Return the new timestamp in text form. Use value of any existing
timestamp as the base for partial/relative input unless IGNORE is
set, in which case use the current time as a base instead. If
LENGTH is specified it overrides the length of any current
timestamp as a default for the length/precision of the returned
timestamp. The actual returned length may be longer to accomodate
the precision of the user input."
 (let*
  ((OldTs (dkdo-TaskGetTimestamp))
   (Len (if Length Length (if OldTs (length OldTs) dkmisc-TimeYmdLen)))
   (Base (if (or Ignore (not OldTs)) (dkmisc-TimeCurrentText Len) OldTs))
   (Ts (dkmisc-TimePromptfor Base)))
  Ts))

(defun dkdo-TaskReplaceTimestamp(Timestamp)
 "Insert timestamp in the current task.
First remove any existing timestamp. Return the new timestamp in
float form."
 (let*
  ((Seconds (dkmisc-TimeParse Timestamp)))
  (dkdo-TaskRemoveTimestampIfAny)
  (dkdo-TaskInsertTimestamp Timestamp)
  Seconds))

(defun dkdo-TaskRemoveTimestampIfInDone()
 "Remove the timestamp from the current task if in section DONE."
 (and
  (dkdo-InSection 'dkdo-Done)
  (dkdo-TaskRemoveTimestampIfAny)))

(defun dkdo-TaskRemoveTimestampIfAny(&optional RemoveRepeater)
 "Remove the timestamp from the beginning of a task content (if any).
If REMOVEREPEATER also remove the repeater (if any). Also remove
any trailing space. Return the timestamp."
 (let*
  ((Ts (dkdo-TaskGetTimestamp))
   (Rv Ts))
  (if Ts
   (let* ((Re (concat (regexp-quote Ts) "[[:blank:]]*")))
    (if RemoveRepeater
     (let* ((Tr (dkdo-TaskGetTimestampRepeater)))
      (if Tr
       (progn
        (setq Rv (concat Ts " " Tr))
        (setq Re (concat Re (regexp-quote Tr) "[[:blank:]]*"))))))
   (save-excursion
    (dkdo-ToTaskStart)
    (if (re-search-forward Re nil t) (replace-match "")))))
  Rv))

(defun dkdo-TaskTimestampRemoveRepeaterMaybe()
 "Remove a repeater (if any), but leave the timestamp.
Work recursively through timestamps until repeater found."
 (let*
  ((Ts (dkdo-TaskGetTimestamp))
   (Repeater (dkdo-TaskGetTimestampRepeater)))
  (if Ts
   (progn
    (dkdo-TaskRemoveTimestampIfAny t)
    (if (not Repeater) (dkdo-TaskTimestampRemoveRepeaterMaybe))
    (dkdo-TaskInsertTimestamp Ts)))))

(defun dkdo-TaskInsertTimestamp(&optional Timestamp)
 "Inserts TIMESTAMP (text form) in the current task.
Return the value inserted (text form)."
 (save-excursion
  (let*
   ((Ts Timestamp))

   (if (not Timestamp)
    ; Default to current time and default length.
    (progn
     (setq Ts (dkmisc-TimeCurrentText dkdo-DoneTimestampLength))))
   (dkdo-ToTaskContent)
   (insert Ts)
   (or (looking-at "[[:blank:]]") (insert " "))
   Ts)))

(defun dkdo-TaskTimestampApplyRepeater()
 "Offset the task timestamp by the amount indicated its repeater.
Return the altered timestamp in float seconds. Preserve the
length of timestamp unless the offset is less than a day in which
case the timestamp is expanded to full length. Do nothing and
return nil if no timestamp or no repeater. "
 (save-excursion
  (let*
   ((Rv nil)
    (Ts (dkdo-TaskGetTimestamp)))
   (if Ts
    (let*
     ((Repeater (dkdo-TaskGetTimestampRepeater)))
     (if Repeater
      (let*
       ((NewTs (dkmisc-TimeApplyShift Ts Repeater))
        (Rv (dkmisc-TimeParse NewTs)))
       (dkdo-TaskRemoveTimestampIfAny)
       (dkdo-TaskInsertTimestamp NewTs)
       Rv)))))))

(defun dkdo-TaskActionable()
 "Return t if the current task is actionable."
 (let*
  ((Tts (dkdo-TaskConvertTimestamp)))
  (and Tts (dkdo-TimeActionable Tts))))

(defun dkdo-TimeActionable(Time)
 "Returns t if having due date TIME would render a task actionable."
 (dkmisc-TimeActionable Time (* dkdo-ActionableNoticeHours 3600)
  dkdo-ActionableSkipBeforeMidnight dkdo-ActionableSkipAfterMidnight))

(defun dkdo-TaskMoveToSection(Section &optional Subtask)
 "Move the current task/subtask to the beginning of SECTION.
If SUBTASK is non-nil, move a subtask, otherwise move a task A
subtask becomes a task when it is moved or copied."
 (dkdo-TaskCopyToSection Section t Subtask))

(defun dkdo-TaskMoveTo(Point &optional Subtask)
 "Move the current task/subtask to POINT.
If SUBTASK is non-nil, move a subtask, otherwise move a task. A
subtask becomes a task when it is moved or copied."
 (dkdo-TaskCopyTo Point t Subtask))

(defun dkdo-TaskCopyToSection(Section &optional Move Subtask)
 "Copy/move the current task/subtask to beginning of SECTION.
The current and target sections must be different unless the
current sectio is NOW. If MOVE, move instead of copying. If
SUBTASK is non-nil, copy/move a subtask, otherwise copy/move a
task. A subtask becomes a task when it is moved or copied."
 (let* ((Cs (dkdo-SectionCurrent)))
  (dkdo-TaskCopyTo
   (dkdo-PointAfter 'dkdo-ToFirstTaskPosition Section) Move Subtask)))

(defun dkdo-TaskCopyTo(Point &optional Move Subtask)
 "Copy/move the current task/subtask to POINT.
If MOVE, move instead of copying. If SUBTASK is non-nil,
copy/move a subtask, otherwise copy/move a task. A subtask
becomes a task when it is moved or copied. Return the position
immediately after the insert to facilitate a possible subsequent
insert."
 (let*
  ((Rv nil)
   (Cp (dkdo-PointAfter (if Subtask 'dkdo-SubtaskToStart 'dkdo-ToTaskStart))))

  (if (equal Cp Point) (error "Already there"))
  ; Deleting the current task/subtask invalidates Point if later in
  ; the buffer. Therefore copy first and then delete.
  (save-excursion
   (let*
   ((Tt (dkdo-TaskAsString Subtask)))

   ; Promote subtask to task. 
   (if Subtask (setq Tt (substring Tt 1)))
   (setq Rv (dkdo-TaskInsertText Tt Point))))
  (if Move (dkdo-TaskDelete Subtask))
  Rv))

(defun dkdo-TaskInsertText(TaskText Point)
 "Insert TASKTEXT at POINT.
Assume TASKTEXT is a valid task."
 (let* ((Rv nil))
  (save-excursion
   (goto-char Point)
   (insert TaskText)
   (setq Rv (point))

   ; Back into inserted task.
   (forward-char -1)
   (dkdo-TaskHideSubtree))
  Rv))

(defun dkdo-TaskHideSubtree()
 "Hide the task subtree without swallowing trailing empty lines."
 (save-excursion
  (let* ((Start (dkdo-ToTaskStart)))
   (dkdo-ToTaskEnd)
   ; Back so endline is not included.
   (if (bolp) (forward-char -1))
   (save-restriction
    (narrow-to-region Start (point))
    (goto-char Start)
    (hide-subtree)))))

(defun dkdo-TaskAsString(&optional Subtask)
 "Return the current task/subtask as a string.
If SUBTASK is non-nil, return subtask, otherwise return a task."
 (save-excursion
  (let*
   ((Start (if Subtask (dkdo-SubtaskToStart) (dkdo-ToTaskStart)))
    (End (if Subtask (dkdo-SubtaskToEnd) (dkdo-ToTaskEnd))))
   (buffer-substring Start End))))

(defun dkdo-TaskDelete(&optional Subtask)
 "Delete the current task/subtask.
If SUBTASK is non-nil, delete a subtask, otherwise delete a task."
 (save-excursion
  (let*
   ((Start (if Subtask (dkdo-SubtaskToStart) (dkdo-ToTaskStart)))
    (End (if Subtask (dkdo-SubtaskToEnd) (dkdo-ToTaskEnd))))
   (delete-region Start End))))

(defun dkdo-PointAfter(Function &rest Args)
 "Return the buffer position after invoking the supplied function.
Current buffer position is not affected."
 (save-excursion
  (apply Function Args)))

(defun dkdo-CheckboxStatistics()
 "Run when checkbox statistics are updated.
Check if the current task/subtask has a completed checkbox
cookie, and if so, shift it forward from NOW. Only active in
section NOW, and when `dkdo-AutoFinishCheckedTasks' is t."
 (if
  (and dkdo-AutoFinishCheckedTasks(dkdo-TaskAtPoint)
   (eq (dkdo-SectionCurrent) 'dkdo-Now))
  (save-excursion
   (if (dkdo-SubtaskAtPoint) (dkdo-SubtaskToStart) (dkdo-ToTaskStart))
   (let*
    ((Limit (save-excursion (end-of-line) (point))))
   (if (re-search-forward dkdo-ReCheckboxesDone Limit t)
    (dkdo-TaskToDone))))))
   
(defun dkdo-TaskStart()
 "Insert the beginning of a new task/subtask after the current."
 (interactive)
 (if (dkdo-SubtaskAtPoint)
  (progn
   (dkdo-SubtaskToEnd)
   (insert dkdo-PrefixSubtask "\n")
   (forward-char -1))
  (if (dkdo-TaskAtPoint)
   (progn
    (dkdo-ToTaskEnd)
    (insert dkdo-PrefixTask "\n")
    (forward-char -1)))))

(defun dkdo-IcalEventToDoList()
 "Extracts event from current buffer to task in do list."
 (let*
  ((Event (dkmisc-IcalGetEvent))
   (Task (dkdo-IcalEventToTaskString Event)))

  (dkdo-Edit nil)
  (dkdo-ToFirstTaskPosition 'dkdo-Later)
  (dkdo-TaskInsertText Task (point))))
  
(defun dkdo-IcalEventToTaskString(Event)
 "Converts the calendar event to a task string.
  Event is a list (DTSTART DTEND SUMMARY LOCATION) as
  returned by dkmisc-IcalGetEvent()."
 (let*
  ((Start (dkmisc-TimeParse (pop Event) t))
   (End (dkmisc-TimeParse (pop Event) t))
   (DurationSec (- End Start))
   (Summary (pop Event))
   (Location (pop Event)))
  (if (not (null dkdo-IcalLocation-function))
   (setq Location
    (apply dkdo-IcalLocation-function (list Location))))
  (format "** %s Cal: %s %s %s.\n"
   (substring (dkmisc-DateTimeToText Start) 0 -3)
   Summary
   Location
   (dkmisc-SecondsToShiftSingleUnit DurationSec))))

(provide 'dkdo)
