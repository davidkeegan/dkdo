; Do mode for todo files.
; (c) David Keegan 2010.
(require 'dkMisc)

(defvar dkdFilename "~/dk.do" "The dolist file name.")

(defconst dkdDefaultDoneTimestampLength dkmTimeYmdhmLen
 "Date plus hours and minutes (no seconds).")

(defun dkdEditDolist(PrefixArg)
"Prepares to edit a dolist.
With a prefix argument, prompts for the filename, and visits it.
 Otherwise visits dknFilename unless already in a notebook."
(interactive "P")
(if PrefixArg
 (call-interactively 'find-file)
 (if (not (dkdInDolist))
  (find-file dkdFilename))))

(defun dkdInDolist()
"Returns t if current file is a dolist."
 (and
  (buffer-file-name)
  (string-match "\\.do$" (buffer-file-name))))

(define-derived-mode dkDo-mode org-mode "Do List Mode"
"A do file has top-level Sections NOW, LATER, and DONE. Each
second-level header within a section is a task. A third-level
header within a task is a subtask. The task/subtask content
starts immediately after the header prefix and includes the
remainder of the header and any content, including any lower
level sections (which are not significant to this mode).

Dated tasks start life in LATER, other tasks in NOW. Tasks are
generally shifted LATER->NOW to be worked on (this shift can be
manual or automatic) and NOW->DONE when finished. On entering
DONE tasks are prefixed with a timestamp indicating the date of
completion. Tasks can also be shifted DONE->NOW if they are being
re-visited (the DONE timestamp is removed), or NOW-LATER to be
deferred (eg pending some other event). A task shifting to LATER
is prefixed with a new due date which is prompted for.

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
 (add-hook 'kill-buffer-hook 'dkdRefreshTimerStop nil t)
 (add-hook 'org-checkbox-statistics-hook 'dkdCheckboxStatistics nil t)
 (if (equal (buffer-size) 0)
  (dkdBufferPrepare)
  (dkdBufferRefresh)
  (if (buffer-modified-p) (basic-save-buffer)))
 (dkdRefreshTimerStart)
 (beginning-of-buffer)

 ; DkTbd: Mode keys for shift to section.
 (define-key dkDo-mode-map "\C-c\C-xd" 'dkdTaskToDone)
 (define-key dkDo-mode-map "\C-c\C-xl" 'dkdTaskToLater)
 (define-key dkDo-mode-map "\C-c\C-xn" 'dkdTaskToNow)
 (define-key dkDo-mode-map "\C-c\C-xr" 'dkdBufferRefresh)
 (define-key dkDo-mode-map "\C-c\C-xs" 'dkdTaskStart))

(defun dkdDo()
"Package implementing Do List Mode.

For more information see:
 `dkDo-mode'"
 (interactive)
 (describe-function 'dkdDo))

(defgroup dkdDo nil
"Do List Mode."
 :version "21.2"
 :tag "Do List Mode"
 :group 'dkmDk)

(defconst dkDo-mode-hook nil
"Hooks called on entering Do List mode.")

(defconst dkdSections
 '((dkdNow . "NOW")
   (dkdLater . "LATER")
   (dkdDone . "DONE"))
 "Do List section symbols/name association.")

(defun dkdSectionKeys()
"Returns a list containing the keys of dkdSections."
 (let*
  ((Rv nil))
  (dolist (Elem dkdSections)
   (setq Rv (cons (car Elem) Rv)))
  (reverse Rv)))

(defun dkdSectionText(Key)
"Returns the Section Header text for symbol Key"
 (or
  (cdr (assq Key dkdSections))
  "<none>"))

(defun dkdSectionSymbol(SectionText)
"Returns the symbol corresponding to SectionText.
Returns nil if unsupported section."
 (or
  (car (rassoc SectionText dkdSections))))

(defun dkdInSection(Section)
"Returns t if point is in Section."
 (eq (dkdSectionCurrent) Section))

(defconst dkdAutoFinishCheckedTasks nil
"If t tasks with checkboxes are auto-finished from NOW when
all boxes are checked.")

(defconst dkdSectionHeaderPrefix "* ")
(defconst dkdSectionHeaderSuffix ":")

(defconst dkdSectionHeaderRe (concat "^\\" dkdSectionHeaderPrefix)
"Start of a section header. Use function of same name.")

(defconst dkdReHeadingTextAny "[^ 	\n]"
"First character of valid section header text.")

(defconst dkdReHeading "^[*]\\{1,3\\} "
"Only outline levels 1, 2, and 3 are significant.")

(defconst dkdPrefixTask "** ")
(defconst dkdReTask "^\\*\\* ")
(defconst dkdPrefixSubtask "*** ")
(defconst dkdReSubtask "^\\*\\*\\* ")

(defun dkdSectionHeader(Key)
"Returns the full section header for Key."
 (concat dkdSectionHeaderPrefix (dkdSectionText Key)
  dkdSectionHeaderSuffix))

(defun dkdSectionHeaderRe(&optional Key)
"Returns a section header regex for Key.
 If Key is nil, matches any non-empty section header."
 (concat dkdSectionHeaderRe
  (if Key (dkdSectionHeader Key) dkdReHeadingTextAny)))

(defun dkdSectionPresent(Key)
"Returns the position of the section, or nil if it is not present."
 (save-excursion
  (dkdSectionTo Key t)))

(defun dkdSectionTo(&optional Key NoError)
 "Moves to the start of section Key. If Key is nil, moves to the
 start of the current section. Section start is beginning of
 header line. Returns the target buffer position, or nil if
 section not found."
 (let*
  ((Re (dkdSectionHeaderRe Key))
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
      (dkdSectionText Key))
     (error "No current section!"))))
  Target))

(defun dkdSectionToEnd(&optional Key NoError)
"Moves to the just after the end of section Key.
If Key is nil, moves to the end of the current section. Note this
is may be at the start of the next section which is in the next
section. However trailing empty lines are NOT included in the
section body. Returns the target buffer position, or nil if
section not found."
 (if Key (dkdSectionTo Key NoError))
 (or (dkdSectionCurrent) (error "Not in a section!"))

 ; Avoid spurious match at start of current section.
 (if (looking-at dkdSectionHeaderRe) (forward-char))
 (if (re-search-forward dkdSectionHeaderRe nil t)
  (goto-char (match-beginning 0))
  (goto-char (point-max)))
 (dkdSkipEmptyLinesBackward)
 (point))

(defun dkdSectionCurrent()
"Returns the symbol for the current section, or nil if none."
(save-excursion
 (dkdSectionTo nil t)
 (if (not (looking-at (dkdSectionHeaderRe)))
  nil
  (let*
   ((Start (re-search-forward dkdSectionHeaderPrefix))
    (End
     (progn
      (re-search-forward dkdSectionHeaderSuffix)
      (re-search-backward dkdSectionHeaderSuffix)))
    (Text (buffer-substring Start End)))
   (or (dkdSectionSymbol Text) Text)))))

(defun dkdSectionSortMaybe(&optional Section)
"Sorts Section (default current), but only if appropriate.
NOTE: This should be avoided as it seems to mess up undo."
 (save-excursion
  (and Section (dkdSectionTo Section))
  (let*
   ((Section (dkdSectionCurrent)))
   (if (and Section (not (eq Section 'dkdNow)))
    (progn
     (dkdSectionTo)
     (condition-case nil
       (progn
        (org-sort-entries nil ?a))
       (error nil)))))
  (dkdBufferFixAppearance)))

(defun dkdSectionForeach(Function &optional SkipMissing)
"Applies Function to each section symbol."
 (save-excursion
  (dolist (Section (dkdSectionKeys))
   (if (or (not SkipMissing) (dkdSectionPresent Section))
    (funcall Function Section)))))

(defconst dkdRefreshTimerSeconds 1800
"The interval in seconds between automatic refreshes.")

(defvar dkdRefreshTimer nil
 "Timer for repeatedly refreshing the buffer.")

(defun dkdRefreshTimerStart()
"Starts the Refresh Timer (stops any existing timer first)."
 (interactive)
 (if (not (equal major-mode 'dkDo-mode))
  (error "Not in dkDo-mode!"))
 (dkdRefreshTimerStop)
 (message "Starting Refresh Timer...")
 (make-local-variable 'dkdRefreshTimer)
 (setq dkdRefreshTimer (run-at-time t
  dkdRefreshTimerSeconds 'dkdBufferTimerRefresh (current-buffer))))

(defun dkdRefreshTimerStop()
"Stops the current buffer refresh timer (if any).
Return t if a timer was actually in effect and stopped, nil
otherwise."
 (interactive)
 (if (equal major-mode 'dkDo-mode)
  (progn
   (make-local-variable 'dkdRefreshTimer)
   (let* ((Rv nil))
    (if dkdRefreshTimer
     (progn
      (message "Cancelling Refresh Timer...")
      (cancel-timer dkdRefreshTimer)
      (setq dkdRefreshTimer nil)
      (setq Rv t)))))))

(defun dkdBufferTimerRefresh(Buffer)
"Invokes dkdBufferRefresh.
To be called by the Refresh Timer."
 (save-excursion
  (condition-case nil
   (progn
    (set-buffer Buffer)
    (dkdBufferRefresh t t))
   (error (dkdRefreshTimerStop)))))

(defun dkdBufferRefresh(&optional NoSort NoFixAppearance)
"Scans the buffer and performs various adjustments.
Ensures all necessary sections are present."
 (interactive)

 ; It's a bad idea to move tasks around when there are
 ; merge conflicts to be resolved.
 (dkdBufferCheckForConflictMarker)
 (dkdBufferCheckStructure)
 (or NoSort (dkdSectionForeach 'dkdSectionSortMaybe))
 (if (dkdSectionPresent 'dkdLater)
  (let*
   ((Moved (dkdLaterTasksDueToNow)))
   (if (> Moved 0)
    (progn
     (dkBeep) 
     (message "Found %d Task(s) due." Moved)))
   (or NoFixAppearance (dkdBufferFixAppearance)))))

(defun dkdBufferCheckStructure()
"Checks all sections are present."
 (dkdSectionForeach (lambda(Section) "" (dkdSectionTo Section))))

(defun dkdBufferCheckForConflictMarker()
"Error if buffer has merge conflict marker."
 (save-excursion
  (goto-char (point-min))
  (if (re-search-forward dkmConflictMarkerRe nil t)
   (error "File has merge conflicts! Cannot refresh."))))

(defun dkdBufferPrepare()
"Prepares the buffer by creating necessary sections.
Buffer must be empty to begin with."
(unless
 (eq (buffer-size) 0)
 (error "Cannot prepare this buffer! It's not empty!"))
(if (yes-or-no-p "Empty dolist. Insert sections? ")
 (dkdSectionForeach
  (lambda(Section)
   (insert "\n")
   (insert (concat (dkdSectionHeader Section) "\n")))))
(goto-char (point-min)))

(defun dkdBufferFixAppearance()
"Fixes up the buffer presentation."
 (org-content 2)
 (org-cycle-show-empty-lines t))

(defun dkdTaskToLater()
"Shifts the current task to LATER.
Prompts for a new date/time or period of deferral."
(interactive)
(dkdTaskTo 'dkdLater))

(defun dkdTaskToNow()
"Shifts the current task to NOW."
(interactive)
(dkdTaskTo 'dkdNow))

(defun dkdTaskToDone()
"Shifts the current task to DONE."
(interactive)
(dkdTaskTo 'dkdDone))

(defun dkdTaskTo(To)
"Shifts the current task to section To.
Returns the final position."
 (interactive)
 (if (not (dkdTaskAtPoint)) (error "Not in a task!"))
 (save-excursion
  (dkdPromoteIfSubtask)

  (let*
   ((Seconds nil)
    (From (dkdSectionCurrent))
    (Destination nil)
    (Move t)
    (Repeater nil))

   ; Check for implicit subtask completion.
   (if (eq To 'dkdDone)
    (progn
     (if (and (not (dkdSubtaskAtPoint)) (dkdTaskHasSubtask))
      (error "Task has subtask(s). Please shift to DONE individually!"))))

   ; Find the repeater if any.
   (if (and (eq From 'dkdLater) (not (eq To 'dkdLater)))
    (setq Repeater (dkdTaskConvertTimestampRepeater)))

   ; Maybe insert/adjust timestamp.
   (if (eq To 'dkdNow)
    ; L->N N->N D->N No timestamp required.
    (progn
     (dkdTaskRemoveTimestampIfInDone)
     (setq Seconds (dkdTaskConvertTimestamp)))

    ; Marking task as finished.
    (if (and (eq To 'dkdDone) (not (eq From 'dkdDone)))
     ; L->D N->D Auto-stamp with current time.
     (setq Seconds (dkmTimeParse (dkdTaskInsertTimestamp)))

     ; L->L N->L D->L D->D Prompt for timestamp.
     (let*
      ((Ignore nil)
       (Length nil)
       (Ts nil))

      (if (and (eq To 'dkdLater) (not (eq From 'dkdLater)))
       (progn
        ; N->L D->L Ignore current task timestamp.
        (setq Ignore t)
        (if (eq From 'dkdDone) (setq Length dkmTimeYmdLen))))

      (setq Ts (dkdTaskPromptforTimestamp Ignore Length))
      (dkdTaskRemoveTimestampIfInDone)
      (setq Seconds (dkdTaskReplaceTimestamp Ts)))))

   ; Shift the task.
   (setq Destination
    (dkdTaskCopyTo (dkdSectionFindInsertPoint To Seconds) (not Repeater)))

   (if Repeater
    (progn
     ; Remove the repeater from the copied task to avoid duplication.
     (save-excursion
      ; Back into copied task.
      (goto-char (- Destination 1))
      (dkdTaskTimestampRemoveRepeaterMaybe)

      ; Adjust the return position to account for any deleted characters.
      (dkdToTaskEnd)
      (setq Destination (point)))

     ; Remove the inserted timestamp to expose the repeater.
     (if (eq To 'dkdDone) (dkdTaskRemoveTimestampIfAny))

     ; Adjust the date and task position for the repeat.
     (let*
      ((NewSeconds (dkdTaskTimestampApplyRepeater)))
      (if NewSeconds
       (dkdTaskMoveTo (dkdSectionFindInsertPoint 'dkdLater NewSeconds))))))

   Destination)))

(defun dkdPromoteIfSubtask()
"Promotes a subtask to a task in the same section.
Moves the subtask to the beginning of the current section and
changes it to a task. Leaves point on the promoted task ready
for a subsequent operation. Does not adjust dates etc. Does
nothing if not a subtask."
 (if (dkdSubtaskAtPoint)
  (progn
   (dkdErrorIfSubtaskInDone)
   (let* ((Cs (dkdSectionCurrent)))
    (dkdTaskMoveToSection Cs t)
    (dkdToFirstTaskPosition Cs)))))

(defun dkdErrorIfSubtaskInDone()
"Error if in subtask in DONE."
 (and
  (dkdSubtaskAtPoint)
  (dkdInSection 'dkdDone)
  (error "Subtask in DONE is illegal")))

(defun dkdBadStartingSection(CurrentSection)
"Error message and throw."
 (error "Bad starting Section: \"%s\"!" (dkdSectionText CurrentSection)))

(defun dkdSectionFindInsertPoint(Section Seconds)
"Finds the location in Section for a task with the specified time/date.
If Seconds is nil the location is at the section start."
 (save-excursion
  (let*
   ((Target nil)
    (Done nil)
    (DtSeconds nil))

   ; Default to start of section.
   (dkdToFirstTaskPosition Section)

   ; Already done?
   (if (or (not Seconds) (not (dkdTaskAtPoint)))
    (setq Done t)
    (if (and (not (dkdTaskGetTimestamp)) (not (dkdToNextTimestampTask)))
     (setq Done t)))

   (while (not Done)
    (setq DtSeconds (dkdTaskConvertTimestamp))
    ; Ignore non-date entry. New entry inserts AFTER existing entry
    ; with same date.
    (if (and DtSeconds (> DtSeconds Seconds))
     (setq Done t)
     ; To Next timestamped task.
     (if (not (dkdToNextTimestampTask))
      ; No next task. Target is end of current task.
      (setq Done t Target (dkdToTaskEnd)))))
   (setq Target (point)))))

(defun dkdLaterTasksDueToNow()
"Moves any due later entries to NOW.
Returns the number of tasks actually moved."
 ; Avoid unintentional processing of DONE tasks where DONE header is
 ; missing/corrupt.
 (dkdBufferCheckStructure)
 (save-excursion
  (let*
   ((Done nil)
    (Rv 0))
   (if (and (dkdToFirstTaskPosition 'dkdLater) (dkdTaskAtPoint))
    ; Traverse all later entries.
    (while (not Done)
     (if (not (dkdTaskDue))
      (or (dkdToNextTask) (setq Done t))
      (dkdTaskToNow)
      (setq Rv (+ Rv 1))
      (unless (dkdTaskAtPoint) (setq Done t)))))
   Rv)))

(defun dkdToFirstTaskPosition(Key)
"Moves point to the start of the first task of section Key.
 If the section is empty, moves to the point at which a first
 task should be inserted. Returns the new value of point.
 Error if no suitable point found."
 (let*
  ((Target nil))
  (save-excursion
   (dkdSectionTo Key)
   (save-excursion
    ; Try for first task in section.
    (if (re-search-forward dkdReTask nil t)
     (setq Target (match-beginning 0)))
    (unless (equal (dkdSectionCurrent) Key)
     (setq Target nil)))
   (if (null Target)
   ; No task. Move beyond section header.
    (setq Target (search-forward "\n" nil t))))
  (if (null Target)
   nil
   (goto-char Target))))

(defun dkdTaskAtPoint()
"Returns t if point is currently in an task."
 (and
  (not (looking-at dkdSectionHeaderRe))
  (or
   (looking-at dkdReTask)
   (let*
    ((Cs (dkdSectionCurrent)))
    (save-excursion
     ; Would miss match if already in pattern.
     (end-of-line)
     (and
      (re-search-backward dkdReTask nil t)
      (equal Cs (dkdSectionCurrent))))))))

(defun dkdToNextTimestampTask()
"Like following, but goes to the next task with a timestamp."
 (let*
  ((Done nil)
   (There nil)
   (Rv nil))
  (save-excursion
   (while (not Done)
    (setq There (dkdToNextTask))
    (if (not There)
     (setq Done t)
     (if (dkdTaskGetTimestamp) (setq Done t Rv (point))))))
  (and Rv (goto-char Rv))
  Rv))

(defun dkdToNextTask()
"Moves point to the start of the next task in the current section.
 Returns the new value of point. If no such task, returns nil and
 does not move point."
 (or
  (dkdTaskAtPoint)
  (error "Not in an task! Call dkdToFirstTaskPosition first!"))
 (let*
  ((Target nil)
   (Cs (dkdSectionCurrent)))
  (save-excursion
   (if (looking-at dkdReTask) (forward-char 1))
   (if (re-search-forward dkdReTask nil t)
    (setq Target (match-beginning 0)))
   (unless (equal Cs (dkdSectionCurrent))
    (setq Target nil)))
  (if (null Target)
   nil
   (goto-char Target))))

(defun dkdToTaskStart()
"Moves point to the start of the current task.
Returns the new position."
 (or (dkdTaskAtPoint) (error "Not in a task!"))

 ; Already there?
 (if (looking-at dkdReTask)
  (point)
  (let*
   ((Target nil)
    (Cs (dkdSectionCurrent)))
   (save-excursion
    ; Get out of pattern to avoid missing it.
    (end-of-line)
    (if (re-search-backward dkdReTask nil t)
     (setq Target (point)))
    (unless (equal Cs (dkdSectionCurrent))
     (setq Target nil)))
   (if (null Target)
    nil
    (goto-char Target)))))

(defun dkdToTaskContent()
"Moves point to the start of the current task's content.
 Returns the new position."
 (dkdToTaskStart)
 (re-search-forward dkdReTask))

(defun dkdToTaskEnd()
"Moves point to just after the end of the current task body.
Note this is may be at the start of the next task which
is in the next task. However trailing empty lines are
NOT included in the task body. Returns the final position."
 (or (dkdTaskAtPoint) (error "Not in a task!"))
 (or
  (dkdToNextTask)
  (and
   (re-search-forward dkdSectionHeaderRe nil t)
   (goto-char (match-beginning 0)))
  (goto-char (point-max)))
 (dkdSkipEmptyLinesBackward)
 (point))

(defun dkdTaskHasSubtask()
"Returns t if the current task has at least one subtask."
 (let*
  ((End (dkdPointAfter 'dkdToTaskEnd))
   (Rv nil))
  (save-excursion
   (dkdToTaskStart)
   (if (re-search-forward dkdReSubtask End t)
    (setq Rv t)))
  Rv))

(defun dkdSubtaskAtPoint()
"Returns t if point is currently in a subtask."
 (or
  (looking-at dkdReSubtask)
  (let*
   ((Cs (dkdSectionCurrent)))
   (save-excursion
    ; Would miss match if already in pattern.
    (end-of-line)
    (and
     (re-search-backward dkdReHeading nil t)
     (looking-at dkdReSubtask))))))

(defun dkdSubtaskToStart()
"Moves point to the start of the current subtask.
Returns the new position."
 (or (dkdSubtaskAtPoint) (error "Not in a subtask!"))
 (if (looking-at dkdReSubtask)
  (point)
  (let*
   ((Target nil))
   (save-excursion
    ; Get out of pattern to avoid missing it.
    (end-of-line)
    (setq Target (re-search-backward dkdReSubtask nil t)))
   (if Target (goto-char Target)))))
    
(defun dkdSubtaskToEnd()
"Moves to just after the end of the current subtask body.
Note this is may be at the start of the next task/subtask which
is in the next task/subtask. However trailing empty lines are NOT
included in the subtask body. Returns the final position."
 (or (dkdSubtaskAtPoint) (error "Not in a subtask!"))
 (let*
  ((Target nil))
  (save-excursion
   ; Would miss match if already in pattern.
   (end-of-line)
   (if (re-search-forward dkdReHeading)
    (setq Target (match-beginning 0))))
  (if Target
   (goto-char Target)
   (goto-char (point-max)))
  (dkdSkipEmptyLinesBackward)
  (point)))

(defun dkdSkipEmptyLinesBackward()
"Moves back over preceding empty lines.
Stops at the beginning of the last empty line encountered. The
line preceding the final position is non-empty or non-existent.
Returns the number of lines skipped."
 (let* ((Rv 0))
  (while (dkdPreviousLineEmpty)
   (forward-line -1)
   (setq Rv (+ Rv 1)))
  Rv))

(defun dkdPreviousLineEmpty()
"Returns t if there is a previous line and it is empty."
 (save-excursion
  (let* ((Rv nil))
   (if (and (equal (forward-line -1) 0) (looking-at "^$"))
    (setq Rv t))
   Rv)))

(defun dkdTaskGetTimestamp()
"Extracts a timestamp from the beginning of the current task.
 Returns the timestamp as a string, or nil if none is present."
 (let*
  ((Rv nil))
  (save-excursion
   (dkdToTaskContent)
   (if (looking-at dkmDateTimeRe)
    (setq Rv (buffer-substring (match-beginning 0) (match-end 0)))))
  Rv))

(defun dkdTaskConvertTimestamp()
"Extracts and converts a timestamp from the beginning of the current task.
 Returns the timestamp as float seconds, or nil if none is present."
 (let*
  ((Tss (dkdTaskGetTimestamp))
   (Rv nil))
  (if Tss
   (condition-case nil
    (setq Rv (dkmTimeParse Tss))
    (error nil)))))

(defun dkdTaskGetTimestampRepeater()
"Extracts a timestamp repeater from the beginning of the current task.
 Returns the repeater as a string, or nil if none is present."
 (let*
  ((Rv nil)
   (Re dkmDateTimeRepeaterRe))
  (save-excursion
   (dkdToTaskContent)
   (if (looking-at Re)
    (let*
     ((Start (match-beginning 8))
      (End (match-end 8)))
     (if (and Start End)
      (setq Rv (buffer-substring Start End)))))
  Rv)))

(defun dkdTaskConvertTimestampRepeater()
"Extracts and converts timestamp repeater from the current task.
 Returns the repeater as per dkmTimeParseShift, or nil if none."
 (let*
  ((Trs (dkdTaskGetTimestampRepeater))
   (Rv nil))
  (if Trs
   (condition-case nil
    (setq Rv (dkmTimeParseShift Trs))
    (error nil)))))

(defun dkdTaskPromptforTimestamp(&optional Ignore Length)
"Prompts for a timestamp for the current task.
 Returns the new timestamp in text form. The value of any
 existing timestamp is used as the base for partial/relative
 input unless Ignore is set in which case the current time is
 used instead. If Length is specified it overrides the length of
 any current timestamp as a default for the length/precision of
 the returned timestamp. The actual returned length may be longer
 to accomodate the precision of the user input."
 (let*
  ((OldTs (dkdTaskGetTimestamp))
   (Len (if Length Length (if OldTs (length OldTs) dkmTimeYmdLen)))
   (Base (if (or Ignore (not OldTs)) (dkmTimeCurrentText Len) OldTs))
   (Ts (dkmTimePromptfor Base)))
  Ts))

(defun dkdTaskReplaceTimestamp(Timestamp)
"Inserts timestamp in the current task.
 Any existing timestamp is removed first. Returns the new timestamp
 in float form."
 (let*
  ((Seconds (dkmTimeParse Timestamp)))
  (dkdTaskRemoveTimestampIfAny)
  (dkdTaskInsertTimestamp Timestamp)
  Seconds))

(defun dkdTaskRemoveTimestampIfInDone()
"Removes the timestamp from the current task if in DONE."
 (and
  (dkdInSection 'dkdDone)
  (dkdTaskRemoveTimestampIfAny)))

(defun dkdTaskRemoveTimestampIfAny(&optional RemoveRepeater)
"Removes the timestamp from the beginning of a task content (if any).
If RemoveRepeater also removes the repeater (if any).
Also removes any trailing space. Returns the timestamp."
 (let*
  ((Ts (dkdTaskGetTimestamp))
   (Rv Ts))
  (if Ts
   (let* ((Re (concat (regexp-quote Ts) "[[:blank:]]*")))
    (if RemoveRepeater
     (let* ((Tr (dkdTaskGetTimestampRepeater)))
      (if Tr
       (progn
        (setq Rv (concat Ts " " Tr))
        (setq Re (concat Re (regexp-quote Tr) "[[:blank:]]*"))))))
   (save-excursion
    (dkdToTaskStart)
    (if (re-search-forward Re nil t) (replace-match "")))))
  Rv))

(defun dkdTaskTimestampRemoveRepeaterMaybe()
"Removes a repeater (if any), but leaves the timestamp.
Works recursively through timestamps until repeater found."
 (let*
  ((Ts (dkdTaskGetTimestamp))
   (Repeater (dkdTaskGetTimestampRepeater)))
  (if Ts
   (progn
    (dkdTaskRemoveTimestampIfAny t)
    (if (not Repeater) (dkdTaskTimestampRemoveRepeaterMaybe))
    (dkdTaskInsertTimestamp Ts)))))

(defun dkdTaskInsertTimestamp(&optional Timestamp)
"Inserts a timestamp (text form) in the current task.
Returns the value inserted (text form)."
 (save-excursion
  (let*
   ((Ts Timestamp))

   (if (not Timestamp)
    ; Default to current time and default length.
    (progn
     (setq Ts (dkmTimeCurrentText dkdDefaultDoneTimestampLength))))
   (dkdToTaskContent)
   (insert Ts)
   (or (looking-at "[[:blank:]]") (insert " "))
   Ts)))

(defun dkdTaskTimestampApplyRepeater()
"Offsets the task timestamp by the amount indicated its repeater.
Returns the altered timestamp in float seconds. Preserves the
length of timestamp unless the offset is less than a day in which
case the timestamp is expanded to full length. Does nothing and
returns nil if no timestamp or no repeater. "
 (save-excursion
  (let*
   ((Rv nil)
    (Ts (dkdTaskGetTimestamp)))
   (if Ts
    (let*
     ((Repeater (dkdTaskGetTimestampRepeater)))
     (if Repeater
      (let*
       ((NewTs (dkmTimeApplyShift Ts Repeater))
        (Rv (dkmTimeParse NewTs)))
       (dkdTaskRemoveTimestampIfAny)
       (dkdTaskInsertTimestamp NewTs)
       Rv)))))))

(defconst dkdDueHours 9.0
"Dated items become due this many hours before the timestamp.")

(defconst dkdDueSkipBeforeMidnight 3.0
"Hours before midnight to skip in due calculation.")

(defconst dkdDueSkipAfterMidnight 7.0
"Hours before midnight to skip in due calculation.")

(defun dkdTaskDue()
"Returns t if the current task is due."
 (let*
  ((Tts (dkdTaskConvertTimestamp)))
  (and Tts (dkdTimeDue Tts))))

(defun dkdTimeDue(Time)
"Returns t if Time is 'due'."
 (dkmTimeDue Time (* dkdDueHours 3600)
  dkdDueSkipBeforeMidnight dkdDueSkipAfterMidnight))

(defun dkdTaskMoveToSection(Section &optional Subtask)
"Moves the current task/subtask to the beginning of Section.
A subtask becomes a task when it is moved or copied."
 (dkdTaskCopyToSection Section t Subtask))

(defun dkdTaskMoveTo(Point &optional Subtask)
"Moves the current task/subtask to Point.
A subtask becomes a task when it is moved or copied."
 (dkdTaskCopyTo Point t Subtask))

(defun dkdTaskCopyToSection(Section &optional Move Subtask)
"Copies the current task/subtask to beginning of Section.
The current and target sections must be different except for NOW.
A subtask becomes a task when it is moved or copied."
 (let* ((Cs (dkdSectionCurrent)))
  (dkdTaskCopyTo
   (dkdPointAfter 'dkdToFirstTaskPosition Section) Move Subtask)))

(defun dkdTaskCopyTo(Point &optional Move Subtask)
"Copies the current task/subtask to Point.
Returns the position immediately after the insert to facilitate a
possible subsequent insert. A subtask becomes a task when it is
moved or copied."
 (let*
  ((Rv nil)
   (Cp (dkdPointAfter (if Subtask 'dkdSubtaskToStart 'dkdToTaskStart))))

  (if (equal Cp Point) (error "Already there"))
  ; Deleting the current task/subtask invalidates Point if later in
  ; the buffer. Therefore copy first and then delete.
  (save-excursion
   (let*
   ((Tt (dkdTaskAsString Subtask)))

   ; Promote subtask to task. 
   (if Subtask (setq Tt (substring Tt 1)))
   (setq Rv (dkdTaskInsertText Tt Point))))
  (if Move (dkdTaskDelete Subtask))
  Rv))

(defun dkdTaskInsertText(TaskText Point)
"Inserts TaskText at Point.
Assumes TaskText is a valid task."
 (let* ((Rv nil))
  (save-excursion
   (goto-char Point)
   (insert TaskText)
   (setq Rv (point))

   ; Back into inserted task.
   (forward-char -1)
   (dkdTaskHideSubtree))
  Rv))

(defun dkdTaskHideSubtree()
"Hides the task subtree without swallowing trailing empty lines."
 (save-excursion
  (let* ((Start (dkdToTaskStart)))
   (dkdToTaskEnd)
   ; Back so endline is not included.
   (if (bolp) (forward-char -1))
   (save-restriction
    (narrow-to-region Start (point))
    (goto-char Start)
    (hide-subtree)))))

(defun dkdTaskAsString(&optional Subtask)
"Returns the current task/subtask as a string."
 (save-excursion
  (let*
   ((Start (if Subtask (dkdSubtaskToStart) (dkdToTaskStart)))
    (End (if Subtask (dkdSubtaskToEnd) (dkdToTaskEnd))))
   (buffer-substring Start End))))

(defun dkdTaskDelete(&optional Subtask)
"Deletes the current task/subtask."
 (save-excursion
  (let*
   ((Start (if Subtask (dkdSubtaskToStart) (dkdToTaskStart)))
    (End (if Subtask (dkdSubtaskToEnd) (dkdToTaskEnd))))
   (delete-region Start End))))

(defun dkdPointAfter(Function &rest Args)
"Returns the buffer position after invoking the supplied function.
 Current buffer position is not affected."
 (save-excursion
  (apply Function Args)))

(defconst dkdReCheckboxesNumericDone "\\[\\([0-9+]\\)/\\1\\]"
"Org mode checkbox statistics cookie indicating all done.")

(defconst dkdReCheckboxesPercentDone "\\[100%\\]"
"Org mode percent checkbox statistics cookie indicating all done.")

(defconst dkdReCheckboxesDone
 (concat dkdReCheckboxesNumericDone "\\|" dkdReCheckboxesPercentDone))

(defun dkdCheckboxStatistics()
"Run when checkbox statistics are updated.
Checks if the current task/subtask is has a completed checkbox
cookie, and if so shifts it forward from NOW. Only active in
section NOW, and when dkdAutoFinishCheckedTasks is t."
 (if
  (and dkdAutoFinishCheckedTasks(dkdTaskAtPoint)
   (eq (dkdSectionCurrent) 'dkdNow))
  (save-excursion
   (if (dkdSubtaskAtPoint) (dkdSubtaskToStart) (dkdToTaskStart))
   (let*
    ((Limit (save-excursion (end-of-line) (point))))
   (if (re-search-forward dkdReCheckboxesDone Limit t)
    (dkdTaskToDone))))))
   
(defun dkdTaskStart()
"Inserts the beginning of a new task/subtask after the current."
 (interactive)
 (if (dkdSubtaskAtPoint)
  (progn
   (dkdSubtaskToEnd)
   (insert dkdPrefixSubtask "\n")
   (forward-char -1))
  (if (dkdTaskAtPoint)
   (progn
    (dkdToTaskEnd)
    (insert dkdPrefixTask "\n")
    (forward-char -1)))))

(provide 'dkDo)
