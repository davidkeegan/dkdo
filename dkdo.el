; Do mode for do list.
; (c) David Keegan 2010.
(require 'dkmisc)

(defvar dkdo-Filename "~/dk.do" "The dolist file name.")

(defconst dkdo-DefaultDoneTimestampLength dkmisc-TimeYmdhmLen
 "Date plus hours and minutes (no seconds).")

(defun dkdo-EditDolist(PrefixArg)
"Prepares to edit a dolist.
With a prefix argument, prompts for the filename, and visits it.
 Otherwise visits dknFilename unless already in a notebook."
(interactive "P")
(if PrefixArg
 (call-interactively 'find-file)
 (if (not (dkdo-InDolist))
  (find-file dkdo-Filename))))

(defun dkdo-InDolist()
"Returns t if current file is a dolist."
 (and
  (buffer-file-name)
  (string-match "\\.do$" (buffer-file-name))))

(define-derived-mode dkdo-mode org-mode "Do List Mode"
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
 (add-hook 'kill-buffer-hook 'dkdo-RefreshTimerStop nil t)
 (add-hook 'org-checkbox-statistics-hook 'dkdo-CheckboxStatistics nil t)
 (if (equal (buffer-size) 0)
  (dkdo-BufferPrepare)
  (dkdo-BufferRefresh)
  (if (buffer-modified-p) (basic-save-buffer)))
 (dkdo-RefreshTimerStart)
 (goto-char (point-min))

 ; DkTbd: Mode keys for shift to section.
 (define-key dkdo-mode-map "\C-c\C-xd" 'dkdo-TaskToDone)
 (define-key dkdo-mode-map "\C-c\C-xl" 'dkdo-TaskToLater)
 (define-key dkdo-mode-map "\C-c\C-xn" 'dkdo-TaskToNow)
 (define-key dkdo-mode-map "\C-c\C-xr" 'dkdo-BufferRefresh)
 (define-key dkdo-mode-map "\C-c\C-xs" 'dkdo-TaskStart))

(defun dkdo()
"Package implementing Do List Mode.

For more information see:
 `dkdo-mode'"
 (interactive)
 (describe-function 'dkdo))

(defconst dkdo-mode-hook nil
"Hooks called on entering Do List mode.")

(defconst dkdo-Sections
 '((dkdo-Now . "NOW")
   (dkdo-Later . "LATER")
   (dkdo-Done . "DONE"))
 "Do List section symbols/name association.")

(defun dkdo-SectionKeys()
"Returns a list containing the keys of dkdo-Sections."
 (let*
  ((Rv nil))
  (dolist (Elem dkdo-Sections)
   (setq Rv (cons (car Elem) Rv)))
  (reverse Rv)))

(defun dkdo-SectionText(Key)
"Returns the Section Header text for symbol Key"
 (or
  (cdr (assq Key dkdo-Sections))
  "<none>"))

(defun dkdo-SectionSymbol(SectionText)
"Returns the symbol corresponding to SectionText.
Returns nil if unsupported section."
 (or
  (car (rassoc SectionText dkdo-Sections))))

(defun dkdo-InSection(Section)
"Returns t if point is in Section."
 (eq (dkdo-SectionCurrent) Section))

(defconst dkdo-AutoFinishCheckedTasks nil
"If t tasks with checkboxes are auto-finished from NOW when
all boxes are checked.")

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

(defun dkdo-SectionHeader(Key)
"Returns the full section header for Key."
 (concat dkdo-SectionHeaderPrefix (dkdo-SectionText Key)
  dkdo-SectionHeaderSuffix))

(defun dkdo-SectionHeaderRe(&optional Key)
"Returns a section header regex for Key.
 If Key is nil, matches any non-empty section header."
 (concat dkdo-SectionHeaderRe
  (if Key (dkdo-SectionHeader Key) dkdo-ReHeadingTextAny)))

(defun dkdo-SectionPresent(Key)
"Returns the position of the section, or nil if it is not present."
 (save-excursion
  (dkdo-SectionTo Key t)))

(defun dkdo-SectionTo(&optional Key NoError)
 "Moves to the start of section Key. If Key is nil, moves to the
 start of the current section. Section start is beginning of
 header line. Returns the target buffer position, or nil if
 section not found."
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
"Moves to the just after the end of section Key.
If Key is nil, moves to the end of the current section. Note this
is may be at the start of the next section which is in the next
section. However trailing empty lines are NOT included in the
section body. Returns the target buffer position, or nil if
section not found."
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
"Returns the symbol for the current section, or nil if none."
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
"Sorts Section (default current), but only if appropriate.
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
"Applies Function to each section symbol."
 (save-excursion
  (dolist (Section (dkdo-SectionKeys))
   (if (or (not SkipMissing) (dkdo-SectionPresent Section))
    (funcall Function Section)))))

(defconst dkdo-RefreshTimerSeconds 1800
"The interval in seconds between automatic refreshes.")

(defvar dkdo-RefreshTimer nil
 "Timer for repeatedly refreshing the buffer.")

(defun dkdo-RefreshTimerStart()
"Starts the Refresh Timer (stops any existing timer first)."
 (interactive)
 (if (not (equal major-mode 'dkdo-mode))
  (error "Not in dkdo-mode!"))
 (dkdo-RefreshTimerStop)
 (message "Starting Refresh Timer...")
 (make-local-variable 'dkdo-RefreshTimer)
 (setq dkdo-RefreshTimer (run-at-time t
  dkdo-RefreshTimerSeconds 'dkdo-BufferTimerRefresh (current-buffer))))

(defun dkdo-RefreshTimerStop()
"Stops the current buffer refresh timer (if any).
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
"Invokes dkdo-BufferRefresh.
To be called by the Refresh Timer."
 (save-excursion
  (condition-case nil
   (progn
    (set-buffer Buffer)
    (dkdo-BufferRefresh t t))
   (error (dkdo-RefreshTimerStop)))))

(defun dkdo-BufferRefresh(&optional NoSort NoFixAppearance)
"Scans the buffer and performs various adjustments.
Ensures all necessary sections are present."
 (interactive)

 ; It's a bad idea to move tasks around when there are
 ; merge conflicts to be resolved.
 (dkdo-BufferCheckForConflictMarker)
 (dkdo-BufferCheckStructure)
 (or NoSort (dkdo-SectionForeach 'dkdo-SectionSortMaybe))
 (if (dkdo-SectionPresent 'dkdo-Later)
  (let*
   ((Moved (dkdo-LaterTasksDueToNow)))
   (if (> Moved 0)
    (progn
     (dkmisc-Beep) 
     (message "Found %d Task(s) due." Moved)))
   (or NoFixAppearance (dkdo-BufferFixAppearance)))))

(defun dkdo-BufferCheckStructure()
"Checks all sections are present."
 (dkdo-SectionForeach (lambda(Section) "" (dkdo-SectionTo Section))))

(defun dkdo-BufferCheckForConflictMarker()
"Error if buffer has merge conflict marker."
 (save-excursion
  (goto-char (point-min))
  (if (re-search-forward dkmisc-ConflictMarkerRe nil t)
   (error "File has merge conflicts! Cannot refresh."))))

(defun dkdo-BufferPrepare()
"Prepares the buffer by creating necessary sections.
Buffer must be empty to begin with."
(unless
 (eq (buffer-size) 0)
 (error "Cannot prepare this buffer! It's not empty!"))
(if (yes-or-no-p "Empty dolist. Insert sections? ")
 (dkdo-SectionForeach
  (lambda(Section)
   (insert "\n")
   (insert (concat (dkdo-SectionHeader Section) "\n")))))
(goto-char (point-min)))

(defun dkdo-BufferFixAppearance()
"Fixes up the buffer presentation."
 (org-content 2)
 (org-cycle-show-empty-lines t))

(defun dkdo-TaskToLater()
"Shifts the current task to LATER.
Prompts for a new date/time or period of deferral."
(interactive)
(dkdo-TaskTo 'dkdo-Later))

(defun dkdo-TaskToNow()
"Shifts the current task to NOW."
(interactive)
(dkdo-TaskTo 'dkdo-Now))

(defun dkdo-TaskToDone()
"Shifts the current task to DONE."
(interactive)
(dkdo-TaskTo 'dkdo-Done))

(defun dkdo-TaskTo(To)
"Shifts the current task to section To.
Returns the final position."
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
       (dkdo-TaskMoveTo (dkdo-SectionFindInsertPoint 'dkdo-Later NewSeconds))))))

   Destination)))

(defun dkdo-PromoteIfSubtask()
"Promotes a subtask to a task in the same section.
Moves the subtask to the beginning of the current section and
changes it to a task. Leaves point on the promoted task ready
for a subsequent operation. Does not adjust dates etc. Does
nothing if not a subtask."
 (if (dkdo-SubtaskAtPoint)
  (progn
   (dkdo-ErrorIfSubtaskInDone)
   (let* ((Cs (dkdo-SectionCurrent)))
    (dkdo-TaskMoveToSection Cs t)
    (dkdo-ToFirstTaskPosition Cs)))))

(defun dkdo-ErrorIfSubtaskInDone()
"Error if in subtask in DONE."
 (and
  (dkdo-SubtaskAtPoint)
  (dkdo-InSection 'dkdo-Done)
  (error "Subtask in DONE is illegal")))

(defun dkdo-BadStartingSection(CurrentSection)
"Error message and throw."
 (error "Bad starting Section: \"%s\"!" (dkdo-SectionText CurrentSection)))

(defun dkdo-SectionFindInsertPoint(Section Seconds)
"Finds the location in Section for a task with the specified time/date.
If Seconds is nil the location is at the section start."
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

(defun dkdo-LaterTasksDueToNow()
"Moves any due later entries to NOW.
Returns the number of tasks actually moved."
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
     (if (not (dkdo-TaskDue))
      (or (dkdo-ToNextTask) (setq Done t))
      (dkdo-TaskToNow)
      (setq Rv (+ Rv 1))
      (unless (dkdo-TaskAtPoint) (setq Done t)))))
   Rv)))

(defun dkdo-ToFirstTaskPosition(Key)
"Moves point to the start of the first task of section Key.
 If the section is empty, moves to the point at which a first
 task should be inserted. Returns the new value of point.
 Error if no suitable point found."
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
"Returns t if point is currently in an task."
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
"Like following, but goes to the next task with a timestamp."
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
"Moves point to the start of the next task in the current section.
 Returns the new value of point. If no such task, returns nil and
 does not move point."
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
"Moves point to the start of the current task.
Returns the new position."
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
"Moves point to the start of the current task's content.
 Returns the new position."
 (dkdo-ToTaskStart)
 (re-search-forward dkdo-ReTask))

(defun dkdo-ToTaskEnd()
"Moves point to just after the end of the current task body.
Note this is may be at the start of the next task which
is in the next task. However trailing empty lines are
NOT included in the task body. Returns the final position."
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
"Returns t if the current task has at least one subtask."
 (let*
  ((End (dkdo-PointAfter 'dkdo-ToTaskEnd))
   (Rv nil))
  (save-excursion
   (dkdo-ToTaskStart)
   (if (re-search-forward dkdo-ReSubtask End t)
    (setq Rv t)))
  Rv))

(defun dkdo-SubtaskAtPoint()
"Returns t if point is currently in a subtask."
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
"Moves point to the start of the current subtask.
Returns the new position."
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
"Moves to just after the end of the current subtask body.
Note this is may be at the start of the next task/subtask which
is in the next task/subtask. However trailing empty lines are NOT
included in the subtask body. Returns the final position."
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
"Moves back over preceding empty lines.
Stops at the beginning of the last empty line encountered. The
line preceding the final position is non-empty or non-existent.
Returns the number of lines skipped."
 (let* ((Rv 0))
  (while (dkdo-PreviousLineEmpty)
   (forward-line -1)
   (setq Rv (+ Rv 1)))
  Rv))

(defun dkdo-PreviousLineEmpty()
"Returns t if there is a previous line and it is empty."
 (save-excursion
  (let* ((Rv nil))
   (if (and (equal (forward-line -1) 0) (looking-at "^$"))
    (setq Rv t))
   Rv)))

(defun dkdo-TaskGetTimestamp()
"Extracts a timestamp from the beginning of the current task.
 Returns the timestamp as a string, or nil if none is present."
 (let*
  ((Rv nil))
  (save-excursion
   (dkdo-ToTaskContent)
   (if (looking-at dkmisc-DateTimeRe)
    (setq Rv (buffer-substring (match-beginning 0) (match-end 0)))))
  Rv))

(defun dkdo-TaskConvertTimestamp()
"Extracts and converts a timestamp from the beginning of the current task.
 Returns the timestamp as float seconds, or nil if none is present."
 (let*
  ((Tss (dkdo-TaskGetTimestamp))
   (Rv nil))
  (if Tss
   (condition-case nil
    (setq Rv (dkmisc-TimeParse Tss))
    (error nil)))))

(defun dkdo-TaskGetTimestampRepeater()
"Extracts a timestamp repeater from the beginning of the current task.
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
"Extracts and converts timestamp repeater from the current task.
 Returns the repeater as per dkmisc-TimeParseShift, or nil if none."
 (let*
  ((Trs (dkdo-TaskGetTimestampRepeater))
   (Rv nil))
  (if Trs
   (condition-case nil
    (setq Rv (dkmisc-TimeParseShift Trs))
    (error nil)))))

(defun dkdo-TaskPromptforTimestamp(&optional Ignore Length)
"Prompts for a timestamp for the current task.
 Returns the new timestamp in text form. The value of any
 existing timestamp is used as the base for partial/relative
 input unless Ignore is set in which case the current time is
 used instead. If Length is specified it overrides the length of
 any current timestamp as a default for the length/precision of
 the returned timestamp. The actual returned length may be longer
 to accomodate the precision of the user input."
 (let*
  ((OldTs (dkdo-TaskGetTimestamp))
   (Len (if Length Length (if OldTs (length OldTs) dkmisc-TimeYmdLen)))
   (Base (if (or Ignore (not OldTs)) (dkmisc-TimeCurrentText Len) OldTs))
   (Ts (dkmisc-TimePromptfor Base)))
  Ts))

(defun dkdo-TaskReplaceTimestamp(Timestamp)
"Inserts timestamp in the current task.
 Any existing timestamp is removed first. Returns the new timestamp
 in float form."
 (let*
  ((Seconds (dkmisc-TimeParse Timestamp)))
  (dkdo-TaskRemoveTimestampIfAny)
  (dkdo-TaskInsertTimestamp Timestamp)
  Seconds))

(defun dkdo-TaskRemoveTimestampIfInDone()
"Removes the timestamp from the current task if in DONE."
 (and
  (dkdo-InSection 'dkdo-Done)
  (dkdo-TaskRemoveTimestampIfAny)))

(defun dkdo-TaskRemoveTimestampIfAny(&optional RemoveRepeater)
"Removes the timestamp from the beginning of a task content (if any).
If RemoveRepeater also removes the repeater (if any).
Also removes any trailing space. Returns the timestamp."
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
"Removes a repeater (if any), but leaves the timestamp.
Works recursively through timestamps until repeater found."
 (let*
  ((Ts (dkdo-TaskGetTimestamp))
   (Repeater (dkdo-TaskGetTimestampRepeater)))
  (if Ts
   (progn
    (dkdo-TaskRemoveTimestampIfAny t)
    (if (not Repeater) (dkdo-TaskTimestampRemoveRepeaterMaybe))
    (dkdo-TaskInsertTimestamp Ts)))))

(defun dkdo-TaskInsertTimestamp(&optional Timestamp)
"Inserts a timestamp (text form) in the current task.
Returns the value inserted (text form)."
 (save-excursion
  (let*
   ((Ts Timestamp))

   (if (not Timestamp)
    ; Default to current time and default length.
    (progn
     (setq Ts (dkmisc-TimeCurrentText dkdo-DefaultDoneTimestampLength))))
   (dkdo-ToTaskContent)
   (insert Ts)
   (or (looking-at "[[:blank:]]") (insert " "))
   Ts)))

(defun dkdo-TaskTimestampApplyRepeater()
"Offsets the task timestamp by the amount indicated its repeater.
Returns the altered timestamp in float seconds. Preserves the
length of timestamp unless the offset is less than a day in which
case the timestamp is expanded to full length. Does nothing and
returns nil if no timestamp or no repeater. "
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

(defconst dkdo-DueHours 9.0
"Dated items become due this many hours before the timestamp.")

(defconst dkdo-DueSkipBeforeMidnight 3.0
"Hours before midnight to skip in due calculation.")

(defconst dkdo-DueSkipAfterMidnight 7.0
"Hours before midnight to skip in due calculation.")

(defun dkdo-TaskDue()
"Returns t if the current task is due."
 (let*
  ((Tts (dkdo-TaskConvertTimestamp)))
  (and Tts (dkdo-TimeDue Tts))))

(defun dkdo-TimeDue(Time)
"Returns t if Time is 'due'."
 (dkmisc-TimeDue Time (* dkdo-DueHours 3600)
  dkdo-DueSkipBeforeMidnight dkdo-DueSkipAfterMidnight))

(defun dkdo-TaskMoveToSection(Section &optional Subtask)
"Moves the current task/subtask to the beginning of Section.
A subtask becomes a task when it is moved or copied."
 (dkdo-TaskCopyToSection Section t Subtask))

(defun dkdo-TaskMoveTo(Point &optional Subtask)
"Moves the current task/subtask to Point.
A subtask becomes a task when it is moved or copied."
 (dkdo-TaskCopyTo Point t Subtask))

(defun dkdo-TaskCopyToSection(Section &optional Move Subtask)
"Copies the current task/subtask to beginning of Section.
The current and target sections must be different except for NOW.
A subtask becomes a task when it is moved or copied."
 (let* ((Cs (dkdo-SectionCurrent)))
  (dkdo-TaskCopyTo
   (dkdo-PointAfter 'dkdo-ToFirstTaskPosition Section) Move Subtask)))

(defun dkdo-TaskCopyTo(Point &optional Move Subtask)
"Copies the current task/subtask to Point.
Returns the position immediately after the insert to facilitate a
possible subsequent insert. A subtask becomes a task when it is
moved or copied."
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
"Inserts TaskText at Point.
Assumes TaskText is a valid task."
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
"Hides the task subtree without swallowing trailing empty lines."
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
"Returns the current task/subtask as a string."
 (save-excursion
  (let*
   ((Start (if Subtask (dkdo-SubtaskToStart) (dkdo-ToTaskStart)))
    (End (if Subtask (dkdo-SubtaskToEnd) (dkdo-ToTaskEnd))))
   (buffer-substring Start End))))

(defun dkdo-TaskDelete(&optional Subtask)
"Deletes the current task/subtask."
 (save-excursion
  (let*
   ((Start (if Subtask (dkdo-SubtaskToStart) (dkdo-ToTaskStart)))
    (End (if Subtask (dkdo-SubtaskToEnd) (dkdo-ToTaskEnd))))
   (delete-region Start End))))

(defun dkdo-PointAfter(Function &rest Args)
"Returns the buffer position after invoking the supplied function.
 Current buffer position is not affected."
 (save-excursion
  (apply Function Args)))

(defconst dkdo-ReCheckboxesNumericDone "\\[\\([0-9+]\\)/\\1\\]"
"Org mode checkbox statistics cookie indicating all done.")

(defconst dkdo-ReCheckboxesPercentDone "\\[100%\\]"
"Org mode percent checkbox statistics cookie indicating all done.")

(defconst dkdo-ReCheckboxesDone
 (concat dkdo-ReCheckboxesNumericDone "\\|" dkdo-ReCheckboxesPercentDone))

(defun dkdo-CheckboxStatistics()
"Run when checkbox statistics are updated.
Checks if the current task/subtask is has a completed checkbox
cookie, and if so shifts it forward from NOW. Only active in
section NOW, and when dkdo-AutoFinishCheckedTasks is t."
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
"Inserts the beginning of a new task/subtask after the current."
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

(provide 'dkdo)
