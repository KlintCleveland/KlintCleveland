;;; ============================================================
;;; FILE:        CentroidTag-V1-0.lsp
;;; DESCRIPTION: Prompts the user to select any set of entities,
;;;              computes the centroid of the selection as the
;;;              average of each entity's bounding-box center, then
;;;              draws a POINT at that centroid and a TEXT tag that
;;;              reads "CENTROID" slightly offset from the point.
;;;
;;; COMPANY:     digitalBitterroot
;;; AUTHOR:      Klint Cleveland
;;; CREATED:     2026-11-07
;;;
;;; LAST EDITED: 2026-11-07
;;; EDITED BY:   Klint Cleveland
;;;
;;; VERSION:     1.0.0
;;; CLIENT:      digitalBitterroot
;;; DOMAIN:      ANNOTATION
;;; ============================================================
;;;
;;; REVISION HISTORY:
;;;   1.0.0  2026-11-07  Klint Cleveland  Initial release
;;;
;;; ============================================================
;;;
;;; DEPENDENCIES:
;;;   - dB-Core: dB:save-state, dB:restore-state, dB:error-handler
;;;              (copied inline below — file is self-contained)
;;;   - LeeMac:  None — selection uses native (ssget)
;;;   - AutoCAD: 2000 or later (requires Visual LISP COM for
;;;              vla-GetBoundingBox; see NOTE in _EntityCenter)
;;;
;;; USAGE:
;;;   Load with (load "CentroidTag-V1-0.lsp") or via acaddoc.lsp
;;;   Commands: dB-CentroidTag
;;;
;;; ============================================================


;;; ============================================================
;;; ERROR HANDLING — GENERIC CORE
;;; ============================================================
;;; PURPOSE:
;;;   Standard digitalBitterroot error handler. Restores AutoCAD
;;;   state and reports errors cleanly.
;;;
;;; DO NOT INSTALL THIS DIRECTLY IN COMMANDS.
;;;   This function is called by the file-specific error handler
;;;   (dB:CentroidTag-error) which is what commands install.
;;;   Any command-specific error logic lives there, NOT here.
;;; ------------------------------------------------------------

(defun dB:error-handler (msg)
  ;;
  ;; LOCAL VARIABLES:
  ;;   msg  [STRING]  Error message passed by AutoLISP automatically
  ;;

  ;; User cancel is not an error — restore state but stay silent
  (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
    (princ (strcat "\n** dB Error: " msg " **"))
  )

  ;; Restore AutoCAD to pre-command state
  (dB:restore-state)

  ;; Close any open UNDO group
  (command "._UNDO" "_End")
  (princ)
)


;;; ============================================================
;;; ERROR HANDLING — FILE SPECIFIC
;;; ============================================================
;;; PURPOSE:
;;;   Entry point for all error handling in THIS FILE.
;;;   This is the function installed by every command in this file.
;;;
;;; DO NOT put command-specific logic in dB:error-handler.
;;;   That function is generic. This one is the right place for
;;;   anything specific to this file's commands.
;;; ------------------------------------------------------------

(defun dB:CentroidTag-error (msg)
  ;;
  ;; LOCAL VARIABLES:
  ;;   msg  [STRING]  Error message passed by AutoLISP automatically
  ;;

  ;; =========================================================
  ;; COMMAND-SPECIFIC ERROR CLEANUP — ADD CUSTOM LOGIC HERE
  ;; ---------------------------------------------------------
  ;;   (none — pass-through to generic handler)
  ;; =========================================================

  (dB:error-handler msg)
)


;;; ============================================================
;;; STATE MANAGEMENT
;;; ============================================================

;;; ------------------------------------------------------------
;;; FUNCTION: dB:save-state
;;; ------------------------------------------------------------
;;; PURPOSE:
;;;   Captures AutoCAD environment settings before command runs.
;;;   Only the variables this file actually changes are relevant;
;;;   the full canonical set is captured for safe restore.
;;; ------------------------------------------------------------

(defun dB:save-state ( / )
  (setq dB:saved-layer   (getvar "CLAYER"))
  (setq dB:saved-osmode  (getvar "OSMODE"))
  (setq dB:saved-cmdecho (getvar "CMDECHO"))
  (setq dB:saved-error   *error*)
)

;;; ------------------------------------------------------------
;;; FUNCTION: dB:restore-state
;;; ------------------------------------------------------------
;;; PURPOSE:
;;;   Restores AutoCAD to pre-command state.
;;;   Called at normal exit AND inside the error handler.
;;; ------------------------------------------------------------

(defun dB:restore-state ( / )
  (if dB:saved-layer   (setvar "CLAYER"   dB:saved-layer))
  (if dB:saved-osmode  (setvar "OSMODE"   dB:saved-osmode))
  (if dB:saved-cmdecho (setvar "CMDECHO"  dB:saved-cmdecho))
  (if dB:saved-error   (setq *error* dB:saved-error))
)


;;; ============================================================
;;; COMMAND WRAPPERS
;;; ============================================================
;;; Thin c: wrappers only. No logic lives here.
;;; All logic lives in the main program function below.

(defun c:dB-CentroidTag nil (dB-CentroidTag nil))


;;; ============================================================
;;; MAIN PROGRAM FUNCTION
;;; ============================================================

;;; ------------------------------------------------------------
;;; FUNCTION: dB-CentroidTag
;;; ------------------------------------------------------------
;;; PURPOSE:
;;;   Selects any set of entities, computes the selection centroid,
;;;   and annotates it. The centroid is defined (see ASSUMPTION
;;;   below) as the simple average of every selected entity's
;;;   bounding-box center. A bounding-box center is the midpoint of
;;;   the min and max corners returned by vla-GetBoundingBox.
;;;   This is robust across mixed entity types but is NOT an
;;;   area- or mass-weighted centroid.
;;;
;;; ASSUMPTION (centroid definition):
;;;   centroid = average over selected entities of
;;;              ( (bboxMin + bboxMax) / 2 )
;;;   Entities whose bounding box cannot be obtained are skipped.
;;;
;;; ARGUMENTS:
;;;   flag  [NIL/T]  Reserved for future command variants.
;;;                  Currently unused; pass nil.
;;;
;;; RETURNS:
;;;   [T]    on success (POINT and TEXT created)
;;;   [nil]  on empty/invalid selection or when no entity in the
;;;          selection yields a usable bounding box.
;;;
;;; CALLED BY:
;;;   User via command: dB-CentroidTag
;;;
;;; CALLS:
;;;   _EntityCenter  _AveragePoints  _EnsureLayer
;;;   dB:save-state  dB:restore-state  dB:CentroidTag-error
;;; ------------------------------------------------------------

(defun dB-CentroidTag ( flag / *error* _EntityCenter _AveragePoints
                               _EnsureLayer
                               sel ent lst cen pt1 idx cnt rtn )
  ;;
  ;; LOCAL VARIABLES:
  ;;   flag  [NIL/T]    Reserved variant flag (unused)
  ;;   sel   [PICKSET]  User selection set
  ;;   ent   [ENAME]    Current entity name in the loop
  ;;   lst   [LIST]     Accumulated bounding-box center points
  ;;   cen   [LIST]     Final centroid point (x y z)
  ;;   pt1   [LIST]     Offset insertion point for the text tag
  ;;   idx   [INTEGER]  Selection-set loop index
  ;;   cnt   [INTEGER]  Count of entities in the selection
  ;;   rtn   [T/NIL]    Return value
  ;;

  ;;----------------------------------------------------------
  ;; PRIVATE HELPER FUNCTIONS
  ;;----------------------------------------------------------

  ;; -- _EntityCenter -----------------------------------------
  ;; PRIVATE — internal to dB-CentroidTag
  ;; Returns the bounding-box center of one entity, or nil if the
  ;; bounding box cannot be obtained (e.g. some empty/degenerate
  ;; or non-graphical objects).
  ;; NOTE: vla-GetBoundingBox requires Visual LISP COM (vl-load-com
  ;;       at footer). Wrapped in vl-catch-all-apply so a single
  ;;       unsupported entity does not abort the whole command.
  (defun _EntityCenter ( ent / obj box mn mx )
    (setq obj (vlax-ename->vla-object ent))
    (if (not (vl-catch-all-error-p
               (vl-catch-all-apply
                 'vla-GetBoundingBox (list obj 'mn 'mx))))
      (progn
        (setq mn (vlax-safearray->list mn)
              mx (vlax-safearray->list mx))
        ;; Midpoint of the two corners = entity center
        (mapcar '(lambda (a b) (/ (+ a b) 2.0)) mn mx)
      )
    )
  )

  ;; -- _AveragePoints ----------------------------------------
  ;; PRIVATE — internal to dB-CentroidTag
  ;; Averages a non-empty list of points component-wise and
  ;; returns the mean point.
  (defun _AveragePoints ( box / cnt )
    (setq cnt (float (length box)))
    (mapcar '(lambda (v) (/ v cnt))
            (apply 'mapcar (cons '+ box)))
  )

  ;; -- _EnsureLayer ------------------------------------------
  ;; PRIVATE — internal to dB-CentroidTag
  ;; Creates the output layer if it does not already exist.
  ;; Leaves an existing layer untouched (color/state preserved).
  (defun _EnsureLayer ( lay col )
    (if (not (tblsearch "LAYER" lay))
      (entmake
        (list
          '(0 . "LAYER")
          '(100 . "AcDbSymbolTableRecord")
          '(100 . "AcDbLayerTableRecord")
          (cons 2 lay)
          (cons 70 0)
          (cons 62 col)
          '(6 . "Continuous")
        )
      )
    )
  )

  ;;----------------------------------------------------------
  ;; FILE-SPECIFIC ERROR HANDLER INSTALL POINT
  ;; The actual handler (dB:CentroidTag-error) is defined at
  ;; file scope; it is installed in the MAIN LOGIC block below.
  ;;----------------------------------------------------------

  ;;----------------------------------------------------------;;
  ;;                      Adjustments                          ;;
  ;;  Developer-facing settings for this program.              ;;
  ;;  Set per client / per deployment.                         ;;
  ;;  Users do not change these directly.                      ;;
  ;;----------------------------------------------------------;;
  (setq
    ;; Layer that the POINT and TEXT output are created on
    dB-lay    "dB-Centroid"

    ;; Color index assigned to the output layer when created
    dB-col    1

    ;; Text height for the "CENTROID" tag (drawing units)
    dB-thgt   0.125

    ;; X/Y offset of the text tag from the centroid point so the
    ;; label does not sit on top of the point node (drawing units)
    dB-off    0.125

    ;; Text string placed at the centroid
    dB-tag    "CENTROID"
  )
  ;;----------------------------------------------------------;;

  ;; NOTE: No getenv/setenv persistence is used. Every setting
  ;;       above is a developer-facing constant, not a runtime
  ;;       user choice remembered across sessions, so per the
  ;;       standard (Section 8/9) the registry layer is omitted.

  ;;----------------------------------------------------------
  ;; MAIN LOGIC
  ;;----------------------------------------------------------
  (dB:save-state)
  (setq *error* dB:CentroidTag-error)
  (command "._UNDO" "_Begin")
  (setvar "CMDECHO" 0)

  ;; Prompt for the selection (any entity types)
  (prompt "\nSelect entities for centroid: ")
  (setq sel (ssget))

  (cond
    ;; Empty or invalid selection — report and exit cleanly
    ((null sel)
     (prompt "\nNo entities selected. Nothing to do.")
     (setq rtn nil)
    )

    (t
     ;; Collect every obtainable bounding-box center
     (setq cnt (sslength sel)
           idx 0
           lst nil)
     (while (< idx cnt)
       (setq ent (ssname sel idx))
       (if (setq cen (_EntityCenter ent))
         (setq lst (cons cen lst))
       )
       (setq idx (1+ idx))
     )

     (cond
       ;; No entity yielded a usable bounding box
       ((null lst)
        (prompt "\nNo selected entity returned a bounding box. Nothing drawn.")
        (setq rtn nil)
       )

       (t
        ;; Average the collected centers into the final centroid
        (setq cen (_AveragePoints lst))

        ;; Ensure the output layer exists, then draw output on it
        (_EnsureLayer dB-lay dB-col)

        ;; POINT at the centroid
        (entmake
          (list
            '(0 . "POINT")
            (cons 8 dB-lay)
            (cons 10 cen)
          )
        )

        ;; TEXT tag, offset in +X/+Y so it clears the point node
        (setq pt1 (list (+ (car  cen) dB-off)
                        (+ (cadr cen) dB-off)
                        (caddr cen)))
        (entmake
          (list
            '(0 . "TEXT")
            (cons 8 dB-lay)
            (cons 10 pt1)
            (cons 40 dB-thgt)
            (cons 1 dB-tag)
          )
        )

        (prompt (strcat "\nCentroid placed at "
                        (rtos (car cen) 2 4) ", "
                        (rtos (cadr cen) 2 4) "."))
        (setq rtn t)
       )
     )
    )
  )

  (command "._UNDO" "_End")
  (dB:restore-state)
  (princ)
  rtn
)


;;; ============================================================
;;; SHARED LIBRARY FUNCTIONS
;;; ============================================================
;;; dB: functions used by this file are defined inline above
;;; (generic error handler, file-specific error handler, and
;;; state save/restore) so the file is fully self-contained.
;;; No additional dB-Core utilities are required.
;;; No Lee Mac functions are used by this file.


;;; ============================================================
;;; FILE LOAD INITIALIZATION
;;; ============================================================

;; Load COM support — required for vla-* functions
;; (vla-GetBoundingBox). Always present in every file.
(vl-load-com)

;; Load confirmation message
(princ
    (strcat
        "\n:: CentroidTag-V1-0.lsp"
        " | digitalBitterroot"
        " | Type dB-CentroidTag to run ::"
    )
)
(princ)   ; suppress nil echo at command line

;;; ============================================================
;;; END OF FILE: CentroidTag-V1-0.lsp
;;; digitalBitterroot
;;; ============================================================
