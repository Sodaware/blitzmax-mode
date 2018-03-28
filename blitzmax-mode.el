;;; blitzmax-mode.el --- A mode for editing BlitzMax programs.

;; Copyright (C) 2012-2017 Phil Newton

;; Author: Phil Newton
;; Keywords: language modes blitzmax

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Configuration:

;; Variables you may want to customize.

(defvar blitzmax-mode-indent 4
  "Default indentation per nesting level.")

(defvar blitzmax-mode-fontify-p t
  "Whether to fontify Basic buffers.")

(defvar blitzmax-mode-compiler-pathname nil
  "The full pathname of the BlitzMax compiler (i.e. bmk.")

(defvar blitzmax-mode-ide-pathname nil
  "The full pathname of the BlitzMax IDE (e.g. blide, blitzmax ide etc.")


;;; Code:

(defvar blitzmax-mode-hook ())

(defun blitzmax-mode--insert-comment (arg)
  "Comment or uncomment current line or region specific in ARG."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil)
        (comment-start "'")
        (comment-end ""))
    (comment-dwim arg)))

(defconst blitzmax-mode-type-start-regexp
  (concat
   "^[ \t]*[Tt]ype"
   "[ \t]+\\(\\w+\\)[ \t]*?"))

(defconst blitzmax-mode-type-end-regexp
  "^[ \t]*[Ee]nd[ ]*[Tt]ype")

(defconst blitzmax-mode-defun-start-regexp
  (concat
   "^[\t ]*\\([Mm]ethod\\|[Ff]unction\\)"
   "[ \t ]+\\(\\w+\\)[ \t ]*(?"))

(defconst blitzmax-mode-defun-end-regexp
  "^[ \t]*[Ee]nd[ \t]*\\([Mm]ethod\\|[Ff]unction\\)")

(defconst blitzmax-mode-if-regexp "^[ \t]*[Ii]f")
(defconst blitzmax-mode-if-oneline-regexp "^[ \t]*[Ii]f.*[Th]en[\\ t:]*[[:alnum:]]+[^']+?")
(defconst blitzmax-mode-else-regexp "^[ \t]*[Ee]lse\\([Ii]f\\)?")
(defconst blitzmax-mode-endif-regexp "[ \t]*[Ee]nd[ \t]*[Ii]f")

(defconst blitzmax-mode-continuation-regexp "[ \t]*\\.\\.")
(defconst blitzmax-mode-label-regexp "^[ \t]*#[a-zA-Z0-9_]+$")

(defconst blitzmax-mode-select-regexp "^[ \t]*[Ss]elect[ \t]+[Cc]ase")
(defconst blitzmax-mode-case-regexp "^[ \t]*[Cc]ase")
(defconst blitzmax-mode-select-end-regexp "^[ \t]*[Ee]nd[ \t]+[Ss]elect")

(defconst blitzmax-mode-for-regexp "^[ \t]*[Ff]or")
(defconst blitzmax-mode-next-regexp "^[ \t]*[Nn]ext")

(defconst blitzmax-mode-while-regexp "^[ \t]*[Ww]hile")
(defconst blitzmax-mode-wend-regexp "^[ \t]*[Ww]end")

(defconst blitzmax-mode-repeat-regexp "^[ \t]*[Rr]epeat")
(defconst blitzmax-mode-until-regexp "^[ \t]*[Uu]ntil")

(defconst blitzmax-mode-blank-regexp "^[ \t]*$")
(defconst blitzmax-mode-comment-regexp "^[ \t]*\\s<.*$")


;; BlitzMax Keywords

(defconst blitzmax-mode-all-keywords
  '("CreateStaticAudioSample" "TStreamWriteException" "TStreamReadException"
    "bglFixedFontBitmaps" "CountGraphicsModes" "CreateSocketStream"
    "CreateStaticPixmap" "GraphicsModeExists" "LittleEndianStream"
    "TAudioSampleLoader" "bglSetMouseVisible" "bglSetSwapInterval"
    "CreateAudioSample" "CreateGNetMessage" "GNetMessageObject" "GNetPeerTCPSocket"
    "GNetPeerUDPSocket" "GNetTotalBytesOut" "SetGraphicsDriver" "CreateBankStream"
    "CreateGNetObject" "CreateStaticBank" "GNetObjectRemote" "GNetTotalBytesIn"
    "SetChannelVolume" "SocketRemotePort" "StandardIOStream" "TStreamException"
    "bglAdjustTexSize" "bglCreateContext" "bglDeleteContext" "bglTexFromPixmap"
    "BigEndianStream" "CloseGNetObject" "CreateRamStream" "CreateTCPSocket"
    "CreateUDPSocket" "D3D7Max2DDriver" "GNetObjectLocal" "GNetObjectState"
    "GetGraphicsMode" "LoadAudioSample" "ResetCollisions" "SendGNetMessage"
    "SetChannelDepth" "SocketConnected" "SocketLocalPort" "SocketReadAvail"
    "bglDisplayModes" "AutoImageFlags" "ChannelPlaying" "CreateGNetHost"
    "GraphicsHeight" "ImagesCollide2" "MidHandleImage" "PixmapPixelPtr"
    "SetChannelRate" "SetImageHandle" "SocketRemoteIP" "TStreamFactory"
    "TStreamWrapper" "bglSwapBuffers" "AutoMidHandle" "CasedFileName"
    "CloseGNetHost" "CloseGNetPeer" "ConnectSocket" "ConvertPixmap" "DrawImageRect"
    "GLMax2DDriver" "GetGNetString" "GetGNetTarget" "GraphicsWidth" "ImagesCollide"
    "JoyButtonCaps" "ListFromArray" "LoadAnimImage" "LoadByteArray" "LoadImageFont"
    "LoadPixmapPNG" "ResumeChannel" "SaveByteArray" "SavePixmapPNG" "SetChannelPan"
    "SetGNetString" "SetGNetTarget" "SocketLocalIP" "TPixmapLoader" "AllocChannel"
    "BankCapacity" "CollideImage" "CreatePixmap" "GNetMessages" "GetGNetFloat"
    "GetImageFont" "GetLineWidth" "GetMaskColor" "ListAddFirst" "ListContains"
    "ListFindLink" "PauseChannel" "PixmapFormat" "PixmapHeight" "PixmapWindow"
    "ResizePixmap" "RuntimeError" "ScriptEngine" "SetGNetFloat" "SetImageFont"
    "SetLineWidth" "SetMaskColor" "SetTransform" "SocketAccept" "SocketListen"
    "TAudioSample" "CloseSocket" "CloseStream" "CollideRect" "CreateImage"
    "CreateTimer" "CurrentDate" "CurrentTime" "EndFunction" "EndGraphics"
    "FlushStream" "GNetConnect" "GNetObjects" "GetClsColor" "GetGraphics"
    "GetRotation" "GetViewport" "ImageHeight" "JoyAxisCaps" "ListAddLast"
    "ListIsEmpty" "ListToArray" "PixmapPitch" "PixmapWidth" "RequestFile"
    "RestoreData" "ReverseList" "SetClsColor" "SetFileMode" "SetRotation"
    "SetViewport" "StopChannel" "SuperStrict" "TBankStream" "UnlockImage"
    "WriteDouble" "WriteStderr" "WriteStdout" "WriteStream" "WriteString"
    "XFlipPixmap" "bglDrawText" "BindSocket" "CopyPixmap" "CopyStream" "CreateBank"
    "CreateFile" "CreateList" "CurrentDir" "DeleteFile" "DrawPixmap" "ExtractDir"
    "ExtractExt" "GNetAccept" "GNetListen" "GetGNetInt" "GrabPixmap" "ImageWidth"
    "ListRemove" "LoadPixmap" "LoadString" "MaskPixmap" "MemAlloced" "ModuleInfo"
    "OpenStream" "PeekDouble" "PokeDouble" "PollSystem" "ReadDouble" "ReadStream"
    "ReadString" "RemoveLink" "RenameFile" "RequestDir" "ResizeBank" "SaveString"
    "SeekStream" "SetGNetInt" "StreamSize" "StripSlash" "TextHeight" "WaitSystem"
    "WriteFloat" "WritePixel" "WriteShort" "uncompress" "ChangeDir" "ClearList"
    "CloseFile" "CopyBytes" "CountList" "CreateDir" "DebugStop" "DeleteDir"
    "DrawImage" "EndExtern" "EndMethod" "EndSelect" "FlushKeys" "Framework"
    "GNetPeers" "GetHandle" "GetOrigin" "GrabImage" "HideMouse" "IncbinLen"
    "IncbinPtr" "LaunchDir" "LoadImage" "LoadSound" "LockImage" "MemExtend"
    "MilliSecs" "MouseDown" "MoveMouse" "PeekFloat" "PeekShort" "PlaySound"
    "PokeFloat" "PokeShort" "ReadFloat" "ReadPixel" "ReadShort" "ReadStdin"
    "RndDouble" "SetHandle" "SetOrigin" "ShowMouse" "StreamPos" "SwapLists"
    "TListEnum" "TextWidth" "TileImage" "WaitMouse" "WaitTimer" "WriteBank"
    "WriteByte" "WriteFile" "WriteLine" "WriteLong" "compress2" "Abstract"
    "AppTitle" "BankSize" "CloseDir" "Continue" "CopyBank" "CueSound" "DebugLog"
    "DottedIP" "DrawLine" "DrawOval" "DrawPoly" "DrawRect" "DrawText" "FileMode"
    "FileSize" "FileTime" "FileType" "FlushMem" "Function" "Function" "GCMalloc"
    "GNetSync" "GetAlpha" "GetBlend" "GetColor" "GetScale" "Graphics" "HostName"
    "JoyCount" "JoyPitch" "JoyWheel" "LoadBank" "MemAlloc" "MemClear" "MemUsage"
    "MouseHit" "NextFile" "OpenFile" "PeekByte" "PeekLong" "PokeByte" "PokeLong"
    "ReadBank" "ReadByte" "ReadData" "ReadFile" "ReadLine" "ReadLong" "RealPath"
    "RndFloat" "SaveBank" "SetAlpha" "SetBlend" "SetColor" "SetScale" "SortList"
    "StripAll" "StripDir" "StripExt" "TCStream" "TChannel" "WaitChar" "WriteInt"
    "compress" "AppArgs" "AppFile" "BankBuf" "Confirm" "DefData" "Default" "EndType"
    "Extends" "Forever" "GetChar" "HostIps" "Include" "JoyDown" "JoyName" "JoyRoll"
    "KeyDown" "LoadDir" "LongBin" "LongHex" "MemCopy" "MemFree" "MemMove" "PeekInt"
    "PokeInt" "Private" "Proceed" "ReadDir" "Readint" "Release" "Replace" "Restore"
    "RndSeed" "SeedRnd" "TPixmap" "TStream" "WaitKey" "AppDir" "Assert" "Before"
    "Delete" "EachIn" "ElseIf" "EndRem" "EndTry" "Extern" "Global"
    "HostIp" "Import" "Incbin" "Insert" "JoyHat" "JoyYaw" "KeyHit" "Method" "Module"
    "MouseX" "MouseY" "MouseZ" "Notify" "Public" "Repeat" "Return" "Select"
    "SizeOf" "Strict" "TSound" "Varptr" "ATan2" "After" "Catch" "Const"
    "Delay" "EndIf"  "Field" "Final" "First" "Floor" "Gosub"
    "Input" "Instr" "Local" "Log10" "Lower" "OnEnd" "Print" "Right" "Super"
    "TBank" "TLink" "TList" "Throw" "Until" "Upper" "While" "ACos" "ASin" "ATan"
    "Case" "Ceil" "Cosh" "Data" "Each" "Else"  "Exit" "Flip" "Goto" "JoyR" "JoyU"
    "JoyV" "JoyX" "JoyY" "JoyZ" "LSet" "Last" "Left" "Next" "Plot"
    "RSet" "Rand" "Read" "Self" "Sinh" "Step" "Tanh" "Then" "Trim" "Type"
    "Wend" "Abs" "And" "Asc" "Bin" "Chr" "Cls" "Cos" "Dim" "End" "Eof" "Exp" "For"
    "Hex" "Len" "Log" "Max" "Mid" "Min" "Mod" "New" "Not" "Ptr" "Rem" "Rnd"
    "Sar" "Sgn" "Shl" "Shr" "Sin" "Sqr" "Str" "TIO" "Tan" "Try" "Var" "Xor" "YFl"
    "If" "Or" "Pi" "To"))

(defconst blitzmax-mode-type-keywords
  '("Int" "String" "Float" "Object" "Short" "Byte" "Long" "Double"))

(defconst blitzmax-mode-constant-keywords
  '("True" "False" "Null"))

;; Font-lock functions

(defun blitzmax-mode--fontify-buffer ()
  "Enable BlitzMax syntax highlighting for the current buffer."
  (let ((blitzmax-mode-keywords-regexp  (regexp-opt blitzmax-mode-all-keywords  'symbols))
        (blitzmax-mode-types-regexp     (regexp-opt blitzmax-mode-type-keywords 'symbols))
        (blitzmax-mode-constants-regexp (regexp-opt blitzmax-mode-constant-keywords 'symbols)))

    ;; Build highlight table.
    (make-local-variable 'blitzmax-mode-font-lock-keywords)
    (setq blitzmax-mode-font-lock-keywords
          `((,blitzmax-mode-types-regexp     . font-lock-type-face)
            (,blitzmax-mode-keywords-regexp  . font-lock-keyword-face)
            (,blitzmax-mode-constants-regexp . font-lock-constant-face)))

    ;; Highlight keywords.
    (setq font-lock-defaults
          '(blitzmax-mode-font-lock-keywords nil t))))


;; Indentation Functions

(defun blitzmax-mode--one-line-if-p ()
  "Check if the current IF statement is complete on a single line."
  (looking-at blitzmax-mode-if-oneline-regexp))

(defun blitzmax-mode--previous-line-of-code ()
  "Move to the previous line of code."
  (if (not (bobp))
      (forward-line -1))
  (while (and (not (bobp))
              (or (looking-at blitzmax-mode-blank-regexp)
                  (looking-at blitzmax-mode-comment-regexp)))
    (forward-line -1)))

(defun blitzmax-mode--find-original-statement ()
  "Move to original statement if current line is part of a continuation."
  (let ((here (point)))
    (blitzmax-mode--previous-line-of-code)
    (while (and (not (bobp))
                (looking-at blitzmax-mode-continuation-regexp))
      (setq here (point))
      (blitzmax-mode--previous-line-of-code))
    (goto-char here)))

(defun blitzmax-mode--find-matching-statement (open-regexp close-regexp)
  "Find the start of a pair that begins with OPEN-REGEXP and ends with CLOSE-REGEXP."
  ;; Searching backwards
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (blitzmax-mode--previous-line-of-code)
      (blitzmax-mode--find-original-statement)
      (cond ((looking-at close-regexp)
             (setq level (+ level 1)))
            ((looking-at open-regexp)
             (setq level (- level 1)))))))

(defun blitzmax-mode--find-matching-if ()
  "Find the start of an If/End If statement."
  (blitzmax-mode--find-matching-statement blitzmax-mode-if-regexp blitzmax-mode-endif-regexp))

(defun blitzmax-mode--find-matching-select ()
  "Find the start of an Select/End Select statement."
  (blitzmax-mode--find-matching-statement blitzmax-mode-select-regexp blitzmax-mode-select-end-regexp))

(defun blitzmax-mode--find-matching-for ()
  "Find the start of an For/Next statement."
  (blitzmax-mode--find-matching-statement blitzmax-mode-for-regexp blitzmax-mode-next-regexp))

(defun blitzmax-mode--find-matching-while ()
  "Find the start of an While/Wend statement."
  (blitzmax-mode--find-matching-statement blitzmax-mode-while-regexp blitzmax-mode-wend-regexp))

(defun blitzmax-mode--find-matching-repeat ()
  "Find the start of an Repeat/Until statement."
  (blitzmax-mode--find-matching-statement blitzmax-mode-repeat-regexp blitzmax-mode-until-regexp))

(defun blitzmax-mode--find-matching-defun ()
  "Find the start of a Method/End Method OR Function/End Function statement."
  (blitzmax-mode--find-matching-statement blitzmax-mode-defun-start-regexp blitzmax-mode-defun-end-regexp))

(defun blitzmax-mode--calculate-indent ()
  "Calculate the indent level for the current line."
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)

      ;; Don't indent if at the start or end of a type.
      (cond ((or (looking-at blitzmax-mode-type-start-regexp)
                 (looking-at blitzmax-mode-type-end-regexp))
             0)

            ;; Don't indent if on a label (#whatever).
            ((or (looking-at blitzmax-mode-label-regexp))
             0)

            ;; If in an Else/End If - indent to current indentation.
            ((or (looking-at blitzmax-mode-else-regexp)
                 (looking-at blitzmax-mode-endif-regexp))
             (blitzmax-mode--find-matching-if)
             (current-indentation))

            ;; Remove indent for end function / end method
            ((looking-at blitzmax-mode-defun-end-regexp)
             (blitzmax-mode--find-matching-defun)
             (current-indentation))

            ;; All the other matching pairs act alike.
            ;; For/Next
            ((looking-at blitzmax-mode-next-regexp)
             (blitzmax-mode--find-matching-for)
             (current-indentation))

            ;; while/wend
            ((looking-at blitzmax-mode-wend-regexp)
             (blitzmax-mode--find-matching-while)
             (current-indentation))

            ;; repeat/until
            ((looking-at blitzmax-mode-until-regexp)
             (blitzmax-mode--find-matching-repeat)
             (current-indentation))

            ;; select case/end select
            ((looking-at blitzmax-mode-select-end-regexp)
             (blitzmax-mode--find-matching-select)
             (current-indentation))

            ;; CASE within a SELECT block.
            ((looking-at blitzmax-mode-case-regexp)
             (blitzmax-mode--find-matching-select)
             (+ (current-indentation) blitzmax-mode-indent))

            ;; All other indentation depends on the previous lines.
            (t
             (blitzmax-mode--previous-line-of-code)

             ;; Skip over label lines, which always have 0 indent.
             (while (looking-at blitzmax-mode-label-regexp)
               (blitzmax-mode--previous-line-of-code))

             (cond
              ((looking-at blitzmax-mode-continuation-regexp)
               (blitzmax-mode--find-original-statement)
               ;; Indent continuation line under matching open paren,
               ;; or else one word in.
               (let* ((orig-stmt (point))
                      (matching-open-paren
                       (condition-case ()
                           (save-excursion
                             (goto-char original-point)
                             (beginning-of-line)
                             (backward-up-list 1)
                             ;; Only if point is now w/in cont. block.
                             (if (<= orig-stmt (point))
                                 (current-column)))
                         (error nil))))
                 (cond (matching-open-paren
                        (1+ matching-open-paren))
                       (t
                        ;; Else, after first word on original line.
                        (back-to-indentation)
                        (current-column)))))
              (t
               (blitzmax-mode--find-original-statement)
               (let ((indent (current-indentation)))
                 ;; All the various +indent regexps.
                 (cond ((looking-at blitzmax-mode-defun-start-regexp)
                        (+ indent blitzmax-mode-indent))

                       ((looking-at blitzmax-mode-type-start-regexp)
                        (+ indent blitzmax-mode-indent))

                       ;; "Else"/"ElseIf is always indented
                       ((looking-at blitzmax-mode-else-regexp)
                        (+ indent blitzmax-mode-indent))

                       ;; Check if the "If" is a multi-line one.
                       ((and (looking-at blitzmax-mode-if-regexp)
                             (not (blitzmax-mode--one-line-if-p)))
                        (+ indent blitzmax-mode-indent))

                       ((or (looking-at blitzmax-mode-select-regexp)
                            (looking-at blitzmax-mode-case-regexp))
                        (+ indent blitzmax-mode-indent))

                       ((or (looking-at blitzmax-mode-for-regexp)
                            (looking-at blitzmax-mode-while-regexp)
                            (looking-at blitzmax-mode-repeat-regexp))
                        (+ indent blitzmax-mode-indent))

                       ;; By default, just copy indent from prev line.
                       (t
                        indent))))))))))

(defun blitzmax-mode--indent-to-column (col)
  "Indent current line to COL."
  (let* ((bol (save-excursion
                (beginning-of-line)
                (point)))
         (point-in-whitespace
          (<= (point) (+ bol (current-indentation))))
         (blank-line-p
          (save-excursion
            (beginning-of-line)
            (looking-at blitzmax-mode-blank-regexp))))

    (cond ((/= col (current-indentation))
           (save-excursion
             (beginning-of-line)
             (back-to-indentation)
             (delete-region bol (point))
             (indent-to col))))

    ;; If point was in the whitespace, move back-to-indentation.
    (cond (blank-line-p
           (end-of-line))
          (point-in-whitespace
           (back-to-indentation)))))

(defun blitzmax-mode-indent-line ()
  "Indent the current line."
  (interactive)
  (blitzmax-mode--indent-to-column (blitzmax-mode--calculate-indent)))

;; Mode Registration

(define-derived-mode blitzmax-mode fundamental-mode
  "BlitzMax mode"
  "Major mode for editing BlitzMax source files."

  ;; Fontify buffer if enabled.
  (when blitzmax-mode-fontify-p
    (blitzmax-mode--fontify-buffer))

  ;; Indent lines using BlitzMax mode.
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'blitzmax-mode-indent-line)

  ;; Add keys.
  (define-key blitzmax-mode-map [remap comment-dwim] 'blitzmax-mode--insert-comment)

  ;; Comment: "'".
  (modify-syntax-entry ?\' "< b" blitzmax-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" blitzmax-mode-syntax-table)

  ;; Run hooks.
  (run-hooks 'blitzmax-mode-hook))

;;;###Autoload
(add-to-list 'auto-mode-alist '("\\.bmx\\'" . blitzmax-mode))

(provide 'blitzmax-mode)
;;; blitzmax-mode.el ends here
