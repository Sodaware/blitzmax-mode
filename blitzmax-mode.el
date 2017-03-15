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

(defvar blitzmax-fontify-p t
  "Whether to fontify Basic buffers.")

(defvar blitzmax-capitalize-keywords-p t
  "Whether to capitalize keywords.")

(defvar blitzmax-file-extension "*.bmx"
  "Wildcard pattern for BlitzMax source files.")

(defvar blitzmax-compiler-pathname nil
  "The full pathname of the BlitzMax compiler (i.e. bmk.")

(defvar blitzmax-ide-pathname nil
  "The full pathname of the BlitzMax IDE (e.g. blide, blitzmax ide etc.")


;;; Code:

(defvar blitzmax-keywords-to-highlight
  t
  "*A list of keywords to highlight in Basic mode, or T, meaning all keywords.")

(defvar blitzmax-defn-templates
  (list "Method ()\nEnd Method\n\n"
        "Function ()\nEnd Function\n\n")
  "*List of function templates though which blitzmax-new-sub cycles.")

(defvar blitzmax-mode-abbrev-table nil)

(defvar blitzmax-mode-hook ())

(defun blitzmax-mode--insert-comment (arg)
  "Comment or uncomment current line or region specific in ARG."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil)
        (comment-start "'")
        (comment-end ""))
    (comment-dwim arg)))

(defconst blitzmax-type-start-regexp
  (concat
   "^[\t ]*\\[Tt]ype\\"
   "[ \t ]+\\(\\w+\\)[ \t ]*(?"))

(defconst blitzmax-type-end-regexp
  "^[ \t]*[Ee]nd [Tt]ype")

(defconst blitzmax-defun-start-regexp
  (concat
   "^[\t ]*\\([Me]ethod\\|[Ff]unction\\)"
   "[ \t ]+\\(\\w+\\)[ \t ]*(?"))

(defconst blitzmax-defun-end-regexp
  "^[ \t]*[Ee]nd \\([Mm]ethod\\|[Ff]unction\\)")

;; Includes the compile-time #if variation.
(defconst blitzmax-if-regexp "^[ \t]*#?[Ii]f")
(defconst blitzmax-else-regexp "^[ \t]*#?[Ee]lse\\([Ii]f\\)?")
(defconst blitzmax-endif-regexp "[ \t]*#?[Ee]nd[ \t]*[Ii]f")

(defconst blitzmax-continuation-regexp "[ \t]+")
(defconst blitzmax-label-regexp "^[ \t]*#[a-zA-Z0-9_]+$")

(defconst blitzmax-select-regexp "^[ \t]*[Ss]elect[ \t]+[Cc]ase")
(defconst blitzmax-case-regexp "^[ \t]*[Cc]ase")
(defconst blitzmax-select-end-regexp "^[ \t]*[Ee]nd[ \t]+[Ss]elect")

(defconst blitzmax-for-regexp "^[ \t]*[Ff]or")
(defconst blitzmax-next-regexp "^[ \t]*[Nn]ext")

(defconst blitzmax-while-regexp "^[ \t]*[Ww]hile")
(defconst blitzmax-wend-regexp "^[ \t]*[Ww]end")

(defconst blitzmax-repeat-regexp "^[ \t]*[Rr]epeat")
(defconst blitzmax-repeat-regexp "^[ \t]*[Uu]ntil")

(defconst blitzmax-blank-regexp "^[ \t]*$")
(defconst blitzmax-comment-regexp "^[ \t]*\\s<.*$")


;; BlitzMax Keywords
(defconst blitzmax-all-keywords
  '("Abs" "After" "And" "Before" "Case" "Const" "Data" "Default" "Delete" "Dim"
    "Each" "Else" "ElseIf" "End" "EndIf" "Exit" "False" "Field" "First" "Float"
    "For" "Forever" "Function" "Global" "Gosub" "Goto" "If" "Include" "Insert" "Int"
    "Last" "Local" "Mod" "New" "Next" "Not" "Null" "Or" "Pi" "Read" "Repeat" "Restore"
    "Return" "Sar" "Select" "Sgn" "Shl" "Shr" "Step" "Str" "Then" "To" "True" "Type"
    "Until" "Wend" "While" "Xor" "Abs" "Abstract" "ACos" "AllocChannel" "And" "AppArgs"
    "AppDir" "AppFile" "AppTitle" "Asc" "ASin" "Assert" "ATan" "ATan2" "AutoImageFlags"
    "AutoMidHandle" "BankBuf" "BankCapacity" "BankSize" "bglAdjustTexSize"
    "bglCreateContext" "bglDeleteContext" "bglDisplayModes" "bglDrawText"
    "bglFixedFontBitmaps" "bglSetMouseVisible" "bglSetSwapInterval" "bglSwapBuffers"
    "bglTexFromPixmap" "BigEndianStream" "Bin" "BindSocket" "Byte" "Case" "CasedFileName"
    "Catch" "Ceil" "ChangeDir" "ChannelPlaying" "Chr" "ClearList" "CloseDir" "CloseFile"
    "CloseGNetHost" "CloseGNetObject" "CloseGNetPeer" "CloseSocket" "CloseStream" "Cls"
    "CollideImage" "CollideRect" "compress" "compress2" "Confirm" "ConnectSocket" "Const"
    "Continue" "ConvertPixmap" "CopyBank" "CopyBytes" "CopyPixmap" "CopyStream" "Cos"
    "Cosh" "CountGraphicsModes" "CountList" "CreateAudioSample" "CreateBank"
    "CreateBankStream" "CreateDir" "CreateFile" "CreateGNetHost" "CreateGNetMessage"
    "CreateGNetObject" "CreateImage" "CreateList" "CreatePixmap" "CreateRamStream"
    "CreateSocketStream" "CreateStaticAudioSample" "CreateStaticBank" "CreateStaticPixmap"
    "CreateTCPSocket" "CreateTimer" "CreateUDPSocket" "CueSound" "CurrentDate" "CurrentDir"
    "CurrentTime" "D3D7Max2DDriver" "DebugLog" "DebugStop" "Default" "DefData" "Delay"
    "Delete" "DeleteDir" "DeleteFile" "DottedIP" "Double" "DrawImage" "DrawImageRect"
    "DrawLine" "DrawOval" "DrawPixmap" "DrawPoly" "DrawRect" "DrawText" "EachIn" "Else"
    "ElseIf" "End" "EndExtern" "EndFunction" "EndGraphics" "EndIf" "EndMethod" "EndRem"
    "EndSelect" "EndTry" "EndType" "Eof" "Exit" "Exp" "Extends" "Extern" "ExtractDir"
    "ExtractExt" "False" "Field" "FileMode" "FileSize" "FileTime" "FileType" "Final" "Flip"
    "Float" "Floor" "FlushKeys" "FlushMem" "FlushStream" "For" "Forever" "Framework"
    "Function" "GCMalloc" "GetAlpha" "GetBlend" "GetChar" "GetClsColor" "GetColor"
    "GetGNetFloat" "GetGNetInt" "GetGNetString" "GetGNetTarget" "GetGraphics"
    "GetGraphicsMode" "GetHandle" "GetImageFont" "GetLineWidth" "GetMaskColor" "GetOrigin"
    "GetRotation" "GetScale" "GetViewport" "GLMax2DDriver" "Global" "GNetAccept" "GNetConnect"
    "GNetListen" "GNetMessageObject" "GNetMessages" "GNetObjectLocal" "GNetObjectRemote"
    "GNetObjects" "GNetObjectState" "GNetPeers" "GNetPeerTCPSocket" "GNetPeerUDPSocket"
    "GNetSync" "GNetTotalBytesIn" "GNetTotalBytesOut" "Goto" "GrabImage" "GrabPixmap"
    "Graphics" "GraphicsHeight" "GraphicsModeExists" "GraphicsWidth" "Hex" "HideMouse"
    "HostIp" "HostIps" "HostName" "If" "ImageHeight" "ImagesCollide" "ImagesCollide2"
    "ImageWidth" "Import" "Incbin" "IncbinLen" "IncbinPtr" "Include" "Input" "Instr" "Int"
    "JoyAxisCaps" "JoyButtonCaps" "JoyCount" "JoyDown" "JoyHat" "JoyName" "JoyPitch" "JoyR"
    "JoyRoll" "JoyU" "JoyV" "JoyWheel" "JoyX" "JoyY" "JoyYaw" "JoyZ" "KeyDown" "KeyHit"
    "LaunchDir" "Left" "Len" "ListAddFirst" "ListAddLast" "ListContains" "ListFindLink"
    "ListFromArray" "ListIsEmpty" "ListRemove" "ListToArray" "LittleEndianStream"
    "LoadAnimImage" "LoadAudioSample" "LoadBank" "LoadByteArray" "LoadDir" "LoadImage"
    "LoadImageFont" "LoadPixmap" "LoadPixmapPNG" "LoadSound" "LoadString" "Local" "LockImage"
    "Log" "Log10" "Long" "LongBin" "LongHex" "Lower" "LSet" "MaskPixmap" "Max" "MemAlloc"
    "MemAlloced" "MemClear" "MemCopy" "MemExtend" "MemFree" "MemMove" "MemUsage" "Method"
    "Mid" "MidHandleImage" "MilliSecs" "Min" "Mod" "Module" "ModuleInfo" "MouseDown" "MouseHit"
    "MouseX" "MouseY" "MouseZ" "MoveMouse" "New" "Next" "NextFile" "Not" "Notify" "Null"
    "Object" "OnEnd" "OpenFile" "OpenStream" "Or" "PauseChannel" "PeekByte" "PeekDouble" 
    "PeekFloat" "PeekInt" "PeekLong" "PeekShort" "Pi" "PixmapFormat" "PixmapHeight" "PixmapPitch" 
    "PixmapPixelPtr" "PixmapWidth" "PixmapWindow" "PlaySound" "Plot" "PokeByte" "PokeDouble"
    "PokeFloat" "PokeInt" "PokeLong" "PokeShort" "PollSystem" "Print" "Private" "Proceed" "Ptr"
    "Public" "Rand" "ReadBank" "ReadByte" "ReadData" "ReadDir" "ReadDouble" "ReadFile" "ReadFloat"
    "Readint" "ReadLine" "ReadLong" "ReadPixel" "ReadShort" "ReadStdin" "ReadStream" "ReadString"
    "RealPath" "Release" "Rem" "RemoveLink" "RenameFile" "Repeat" "Replace" "RequestDir"
    "RequestFile" "ResetCollisions" "ResizeBank" "ResizePixmap" "RestoreData" "ResumeChannel" 
    "Return" "ReverseList" "Right" "Rnd" "RndDouble" "RndFloat" "RndSeed" "RSet" "RuntimeError"
    "Sar" "SaveBank" "SaveByteArray" "SavePixmapPNG" "SaveString" "ScriptEngine" "SeedRnd"
    "SeekStream" "Select" "Self" "SendGNetMessage" "SetAlpha" "SetBlend" "SetChannelDepth"
    "SetChannelPan" "SetChannelRate" "SetChannelVolume" "SetClsColor" "SetColor" "SetFileMode"
    "SetGNetFloat" "SetGNetInt" "SetGNetString" "SetGNetTarget" "SetGraphicsDriver" "SetHandle"
    "SetImageFont" "SetImageHandle" "SetLineWidth" "SetMaskColor" "SetOrigin" "SetRotation"
    "SetScale" "SetTransform" "SetViewport" "Sgn" "Shl" "Short" "ShowMouse" "Shr" "Sin" "Sinh"
    "SizeOf" "SocketAccept" "SocketConnected" "SocketListen" "SocketLocalIP" "SocketLocalPort"
    "SocketReadAvail" "SocketRemoteIP" "SocketRemotePort" "SortList" "Sqr" "StandardIOStream"
    "Step" "StopChannel" "StreamPos" "StreamSize" "Strict" "SuperStrict" "String" "StripAll" "StripDir"
    "StripExt" "StripSlash" "Super" "SwapLists" "Tan" "Tanh" "TAudioSample" "TAudioSampleLoader"
    "TBank" "TBankStream" "TChannel" "TCStream" "TextHeight" "TextWidth" "Then" "Throw"
    "TileImage" "TIO" "TLink" "TList" "TListEnum" "To" "TPixmap" "TPixmapLoader" "Trim" "True"
    "Try" "TSound" "TStream" "TStreamException" "TStreamFactory" "TStreamReadException"
    "TStreamWrapper" "TStreamWriteException" "Type" "uncompress" "UnlockImage" "Until" "Upper"
    "Var" "Varptr" "WaitChar" "WaitKey" "WaitMouse" "WaitSystem" "WaitTimer" "Wend" "While"
    "WriteBank" "WriteByte" "WriteDouble" "WriteFile" "WriteFloat" "WriteInt" "WriteLine"
    "WriteLong" "WritePixel" "WriteShort" "WriteStderr" "WriteStdout" "WriteStream" "WriteString"
    "XFlipPixmap" "YFl"))


(define-derived-mode blitzmax-mode fundamental-mode
  "BlitzMax mode"
  "Major mode for editing BlitzMax source files."

  ;; Highlight keywords.
  (setq font-lock-defaults '(blitzmax-all-keywords))

  ;; Add keys.
  (define-key blitzmax-mode-map [remap comment-dwim] 'blitzmax-mode--insert-comment)

  ;; Comment: "'".
  (modify-syntax-entry ?\' "< b" blitzmax-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" blitzmax-mode-syntax-table))

;;;###Autoload
(add-to-list 'auto-mode-alist '("\\.bmx\\'" . blitzmax-mode))

(provide 'blitzmax-mode)
;;; blitzmax-mode.el ends here
