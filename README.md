# blitzmax-mode - BlitzMax support for Emacs


## Intro

This is a very (very) WIP Emacs mode for editing BlitzMax source
files. Highlighting and inte

## Installation

### Checkout from Git

```bash
cd ~/path/to/.emacs.d/
git clone git@github.com:Sodaware/blitzmax-mode.git blitzmax-mode
```

### Add to .emacs

```emacs-lisp
(add-to-list 'load-path "~/path/to/blitzmax-mode/")
(require 'blitzmax-mode)
```

### Configure Options

```emacs-lisp
;; Automatically use blitzmax-mode for .bmx files
(autoload 'blitzmax-mode "blitzmax-mode" "BlitzMax Mode" t)
(add-to-list 'auto-mode-alist '("\\.bmx\\'"   . blitzmax-mode))
```

## Current Features

* Basic syntax highlighting


## Planned Features

* Compile current buffer
* Much better syntax highlighting
* Proper indenting
* Auto-completion
* Integration with bmk
* Integration with blam


## Licence

This file is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.

This file is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with GNU
Emacs; see the file COPYING.  If not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
