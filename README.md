# blitzmax-mode - BlitzMax editing for GNU Emacs

![GPLv2](https://img.shields.io/github/license/Sodaware/blitzmax-mode.svg)
![GitHub release](https://img.shields.io/github/release/Sodaware/blitzmax-mode.svg)

This project adds a new major mode to GNU Emacs for editing BlitzMax source
files. It supports keyword highlighting, keyword capitalization, and automatic
indentation.

![blitzmax-mode in action](https://www.sodaware.net/assets/images/projects/blitzmax-mode/blitzmax-mode-screenshot.png)


## Installation

To manually install this extension:

  1. Download the source code and put it somewhere Emacs can find it (probably
     `~/.emacs.d/`).
  2. Add that directory to your `load-path` if it's not yet there: 
    `(add-to-list 'load-path "/path/to/blitzmax-mode")`
  3. Require the mode:
     `(require 'blitzmax-mode)`

Once everything is loaded, blitzmax-mode can be enabled for a buffer by running
`M-x blitzmax-mode`. The mode also associates itself with `.bmx` files, so they
will automatically switch to `blitzmax-mode` when enabled.


## Configuration

**blitzmax-mode** provides the following configuration options:

* `blitzmax-mode-indent` - The number of spaces to indent by. By default
  **blitzmax-mode** indents by 4 spaces which is converted to a single tab.

* `blitzmax-mode-capitalize-keywords-p` - Disable automatic capitalization of
  keywords by setting this to `nil`. `t` by default.

* `blitzmax-mode-smart-indent-p` - Disable smart indentation by setting this to
  `nil`. `t` by default.

* `blitzmax-mode-compiler-pathname` - Full pathname to the BlitzMax compiler
  `bmk`. "bmk" by default.


## Running the current buffer using quickrun

[quickrun.el](https://github.com/syohex/emacs-quickrun/) is an extension for
compiling and executing the current buffer.

To enable quickrun support, add the following to your Emacs initialization file:

```elisp
(with-eval-after-load 'quickrun
  (blitzmax-mode-quickrun-integration))
```

To bind quickrun to a key press (`C-c C-c` in this example), add the following
to `init.el` (or wherever your Emacs config resides):

```elisp
(eval-after-load "blitzmax-mode"
  '(define-key blitzmax-mode-map (kbd "C-c C-c") 'quickrun))
```

When called, quickrun will compile and execute the current buffer with debug and
threading enabled. Once the process has finished running the executable file
will be deleted.


## Compiling Projects with Projectile

[Projectile](https://github.com/bbatsov/projectile) is a great Emacs package for
organizing and navigating large projects. It can be configured to build a
BlitzMax application by setting `projectile-project-compilation-cmd` in the
project's `.dir-locals.el` file.

For example, the following would go in `.dir-locals.el` in the projects
folder. It would compile the contents of `src/my_app.bmx` in release + threaded
mode and save the executable as `my_app`:

```elisp
((nil . ((projectile-project-compilation-cmd . "bmk makeapp -r -h -o my_app src/my_app.bmx"))))
```

The current project can then be compiled by running `projectile-compile-project`
(bound to `C-c p c` by default).


## Current Features

* Basic syntax highlighting
* Automatic indentation
* Capitalizes keywords automatically
* Quickrun support


## Planned Features

* Compile current buffer
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
