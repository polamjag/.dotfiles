`helm' interface for flymake.
When `flymake-mode' is t, M-x `helm-flymake' lists warning and error
messages in *helm flymake* buffer.
C-u M-x `helm-flymake' insert the line number of current cursor position
into minibuffer.


Installation:

Add followings on your .emacs.

  (require 'helm-config)
  (require 'helm-flymake)
