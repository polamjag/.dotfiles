# Your Old Emacs Config Files

This directory contains any Emacs configuration files that had existed prior
to installing Emacs Live.

To see which files have been preserved:

    ls -allh /home/satoru/emacs-live-old-config

To revert back to your old Emacs configs simply:

    rm -rf ~/.emacs.d
    mv /home/satoru/emacs-live-old-config/.emacs* ~/
    rm -rf /home/satoru/emacs-live-old-config
