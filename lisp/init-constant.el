;; init-constant.el --- Define constants.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Define constants.
;;

;;; Code:

;; some constant configuration

(defconst *t_fighting-homepage*
  "https://github.com/tfighting/TF_Emacs"
  "The Github page of T_Fighting Emacs.")

(defconst *sys/gui*
  (display-graphic-p)
  "Are we running on a GUI emacs")

(defconst *rg*
  (executable-find "rg")
  "Do we have ripgrep")

(defconst *sys/win32p*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst *sys/mac-gui*
  (and *sys/gui* *sys/mac*)  "Are we running under X on a Mac system?")

(defconst *sys/mac-cocoa*
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst *sys/linux-gui*
  (and *sys/gui* *sys/linux*)  "Are we running under X on a GNU/Linux system?")

(defconst *sys/root*
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst *emacs/>=27p*
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(provide 'init-constant)
;;; init-const.el ends here
