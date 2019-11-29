;;; editor/durand-template/config.el -*- lexical-binding: t; -*-

(set-file-template! "\\.tex$"
  :mode 'latex-mode
  :trigger "__init")
