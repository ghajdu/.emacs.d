(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(setq emmet-move-cursor-between-quotes t) ;; If you want the cursor to be positioned between first empty quotes after expanding:
(setq emmet-expand-jsx-className? t) ;; If you want to use emmet with react-js's JSX, you probably want emmet to expand 'className="..."' instead of 'class="..."':

(provide 'setup-emmet-mode)
