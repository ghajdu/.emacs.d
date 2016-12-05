(setq webjump-sites
      '(("DuckDuckGo" .
         [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
        ("Google" .
         [simple-query "www.google.com" "www.google.com/search?q=" ""])
        ("sv -> de" .
         [simple-query "translate.google.com" "translate.google.com/#sv/de/" ""])
        ("de -> sv" .
         [simple-query "translate.google.com" "translate.google.com/#de/sv/" ""])
        ("Emacs Wiki" .
         [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/" ""])
        ("Wikipedia" .
         [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
        ("Google Groups" .
         [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
        ("Savannah Emacs page" . "savannah.gnu.org/projects/emacs")))

(provide 'setup-webjump)
