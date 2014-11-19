(
 (nil . ((eval . (setq dir-local-curdir
                       (file-name-directory
                        (let ((d (dir-locals-find-file ".")))
                          (if (stringp d) d (car d))))))))
 (haskell-mode . ((eval .
   (progn
     (setq flycheck-ghc-search-path (list (concat dir-local-curdir "/bottlelib")))
     (setq flycheck-ghc-search-path (list (concat dir-local-curdir "/submodules/AlgoW")))
     (setq flycheck-idle-change-delay 1.0)))))
)
