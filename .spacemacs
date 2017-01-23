;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     markdown
     html
     (auto-completion :variables auto-completion-enable-sort-by-usage t
                                 auto-completion-enable-snippets-in-popup t)
     (ess :variables ess-use-auto-complete t
          ess-enable-smart-equals nil)
     (spacemacs-layouts :variables layouts-enable-autosave nil
                                   layouts-autosave-delay 300)
     (spell-checking :variables enable-flyspell-auto-completion nil
                     spell-checking-enable-by-default nil)
     better-defaults
     emacs-lisp
     git
     helm
     latex
     bibtex
     org
     pandoc
     polymode
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      swiper-helm
                                      pangu-spacing
                                      darkokai-theme
                                      chinese-fonts-setup
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    pandoc-mode
                                    ;; yasnippet
                                    magit-gitflow
                                    org-projectile
                                    holy-mode
                                    highligh-indentation
                                    eyebrowse
                                    org-download
                                    org-timer
                                    git-gutter
                                    git-gutter-fringe
                                    ac-ispell
                                    ace-jump-mode
                                    auto-complete
                                    auto-dictionary
                                    define-word
                                    google-translate
                                    fancy-battery
                                    orgit
                                    spacemacs-theme
                                    clean-aindent-mode
                                    ace-jump-helm-line
                                    helm-make
                                    helm-themes
                                    ;; helm-swoop
                                    helm-flx
                                    smeargle
                                    ;; spaceline
                                    ido-vertical
                                    ;; purpose-mode
                                    ;; window-purpose
                                    ;; helm-purpose
                                    spacemacs-purpose-popwin
                                    )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings27.0 27.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil t27.0 27hen spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; Example for 5 recent files and 7 projects: '((recents . 5) (projects . 7))
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; (default nil)
   dotspacemacs-startup-lists '((recents . 5) (projects . 5))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         darkokai
                         sanityinc-solarized-dark
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("DroidSansMonoForPowerline NF"
                               :size 11
                               :weight demibold
                               :width normal
                               :powerline-scale 1.4)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("pt" "ag" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; (setq org-ref-insert-cite-key "C-c """)
  ;; https://github.com/syl20bnr/spacemacs/issues/2705
  ;; (setq tramp-mode nil)
  (setq-default
   ispell-program-name "aspell"
   tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"
   byte-compile-warnings '(not obsolete)
   warning-minimum-level :error
   evil-shift-round nil
   menu-bar-mode t
   ;; whitespace-style '(face tabs trailing space-before-tab newline indentation empty space-after-tab tab-mark newline-mark)
   ;; whitespace-display-mappings
   ;; '((newline-mark 10 [172 10])
   ;;   (tab-mark 9 [9655 9]))
  )
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (load-theme 'darkokai t)
  ;;Chinese fonts setup
  (require 'chinese-fonts-setup)
  (chinese-fonts-setup-enable)
  (cfs-set-spacemacs-fallback-fonts)
  ;;Powerline setup
  (setq ns-use-srgb-colorspace nil)
  (setq powerline-default-separator 'arrow-fade)
  (global-set-key (kbd "C-s") 'swiper)

  (require 'pangu-spacing)
  (global-pangu-spacing-mode 1)
  (add-hook 'org-mode-hook
            '(lambda ()
               (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

  ;;Vim-like key bindings
  (define-key evil-motion-state-map "H" 'evil-first-non-blank)
  (define-key evil-motion-state-map "L" 'evil-end-of-line)

  ;;Spell configurations
  (when (and (spacemacs/system-is-mswindows) window-system)
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
  (setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict/"))
  ;; (ispell-change-dictionary "american" t)

  ;;Turn off the automatic replacement of underscores by <- in ESS mode.
  (add-hook 'ess-mode-hook
            (lambda ()
              (ess-toggle-underscore nil)))
  (spacemacs|add-company-hook 'text-mode)

  ;;Only toggle relative line number mode when press F8
  (global-set-key (kbd "<f8>") 'linum-relative-mode)

  ;;Not save abbrevs
  (setq save-abbrevs nil)
  ;; turn on abbrev mode globally
  (setq-default abbrev-mode t)
  ;;Don't ask me when close emacs with process is running
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (flet ((process-list ())) ad-do-it))
  ;;Don't ask me when kill process buffer
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (setq comint-prompt-read-only nil)
  (with-eval-after-load 'smartparens
    (show-smartparens-global-mode -1))

  (setq-default fill-column 80)

  ;;Org-mode settings----------------------------------------------------------------
  (with-eval-after-load 'org

  (setq org-ellipsis "⤵")
  (setq org-bullets-bullet-list (quote ("✽" "✱" "✻" "✲" "✲")))
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-leading-stars t)
  (setq org-highlight-latex-and-related (quote (latex script entities)))

  (setq org-confirm-babel-evaluate nil)
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-fontify-natively t)
  (setq org-use-sub-superscripts '{})
  (add-hook 'org-mode-hook '(lambda ()
  ;;                             (visual-line-mode)
                              (org-indent-mode)))

  ;; ;;Use ox-pandoc package to export docx files through org files.
  (setq org-pandoc-options-for-docx '((reference-docx . "~/pandoc_templates/default.docx")))
  (setq org-pandoc-options-for-latex-pdf '((template . "~/pandoc_templates/default.latex")(latex-engine . "xelatex")))
  (setq org-pandoc-options-for-beamer-pdf '((template . "~/pandoc_templates/default.beamer")(latex-engine . "xelatex")))

  (defun turn-on-org-show-all-inline-images ()
    (org-display-inline-images t t))
  (add-hook 'org-mode-hook 'turn-on-org-show-all-inline-images)

  ;; Org-Ref settings
  (when (and (spacemacs/system-is-mswindows) window-system)
    (setq w32-pass-alt-to-system nil)
    (setq reftex-default-bibliography '("c:/Dropbox/3-Literatures/reference.bib"))
    (setq org-ref-bibliography-notes "c:/Dropbox/org/literatures_notes.org"
          org-ref-default-bibliography '("c:/Dropbox/3-Literatures/reference.bib")
          org-ref-pdf-directory "c:/Dropbox/3-Literatures/Dissertation/")
    (setq bibtex-completion-bibliography "c:/Dropbox/3-Literatures/reference.bib"
          bibtex-completion-library-path "c:/Dropbox/3-Literatures/Dissertation/"))
  (when (and (spacemacs/system-is-linux) window-system)
  (setq reftex-default-bibliography '("~/Dropbox/3-Literatures/reference.bib"))
  (setq org-ref-bibliography-notes "~/Dropbox/org/literatures_notes.org"
        org-ref-default-bibliography '("~/Dropbox/3-Literatures/reference.bib")
        org-ref-pdf-directory "~/Dropbox/3-Literatures/Dissertation/")
  (setq bibtex-completion-bibliography "~/Dropbox/3-Literatures/reference.bib"
        bibtex-completion-library-path "~/Dropbox/3-Literatures/Dissertation/"
        bibtex-completion-pdf-field "file"))

  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (car (bibtex-completion-find-pdf key))))
      (if (file-exists-p pdf-file)
          (org-open-file pdf-file)
        (message "No PDF found for %s" key))))
  (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

  (when (and (spacemacs/system-is-linux) window-system)
    (setq org-latex-create-formula-image-program 'imagemagick))
  (when (and (spacemacs/system-is-mswindows) window-system)
    (setq org-latex-create-formula-image-program 'dvipng))
    ;; (setq org-latex-create-formula-image-program 'imagemagick))

  (defun zilongshanren/org-insert-src-block (src-code-type)
    "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
    (interactive
     (let ((src-code-types
            '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
              "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
              "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
              "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby" "R"
              "scheme" "sqlite")))
       (list (ido-completing-read "Source code type: " src-code-types))))
    (progn
      (newline-and-indent)
      (insert (format "#+BEGIN_SRC %s\n" src-code-type))
      (newline-and-indent)
      (insert "#+END_SRC\n")
      (previous-line 2)
      (org-edit-src-code)))

      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "," 'org-priority)
      (require 'org-compat)
      (require 'org)
      (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)

      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-targets
            '((nil :maxlevel . 4)
              (org-agenda-files :maxlevel . 4)))
      ;; config stuck project
      (setq org-stuck-projects
            '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

      (setq org-agenda-inhibit-startup t)   ;; ~50x speedup
      (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
      (setq org-agenda-window-setup 'current-window)
      (setq org-log-done t)

      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                    (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))

      (setq org-tags-match-list-sublevels nil)

      (add-hook 'org-mode-hook '(lambda ()
                                  ;; keybinding for editing source code blocks
                                  ;; keybinding for inserting code blocks
                                  (local-set-key (kbd "C-c i s")
                                                 'zilongshanren/org-insert-src-block)))

      (add-to-list 'org-latex-classes '("elsarticle" "\\documentclass{elsarticle}

                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage{amssymb}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{longtable}
                                        \\usepackage{booktabs}
                                        \\usepackage{tabularx}
                                        \\usepackage[numbers,sort&compress]{natbib}
                                        \\usepackage{siunitx}
                                        \\usepackage{titlesec}
                                        \\titlespacing\\subsection{0pt}{12pt plus 2pt minus 2pt}{10pt plus 2pt minus 2pt}
                                        \\titlespacing\\subsubsection{0pt}{12pt plus 4pt minus 2pt}{2pt plus 2pt minus 2pt}
                                        \\usepackage[labelfont=bf,font=small]{caption}
                                        \\captionsetup[table]{format=plain,labelsep=newline,singlelinecheck=false,justification=raggedright,skip=0pt}
                                        \\captionsetup[figure]{labelsep=period, name=Fig.}
                                        \\renewcommand{\\figureautorefname}{Fig.}
                                        \\usepackage[labelformat=simple]{subfig}
                                        \\renewcommand\\thesubfigure{(\\alph{subfigure})}
                                        \\newcommand{\\subfigureautorefname}{\\figureautorefname}
                                        \\usepackage[x11names]{xcolor}
                                        \\usepackage[bookmarksopen=true,
                                                     pdfstartview=FitB,
                                                     colorlinks=true,
                                                     citecolor=DeepSkyBlue3,
                                                     linkcolor=DeepSkyBlue3,
                                                     anchorcolor=DeepSkyBlue3]{hyperref}
                                        \\AtBeginDocument{\\hypersetup{citecolor=DeepSkyBlue3, linkcolor=DeepSkyBlue3, anchorcolor=DeepSkyBlue3}}
                                        "
                                        ("\\section{%s}" . "\\section*{%s}")
                                        ("\\subsection{%s}" . "\\subsection*{%s}")
                                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                        ("\\section{%s}" . "\\section*{%s}")
                                        ("\\subsection{%s}" . "\\subsection*{%s}")
                                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      ;; {{ export org-mode in Chinese into PDF
      ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
      ;; and you need install texlive-xetex on different platforms
      ;; To install texlive-xetex:
      ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
      ;; }}

      (setq org-latex-default-class "ctexart")
      (when (and (spacemacs/system-is-linux) window-system)
      (setq org-latex-pdf-process
            '("latexmk -xelatex -f -interaction=nonstopmode -output-directory=%o -bibtex %f"
              "rm -fr %b.out %b.log %b.tex auto")))
      (when (and (spacemacs/system-is-mswindows) window-system)
        (setq org-latex-pdf-process
              '("latexmk -xelatex -f -interaction=nonstopmode -output-directory=%o -bibtex %f"
                "del /Q /F %b.out %b.log %b.tex %b.spl auto")))

      (setq org-latex-listings t)

      ;;reset subtask
      (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((R . t)
         (latex .t)
         (sh . t)
         (python . t)
         (emacs-lisp . t)
         (plantuml . t)
         (dot . t)
         (ditaa . t)))

      ;; define the refile targets
      (when (and (spacemacs/system-is-linux) window-system)
      (setq org-agenda-files (quote ("~/Dropbox/Org" )))
      (setq org-default-notes-file "~/Dropbox/Org/gtd.org"))
      (when (and (spacemacs/system-is-mswindows) window-system)
        (setq org-agenda-files (quote ("c:/Dropbox/Org" )))
        (setq org-default-notes-file "c:/Dropbox/Org/gtd.org"))

      ;; (with-eval-after-load 'org-agenda
      ;;   (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
      ;;     "." 'spacemacs/org-agenda-transient-state/body)
      ;;   )
      ;; the %i would copy the selected text into the template
      ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;;add multi-file journal
      (when (and (spacemacs/system-is-linux) window-system)
      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline "~/Dropbox/org/gtd.org" "Daily")
               "* TODO [#D] %?\n  %i\n"
               :empty-lines 1)
              ("w" "Work" entry (file+headline "~/Dropbox/org/gtd.org" "Work")
               "* TODO [#W] %?\n  %i\n %U"
               :empty-lines 1)
              ("n" "Notes" entry (file+headline "~/Dropbox/org/notes.org" "Quick notes")
               "* %?\n  %i\n %U"
               :empty-lines 1)
              ("l" "Links" entry (file+headline "~/Dropbox/org/notes.org" "Quick notes")
               "* TODO [#C] %?\n  %i\n %a \n %U"
               :empty-lines 1)
              ("s" "Code Snippet" entry
               (file "~/Dropbox/Org/snippets.org")
               "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
              ("j" "Journal Entry"
               entry (file+datetree "~/Dropbox/org/journal.org")
               "* %?"
               :empty-lines 1))))

      (when (and (spacemacs/system-is-mswindows) window-system)
      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline "c:/Dropbox/org/gtd.org" "Daily")
               "* TODO [#D] %?\n  %i\n"
               :empty-lines 1)
              ("w" "Work" entry (file+headline "c:/Dropbox/org/gtd.org" "Work")
               "* TODO [#W] %?\n  %i\n %U"
               :empty-lines 1)
              ("n" "Notes" entry (file+headline "c:/Dropbox/org/notes.org" "Quick notes")
               "* %?\n  %i\n %U"
               :empty-lines 1)
              ("l" "Links" entry (file+headline "c:/Dropbox/org/notes.org" "Quick notes")
               "* TODO [#C] %?\n  %i\n %a \n %U"
               :empty-lines 1)
              ("s" "Code Snippet" entry
               (file "c:/Dropbox/org/snippets.org")
               "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
              ("j" "Journal Entry"
               entry (file+datetree "c:/Dropbox/org/journal.org")
               "* %?"
               :empty-lines 1))))
      ;;An entry without a cookie is treated just like priority ' B '.
      ;;So when create new task, they are default 重要且紧急
      (setq org-agenda-custom-commands
            '(
              ("w" . "任务安排")
              ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
              ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
              ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
              ("d" "日常生活" tags-todo "DAILY")
              ("p" . "项目安排")
              ("W" "Weekly Review"
               ((stuck "") ;; review stuck projects as designated by org-stuck-projects
                (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                ))))

      (defun zilong/org-summary-todo (n-done n-not-done)
        "Switch entry to DONE when all subentries are done, to TODO otherwise."
        (let (org-log-done org-log-states)    ; turn off logging
          (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

      (add-hook 'org-after-todo-statistics-hook 'zilong/org-summary-todo)
      ;; used by zilong/org-clock-sum-today-by-tags

      (define-key org-mode-map (kbd "s-p") 'org-priority)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "tl" 'org-toggle-link-display)
      (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
      (setq org-mobile-directory "~/Dropbox/Org"))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(cfs--current-profile "profile1" t)
 '(cfs--profiles-steps (quote (("profile1" . 2))) t)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" "1dffeecd1565d04cd2059234e872cd80fcbe813488602d5c42b5c9e576924d9f" default)))
 '(ess-R-font-lock-keywords
   (quote
    ((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t)
     (ess-R-fl-keyword:%op% . t))))
 '(ess-ask-for-ess-directory nil)
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#073642" t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files
   (quote
    ("c:/Dropbox/Org/gtd.org" "c:/Dropbox/Org/journal.org" "c:/Dropbox/Org/literatures_notes.org" "c:/Dropbox/Org/notes.org" "c:/Dropbox/papers/validation_radiant_model/verification_radiant_model.org")))
 '(org-bullets-bullet-list (quote ("*" "*" "*" "*" "*")))
 '(org-hide-emphasis-markers nil)
 '(org-hide-leading-stars t)
 '(org-highlight-latex-and-related (quote (latex script entities)))
 '(package-selected-packages
   (quote
    (polymode swiper-helm window-purpose imenu-list tablist dactyl-mode hide-comnt ox-pandoc git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter diff-hl company-auctex auctex-latexmk auctex ivy-purpose helm-swoop helm-purpose helm-projectile helm-mode-manager helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag flyspell-correct-helm org-ref key-chord helm-bibtex biblio parsebib biblio-core web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode haml-mode emmet-mode company-web web-completion-data ranger youdao-dictionary names chinese-word-at-point rainbow-mode rainbow-identifiers magit-gh-pulls gh marshal logito pcache ht helm-themes fcitx color-identifiers-mode ace-jump-helm-line zotxt request-deferred deferred color-theme-sanityinc-solarized ess-smart-equals ess-R-object-popup ess-R-data-view ctable ess julia-mode xterm-color shell-pop mwim multi-term flycheck-pos-tip flycheck eshell-z eshell-prompt-extras esh-help chinese-wbim molokai-theme pangu-spacing find-by-pinyin-dired chinese-pyim chinese-pyim-basedict pos-tip ace-pinyin pinyinlib ace-jump-mode chinese-fonts-setup smeargle orgit org-projectile org-present org org-pomodoro alert log4e gntp org-download mmm-mode markdown-toc markdown-mode magit-gitflow htmlize gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flyspell-correct-ivy flyspell-correct evil-magit magit magit-popup git-commit with-editor company-statistics company auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete ws-butler window-numbering which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash async aggressive-indent adaptive-wrap ace-window ace-link avy quelpa package-build spacemacs-theme)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#414339"))))
 '(company-scrollbar-fg ((t (:background "#34352d"))))
 '(company-tooltip ((t (:inherit default :background "#2c2d26"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(org-block ((t (:background "gray12" :foreground "#F8F8F0"))))
 '(org-block-begin-line ((t (:background "black" :foreground "honeydew" :slant italic :weight bold))))
 '(org-block-end-line ((t (:background "black" :foreground "honeydew" :slant italic :weight bold))))
 '(org-code ((t (:foreground "LemonChiffon1" :weight semi-bold))))
 '(org-document-info-keyword ((t (:inherit shadow :foreground "aquamarine"))))
 '(org-document-title ((t (:foreground "#F8F8F0" :weight bold :height 1.05))))
 '(org-ellipsis ((t (:foreground "light gray"))))
 '(org-level-1 ((t (:inherit default :foreground "#FD971F" :weight bold :height 1.05))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "#A6E22E" :height 1.05))))
 '(org-level-3 ((t (:inherit default :foreground "#66D9EF" :height 1.05))))
 '(org-level-4 ((t (:inherit default :foreground "#E6DB74" :height 1))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :background "black" :foreground "white" :weight normal))))
 '(org-tag ((t (:background "dark slate gray" :foreground "gray100" :weight bold))))
 '(org-verbatim ((t (:inherit shadow :background "dark slate gray" :foreground "ghost white" :inverse-video nil)))))
