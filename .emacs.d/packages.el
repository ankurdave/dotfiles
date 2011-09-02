(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

(setq el-get-sources
      '((:name thrift-mode
               :type http
               :url "http://svn.apache.org/repos/asf/thrift/trunk/contrib/thrift.el"
               :after (lambda ()
                        (require 'thrift-mode "thrift.el")
                        (add-to-list 'auto-mode-alist '("\\.thrift$" . thrift-mode))))
        (:name highlight-80+-mode
               :type http
               :url "http://nschum.de/src/emacs/highlight-80+/highlight-80+.el"
               :after (lambda () (add-hook 'c++-mode-hook (lambda () (highlight-80+-mode t)))))
        (:name ensime-prebuilt
               :type http-tar
               :url "https://github.com/downloads/aemoncannon/ensime/ensime_2.8.1-0.5.0.tar.gz"
               :options ("-xz" "--strip-components=1" "-f")
               :load-path "elisp"
               :after (lambda ()
                        (require 'ensime)))
        (:name xcscope-ankurdave
               :type git
               :url "git://github.com/ankurdave/xcscope.git"
               :features xcscope)
        (:name apache-mode
               :type http
               :url "http://www.emacswiki.org/cgi-bin/wiki/download/apache-mode.el"
               :after (lambda ()
                        (require 'apache-mode)))))

(setq my-packages
      '(el-get js2-mode thrift-mode highlight-80+-mode markdown-mode switch-window scala-mode undo-tree dtrt-indent smex nxhtml maxframe ensime-prebuilt xcscope-ankurdave apache-mode))

(el-get 'wait my-packages)

(require 'undo-tree)
