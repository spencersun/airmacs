(provide 'air-complete)

(require 'cl)

(setq air-dir-completions
      '(
        ("usr" "/usr")
        ("etc" "/etc")
        ("roo" "/root")
        ("hom" "~/")
        ("var" "/var")
        ("/pb" "~/src/purity/pb/")
        ("ppy" "~/src/purity/pb/pb-py/")
        ; ("bin" "~/src/purity/pb/bin/")
        ("bld" "~/src/purity/pb/builders/")
        ("run" "~/src/purity/pb/builders/runtests/")
        ("api" "~/src/purity/pb/services/api_server/")
        ("han" "~/src/purity/pb/services/api_server/handlers/")
        ("tes" "~/src/purity/pb/tests/")

        ("moc" "~/src/pure_tools/ci/mockingbird/")
        ("ems" "~/src/pure_tools/ci/mockingbird/webapps/ems/")

        ("lab" "~/src/pure_tools/ci/labels/")

        ("too" "~/src/pure_tools/")
        ("/ci" "~/src/pure_tools/ci/")
        ("ans" "~/src/pure_tools/ci/ansible/")
        ("rol" "~/src/pure_tools/ci/ansible/roles/")
        ("bin" "~/src/pure_tools/ci/bin/")
        ("cil" "~/src/pure_tools/ci/cimetrics_loader/")
        ("ras" "~/src/pure_tools/ci/ras/")
        ("cla" "~/src/pure_tools/ci/ras/claim_service/")
        ("clh" "~/src/pure_tools/ci/ras/claim_service/handlers/")
        ("cli" "~/src/pure_tools/ci/ras/cli/")
        ("grv" "~/src/pure_tools/ci/ras/groovy/")
        ("lib" "~/src/pure_tools/ci/ras/lib/")
        ("log" "~/src/pure_tools/ci/ras/logger/")
        ("que" "~/src/pure_tools/ci/ras/queue_service/")
        ("web" "~/src/pure_tools/ci/ras/web/")
        ("wbh" "~/src/pure_tools/ci/ras/web/handlers/")

        ("pra" "~/src/pure_tools/ci/pure_plugins/pure-resource-allocator/src/main/java/com/purestorage/pureresourceallocator/")

        ("/lb" "~/src/pure_tools/ci/lb/")
        )
      )

;;minibuffer auto-complete
(defun air-lookup-filename-completion ()
  (let ((completion
         (cadr
          (find-if (lambda (el) (looking-at (car el))) 
                   air-dir-completions))))
    (eval completion)))

(defun air-findfile-completion ()
  ;; Extension to the complete word facility of the minibuffer
  (interactive)
  (backward-char 3)
  (let ((directory (air-lookup-filename-completion)))
    (cond 
     (directory 
      (beginning-of-line)
      (delete-region (line-beginning-position) (line-end-position))
      (insert directory))
     (t (forward-char 3) (minibuffer-complete)))))

(message "loaded aircomplete")
