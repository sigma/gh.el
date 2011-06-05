;;; gh-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (gh-api-v3 gh-api) "gh-api" "gh-api.el" (19947
;;;;;;  20831))
;;; Generated autoloads from gh-api.el

(eieio-defclass-autoload 'gh-api 'nil "gh-api" "Github API")

(eieio-defclass-autoload 'gh-api-v3 '(gh-api) "gh-api" "Github API v3")

;;;***

;;;### (autoloads (gh-oauth-authenticator gh-password-authenticator
;;;;;;  gh-authenticator) "gh-auth" "gh-auth.el" (19947 20863))
;;; Generated autoloads from gh-auth.el

(eieio-defclass-autoload 'gh-authenticator 'nil "gh-auth" "Abstract authenticator")

(eieio-defclass-autoload 'gh-password-authenticator '(gh-authenticator) "gh-auth" "Password-based authenticator")

(eieio-defclass-autoload 'gh-oauth-authenticator '(gh-authenticator) "gh-auth" "Oauth-based authenticator")

;;;***

;;;### (autoloads (gh-gist-gist gh-gist-api) "gh-gist" "gh-gist.el"
;;;;;;  (19947 20919))
;;; Generated autoloads from gh-gist.el

(eieio-defclass-autoload 'gh-gist-api '(gh-api-v3) "gh-gist" "Gist API")

(eieio-defclass-autoload 'gh-gist-gist 'nil "gh-gist" "Gist object")

;;;***

;;;### (autoloads nil nil ("gh-common.el" "gh.el") (19947 20937 987481))

;;;***

(provide 'gh-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gh-autoloads.el ends here
