;; Fix broken pgdup/pgdn behaviour
(require 'pager)
(global-set-key [next] 'pager-page-down)
(global-set-key [prior] 'pager-page-up)
