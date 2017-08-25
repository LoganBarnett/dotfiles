;;; money -- monetary calculations
;;; Commentary:
;; A module relating to monetary calculations.
;;; Code:

;; for reference - this was a port of the JS version (it might be incomplete
;; port-wise) on this site:
;; https://home.ubalt.edu/ntsbarsh/Business-stat/otherapplets/CompoundCal.htm#rjava9
;; in the "Accelerating Mortgage Payments" form.
(defun my/debt-months-remaining (principle monthly-payment interest-rate)
  "Calculates the months (payments) remaining on a PRINCIPLE given a MONTHLY-PAYMENT with an INTEREST-RATE."
  (- (log
      (- 1
         (/ (* principle interest-rate) monthly-payment)
         )
      (+ 1 interest-rate)
      )))

(defun my/debt-months-remaining-from-org-table (principle
                                                monthly-payment
                                                interest-rate)
  "(Assumes an org table for inputs) Calculates the months (payments) remaining on a PRINCIPLE given a MONTHLY-PAYMENT with an INTEREST-RATE."
  (my/debt-months-remaining (string-to-number principle)
                            (string-to-number monthly-payment)
                            (/ (string-to-number interest-rate) 1200)
  ))

;;; money.el ends here
