;; Needed to kick off the process.
;; Makefile expects to find stst.s,
;; so the ball starts rolling here.
(system "./retest-true")

;; Load the driver
(load "tests-driver.scm")

;; Run the tests
(lloop-test)
