(mapcar fn
        (do* ((x 1 (1+ x))
              (result (list x) (push x result)))
            ((= x 10) (nreverse result))))
