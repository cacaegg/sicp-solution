(define zero-crossing
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))
