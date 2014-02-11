Since (cons-stream assertion THE-ASSERTIONS) will delay the second argument,
the set! will happen first.
Hence, it will becomes an infinite stream like (define ones (cons-stream 1 ones)) 