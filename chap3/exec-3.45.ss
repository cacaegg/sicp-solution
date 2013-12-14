; when serialized-exchange is called, it try to call a serialized-deposit on account,
; then the deposit will be lock out due the serialized-exchange hasn't finished.
; Then the procedure will never finished.
