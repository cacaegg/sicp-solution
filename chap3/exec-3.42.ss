; If might failed to serialize since they are the same serializer,
; the lock will not block them between different procedures.
;
; That is, the serilizer implements its own local state to make sure 
; it can serilize the other people.
