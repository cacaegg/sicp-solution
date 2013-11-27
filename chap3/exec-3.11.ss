;    |-------------------------------------------------------|
;    | make-account                                          | 
;    |     |                             acc:                |
;    |     |                              |        acc2:----------->O O
;    |_____|______________________________|__________________|      | |
;          |           ^                  |   |-----------|         | |  
;          |           |            |---------|balance:100|------------
;          |     |-------------|    |     |   |-----------|E3       |  
;          |     |dispatch:... |    /     |                         |
;          |     |withdraw:... |<---      |   |-----------|         |
;          |     |deposit:...  |<-------------|balance:30 |         |
;          |     |_____________|          |   |           |         |
;          |     E1    ^                  |   |___________|         |
;          |           |                  |      |      E2          |
;         O O ---------|                 O O ----|    _____________/ 
;    paramter:balance                    |           /                     
;    body:                               v          v   
;      (define: (withdraw amount))     pamameter: msg
;      (define: (deposit amount))      body:
;      (define: (dispath m))             (cond ((eq? msg 'withdraw) withdraw)
;      dispatch                                ...)
;
;
; 1. local state of acc is kept in E2
; 2. The two account's local state is kept in E2 and E3
; 3. acc & acc2 share the body

