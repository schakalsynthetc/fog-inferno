; Ngaro VM
; Copyright (c) 2010 - 2011, Charles Childers
; Copyright (c) 2011, Aleksej Saushev
; -----------------------------------------------------------------------------
(defvar memory  (make-array 1000001) "memory for image"      )
(defvar stack   (make-array 1025)    "data stack"            )
(defvar address (make-array 1025)    "address stack"         )
(defvar ports   (make-array 12)      "I/O ports"             )
(defvar ip      0                    "instruction pointer"   )
(defvar sp      0                    "data stack pointer"    )
(defvar rp      0                    "address stack pointer" )
; -----------------------------------------------------------------------------
(defun initialize-memory ()
  (loop for i from 0 to 1000000 do (setf (aref memory i) 0)))

(defun initialize-stacks ()
  (loop for i from 0 to 1024 do
        (setf (aref stack i) 0)
        (setf (aref address i) 0)
        (setf (aref ports i) 0)))

(defun bytes-to-int (b1 b2 b3 b4)
  (+ b1 (* b2 256) (* b3 65536) (* b4 16777216)))

(defun read-image-file (stream)
  (loop for i from 0 to (1- (/ (file-length stream) 4)) do
        (setf (aref memory i)
              (bytes-to-int (read-byte stream)
                            (read-byte stream)
                            (read-byte stream)
                            (read-byte stream)))
        (if (< 2147483647 (aref memory i))
            (setf (aref memory i) (- (aref memory i) 4294967296)))))

(defun load-image (filename)
  (with-open-stream
   (in (open filename :direction :input
             :element-type '(unsigned-byte 8)))
   (read-image-file in)))

(defun get-byte (offset index)
 (ldb (byte 8 offset) (aref memory index)))

(defun save-image (filename)
  (with-open-file (stream filename :direction :output :if-exists :overwrite
                          :element-type '(unsigned-byte 8))
    (loop for i from 0 to (aref memory 3) do
      (write-byte (get-byte  0 i) stream)
      (write-byte (get-byte  8 i) stream)
      (write-byte (get-byte 16 i) stream)
      (write-byte (get-byte 24 i) stream))))


(defun handle-devices ()
  (if (= (aref ports 0) 0)
      (cond
       ((= (aref ports 1) 1)
        (setf (aref ports 1) (char-code (read-char *standard-input*)))
        (when (= (aref ports 1) 13) (setf (aref ports 1) 10))
        (setf (aref ports 0) 1))
       ((= (aref ports 2) 1)
        (princ (code-char (aref stack sp)))
        (setf (aref ports 2) 0)
        (setq sp (1- sp))
        (setf (aref ports 0) 1))
       ((= (aref ports 4) 1)
        (save-image "retroImage")
        (setf (aref ports 4) 0)
        (setf (aref ports 0) 1))
       (t (case (aref ports 5)
            ((-1)  (setf (aref ports 0) 1) (setf (aref ports 5) 0))
            ((-2)  (setf (aref ports 0) 1) (setf (aref ports 5) 0))
            ((-3)  (setf (aref ports 0) 1) (setf (aref ports 5) 0))
            ((-4)  (setf (aref ports 0) 1) (setf (aref ports 5) 0))
            ((-5)  (setf (aref ports 0) 1) (setf (aref ports 5) sp))
            ((-6)  (setf (aref ports 0) 1) (setf (aref ports 5) rp))
            ((-7)  (setf (aref ports 0) 1) (setf (aref ports 5) 0))
            ((-8)  (setf (aref ports 0) 1) (setf (aref ports 5) (- (get-universal-time) 2208988800)))
            ((-9)  (setf (aref ports 0) 1) (setq ip 1000000) (setf (aref ports 5) 0))
            ((-10) (setf (aref ports 0) 1) (setf (aref ports 5) 0))
            ((-11) (setf (aref ports 0) 1) (setf (aref ports 5) 0))
            ((-12) (setf (aref ports 0) 1) (setf (aref ports 5) 0)))))))

(defun process ()
  (macrolet ((top () '(aref stack sp))
             (next () '(aref stack (1- sp)))
             (drop () '(setq sp (1- sp)))
             (ddrop () '(setq sp (- sp 2)))
             (psh () '(setq sp (1+ sp)))

             (rtop () '(aref address rp))
             (rdrop () '(setq rp (1- rp)))
             (rpsh () '(setq rp (1+ rp)))

             (inext () '(setq ip (1+ ip)))
             (icell () '(aref memory ip)))
    (let ((opcode (icell)))
      (case opcode
        ((0) ()) ; nop
        ((1) (psh) (inext) (setf (top) (icell))) ; lit
        ((2) (psh) (setf (top) (next))) ; dup
        ((3) (drop)) ; drop
        ((4)   ; swap
         (let ((i (top)))
           (setf (top) (next))
           (setf (next) i)))
        ((5) (rpsh) (setf (rtop) (top))  (drop)) ; push
        ((6)  (psh) (setf (top) (rtop)) (rdrop)) ; pop
        ((7)   ; loop
         (setf (top) (1- (top)))
         (if (> (top) 0)
             (progn (inext) (setq ip (1- (icell))))
           (progn (inext) (drop))))
        ((8)   ; jump
         (inext)
         (setq ip (1- (icell)))
         (if (= (aref memory (1+ ip)) 0) (inext))
         (if (= (aref memory (1+ ip)) 0) (inext)))
        ((9) (setq ip (rtop)) (rdrop))  ; return
        ((10)  ; > jump
         (let* ((i (top)) (z (next)))
           (inext)
           (if (> z i) (setq ip (1- (icell))))
           (ddrop)))
        ((11)  ; < jump
         (let* ((i (top)) (z (next)))
           (inext)
           (if (< z i) (setq ip (1- (icell))))
           (ddrop)))
        ((12)  ; != jump
         (let* ((i (top)) (z (next)))
           (inext)
           (if (/= z i) (setq ip (1- (icell))))
           (ddrop)))
        ((13)  ; == jump
         (let* ((i (top)) (z (next)))
           (inext)
           (if (= z i) (setq ip (1- (icell))))
           (ddrop)))
        ((14) (setf (top) (aref memory (top)))) ; @
        ((15) (setf (aref memory (top)) (next)) (ddrop)) ; !
        ((16) (setf (next) (+ (next) (top))) (drop))  ; +
        ((17) (setf (next) (- (next) (top))) (drop))  ; -
        ((18) (setf (next) (* (next) (top))) (drop))  ; *
        ((19)  ; /mod
         (let* ((i (top)) (z (next)))
           (setf (top) (truncate (/ z i)))
           (setf (next) (rem z i))))
        ((20) (setf (next) (logand (next) (top))) (drop))  ; and
        ((21) (setf (next) (logior (next) (top))) (drop))  ; or
        ((22) (setf (next) (logxor (next) (top))) (drop))  ; xor
        ((23) (setf (next) (ash (next) (top))) (drop))  ; <<
        ((24) (setf (next) (ash (next) (- (top)))) (drop))  ; >>
        ((25)  ; 0;
         (when (= (top) 0)
           (setq ip (rtop))
           (rdrop)
           (drop)))
        ((26) (setf (top) (1+ (top))))  ; 1+
        ((27) (setf (top) (1- (top))))  ; 1-
        ((28)  ; in
         (setf (top) (aref ports (top))))
        ((29)  ; out
         (setf (aref ports (top)) (next))
         (ddrop))
        ((30)  ; wait
         (handle-devices))
        (otherwise  ; call (implicit)
         (rpsh)
         (setf (rtop) ip)
         (setq ip (- opcode 1))
         (if (= (aref memory (1+ ip)) 0) (inext))
         (if (= (aref memory (1+ ip)) 0) (inext)))))))

(defun ngaro ()
  (initialize-memory)
  (initialize-stacks)
  (load-image "retroImage")
  (loop while (<= ip 1000000) do (process) (setq ip (1+ ip))))
; -----------------------------------------------------------------------------
(ngaro)
