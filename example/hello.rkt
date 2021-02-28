#lang racket

(require asm
         asm/x86
         binutils
         binutils/elf)

;; This is an amd64 Linux program to print "hello, world".
;; Call (make) which will write it out to /tmp/hello.o and
;; invoke ld to create a /tmp/hello executable.

(define hello-object
  (assemble
   #:section .text
   #:global _start
   ;; Write string
   (mov rax 1)   ; sys_write
   (mov rdi 1)   ; fd (1 = stdout)
   (mov rsi msg) ; buf
   (mov rdx 13)  ; count
   (syscall)
   ;; Exit
   (mov rax 60)  ; sys_exit
   (mov rdi 0)   ; error_code
   (syscall)
   #:section .data
   #:datum msg #"hello, world\n"))

(define hello-elf (bin:object->elf hello-object))

(define (make)
  (call-with-output-file "/tmp/hello.o"
    #:exists 'replace
    (λ (out) (write-elf hello-elf out)))
  (parameterize ([current-input-port (open-input-bytes #"")])
    ;(system "objdump -xD /tmp/hello.o")
    (system "ld -s -o /tmp/hello /tmp/hello.o")))
