#lang racket/base

(require "opcode-map.rkt")

(provide %instruction instruction-arity-map)

(define secondary-opcode-map
  (opcode-map
   ; TODO: this is incomplete
   [#x02 (instruction (LAR Gv Ew))]
   [#x03 (instruction (LSL Gv Ew))]
   [#x05 (instruction (SYSCALL))]
   [#x06 (instruction (CLTS))]
   [#x07 (instruction (SYSRET))]
   [#x08 (instruction (INVD))]
   [#x09 (instruction (WBINVD))]
   [#x0B (instruction (UD2))]
   [#x34 (instruction (SYSENTER) #:legacy)]
   [#x35 (instruction (SYSEXIT) #:legacy)]
   [#x40 (instruction* (CMOVO CMOVNO CMOVB CMOVNB CMOVZ CMOVNZ CMOVBE CMOVENBE CMOVS CMOVNS CMOVP CMOVNP CMOVL CMOVNL CMOVLE CMOVNLE) (Gv Ev))]
   [#x80 (instruction* (JO JNO JB JNB JZ JNZ JBE JNBE JS JNS JP JNP JL JNL JLE JNLE) (Jz) #:default-operand-size/64 64)]
   [#x90 (instruction* (SETO SETNO SETB SETNB SETZ SETNZ SETBE SETNBE SETS SETNS SETP SETNP SETL SETNL SETLE SETNLE) (Eb))]
   [#xA0 (instruction (PUSH FS))]
   [#xA1 (instruction (POP FS))]
   [#xA2 (instruction (CPUID))]
   [#xA3 (instruction (BT Ev Gv))]
   [#xA4 (instruction* SHLD [(Ev Gv Ib) (Ev Gv CL)])]
   [#xA8 (instruction (PUSH GS))]
   [#xA9 (instruction (POP GS))]
   [#xAA (instruction (RSM))]
   [#xAB (instruction (BTS Ev Gv))]
   [#xAC (instruction* SHRD [(Ev Gv Ib) (Ev Gv CL)])]
   [#xAF (instruction (IMUL Gv Ev))]
   [#xB0 (instruction* CMPXCHG [(Eb Gb) (Ev Gv)])]
   [#xB2 (instruction (LSS Gz Mp))]
   [#xB3 (instruction (BTR Ev Gv))]
   [#xB4 (instruction (LFS Gz Mp))]
   [#xB5 (instruction (LGS Gz Mp))]
   [#xB6 (instruction* MOVZX [(Gv Eb) (Gv Ew)])]
   [#xBB (instruction (BTC Ev Gv))]
   [#xBC (instruction (BSF Gv Ev))]
   [#xBD (instruction (BSR Gv Ev))]
   [#xBE (instruction* MOVSX [(Gv Eb) (Gv Ew)])]
   [#xC0 (instruction* XADD [(Eb Gb) (Ev Gv)])]
   [#xC8 (instruction*/+r (BSWAP r??))]))

(define secondary-opcode-map/F3
  (opcode-map
   ; TODO
   ))

(define secondary-opcode-map/66
  (opcode-map
   ; TODO
   ))

(define secondary-opcode-map/F2
  (opcode-map
   ; TODO
   ))

(define primary-opcode-map
  (opcode-map
   [#x00 (instruction* ADD [(Eb Gb) (Ev Gv) (Gb Eb) (Gv Ev) (AL Ib) (rAX Iz)])]
   [#x06 (instruction (PUSH ES) #:legacy)]
   [#x07 (instruction (POP ES) #:legacy)]
   [#x08 (instruction* OR [(Eb Gb) (Ev Gv) (Gb Eb) (Gv Ev) (AL Ib) (rAX Iz)])]
   [#x0E (instruction (PUSH CS) #:legacy)]
   [#x0F (legacy-prefix-map
          [#f secondary-opcode-map]
          [#xF3 secondary-opcode-map/F3]
          [#x66 secondary-opcode-map/66]
          [#xF2 secondary-opcode-map/F2])]
   [#x10 (instruction* ADC [(Eb Gb) (Ev Gv) (Gb Eb) (Gv Ev) (AL Ib) (rAX Iz)])]
   [#x16 (instruction (PUSH SS) #:legacy)]
   [#x17 (instruction (POP SS) #:legacy)]
   [#x18 (instruction* SBB [(Eb Gb) (Ev Gv) (Gb Eb) (Gv Ev) (AL Ib) (rAX Iz)])]
   [#x1E (instruction (PUSH DS) #:legacy)]
   [#x1F (instruction (POP DS) #:legacy)]
   [#x20 (instruction* AND [(Eb Gb) (Ev Gv) (Gb Eb) (Gv Ev) (AL Ib) (rAX Iz)])]
   ; #x26 is ES segment prefix
   [#x27 (instruction (DAA) #:legacy)]
   [#x28 (instruction* SUB [(Eb Gb) (Ev Gv) (Gb Eb) (Gv Ev) (AL Ib) (rAX Iz)])]
   ; #x2E is CS segment prefix
   [#x2F (instruction (DAS) #:legacy)]
   [#x30 (instruction* XOR [(Eb Gb) (Ev Gv) (Gb Eb) (Gv Ev) (AL Ib) (rAX Iz)])]
   ; #x36 is SS segment prefix
   [#x37 (instruction (AAA) #:legacy)]
   [#x38 (instruction* CMP [(Eb Gb) (Ev Gv) (Gb Eb) (Gv Ev) (AL Ib) (rAX Iz)])]
   ; #x3E is DS segment prefix
   [#x3F (instruction (AAS) #:legacy)]
   [#x40 (instruction*/+r (INC e??) #:legacy)]
   [#x48 (instruction*/+r (DEC e??) #:legacy)]
   [#x50 (instruction*/+r (PUSH r??) #:default-operand-size/64 64)]
   [#x58 (instruction*/+r (POP r??)  #:default-operand-size/64 64)]
   [#x60 (instruction/16+32 (PUSHA) (PUSHAD) #:legacy)]
   [#x61 (instruction/16+32 (POPA) (POPAD) #:legacy)]
   ;[#x62 (instruction (BOUND Gv Ma) #:legacy)]
   [#x63 (instruction/legacy+64 (ARPL Ew Gw) (MOVSXD Gv Ez))]
   ; #x64 is FS segment prefix
   ; #x65 is GS segment prefix
   ; #x66 is operand size override prefix
   ; #x67 is address size override prefix
   [#x68 (instruction (PUSH Iz) #:default-operand-size/64 64)]
   [#x69 (instruction (IMUL Gv Ev Iz))]
   [#x6A (instruction (PUSH Ib) #:default-operand-size/64 64)]
   [#x6B (instruction (IMUL Gv Ev Ib))]
   [#x6C (instruction (INSB))]
   [#x6D (instruction/16+32 (INSW) (INSD))]
   [#x6E (instruction (OUTSB))]
   [#x6F (instruction/16+32 (OUTSW) (OUTSD))]
   [#x70 (instruction* (JO JNO JB JNB JZ JNZ JBE JNBE JS JNS JP JNP JL JNL JLE JNLE) (Jb) #:default-operand-size/64 64)]
   [#x80 (modrm-reg-map (instruction* (ADD OR ADC SBB AND SUB XOR CMP) (Eb Ib)))]
   [#x81 (modrm-reg-map (instruction* (ADD OR ADC SBB AND SUB XOR CMP) (Ev Iz)))]
   [#x82 (modrm-reg-map (instruction* (ADD OR ADC SBB AND SUB XOR CMP) (Eb Ib) #:legacy))]
   [#x83 (modrm-reg-map (instruction* (ADD OR ADC SBB AND SUB XOR CMP) (Ev Ib)))]
   [#x84 (instruction* TEST [(Eb Gb) (Ev Gv)])]
   [#x86 (instruction* XCHG [(Eb Gb) (Ev Gv)])]
   [#x88 (instruction* MOV [(Eb Gb) (Ev Gv) (Gb Eb) (Gv Ev) (Mw/Rv Sw)])]
   [#x8D (instruction (LEA Gv M))]
   [#x8E (instruction (MOV Sw Ew))]
   [#x8F (modrm-reg-map
          (instruction (POP Ev) #:default-operand-size/64 64)
          #f #f #f #f #f #f #f)]
   [#x90 (instruction*/+r (XCHG r?? rAX))]
   [#x98 (instruction/16+32+64 (CBW) (CWDE) (CDQE))]
   [#x99 (instruction/16+32+64 (CWD) (CDQ) (CQO))]
   [#x9A (instruction (CALL Ap) #:legacy)]
   ; TODO: WAIT / FWAIT
   [#x9C (instruction/16+32+64 (PUSHF) (PUSHD) (PUSHQ) #:default-operand-size/64 64)]
   [#x9D (instruction/16+32+64 (POPF) (POPD) (POPQ) #:default-operand-size/64 64)]
   [#x9E (instruction (SAHF))]
   [#x9F (instruction (LAHF))]
   [#xA0 (instruction* MOV [(AL Ob) (rAX Ov) (Ob AL) (Ov rAX)])]
   [#xA4 (instruction (MOVSB))]
   [#xA5 (instruction/16+32+64 (MOVSW) (MOVSD) (MOVSQ))]
   [#xA6 (instruction (CMPSB))]
   [#xA7 (instruction/16+32+64 (CMPSW) (CMPSD) (CMPSQ))]
   [#xA8 (instruction* TEST [(AL Ib) (rAX Iz)])]
   [#xAA (instruction (STOSB))]
   [#xAB (instruction/16+32+64 (STOSW) (STOSD) (STOSQ))]
   [#xAC (instruction (LODSB))]
   [#xAD (instruction/16+32+64 (LODSW) (LODSD) (LODSQ))]
   [#xAE (instruction (SCASB))]
   [#xAF (instruction/16+32+64 (SCASW) (SCASD) (SCASQ))]
   [#xB0 (instruction*/+r (MOV b?? Ib))]
   [#xB8 (instruction*/+r (MOV r?? Iv))]
   [#xC0 (modrm-reg-map (instruction* (ROL ROR RCL RCR SHL SHR SAL SAR) (Eb Ib)))]
   [#xC1 (modrm-reg-map (instruction* (ROL ROR RCL RCR SHL SHR SAL SAR) (Ev Ib)))]
   [#xC2 (instruction* RET [(Iw) ()] #:default-operand-size/64 64)]
   [#xC4 (instruction (LES Gz Mp) #:legacy)]
   [#xC5 (instruction (LDS Gz Mp) #:legacy)]
   [#xC6 (modrm-reg-map (instruction (MOV Eb Ib)) #f #f #f #f #f #f #f)]
   [#xC7 (modrm-reg-map (instruction (MOV Ev Iz)) #f #f #f #f #f #f #f)]
;   [#xC8 (instruction (ENTER Iw Ib) #:default-operand-size/64 64)]
   [#xC9 (instruction (LEAVE) #:default-operand-size/64 64)]
   [#xCA (instruction* RETF [(Iw) ()])]
   [#xCC (instruction (INT3))]
   [#xCD (instruction (INT Ib))]
   [#xCE (instruction (INTO) #:legacy)]
   [#xCF (instruction/16+32+64 (IRET) (IRETD) (IRETQ))]
   [#xD0 (modrm-reg-map (instruction* (ROL ROR RCL RCR SHL SHR SAL SAR) (Eb 1)))]
   [#xD1 (modrm-reg-map (instruction* (ROL ROR RCL RCR SHL SHR SAL SAR) (Ev 1)))]
   [#xD2 (modrm-reg-map (instruction* (ROL ROR RCL RCR SHL SHR SAL SAR) (Eb CL)))]
   [#xD3 (modrm-reg-map (instruction* (ROL ROR RCL RCR SHL SHR SAL SAR) (Ev CL)))]
   ;[#xD4 (instruction (AAM #:base [Ib 10]) #:legacy)]
   ;[#xD5 (instruction (AAD #:base [Ib 10]) #:legacy)]
   [#xD7 (instruction (XLATB))]
   ; #xD8 ~ #xDF are x87 instructions
   [#xE0 (instruction (LOOPNE Jb) #:default-operand-size/64 64)]
   [#xE1 (instruction (LOOPE Jb) #:default-operand-size/64 64)]
   [#xE2 (instruction (LOOP Jb) #:default-operand-size/64 64)]
   [#xE3 (instruction/16+32+64 (JCXZ Jb) (JECXZ Jb) (JRCXZ Jb) #:default-operand-size/64 64)]
   [#xE4 (instruction* IN [(AL Ib) (eAX Ib)])]
   [#xE6 (instruction* OUT [(Ib AL) (Ib eAX)])]
   [#xE8 (instruction (CALL Jz) #:default-operand-size/64 64)]
   [#xE9 (instruction (JMP Jz) #:default-operand-size/64 64)]
   [#xEA (instruction (JMP Ap) #:legacy)]
   [#xEB (instruction (JMP Jb) #:default-operand-size/64 64)]
   [#xEC (instruction* IN [(AL DX) (eAX DX)])]
   [#xEE (instruction* OUT [(DX AL) (DX eAX)])]
   ; #xF0 is LOCK prefix
   [#xF1 (instruction (INT1))]
   ; #xF2 is REPNE prefix
   ; #xF3 is REP/REPE prefix
   [#xF4 (instruction (HLT))]
   [#xF5 (instruction (CMC))]
   [#xF6 (modrm-reg-map (instruction* ((TEST Eb Ib) (TEST Eb Ib) NOT NEG MUL IMUL DIV IDIV) (Eb)))]
   [#xF7 (modrm-reg-map (instruction* ((TEST Ev Iz) (TEST Ev Iz) NOT NEG MUL IMUL DIV IDIV) (Ev)))]
   [#xF8 (instruction (CLC))]
   [#xF9 (instruction (STC))]
   [#xFA (instruction (CLI))]
   [#xFB (instruction (STI))]
   [#xFC (instruction (CLD))]
   [#xFD (instruction (STD))]
   [#xFE (modrm-reg-map
          (instruction (INC Eb))
          (instruction (DEC Eb))
          #f #f #f #f #f #f)]
   [#xFF (modrm-reg-map
          (instruction (INC Ev))
          (instruction (DEC Ev))
          (instruction (CALL Ev) #:default-operand-size/64 64)
          (instruction (CALL Mp))
          (instruction (JMP Ev) #:default-operand-size/64 64)
          (instruction (JMP Mv))
          (instruction (PUSH Ev) #:default-operand-size/64 64)
          #f)]))

(define-instruction-set (%instruction instruction-arity-map) primary-opcode-map)
