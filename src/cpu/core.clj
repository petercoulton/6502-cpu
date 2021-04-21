(ns cpu.core
  (:gen-class)
  (:require [cpu.debug :refer :all]
            [cpu.helpers :refer :all]
            [cpu.instructions :refer :all]
            [cpu.assemble :refer :all]))

(defn step
  ([vm]
   (step vm false))
  ([vm debug?]
   (let [opcode      (next-opcode vm)
         instruction (lookup-instruction opcode @instructions)
         func        (if (not (nil? instruction))
                       (:func instruction)
                       (fn [vm] vm))]
     (if debug?
       (func (dump! vm))
       (func vm)))))

(defn run
  ([vm]
   (run vm false))
  ([vm debug?]
   (loop [vm (step vm debug?)]
     (if (not (cpu-brk-flag? vm))
       (recur (step vm debug?))
       vm))))

;
; Main
;

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


;
; Test
;

(def program-1
  "LDA 100 ; foo
   ADC 7
   STA 15
   LDX 12
   INX
   LDY 24
   BRK"
  )

(def program-2
  "LDX 128      ; 0
  LDA 0x77      ; 2
  STAX          ; 4
  INX           ; 5
  LDA 0x68      ; 6
  STAX          ; 8
  INX           ; 9
  LDA 0x6F      ; 10
  STAX          ; 12
  INX           ; 13
  LDA 0x20      ; 14
  STAX          ; 16
  INX           ; 17
  LDA 0x6c      ; 18
  STAX          ; 20
  INX           ; 21
  LDA 0x65      ; 22
  STAX          ; 24
  INX           ; 25
  LDA 0x74      ; 26
  STAX          ; 28
  INX           ; 29
  LDA 0x20      ; 30
  STAX          ; 32
  INX           ; 33
  LDA 0x74      ; 34
  STAX          ; 36
  INX           ; 37
  LDA 0x68      ; 38
  STAX          ; 40
  INX           ; 41
  LDA 0x65      ; 42
  STAX          ; 44
  INX           ; 45
  LDA 0x20      ; 46
  STAX          ; 48
  INX           ; 49
  LDA 0x64      ; 50
  STAX          ; 52
  INX           ; 53
  LDA 0x6F      ; 54
  STAX          ; 56
  INX           ; 57
  LDA 0x67      ; 58
  STAX          ; 60
  INX           ; 61
  LDA 0x73      ; 62
  STAX          ; 64
  INX           ; 65
  LDA 0x20      ; 66
  STAX          ; 68
  INX           ; 69
  LDA 0x6F      ; 70
  STAX          ; 72
  INX           ; 73
  LDA 0x75      ; 74
  STAX          ; 76
  INX           ; 77
  LDA 0x74      ; 78
  STAX          ; 80
  INX           ; 81
  LDA 0x20      ; 82
  STAX          ; 84
  INX           ; 85
  LDY 3         ; 86
  LDA 0x77      ; 88
  STAX          ; 90
  INX           ; 91
  LDA 0x68      ; 92
  STAX          ; 94
  INX           ; 95
  LDA 0x6F      ; 96
  STAX          ; 98
  INX           ; 99
  LDA 0x20      ; 100
  STAX          ; 102
  INX           ; 103
  DEY           ; 104
  CPY 0         ; 105
  BNE -19       ; 107
  BRK           ; 109")

(def program-3
  "LDX 128
  LDA 0x77
  STAX
  INX
  LDA 0x68
  STAX
  INX
  LDA 0x6F
  STAX
  INX
  LDA 0x20
  STAX
  INX
  LDA 0x6c
  STAX
  INX
  LDA 0x65
  STAX
  INX
  LDA 0x74
  STAX
  INX
  LDA 0x20
  STAX
  INX
  LDA 0x74
  STAX
  INX
  LDA 0x68
  STAX
  INX
  LDA 0x65
  STAX
  INX
  LDA 0x20
  STAX
  INX
  LDA 0x64
  STAX
  INX
  LDA 0x6F
  STAX
  INX
  LDA 0x67
  STAX
  INX
  LDA 0x73
  STAX
  INX
  LDA 0x20
  STAX
  INX
  LDA 0x6F
  STAX
  INX
  LDA 0x75
  STAX
  INX
  LDA 0x74
  STAX
  INX
  LDA 0x20
  STAX
  INX
  LDY 3     ; loop (y = 3)
  JSR 97    ;   call 'write who' subroutine
  DEY       ;   y--
  CPY 0     ;   test y==0
  BNE -5    ; while (y != 0)
  BRK
  LDA 0x77  ; start 'write who' subroutine
  STAX
  INX
  LDA 0x68
  STAX
  INX
  LDA 0x6F
  STAX
  INX
  LDA 0x20
  STAX
  INX
  RET       ; end 'write who' subroutine")

(def mem (load-program (asm program-1)))

(def cpu {:pc  0
          :ar  0
          :xr  0
          :yr  0
          :sp  (dec max-mem)
          :brk false
          :eq  false})

(def vm {:cpu cpu :mem mem})

;(dump!! (run vm))


