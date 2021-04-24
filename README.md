# 6502 CPU emulator

A 6502 CPU emulator created during a Dojo.

+ http://www.8bitpickles.com/writing-a-cpu-emulator-as-a-dojo/
+ [6502 Datasheeet](http://archive.6502.org/datasheets/wdc_w65c02s_oct_8_2018.pdf)

## Example

```clojure
(def program
  "LDA 100 ; foo
   ADC 7
   STA 15
   BRK"
  )

(def mem (load-program (assemble (parse program))))

(def cpu {:pc  0
          :ar  0
          :brk false})

(dump!! (run {:cpu cpu :mem mem}))

:pc  = 0x0c   12
:ar  = 0x6b  107
:xr  = 0x0d   13
:yr  = 0x18   24
:sp  = 0x27f  639
:brk = true
:eq  = false
00000000  a9 64 69 07 8d 0f a2 0c  e8 a0 18 00 00 00 00 6b  |.di............k|
00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000270  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
=> nil
```

