# 6502 CPU emulator

A 6502 CPU emulator created during a Dojo.

+ http://www.8bitpickles.com/writing-a-cpu-emulator-as-a-dojo/

## Example

```clojure
(def program
  "LDA 100 ; foo
   ADC 7
   STA 15
   BRK"
  )

(def mem (load-program (asm program)))

(def cpu {:pc  0
          :ar  0
          :brk false})

(dump!! (run {:cpu cpu :mem mem}))

pc  = 0x07    7
ar  = 0x6b  107
brk = true
00000000  a9 64 69 07 8d 0f 00 00  00 00 00 00 00 00 00 6b  |.di............k|
00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
000000f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
=> nil
```

