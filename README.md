# 6502 CPU emulator

A 6502 CPU emulator created during a Dojo.

+ http://www.8bitpickles.com/writing-a-cpu-emulator-as-a-dojo/

## Example

```clojure
(def mem (load-program [0xa9 100   ; LDA 100
                        0x69 7     ; ADC 7 
                        0x8d 15    ; STA 15
                        ]))

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

