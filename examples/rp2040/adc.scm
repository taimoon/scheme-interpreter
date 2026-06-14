;; RP2040 ADC registers
(define ADC_CS     #x4004c000)
(define ADC_RESULT #x4004c004)

;; Pad control registers for GPIO26-29
(define PADS_BANK0_BASE #x4001c000)
(define (pads-gpio-addr gpio) (+ PADS_BANK0_BASE 4 (* 4 gpio)))

;; RESETS
(define RESETS_RESET   #x4000c000)
(define RESETS_DONE    #x4000c008)
(define RESET_BIT_ADC  (ash 1 0))

;; Clear ADC reset bit
(%write-mem-32 RESETS_RESET
  (bitwise-and (%read-mem-32 RESETS_RESET) (bitwise-not RESET_BIT_ADC)))

(define (wait-adc-reset)
  (if (= (bitwise-and (%read-mem-32 RESETS_DONE) RESET_BIT_ADC) 0)
      (wait-adc-reset)
      'done))
(wait-adc-reset)

;; Disable digital functions on a given ADC pad (gpio 26-29 -> channels 0-3)
(define (adc-pad-init channel)
  (let ((addr (pads-gpio-addr (+ 26 channel))))
    (%write-mem-32 addr
      (bitwise-ior (bitwise-and (%read-mem-32 addr) (bitwise-not #x40)) #x80))))

;; Enable ADC (EN=1), AINSEL=0 initially
(%write-mem-32 ADC_CS #x00000001)

(define (wait-adc-ready)
  (if (= (bitwise-and (%read-mem-32 ADC_CS) #x100) 0)
      (wait-adc-ready)
      'ready))
(wait-adc-ready)

;; Select input channel (AINSEL bits 12-14) and trigger conversion
(define (read-adc channel)
  (let ((cs (%read-mem-32 ADC_CS)))
    (%write-mem-32 ADC_CS
      (bitwise-ior
        (bitwise-and cs (bitwise-not (ash #x7 12))) ;; clear AINSEL
        (ash channel 12)))
    (%write-mem-32 ADC_CS
      (bitwise-ior (%read-mem-32 ADC_CS) #x4)) ;; START_ONCE
    (read-adc-wait)))

(define (read-adc-wait)
  (if (= (bitwise-and (%read-mem-32 ADC_CS) #x100) 0)
      (read-adc-wait)
      (%read-mem-32 ADC_RESULT)))

(define TIMER_BASE  #x40054000)
(define TIMERAWL    (+ TIMER_BASE #x28))
(define (ms->us m) (* m 1000))
(define (delay-us us)
  (let ((start (%read-mem-32 TIMERAWL)))
    (let loop ()
      (if (< (- (%read-mem-32 TIMERAWL) start) us)
          (loop)))))

;; Initialize pads for channels 0-3 (GPIO26-29)
(for-each adc-pad-init '(0 1 2 3))

;; Read all four channels for 10 times
(define (read-adc-all*)
  (let loop ((i 10))
    (if (> i 0)
        (begin
          (writeln
            (map
              (lambda (ch) (list ch (read-adc ch)))
              '(0 1 2 3)))
          (delay-us (ms->us 100))
          (loop (- i 1))))))

(read-adc-all*)
