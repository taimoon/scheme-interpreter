(define IO_BANK0_BASE #x40014000)
(define SIO_BASE      #xD0000000)
(define TIMER_BASE    #x40054000)
(define TIMERAWL      (+ TIMER_BASE #x28))

(define GPIO_OUT_SET  (+ SIO_BASE #x14))
(define GPIO_OUT_CLR  (+ SIO_BASE #x18))
(define GPIO_OE_SET   (+ SIO_BASE #x24))
(define GPIO_OE_CLR   (+ SIO_BASE #x28))

(define (gpio-ctrl-addr pin) (+ IO_BANK0_BASE (+ (ash pin 3) #x04)))
(define (gpio-mask pin) (ash 1 pin))

(define (gpio-init pin)
  (%write-mem-32 (gpio-ctrl-addr pin) 5)        ; FUNCSEL = SIO
  (%write-mem-32 GPIO_OE_SET (gpio-mask pin)))  ; enable output

(define (gpio-on pin)
  (%write-mem-32 GPIO_OUT_SET (gpio-mask pin)))

(define (gpio-off pin)
  (%write-mem-32 GPIO_OUT_CLR (gpio-mask pin)))

(define (ms->us m) (* m 1000))

(define (delay-us us)
  (let ((start (%read-mem-32 TIMERAWL)))
    (let loop ()
      (if (< (- (%read-mem-32 TIMERAWL) start) us)
          (loop)))))

(define (blink pin times on-ms off-ms)
  (gpio-init pin)
  (let loop ((i times))
    (if (> i 0)
        (begin
          (gpio-on pin)
          (delay-us (ms->us on-ms))
          (gpio-off pin)
          (delay-us (ms->us off-ms))
          (loop (- i 1))))))

(blink 2 50 100 100)
