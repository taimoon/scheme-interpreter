(define UART0_BASE #x40034000)
(define UART1_BASE #x40038000)

(define UARTDR #x00)
(define UARTFR #x18)

;; UARTFR TXFF bit (bit 5)
(define UARTFR_TXFF (ash 1 5))

(define (uart-putc uart-base ch)
  ;; wait until TX FIFO is not full
  (let loop ()
    (if (= 0 (bitwise-and (%read-mem-32 (+ uart-base UARTFR)) UARTFR_TXFF))
        #t
        (loop)))

  ;; write only low 8 bits into UARTDR
  (%write-mem-32 (+ uart-base UARTDR) (bitwise-and (char->integer ch) #xff)))

(define (uart-puts uart-base s)
  (let loop ((i 0))
    (if (< i (string-length s))
        (begin
          (uart-putc uart-base (string-ref s i))
          (loop (+ i 1))))))

(uart-puts UART0_BASE "hello from scheme to uart0!\r\n")
(uart-puts UART1_BASE "hello from scheme to uart1!\r\n")