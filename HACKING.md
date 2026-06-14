# FFI: Extending Scheme Using C

In a freestanding implementation (e.g.: baremetal), FFI means registering a fixed table of functions at build time. In a hosted implementation, FFI additionally allows discovering functions at runtime.

## Hosted Implementation

Let's implement `sleep-ms` in C, since it requires OS services or hardware access, which is generally unsafe to do directly in Scheme.

Add a new function in `scheme.c`:

```c
#if SCM_RAW_FFI && __STDC_HOSTED__
#include <time.h>
word_t s_sys_sleep_ms(word_t _ms) {
    S_ffi_sword_t ms = s_ffi_to_sword(_ms);
    struct timespec ts;

    ts.tv_sec = ms.x / 1000;
    ts.tv_nsec = (ms.x % 1000) * 1000000;

    while (nanosleep(&ts, &ts) == -1) {
        // If interrupted by a signal, nanosleep updates the remaining time in ts
        continue;
    }
    return VOID_TAG;
}
#endif
```

To test it, write `sleep.scm`:

```scm
(define sleep-ms (make-foreign-procedure s_sys_sleep_ms 1))
(sleep-ms (* 3 1000))
```

Then execute it:

```bash
./test.sh

export SCM_BOOT=kernel-rider.scm
export HEAP_SIZE=16777216

time (BATCH_MODE= ./interp.out kernel-exp.scm sleep.scm)
```

## Baremetal Example: RP2040

NOTE: this approach is also applicable to hosted targets.

Add the following to `scheme.c`:

```c
void s_pr_sleep_ms(); // <--

void s_rt_init(void* heap_start, void* heap_end, int argc, char ** argv) {
    // some code

    // register new primitive
    s_rt_add_prim("sleep-ms", s_pr_sleep_ms);  // <--

    // more code
}

// some code

void s_pr_sleep_ms() {
    _RETC = 1;
    // VAL holds the args as (arg0 arg1 ...)
    S_ffi_sword_t ms = s_ffi_to_sword(CAR(VAL));
#if SCM_RAW_FFI && __STDC_HOSTED__
    struct timespec ts;

    ts.tv_sec = ms.x / 1000;
    ts.tv_nsec = (ms.x % 1000) * 1000000;

    while (nanosleep(&ts, &ts) == -1) {
        continue;
    }
#endif
#ifdef PICO_BOARD
    sleep_ms(ms.x);
#endif
    VAL = VOID_TAG;
    NEXT = _s_apply_cont;
}
```

Add the following to the `sys_port.h` to make the compiler happy.

```c
#ifdef PICO_BOARD
#include "pico/time.h"
#endif
```

Recompile and flash the image. You can now call the new primitive on the RP2040:

```
Scheme REPL
> sleep-ms
#<procedure>

> (sleep-ms 10)

> (define (s->ms s) (* s 1000))

> (sleep-ms (* 3 1000))
```

# Hacking scheme on RP2040

## Limitation

- Cannot handle large programs
- No multitasking support
- Execution cannot be interrupted once the interpreter starts evaluating

## Preparation

Run the following script to build the host interpreter, RP2040 firmware (build/main.uf2), execute tests, and generate both kernel-rider.scm and kernel-free.scm:

```bash
./test.sh
```

You can then run the expansion with:

```bash
export SCM_BOOT=kernel-rider.scm
export HEAP_SIZE=16777216

BATCH_MODE= ./interp.out -F kernel-free.scm kernel.scm
```

You may also concatenate additional Scheme files as needed:

```
BATCH_MODE= ./interp.out -F kernel-free.scm kernel.scm examples/rp2040/lib.scm
```

The generated `kernel-free.scm` is intended to be copied into the REPL.

## Flashing the RP2040

Flash `main.uf2` onto the Pico board. After booting:

1. Press **Enter twice** to open the REPL
2. You should see:
    ```
    Scheme REPL
    >
    ```
3. Paste the contents of `kernel-free.scm` to load the core runtime functions

## Blink LED

1. Connect an LED to GPIO 2.
2. Copy and paste the content of [blink.scm](examples/rp2040/blink.scm)


## Read ADC value

1. Copy and paste the content of [lib.scm](examples/rp2040/lib.scm)
2. Copy and paste the content of [adc.scm](examples/rp2040/adc.scm)

## ADC-Controlled LED Blinking

### Hardware setup
- Connect a rotation sensor to GPIO 26
- Connect an LED to GPIO 2

### Build with all required modules

After the preparation step, you can batch multiple files to reduce manual copying:

```bash
export SCM_BOOT=kernel-rider.scm
export HEAP_SIZE=16777216

BATCH_MODE= ./interp.out -F kernel-free.scm kernel.scm \
  examples/rp2040/lib.scm \
  examples/rp2040/blink.scm \
  examples/rp2040/adc.scm
```

Copy and paste the content of `kernel-free.scm`

Then Copy and paste the content of [adc-led.scm](examples/rp2040/adc-led.scm)

## Embrace REPL

Once the REPL is running, you don’t need to reboot (as long as memory is sufficient) or reflash to make changes. You can redefine functions and write new programs directly in the session, and the hardware responds immediately.

This makes it easy to experiment, tweak behavior, and learn how the RP2040 works through fast, interactive feedback.

See: [Micropython](https://github.com/micropython/micropython)
