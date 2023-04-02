; M365 dashboard compability lisp script v0.6 by Netzpfuscher and 1zuna
; UART Wiring: red=5V black=GND yellow=COM-TX (UART-HDX) green=COM-RX (button)+3.3V with 1K Resistor
; Guide (German): https://rollerplausch.com/threads/vesc-controller-einbau-1s-pro2-g30.6032/
; Tested on VESC 6.0 on PRO2 w/ Makerbase 75100 Alu PCB

; **** User parameters ****
;Calibrate throttle min max
(define cal-thr-lo 41.0)
(define cal-thr-hi 167.0)
(define thr-deadzone 0.05)

;Calibrate brake min max
(define cal-brk-lo 39.0)
(define cal-brk-hi 179.0)
(define brk-deadzone 0.05)
(define brk-minspeed 1)

(define light-default 0)
(define show-faults 1)
(define show-batt-in-idle 1)
(define min-speed 1)
(define button-safety-speed (/ 0.1 3.6)) ; disabling button above 0.1 km/h (due to safety reasons)

; Speed modes with MAX KM/H and MAX WATTS
(define eco-speed (/ 7 3.6))
(define eco-current 0.6)
(define eco-watts 400)
(define drive-speed (/ 17 3.6))
(define drive-current 0.7)
(define drive-watts 500)
(define sport-speed (/ 21 3.6)) ; or 400
(define sport-current 1.0)
(define sport-watts 700) ; or 1500000

(define secret-enabled 1)
(define secret-eco-speed (/ 25 3.6))
(define secret-eco-current 0.6)
(define secret-eco-watts 1500000)
(define secret-drive-speed (/ 45 3.6))
(define secret-drive-current 0.8)
(define secret-drive-watts 1500000)
(define secret-sport-speed (/ 400 3.6))
(define secret-sport-current 1.0)
(define secret-sport-watts 1500000)

; **** Code section ****
(uart-start 115200 'half-duplex)
(gpio-configure 'pin-rx 'pin-mode-in-pu)

(define tx-frame (array-create 14))
(bufset-u16 tx-frame 0 0x55AA)
(bufset-u16 tx-frame 2 0x0821)
(bufset-u16 tx-frame 4 0x6400)

(define uart-buf (array-create type-byte 64))
(define current-speed 0)
(define throttle-in 0)
(define throttle 0)
(define brake-in 0)
(define brake 0)
(define buttonold 0)
(define light 0)
(setvar 'light light-default)
(define c-out 0)
(define code 0)

; Button handling

(define presstime (systime))
(define presses 0)

; Mode states

(define off 0)
(define lock 0)
(define speedmode 4)
(define unlock 0)

; Sound feedback

(define feedback 0)

(defun adc-input(buffer) ; Frame 0x65
    {
        (setvar 'current-speed (* (get-speed) 3.6))
        
        ; Throttle
        (setvar 'throttle-in (bufget-u8 uart-buf 4))
        (setvar 'throttle (/(- throttle-in cal-thr-lo) cal-thr-hi))
        
        (if (< throttle thr-deadzone)
            (setvar 'throttle 0)
        )
        (if (> throttle 1)
            (setvar 'throttle 1)
        )
        
        ; Brake
        (setvar 'brake-in (bufget-u8 uart-buf 5))
        (setvar 'brake (/(- brake-in cal-brk-lo) cal-brk-hi))
        
        (if(< brake brk-deadzone)
            (setvar 'brake 0)
        )
        (if (< current-speed brk-minspeed)
            (setvar 'brake 0)
        )
        (if (> brake 1)
            (setvar 'brake 1)
        )

        (if (= (+ off lock) 0)
            { ; Driving mode
                (if (> current-speed min-speed)
                    (set-current-rel throttle)
                    (set-current-rel 0)
                )
                (if (not (= brake 0))
                    (set-brake-rel brake)
                )
            }
            {
                (set-current-rel 0) ; No throttle input when off or locked
                
                (if (= lock 1) ; Check if it is locked
                    (if (> current-speed min-speed) ; Brake when being pushed while locked
                        (set-brake-rel 1) ; Full power brake
                        (set-brake-rel 0) ; No brake
                    )
                    (set-brake-rel 0) ; No brake input when off
                )
            }
        )
    }
)

(defun update-dash(buffer) ; Frame 0x64
    {
        ; mode field (1=drive, 2=eco, 4=sport, 8=charge, 16=off, 32=lock)
        (if (= off 1)
            (bufset-u8 tx-frame 6 16)
            (if (= lock 1)
                (bufset-u8 tx-frame 6 32) ; lock display
                (if (< (get-temp-fet) 60) ; temp icon will show up above 60 degree
                    (bufset-u8 tx-frame 6 speedmode)
                    (bufset-u8 tx-frame 6 (+ 128 speedmode))
                )
                
            )
        )
        
        ; batt field
        (bufset-u8 tx-frame 7 (*(get-batt) 100))

        ; light field
        (if (= off 0)
            (bufset-u8 tx-frame 8 light)
            (bufset-u8 tx-frame 8 0)
        )
        
        ; beep field
        (if (= lock 1)
            (if (> (* (get-speed) 3.6) min-speed)
                (bufset-u8 tx-frame 9 1) ; beep lock
                (bufset-u8 tx-frame 9 0))
            (if (> feedback 0)
                {
                    (bufset-u8 tx-frame 9 1)
                    (setvar 'feedback (- feedback 1))
                }
                (bufset-u8 tx-frame 9 0)
            )
        )

        ; speed field
        (if (= (+ show-batt-in-idle unlock) 2)
            (if (> (* (get-speed) 3.6) 1)
                (bufset-u8 tx-frame 10 (* (get-speed) 3.6))
                (bufset-u8 tx-frame 10 (*(get-batt) 100)))
            (bufset-u8 tx-frame 10 (* (get-speed) 3.6))
        )
        
        ; error field
        (if (= show-faults 1)
            (bufset-u8 tx-frame 11 (get-fault))
        )

        ; calc crc

        (setvar 'crc 0)
        (looprange i 2 12
            (setvar 'crc (+ crc (bufget-u8 tx-frame i))))
        (setvar 'c-out (bitwise-xor crc 0xFFFF)) 
        (bufset-u8 tx-frame 12 c-out)
        (bufset-u8 tx-frame 13 (shr c-out 8))

        ; write
        (uart-write tx-frame)
    }
)

(defun read-frames()
    (loopwhile t
        {
            (uart-read-bytes uart-buf 3 0)
            (if (= (bufget-u16 uart-buf 0) 0x55aa)
                {
                    (setvar 'len (bufget-u8 uart-buf 2))
                    (setvar 'crc len)
                    (if (and (> len 0) (< len 60)) ; max 64 bytes
                        {
                            (uart-read-bytes uart-buf (+ len 4) 0)
                            (looprange i 0 len
                                (setvar 'crc (+ crc (bufget-u8 uart-buf i))))
                            (if (=(+(shl(bufget-u8 uart-buf (+ len 2))8) (bufget-u8 uart-buf (+ len 1))) (bitwise-xor crc 0xFFFF))
                                (handle-frame (bufget-u8 uart-buf 1))
                            )
                        }
                    )
                }
            )
        }
    )
)

(defun handle-frame(code)
    {
        (if(= code 0x65)
            (adc-input uart-buf)
        )

        ;(if(= code 0x64)
            (update-dash uart-buf)
        ;)
    }
)

(defun handle-button()
    (if (= presses 1) ; single press
        (if (= off 1) ; is it off? turn on scooter again
            {
                (setvar 'off 0) ; turn on
                (setvar 'feedback 1) ; beep feedback
                (setvar 'unlock 0) ; Disable unlock on turn off
                (apply-mode) ; Apply mode on start-up
                (stats-reset) ; reset stats when turning on
            }
            (setvar 'light (bitwise-xor light 1)) ; toggle light
        )
        (if (>= presses 2) ; double press
            {
                (if (> (/(- brake-in cal-brk-lo) cal-brk-hi) brk-deadzone) ; if brake is pressed
                    (if (and (= secret-enabled 1) (> (/(- throttle-in cal-thr-lo) cal-thr-hi) thr-deadzone))
                        {
                            (setvar 'unlock (bitwise-xor unlock 1))
                            (setvar 'feedback 2) ; beep 2x
                            (apply-mode)
                        }
                        {
                            (setvar 'lock (bitwise-xor lock 1)) ; lock on or off
                            (setvar 'feedback 1) ; beep feedback
                        }
                    )
                    {
                        (if (= speedmode 1)
                            (setvar 'speedmode 4)
                            (if (= speedmode 2)
                                (setvar 'speedmode 1)
                                (if (= speedmode 4)
                                    (setvar 'speedmode 2)
                                )
                            )
                        )
                        (apply-mode)
                    }
                )
            }
        )
    )
)

(defun handle-holding-button()
    {
        (if (= (+ lock off) 0) ; it is locked and off?
            {
                (setvar 'unlock 0) ; Disable unlock on turn off
                (apply-mode)
                (setvar 'off 1) ; turn off
                (setvar 'feedback 1) ; beep feedback
            }
        )
    }
)

(defun reset-button()
    {
        (setvar 'presstime (systime)) ; reset press time again
        (setvar 'presses 0)
    }
)

; Speed mode implementation

(defun apply-mode()
    (if (= unlock 0)
        (if (= speedmode 1)
            (configure-speed drive-speed drive-watts drive-current)
            (if (= speedmode 2)
                (configure-speed eco-speed eco-watts eco-current)
                (if (= speedmode 4)
                    (configure-speed sport-speed sport-watts sport-current)
                )
            )
        )
        (if (= speedmode 1)
            (configure-speed secret-drive-speed secret-drive-watts secret-drive-current)
            (if (= speedmode 2)
                (configure-speed secret-eco-speed secret-eco-watts secret-eco-current)
                (if (= speedmode 4)
                    (configure-speed secret-sport-speed secret-sport-watts secret-sport-current)
                )
            )
        )
    )
)

(defun configure-speed(speed watts current)
    {
        (conf-set 'max-speed speed)
        (conf-set 'l-watt-max watts)
        (conf-set 'l-current-max-scale current)
    }
)

; Apply mode on start-up
(apply-mode)

; Spawn UART reading frames thread
(spawn 150 read-frames) 

(loopwhile t
    {
        (if (> buttonold (gpio-read 'pin-rx))
            {
                (setvar 'presses (+ presses 1))
                (setvar 'presstime (systime))
            }
            (if (> (- (systime) presstime) 2500) ; after 2500 ms
                (if (= (gpio-read 'pin-rx) 0) ; check button is still pressed
                    (if (> (- (systime) presstime) 6000) ; long press after 6000 ms
                        {
                            (if (<= (get-speed) button-safety-speed)
                                (handle-holding-button)
                            )
                            (reset-button) ; reset button
                        }
                    )
                    { ; when button not pressed
                        (if (> presses 0) ; if presses > 0
                            {
                                (if (<= (get-speed) button-safety-speed)
                                    (handle-button) ; handle button presses
                                )
                                (reset-button) ; reset button
                            }
                        )
                    }
                )
            )
        )

        (setvar 'buttonold (gpio-read 'pin-rx))
        (sleep 0.05) ; Recude load on the CPU
    }
)