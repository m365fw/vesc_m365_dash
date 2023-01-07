;UART configuration on the COMM port
(uart-start 115200 'half-duplex) ;uses only the tx pin
(gpio-configure 'pin-rx 'pin-mode-in-pu) ;configures rx pin for button presses

;Pre-defined fields for uart output
;Ninebot protocol: 5A A5 bLen bSrcAddr bDstAddr bCmd bArg bPayload[bLen] wChecksumLE
;Output will be: 5A A5 0x05 0x20(ESC) 0x21(BLE) 0x64 0x00 bFlags, bBattLevel, bHeadlightLevel, bBeeps, bSpeed, bErrorCode, wChecksumLE
;(define tx-frame (array-create 15))
;(bufset-u16 tx-frame 0 0x5AA5) ;Ninebot protocol
;(bufset-u8 tx-frame 2 0x05) ;Payload length is 5 bytes
;(bufset-u32 tx-frame 3 0x20216400) ; Packet is from ESC to BLE, 64 command, no arg

;Buffer for string incoming bytes from uart
(define uart-buf (array-create type-byte 64))


;(defun headoutputs(buffer) ;Dash wants information update, 64 command
    (progn
        ;bFlags field, mode field (1=drive, 2=eco, 4=sport, 8=charge, 16=off, 32=lock)
        (bufset-u8 tx-frame 7 4);temp set drive

        ;bBattLevel - 0-100 for NB, 0.0-1.0 from vesc
        (bufset-u8 tx-frame 8 (*(get-batt) 100))

        ;bHeadlightLevel - lamp status
        (bufset-u8 tx-frame 9 0);temp set off

        ;bBeeps - beeper
        (bufset-u8 tx-frame 10 0);temp set 0

        ;bSpeed - current speed, 0.1 kmh units
		(bufset-u8 tx-frame 11 0);temp set 0

        ;bErrorCode - error codes
		(bufset-u8 tx-frame 12 0);temp set 0

        ;wChecksumLE = 0xFFFF xor (16-bit sum of bytes <bLen bSrcAddr bDstAddr bCmd bArg bPayload[]>)
        ;(setvar 'crc 0)
        ;(looprange i 2 13
        ;    (setvar 'crc (+ crc (bufget-u8 tx-frame i))))
        ;(setvar 'c-out (bitwise-xor crc 0xFFFF))
        ;(bufset-u8 tx-frame 13 c-out)
        ;(bufset-u8 tx-frame 14 (shr c-out 8))

        ; write to uart
        ;(uart-write tx-frame)
		;(setvar 'uartout32 (bufget-u32 tx-frame 0))
    )
;)


;(defun head_inputs(buffer) ; Frame 0x65
    (progn
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
            (progn ; Driving mode
                (if (> current-speed min-speed)
                    (set-current-rel throttle)
                    (set-current-rel 0)
                )
                (if (not (= brake 0))
                    (set-brake-rel brake)
                )
            )
            (progn
                (set-current-rel 0) ; No throttle input when off or locked

                (if (= lock 1) ; Check if it is locked
                    (if (> current-speed min-speed) ; Brake when being pushed while locked
                        (set-brake-rel 1) ; Full power brake
                        (set-brake-rel 0) ; No brake
                    )
                    (set-brake-rel 0) ; No brake input when off
                )
            )
        )
    )
;)


;Ninebot: 5A A5 bLen bSrcAddr bDstAddr bCmd bArg bPayload[bLen] wChecksumLE
;wChecksumLE = 0xFFFF xor (16-bit sum of bytes <bLen bSrcAddr bDstAddr bCmd bArg bPayload[]>)
;This function receives data from uart, and decides if to update inputs or outputs of BLE
(defun ninebot-uart-decoder()
    (loopwhile t
        (progn
            (uart-read-bytes uart-buf 3 0)
			(if (= (bufget-u16 uart-buf 0) 0x5aa5) ;Check if protocol matches ninebot
				(progn
					(setvar 'bLen (bufget-u8 uart-buf 2)) ;get bLen, save it as bLen
					(setvar 'crc bLen); save bLen for checksum later
					(if (> bLen 0) ;We have a payload
						(progn
							(uart-read-bytes uart-buf (+ bLen 6) 0) ;read remaining 6 bytes + payload, overwrite buffer
							(setvar 'bSrcAddr (bufget-u8 uart-buf 0))
							(setvar 'bDstAddr (bufget-u8 uart-buf 1))
							(setvar 'bCmd (bufget-u8 uart-buf 2))
							(setvar 'bArg (bufget-u8 uart-buf 3))
							(setvar 'wChecksumLE (bufget-u16 uart-buf (+ bLen 4)))
							(looprange i 0 (+ bLen 4) ;prepare checksum, add values of bSrcAddr bDstAddr bCmd bArg bPayload to bLen
								(setvar 'crc (+ crc (bufget-u8 uart-buf i))))
								;little-endian is broken, do 0xFFFF xor (16-bit sum of bytes <bLen bSrcAddr bDstAddr bCmd bArg bPayload[]>)
							(setvar 'wChecksumLECalculated (bitwise-and (+ (shr (bitwise-xor crc 0xFFFF) 8) (shl (bitwise-xor crc 0xFFFF) 8)) 65535))
							(if (= wChecksumLE wChecksumLECalculated);If the calculated checksum matches with sent checksum, forward command
								(progn ;uart-buf is bSrcAddr bDstAddr bCmd bArg bPayload[bLen] wChecksumLE
									(if(= bCmd 0x65)
										;(head_inputs uart-buf)
									)
									(if(= bCmd 0x64)
										;(headoutputs uart-buf)
									)
								)
							)
						)
					)
				)
			)
        )
    )
)


; Make the ninebot-uart-decoder a parallel task with 150 heap alloc
(spawn 150 ninebot-uart-decoder)