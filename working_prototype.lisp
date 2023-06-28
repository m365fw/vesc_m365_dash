;UART configuration on the COMM port
(uart-start 115200 'half-duplex) ;uses only the tx pin
(gpio-configure 'pin-rx 'pin-mode-in-pu) ;configures rx pin for button presses

;Pre-defined fields for uart output
;Ninebot protocol: 5A A5 bLen bSrcAddr bDstAddr bCmd bArg bPayload[bLen] wChecksumLE
;Output will be: 5A A5 0x05 0x20(ESC) 0x21(BLE) 0x64 0x00 bFlags, bBattLevel, bHeadlightLevel, bBeeps, bSpeed, bErrorCode, wChecksumLE
(define tx-frame (array-create 15))
(bufset-u16 tx-frame 0 0x5AA5) ;Ninebot protocol
(bufset-u8 tx-frame 2 0x06) ;Payload length is 5 bytes
(bufset-u16 tx-frame 3 0x2021) ; Packet is from ESC to BLE
(bufset-u16 tx-frame 5 0x6400) ; Packet is from ESC to BLE

;Buffer for string incoming bytes from uart
(define uart-buf (array-create type-byte 64))


(define prsdbtns 0);Button status. 3 buttons on adc 2 - 2^3 possibilities
;Status bits:
;0 Nothing - ca 0.03
;1 Left - pressed ca 0.48 
;2 Right - defective, needs repairs
;3 Bottom - pressed ca 0.84
;4 L, R - defective
;5 L, B - pressed ca 1,1
;6 R, B - defective
;7 L,R,B - defective    

(define btntimer 0);timer for checking buttons
(define mode 2); default mode eco
(define lastprsdbtns 0)


(defun buttons()
    (loopwhile t
        (progn
            (setvar 'adc (get-adc 2));read ADC
            (if (<= adc 0.1) (setvar 'prsdbtns 0))
            (if (and (> adc 0.15) (< adc 0.49)) (setvar 'prsdbtns 1))
            ;2, defective
            (if (and (> adc 0.82) (< adc 0.86)) (setvar 'prsdbtns 3))
            ;4, defective
            (if (and (> adc 1) (< adc 1.2)) (setvar 'prsdbtns 5))
            ;6, defective
            ;7, defective
            (yield 500000)
        )
    )
)            

(defun modes(); todo write proper mode switching
    (loopwhile t
        (progn  
            ;checks if button has been staying in same position, increases timer when so
            (if (= lastprsdbtns prsdbtns) (setvar 'btntimer (+ btntimer 1)) (setvar 'btntimer 0)) 
            (setvar 'lastprsdbtns prsdbtns)
            (if (and (= btntimer 5) (= prsdbtns 5));If 5 for 5 cycles, mode 4
                (progn
                    (setvar 'mode 4)
                    (conf-set 'max-speed 30)                    
                )
            )
            
            (if (and (= prsdbtns 0) (not (= mode 2))) ;If btn 0, and not already mode 2, set mode 2
                (progn
                    (setvar 'mode 2)
                    (conf-set 'max-speed 5.8)  
                )
            )
                    

            (if (> btntimer 5) (setvar 'btntimer 0))
            (yield 500000)
            

            
        )
    )
)

(defun headoutputs(buffer) ;Dash wants information update, 64 command
    (progn      
        ;bFlags field, mode field (1=drive, 2=eco, 4=sport, 8=charge, 16=off, 32=lock)
        (bufset-u8 tx-frame 7 mode);temp set eco

        ;bBattLevel - 0-100 for NB, 0.0-1.0 from vesc
        (bufset-u8 tx-frame 8 (*(get-batt) 100)) ;temp set 99

        ;bHeadlightLevel - lamp status
        (bufset-u8 tx-frame 9 1);temp set off

        ;bBeeps - beeper
        (bufset-u8 tx-frame 10 0);temp set 0

        ;bSpeed - current speed, 0.1 kmh units
        (bufset-u8 tx-frame 11 (* (get-speed) 3.6))

        ;bErrorCode - error codes
        (bufset-u8 tx-frame 12 0);temp set 0

        ;wChecksumLE = 0xFFFF xor (16-bit sum of bytes <bLen bSrcAddr bDstAddr bCmd bArg bPayload[]>)
        (setvar 'crcout 0)
        (looprange i 2 13
        (setvar 'crcout (+ crcout (bufget-u8 tx-frame i))))
        (setvar 'crcout (bitwise-xor crcout 0xFFFF))
        (bufset-u8 tx-frame 13 crcout)
        (bufset-u8 tx-frame 14 (shr crcout 8))
        (uart-write tx-frame)
    )
)


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
							(setvar 'bCmd (bufget-u8 uart-buf 2)) ;save bCmd
							(setvar 'wChecksumLE (bufget-u16 uart-buf (+ bLen 4)))
							(looprange i 0 (+ bLen 4) ;prepare checksum, add values of bSrcAddr bDstAddr bCmd bArg bPayload to bLen
								(setvar 'crc (+ crc (bufget-u8 uart-buf i))))
								;little-endian is broken, do 0xFFFF xor (16-bit sum of bytes <bLen bSrcAddr bDstAddr bCmd bArg bPayload[]>)
							(setvar 'wChecksumLECalculated (bitwise-and (+ (shr (bitwise-xor crc 0xFFFF) 8) (shl (bitwise-xor crc 0xFFFF) 8)) 65535))
							(if (= wChecksumLE wChecksumLECalculated);If the calculated checksum matches with sent checksum, forward command
								(progn ;uart-buf is bSrcAddr bDstAddr bCmd bArg bPayload[bLen] wChecksumLE
									(if(= bCmd 0x65)
										;(headinputs uart-buf)
									)
									(if(= bCmd 0x64)
										(headoutputs uart-buf)
									)
								)
							)
						)
					)
				)
			)               
         (yield 1000)               
        ) 
    )
)

; Make the ninebot-uart-decoder a parallel task with 150 heap alloc
(spawn 150 ninebot-uart-decoder)
(spawn 30 modes)
(spawn 30 buttons)