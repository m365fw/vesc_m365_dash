# vesc_m365_dash
Connect your XIAOMI MI SCOOTER M365 (PRO) BLE to VESC controller.

## How
Read this guide (German): https://rollerplausch.com/threads/vesc-controller-einbau-1s-pro2-g30.6032/

## Implemented
- [x] Add speed modes (double tap on button)
- [x] Add secret speed mode (hold throttle and brake, double press)
- [x] Add lock mode with beeping and braking (double press while braking)
- [x] Add min-speed feature (makes it more secure)
- [x] Add shutdown feature (turn it off by long press and back on by single tap)
- [x] Add battery in idle feature
- [x] Add separate ADC version
- [x] Add temperature notification icon (60°C)

Features to be added:
- [ ] App communication

## Fixed to be done
- [x] ~~Figure out why 0x64 packets are not being read. (on my setup)~~ (Can be ignored due to the fact that we do not have to receive any 0x64 packets to sent our own 0x64 back)
- [x] ~~Figure out why button reading is randomly~~ (can be fixed with 470R resistor between 3.3v and RX and capacitor on 3.3v+GND)

## Known issues
- Script can crash due to CPU overload (can be fixed by lowering the Zero Vector Frequency in the VESC Tool) \
    (I am working on a faster implementation using the native library support)


## Tested on
### BLEs
- Clone M365 PRO Dashboard (https://s.click.aliexpress.com/e/_9JHFDN)
- Original DE-Edition PRO 2 Dashboard

### VESCs
Flipsky 75100 (https://s.click.aliexpress.com/e/_A5gtoF or https://banggood.onelink.me/zMT7/zmenvmm2) \
Ubox Single 100v 100A (https://spintend.com/products/single-ubox-100v-100a-motor-controller-based-on-vesc -- not recommend, bad thermal design)

#### Requirements on VESC
Requires 6.0 VESC BETA 83 firmware.

## Worth to check out!
https://github.com/Koxx3/SmartESC_STM32_v2 / VESC firmware for Xiaomi ESC