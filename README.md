# VESC M365 Dash
Allows you to connect your XIAOMI or NINEBOT display to VESC controller.

## How
Do you want to use your Xiaomi M365 (PRO) with a VESC controller? This is the right place for you! \
We are using the BLE of the Xiaomi M365 (PRO) to communicate with the VESC controller. \
Read one of the guides below to get started.

- [DE Guide](/guide/DE.md)
- [German Rollerplausch Guide](https://rollerplausch.com/threads/vesc-controller-einbau-1s-pro2-g30.6032/)

## Which version should I use?

If you are running **VESC 6.02**, use these:
- **M365**: https://github.com/m365fw/vesc_m365_dash/blob/main/m365_dash.lisp
- **G30**: https://github.com/m365fw/vesc_m365_dash/blob/main/g30_dash.lisp
- **How-To** Video: None (Use the Guides linked below)

when you are running **VESC 6.05 BETA**, use these:
- **M365**: https://github.com/m365fw/vesc_m365_dash/blob/6_05_adc/m365_dash.lisp
- **G30** https://github.com/m365fw/vesc_m365_dash/blob/6_05_adc/g30_dash.lisp
- **How-To** Video: https://www.youtube.com/watch?v=kX8PsaxfoXQ

## How do I wire it?
<span style="color:rgb(184, 49, 47);">Red </span>to 5V \
<span style="color:rgb(209, 213, 216);">Black </span>to GND \
<span style="color:rgb(250, 197, 28);">Yellow </span>to TX (UART-HDX) \
<span style="color:rgb(97, 189, 109);">Green </span>to RX (Button) \
1k Ohm Resistor from <span style="color:rgb(251, 160, 38);">3.3V</span> to <span style="color:rgb(97, 189, 109);">RX (Button)</span>

![image](guide/imgs/23999.png)

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
- [ ] More unlock combinations

## Fixed to be done
- [x] ~~Figure out why 0x64 packets are not being read. (on my setup)~~ (Can be ignored due to the fact that we do not have to receive any 0x64 packets to sent our own 0x64 back)
- [x] ~~Figure out why button reading is randomly~~ (can be fixed with 470R resistor between 3.3v and RX and capacitor on 3.3v+GND)

## Tested on
### BLEs
- Clone M365 PRO Dashboard ([AliExpress](https://s.click.aliexpress.com/e/_9JHFDN))
- Original DE-Edition PRO 2 Dashboard

### VESCs
- 75100 Box:
    - Makerbase 75100 VESC ([AliExpress](https://s.click.aliexpress.com/e/_DmJxqxr) - 75€)
    - Flipsky 75100 VESC ([Banggood](https://banggood.onelink.me/zMT7/zmenvmm2) - with Honey Add-On about 87€)

- 75100 Alu PCB (Best choice):
    - Makerbase 75100 Alu PCB ([AliExpress](https://s.click.aliexpress.com/e/_DE9TKAl) - 95€)
    - Flipsky 75100 Alu PCB ([AliExpress](https://s.click.aliexpress.com/e/_DEXNhX3) - 151€)

- 75200 Alu PCB (Top Performance):
    - Makerbase 75200 Alu PCB ([AliExpress](https://s.click.aliexpress.com/e/_Dk3ucKd) - 143€)
    - Flipsky 75200 Alu PCB ([AliExpress](https://s.click.aliexpress.com/e/_DkxlJbj) - 266€)

- More recommended VESCs:
    - Single Ubox 100v 100A ([Spintend](https://spintend.com/products/single-ubox-100v-100a-motor-controller-based-on-vesc))
    - Single Ubox 80v 100A Alu PCB ([Spintend](https://spintend.com/collections/frontpage/products/single-ubox-aluminum-controller-80v-100a-based-on-vesc))
    - MP2 300A 100V/150V VESC ([GitHub](https://github.com/badgineer/MP2-ESC) - DIY)
    - and many more... use whatever you like.


#### Requirements on VESC
Requires 6.0 VESC firmware. \
Can be found here: https://vesc-project.com/

## Worth to check out!
https://github.com/Koxx3/SmartESC_STM32_v2 (VESC firmware for Xiaomi ESCs)
