# 8051-eForth

This repository contains a 30 year old variant of Dr. C.H. Ting's eForth for the 8051: John C. Wren ported the code from MASM to the [AVCASE51](https://github.com/jcwren/avcase51) tool chain for much improved performance, especially of arithmetic words.

The first commit is the baseline. One development target is porting to the SDCC toolchain (with sdas8051) and moudlarization in a similar way as [STM8 eForth](https://github.com/TG9541/stm8ef). Intended future hardware targets are recent MCS-51 based µCs like the Nuvoton N76E003 without external bus. 
