# 8051-eForth

This repository contains a 30 year old variant of Dr. C.H. Ting's eForth for the 8051: John C. Wren ported the code from MASM to the [AVCASE51](https://github.com/jcwren/avcase51) tool chain, and implemented, e.g., basic arithmetic words in 8051 assemble for a much improved performance.

The first commit contains the code provided to me by JC Wren (29/May/2022). It was filtered with `dos2unix` and is otherwise the original baseline. 

Development targets are:
* porting to the SDCC toolchain (with sdas8051)
* containerized tool chain with test and build in GitHub Actions
* modularization in a similar way as [STM8 eForth](https://github.com/TG9541/stm8ef)
* target recent MCS-51 based µCs like the Nuvoton N76E003 without external bus

Contributions are welcome. Please fork the repository, make pull requests and make sure that licensing of your contribution with MIT license is possible (i.e., you're the owner of the code, you have the permission of the owner, or the owner uses a compatible license). For discussion or guidance please use a [GitHub issue](https://github.com/TG9541/8051-eForth/issues).
