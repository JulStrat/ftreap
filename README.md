[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.org/JulStrat/ftreap.svg?branch=devop)](https://travis-ci.org/JulStrat/ftreap)

# ftreap

[Order statistic tree](https://en.wikipedia.org/wiki/Order_statistic_tree) based on [Treap](https://en.wikipedia.org/wiki/Treap) data structure.
Compact ObjectPascal ```generic class TTreapNode```.
See [Wiki](https://github.com/JulStrat/ftreap/wiki) for more info.

## Motivation

<div style="text-align: justify">
Why another treap implementation? The story begins from [SPOJ](https://www.spoj.com/) problem [ALLIN1](https://www.spoj.com/problems/ALLIN1/). 
All accepted solutions was in C/C++ and one in D-DMD. I asked myself - why not Pascal? 
My first attempt was based on records (mode DELPHI) - time 2.28s. 
After reading Michalis Kamburelis excellent [Modern Object Pascal Introduction for Programmers](https://castle-engine.io/modern_pascal_introduction.html) I started ```ftreap```.
So, here we are.
</div>

## Built With

* [Lazarus](https://www.lazarus-ide.org/) - The professional Free Pascal RAD IDE.
* [PasDoc](https://github.com/pasdoc/pasdoc) - Documentation tool for ObjectPascal (Free Pascal, Lazarus, Delphi).

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
