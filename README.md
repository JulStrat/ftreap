[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.org/JulStrat/ftreap.svg?branch=devop)](https://travis-ci.org/JulStrat/ftreap)

# ftreap

[Order statistic tree](https://en.wikipedia.org/wiki/Order_statistic_tree) based on [Treap](https://en.wikipedia.org/wiki/Treap) data structure and 
powerful Implicit Treap for interval operations.
Compact ObjectPascal ```generic class TTreapNode``` and ```generic class TImplicitTreapNode```.
See [Wiki](https://github.com/JulStrat/ftreap/wiki) for more info.

## Motivation

Why another treap implementation? The story begins from the [SPOJ](https://www.spoj.com/) problem [ALLIN1](https://www.spoj.com/problems/ALLIN1/). 
All accepted solutions was in C/C++ and one in D-DMD. I asked myself - why not Pascal? 
My first attempt was based on records (mode DELPHI) - time 2.28s. 
After reading Michalis Kamburelis excellent [Modern Object Pascal Introduction for Programmers](https://castle-engine.io/modern_pascal_introduction.html) I started ```ftreap```.
So, here we are.

## Benchmarks

Here some testing results on [IDEONE](https://ideone.com/0YvvRx).

|               | 100000 | 200000 | 400000 | 800000 | 1600000 |
| ------------- | ------ | ------ | ------ | ------ | ------- |
| Insert        | 0.08s  | 0.19s  | 0.42s  | 1.06s  | 2.58s   |

## Built With

* [Lazarus](https://www.lazarus-ide.org/) - The professional Free Pascal RAD IDE.
* [PasDoc](https://github.com/pasdoc/pasdoc) - Documentation tool for ObjectPascal (Free Pascal, Lazarus, Delphi).

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
