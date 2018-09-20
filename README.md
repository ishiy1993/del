# del [![Build Status](https://travis-ci.org/ishiy1993/del.svg?branch=master)](https://travis-ci.org/ishiy1993/del) 
記号微分とコードの生成をするプログラム

## Usage

```
$ stack exec -- del t -f euler1.txt -o fmr
b_tt = b*u_tx + b_t*u_x - b_tx*u - b_x*u_t
u_tt = -b*p_tx - b_t*p_x - u*u_tx - u_t*u_x
p_tt = -gm*p*u_tx - gm*p_t*u_x - p_tx*u - p_x*u_t
```
