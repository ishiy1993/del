# del [![Build Status](https://travis-ci.org/ishiy1993/del.svg?branch=master)](https://travis-ci.org/ishiy1993/del) 
記号微分とコードの生成をするプログラム

## Usage

```
$ stack exec -- del t -f euler1.txt -o fmr
r_tt = -r*u_tx - r_t*u_x - r_tx*u - r_x*u_t
u_tt = -p_tx*(r)**(-1) + p_x*(r)**(-2)*r_t - u*u_tx - u_t*u_x
p_tt = -gm*p*u_tx - gm*p_t*u_x - p_tx*u - p_x*u_t
```
