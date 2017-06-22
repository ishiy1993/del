# mk-sode1
EOMから袖1スキームを導出する

```
$ stack exec mk-sode1
begin function (b_t,u_t,p_t) = d_t((b,u,p),(b_x,u_x,p_x))
b_t = ((b * u_x) - (b_x * u))
u_t = ((-1.0 * (u * u_x)) - (b * p_x))
p_t = (((-1.0 * gamma) * (p * u_x)) - (u * p_x))
end function

begin function (b_tt,u_tt,p_tt) = d_tt((b,u,p),(b_x,u_x,p_x))
b_xx = d_xx(b,b_x)
u_xx = d_xx(u,u_x)
p_xx = d_xx(p,p_x)
b_tt = (((((b * u_x) - (b_x * u)) * u_x) + (b * ((-1.0 * ((u_x * u_x) + (u * u_xx))) - ((b_x * p_x) + (b * p_xx))))) - (((((b_x * u_x) + (b * u_xx)) - ((b_xx * u) + (b_x * u_x))) * u) + (b_x * ((-1.0 * (u * u_x)) - (b * p_x)))))
u_tt = ((-1.0 * ((((-1.0 * (u * u_x)) - (b * p_x)) * u_x) + (u * ((-1.0 * ((u_x * u_x) + (u * u_xx))) - ((b_x * p_x) + (b * p_xx)))))) - ((((b * u_x) - (b_x * u)) * p_x) + (b * (((-1.0 * gamma) * ((p_x * u_x) + (p * u_xx))) - ((u_x * p_x) + (u * p_xx))))))
p_tt = (((-1.0 * gamma) * (((((-1.0 * gamma) * (p * u_x)) - (u * p_x)) * u_x) + (p * ((-1.0 * ((u_x * u_x) + (u * u_xx))) - ((b_x * p_x) + (b * p_xx)))))) - ((((-1.0 * (u * u_x)) - (b * p_x)) * p_x) + (u * (((-1.0 * gamma) * ((p_x * u_x) + (p * u_xx))) - ((u_x * p_x) + (u * p_xx))))))
end function

begin function (b_tx,u_tx,p_tx) = d_tx((b,u,p),(b_x,u_x,p_x))
b_xx = d_xx(b,b_x)
u_xx = d_xx(u,u_x)
p_xx = d_xx(p,p_x)
b_tx = (((b_x * u_x) + (b * u_xx)) - ((b_xx * u) + (b_x * u_x)))
u_tx = ((-1.0 * ((u_x * u_x) + (u * u_xx))) - ((b_x * p_x) + (b * p_xx)))
p_tx = (((-1.0 * gamma) * ((p_x * u_x) + (p * u_xx))) - ((u_x * p_x) + (u * p_xx)))
end function

begin function (b_ttx,u_ttx,p_ttx) = d_ttx((b,u,p),(b_x,u_x,p_x))
b_xx = d_xx(b,b_x)
b_xxx = d_xxx(b,b_x)
u_xx = d_xx(u,u_x)
u_xxx = d_xxx(u,u_x)
p_xx = d_xx(p,p_x)
p_xxx = d_xxx(p,p_x)
b_ttx = (((((((b_x * u_x) + (b * u_xx)) - ((b_xx * u) + (b_x * u_x))) * u_x) + (((b * u_x) - (b_x * u)) * u_xx)) + ((b_x * ((-1.0 * ((u_x * u_x) + (u * u_xx))) - ((b_x * p_x) + (b * p_xx)))) + (b * ((-1.0 * (((u_xx * u_x) + (u_x * u_xx)) + ((u_x * u_xx) + (u * u_xxx)))) - (((b_xx * p_x) + (b_x * p_xx)) + ((b_x * p_xx) + (b * p_xxx))))))) - (((((((b_xx * u_x) + (b_x * u_xx)) + ((b_x * u_xx) + (b * u_xxx))) - (((b_xxx * u) + (b_xx * u_x)) + ((b_xx * u_x) + (b_x * u_xx)))) * u) + ((((b_x * u_x) + (b * u_xx)) - ((b_xx * u) + (b_x * u_x))) * u_x)) + ((b_xx * ((-1.0 * (u * u_x)) - (b * p_x))) + (b_x * ((-1.0 * ((u_x * u_x) + (u * u_xx))) - ((b_x * p_x) + (b * p_xx)))))))
u_ttx = ((-1.0 * (((((-1.0 * ((u_x * u_x) + (u * u_xx))) - ((b_x * p_x) + (b * p_xx))) * u_x) + (((-1.0 * (u * u_x)) - (b * p_x)) * u_xx)) + ((u_x * ((-1.0 * ((u_x * u_x) + (u * u_xx))) - ((b_x * p_x) + (b * p_xx)))) + (u * ((-1.0 * (((u_xx * u_x) + (u_x * u_xx)) + ((u_x * u_xx) + (u * u_xxx)))) - (((b_xx * p_x) + (b_x * p_xx)) + ((b_x * p_xx) + (b * p_xxx)))))))) - ((((((b_x * u_x) + (b * u_xx)) - ((b_xx * u) + (b_x * u_x))) * p_x) + (((b * u_x) - (b_x * u)) * p_xx)) + ((b_x * (((-1.0 * gamma) * ((p_x * u_x) + (p * u_xx))) - ((u_x * p_x) + (u * p_xx)))) + (b * (((-1.0 * gamma) * (((p_xx * u_x) + (p_x * u_xx)) + ((p_x * u_xx) + (p * u_xxx)))) - (((u_xx * p_x) + (u_x * p_xx)) + ((u_x * p_xx) + (u * p_xxx))))))))
p_ttx = (((-1.0 * gamma) * ((((((-1.0 * gamma) * ((p_x * u_x) + (p * u_xx))) - ((u_x * p_x) + (u * p_xx))) * u_x) + ((((-1.0 * gamma) * (p * u_x)) - (u * p_x)) * u_xx)) + ((p_x * ((-1.0 * ((u_x * u_x) + (u * u_xx))) - ((b_x * p_x) + (b * p_xx)))) + (p * ((-1.0 * (((u_xx * u_x) + (u_x * u_xx)) + ((u_x * u_xx) + (u * u_xxx)))) - (((b_xx * p_x) + (b_x * p_xx)) + ((b_x * p_xx) + (b * p_xxx)))))))) - (((((-1.0 * ((u_x * u_x) + (u * u_xx))) - ((b_x * p_x) + (b * p_xx))) * p_x) + (((-1.0 * (u * u_x)) - (b * p_x)) * p_xx)) + ((u_x * (((-1.0 * gamma) * ((p_x * u_x) + (p * u_xx))) - ((u_x * p_x) + (u * p_xx)))) + (u * (((-1.0 * gamma) * (((p_xx * u_x) + (p_x * u_xx)) + ((p_x * u_xx) + (p * u_xxx)))) - (((u_xx * p_x) + (u_x * p_xx)) + ((u_x * p_xx) + (u * p_xxx))))))))
end function
```
