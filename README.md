# mk-sode1
EOMから袖1スキームを導出する

```
dimension :: 1
axes :: x

double :: cfl
double :: s
double :: h
double :: dt = cfl*h

d_xx = fun(a,a_x) 2*(a[i+1] + a[i-1] - 2*a[i] - h*(a_x[i+1] - a_x[i-1])/4)/h/h
d_xxx = fun(a,a_x) (a_x[i+1] + a_x[i-1] - 2*a_x[i])/h/h

smoo = fun(a) -s*a[i] + s*(a[i+1] + a[i-1])/2

begin function (b_t,u_t,p_t) = d_t((b,u,p),(b_x,u_x,p_x))
b_t = ((b * u_x) - (b_x * u))
u_t = (-((u * u_x) + (b * p_x)))
p_t = (-(((gm * p) * u_x) + (u * p_x)))
end function

begin function (b_tt,u_tt,p_tt) = d_tt((b,u,p),(b_x,u_x,p_x))
b_xx = d_xx(b,b_x)
u_xx = d_xx(u,u_x)
p_xx = d_xx(p,p_x)
b_tt = (((-(2.0 * ((b * u) * u_xx))) - ((b * b) * p_xx)) + ((b_xx * u) * u))
u_tt = (((((((((b * gm) * p) * u_xx) + (((b * gm) * p_x) * u_x)) + ((b * p_x) * u_x)) + (2.0 * ((b * p_xx) * u))) + (2.0 * ((b_x * p_x) * u))) + (2.0 * ((u * u_x) * u_x))) + ((u * u) * u_xx))
p_tt = (((((((((((b * gm) * p) * p_xx) + ((b * p_x) * p_x)) + (((b_x * gm) * p) * p_x)) + (2.0 * (((gm * p) * u) * u_xx))) + (((gm * p) * u_x) * u_x)) + (2.0 * (((gm * p_x) * u) * u_x))) + ((((gm * gm) * p) * u_x) * u_x)) + (2.0 * ((p_x * u) * u_x))) + ((p_xx * u) * u))
end function

begin function (b_tx,u_tx,p_tx) = d_tx((b,u,p),(b_x,u_x,p_x))
b_xx = d_xx(b,b_x)
u_xx = d_xx(u,u_x)
p_xx = d_xx(p,p_x)
b_tx = ((b * u_xx) - (b_xx * u))
u_tx = ((((-(b * p_xx)) - (b_x * p_x)) - (u * u_xx)) - (u_x * u_x))
p_tx = ((((-((gm * p) * u_xx)) - ((gm * p_x) * u_x)) - (p_x * u_x)) - (p_xx * u))
end function

begin function (b_ttx,u_ttx,p_ttx) = d_ttx((b,u,p),(b_x,u_x,p_x))
b_xx = d_xx(b,b_x)
b_xxx = d_xxx(b,b_x)
u_xx = d_xx(u,u_x)
u_xxx = d_xxx(u,u_x)
p_xx = d_xx(p,p_x)
p_xxx = d_xxx(p,p_x)
b_ttx = (((((((-(2.0 * ((b * b_x) * p_xx))) - (2.0 * ((b * u) * u_xxx))) - (2.0 * ((b * u_x) * u_xx))) - ((b * b) * p_xxx)) - (2.0 * ((b_x * u) * u_xx))) + (2.0 * ((b_xx * u) * u_x))) + ((b_xxx * u) * u))
u_ttx = ((((((((((((((((b * gm) * p) * u_xxx) + (2.0 * (((b * gm) * p_x) * u_xx))) + (((b * gm) * p_xx) * u_x)) + ((b * p_x) * u_xx)) + (3.0 * ((b * p_xx) * u_x))) + (2.0 * ((b * p_xxx) * u))) + (((b_x * gm) * p) * u_xx)) + (((b_x * gm) * p_x) * u_x)) + (3.0 * ((b_x * p_x) * u_x))) + (4.0 * ((b_x * p_xx) * u))) + (2.0 * ((b_xx * p_x) * u))) + (6.0 * ((u * u_x) * u_xx))) + ((u * u) * u_xxx)) + (2.0 * ((u_x * u_x) * u_x)))
p_ttx = ((((((((((((((((((((b * gm) * p) * p_xxx) + (((b * gm) * p_x) * p_xx)) + (2.0 * ((b * p_x) * p_xx))) + (2.0 * (((b_x * gm) * p) * p_xx))) + (((b_x * gm) * p_x) * p_x)) + ((b_x * p_x) * p_x)) + (((b_xx * gm) * p) * p_x)) + (2.0 * (((gm * p) * u) * u_xxx))) + (4.0 * (((gm * p) * u_x) * u_xx))) + (4.0 * (((gm * p_x) * u) * u_xx))) + (3.0 * (((gm * p_x) * u_x) * u_x))) + (2.0 * (((gm * p_xx) * u) * u_x))) + (2.0 * ((((gm * gm) * p) * u_x) * u_xx))) + ((((gm * gm) * p_x) * u_x) * u_x)) + (2.0 * ((p_x * u) * u_xx))) + (2.0 * ((p_x * u_x) * u_x))) + (4.0 * ((p_xx * u) * u_x))) + ((p_xxx * u) * u))
end function

begin function (b,u,p,bp,up,pp,bh,uh,ph,b_x,u_x,p_x,bp_x,up_x,pp_x,bh_x,uh_x,ph_x) = init()
double :: b = 0,u = 0,p = 0,bp = 0,up = 0,pp = 0,bh = 0,uh = 0,ph = 0,b_x = 0,u_x = 0,p_x = 0,bp_x = 0,up_x = 0,pp_x = 0,bh_x = 0,uh_x = 0,ph_x = 0
end function

begin function (b',u',p',bp',up',pp',bh',uh',ph',b_x',u_x',p_x',bp_x',up_x',pp_x',bh_x',uh_x',ph_x') = step(b,u,p,bp,up,pp,bh,uh,ph,b_x,u_x,p_x,bp_x,up_x,pp_x,bh_x,uh_x,ph_x)
q = (b,u,p)
qp = (bp,up,pp)
qh = (bh,uh,ph)
q_x = (b_x,u_x,p_x)
qp_x = (bp_x,up_x,pp_x)
qh_x = (bh_x,uh_x,ph_x)

q_t = d_t(qp,qp_x)
q_tt = d_tt(qp,qp_x)
q_tx = d_tx(qp,qp_x)
q_ttx = d_ttx(qp,qp_x)

q' = qh + dt*q_t/2 - dt*dt*q_tt/12
q_x' = qh_x + dt*q_tx/2 - dt*dt*q_ttx/12

qp' = q' + dt*q_t + dt*dt*q_tt/2 + smoo(qp)
qp_x' = q_x' + dt*q_tx + dt*dt*q_ttx/2 + smoo(qp_x)

qh' = q' + dt*q_t/2 + dt*dt*q_tt/12 + smoo(qh)
qh_x' = q_x' + dt*q_tx/2 + dt*dt*q_ttx/12 + smoo(qh_x)

(b',u',p') = q'
(bp',up',pp') = qp'
(bh',uh',ph') = qh'
(b_x',u_x',p_x') = q_x'
(bp_x',up_x',pp_x') = qp_x'
(bh_x',uh_x',ph_x') = qh_x'
end function
```
