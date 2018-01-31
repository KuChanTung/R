#############################################################################
# 畫出兩個相連的 3D donut
#
# 作者：淡江大學統計系 陳景祥 2010/2/20
#############################################################################

library(misc3d)
parametric3d(
  fx = function(u, v) cos(u)+0.5*cos(u)*cos(v),
  fy = function(u, v) sin(u)+0.5*sin(u)*cos(v),
  fz = function(u, v) 0.5*sin(v),
  fill=F, umin = -pi, umax = pi, vmin = -pi, vmax = pi,
  n = 40,phi=30,distance=0.8 )

parametric3d(
  fx = function(u, v) 1+cos(u)+.5*cos(u)*cos(v),
  fy = function(u, v) 0.5*sin(v),
  fz = function(u, v) sin(u)+.5*sin(u)*cos(v),
  fill=F, umin = -pi, umax = pi, vmin = -pi, vmax = pi,
n = 30,phi=30,add=T,distance=0.8 )

