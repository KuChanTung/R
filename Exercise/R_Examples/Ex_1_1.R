# Example 1-1

# �x�_���B�������B�򶩥��B�s�˥��B�x���� 1999~2008 �y�����Q���i��
# �N����x�s�b�@�ӦV�q�ܼơGdogs.adopted

dogs.adopted = c(34451,11403,3907,5027, 9006)
mean(dogs.adopted)		# �D�X�˥�������
sd(dogs.adopted)

# �ޥ� Ryacas �M��(package)
library(Ryacas)
x = Sym("x")
# �D�X sin(ax)2cos(bx) ���Ÿ��n��
PrettyForm(yacas("Integrate(x) Sin(a*x)^2*Cos(b*x)"))

# �e�X 3D ��Ƴz����
x = y = seq(-15,15,0.5)	
fxy = function(x,y) sin(sqrt(x^2+y^2))/sqrt(x^2+y^2)
z = outer(x,y,fxy)     	
persp(x,y,z, phi=20, theta=30 )

# ���� 100 �� N(0,1) �˥�(�˥��� 500)�A�õe�X 100 �Өֱƪ������
oldpar=par()
devAskNewPage(ask = TRUE)
par(mfrow=c(10,10),mai=c(0.1,0.1,0.1,0.1))   
for (i in 1:100) hist(rnorm(500,0,1),main="")
par(oldpar)