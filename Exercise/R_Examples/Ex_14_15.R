# Example 14-15 : �H��϶����N�q

mu = 165					# �����һݪ��g = 165 (����)
sigma = 5; n = 50				# ����зǮt�]�� 5 (�����^

x0 = c(160,170)
y0 = c(0,105)
plot(x0,y0,type="n")

abline(v=mu,lty=2)				

for(i in 1:100)				# 100 �Ӱj��
{
  x = rnorm(n,mu,sigma)			# �C�Ӱj�鲣�ͤ@�ռ˥�,n = 50
  # �p��̾� t ���t������X �g�� 95% �H��϶��� c1, c2
  width = qt(0.975,n-1)*sd(x)/sqrt(n)
  c1 = mean(x) - width
  c2 = mean(x) + width

  if ( mu >= c1 && mu <= c2 ){
       lty = 2; lwd = 1			# �Y�϶��]�t�g�G���q�u�e����u
  } else {
       lty = 1; lwd = 2			# ���t�g�G2 ���u�e����u
  }
  # �ϥνu�q�s�� c1,c2�A�e�X�@�ӫH��϶������u
  lines(c(c1,c2),c(i,i),lty=lty,lwd=lwd)
}

