# Example 13-2 : table example

#20 �ӫH�Υd�����̪��d�O�B�~���ϰ�B�H�ΩʧO��ơG
cards = c(
"���d"  , "�ժ��d", "���q�d", "���q�d", "���q�d", "���d"   ,"���q�d", "���q�d",
"���q�d", "���q�d", "���q�d", "���d"  , "���q�d", "�ժ��d" ,"���d"  , "���q�d",
"���q�d", "���q�d", "�ժ��d", "���q�d"
)

area = c(
"�n��", "�_��", "�n��", "�n��", "�_��", "�n��", "�n��", "�n��", "�n��", "�n��",
"�_��", "�n��", "�n��", "�n��", "�n��", "�_��", "�_��", "�_��", "�_��", "�n��"
)

gender = c(
"�k", "�k", "�k", "�k", "�k", "�k", "�k", "�k", "�k", "�k", "�k", "�k", "�k", "�k",
"�k", "�k", "�k", "�k", "�k", "�k"
)

tabulate(factor(cards,levels=c("���q�d","���d","�ժ��d")))

table(cards,dnn="�|������")

members.table = table(cards,gender,dnn=c("�|������","�ʧO"))
members.table

margin.table(members.table,margin=1)

margin.table(members.table,margin=2)

rowSums(members.table)
colSums(members.table)

prop.table(members.table)		# �C�@�檺���
prop.table(members.table,margin=1)	# ��C��ڤ��
prop.table(members.table,margin=2)	# ������ڤ��

table(cards,gender,area)

rowSums(table(cards,gender,area))	# �Ҧ���C�ۥ[
rowSums(table(cards,gender,area),dims=2) # ���ϰ쬰��C
colSums(table(cards,gender,area))	# �Ҧ�����(cards)�ۥ[
colSums(colSums(table(cards,gender,area)))
rowSums(colSums(table(cards,gender,area)))
colSums(table(cards,gender,area),dims=2) # �N area ������

prop.table(table(cards,gender,area))     # �C�Ӯ�l�Ҧ������
prop.table(table(cards,gender,area),margin=1)
prop.table(table(cards,gender,area),margin=2)
prop.table(table(cards,gender,area),margin=3)
