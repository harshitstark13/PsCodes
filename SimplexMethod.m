% Simplex Method
%max z=2x1+5X2
%x1+4x2<=24
%3x1+1x2<=21
%x1+x2<=9
clc
clear all
format short
Noofvariables=2;
C=[2 5];
a=[1 4; 3 1; 1 1]
b=[24; 21; 9]
s=eye(size(a,1))
A=[a s b]
cost=zeros(1,size(A,2))
cost(1:Noofvariables)=C
bv= Noofvariables+1:1:size(A,2)-1
zjcj=cost(bv)*A-cost
zcj=[zjcj; A]
simptable=array2table(zcj);
simptable.Properties.VariableNames(1:size(zcj,2))={'x_1','x_2','s_1','s_2','s_3','sol'}
RUN=true;
while RUN
if any(zjcj<0); %check for (most) negative value
    fprintf(' the current BFS is not optimal \n')
   zc=zjcj(1:end-1);
   [Enter_val, pvt_col]= min(zc) 
   if all(A(:,pvt_col)<=0)
    error('LPP is Unbounded all enteries are <=0 in column %d',pvt_col);
   else
       sol=A(:,end)
       column=A(:,pvt_col)
       for i=1:size(A,1)
         if column(i)>0
            ratio(i)= sol(i)./column(i)
         else
            ratio(i)=inf
         end
       end
       [leaving_val, pvt_row]=min(ratio)
   end
bv(pvt_row)=pvt_col
pvt_key=A(pvt_row, pvt_col)
A(pvt_row,:)=A(pvt_row,:)./pvt_key
for i=1:size(A,1)
    if i~=pvt_row
        A(i,:)=A(i,:)-A(i, pvt_col).*A(pvt_row,:)
    end
end
    zjcj=zjcj-zjcj(pvt_col).*A(pvt_row,:)
    zcj=[zjcj;A]
    table=array2table(zcj)
    table.Properties.VariableNames(1:size(zcj,2))={'x_1','x_2','s_1','s_2','s_3','sol'}
else
    RUN=false;
    fprintf('The current BFS is optimal \n')
end
end