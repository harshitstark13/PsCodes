format short
clear all
clc
% Cost=[-4 -5 0 0 -1000 -1000 0]
% A=[3 1 1 0 0 0 27; 3 2 0 -1 1 0 3; 5 5 0 0 0 1 60]
% % BV=[3 5 6]
Cost=[-2 -1 0 0 -10000 -10000 0]
A=[3 1 0 0 1 0 3; 4 3 -1 0 0 1 6 ;1 2 0 1 0 0 3]
BV=[5 6 4]
 
ZjCj=Cost(BV)*A-Cost
 zcj=[Cost;ZjCj;A];
    bigmtable=array2table(zcj);
    bigmtable.Properties.VariableNames(1:size(zcj,2))={'x_1','x_2','s_1','s_2','A_1','A_2','sol'}
 
RUN= true;
while RUN
    ZC=ZjCj(1:end-1)
    if any(ZC<0)
        fprintf('  The current BFS is not optimal\n')
        [ent_col,pvt_col]=min(ZC)
        fprintf('Entering Col =%d \n' , pvt_col);
        sol=A(:,end)
        Column=A(:,pvt_col)
        if Column<=0
            error('LPP is unbounded');
        else
            for i=1:size(A,1)
                if Column(i)>0
                    ratio(i)=sol(i)./Column(i)
                else
                    ratio(i)=inf
                end
            end
            [MinRatio,pvt_row]=min(ratio)
            fprintf('leaving Row=%d \n', pvt_row);
        end
        BV(pvt_row)=pvt_col;
        pvt_key=A(pvt_row,pvt_col);
        A(pvt_row,:)=A(pvt_row,:)./ pvt_key;
        for i=1:size(A,1)
            if i~=pvt_row
                A(i,:)=A(i,:)-A(i,pvt_col).*A(pvt_row,:);
            end
        end
        ZjCj=ZjCj-ZjCj(pvt_col).*A(pvt_row,:)
        ZCj=[ZjCj;A]
        TABLE=array2table(ZCj);
        TABLE.Properties.VariableNames(1:size(ZCj,2))={'x_1','x_2','s_1','s_2','A_1','A_2','sol'}
    else
        RUN=false;
        fprintf('  Current BFS is Optimal \n');
    end
end