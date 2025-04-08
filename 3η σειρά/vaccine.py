import sys
from collections import deque
filename = sys.argv[1]
def findcomplement(nucleus):
    answer=[]
    for i in range(len(nucleus)):
        if nucleus[i]=='A':
            answer.append('U')
        elif nucleus[i]=='U':
            answer.append('A')
        elif nucleus[i]=='G':
            answer.append('C')
        elif nucleus[i]=='C':
            answer.append('G')
    return ''.join(answer)
def reverse(x):
  return x[::-1]
def checkPush(x,sol):
    if x not in sol:
        return True
    else :
        if x==sol[0]:
            return True
        else:
            return False
def checkReverse(prevMov):
    if prevMov=='r':
        return False
    else :
        return True
def checkComp(prevMov):
    if prevMov=='c':
        return False
    else:
        return True
def solve(nucleus):
    ind=len(nucleus)
    cmpl=findcomplement(nucleus)
    curr=nucleus[ind-1]
    q=deque()
    counter=1
    q.append((ind-1,curr,0,'p',counter))
    moves=sys.maxsize
    flag =0
    currentsol=[]
    smallestindex=ind-1
    size=sys.maxsize
    cnt=0
    l=0
    #dict={curr:1}
    #x=set()
    while q:
        (index,solution,comp,prevMov,counter)=q.popleft()
        #tmp=nucleus[index-1]+solution
        if counter==len(nucleus) :
            temp=reverse(prevMov)
            length=len(prevMov)
            if flag==0:
                currentsol=temp
                l=length
                flag=1
            else :
                if temp < currentsol and length<=l:
                    currentsol = temp
                    l=length
        if flag==1:
            if counter > l:
                #print(currentsol)
                break
            if len(prevMov) > l:
                continue
        if "rcr" in prevMov:
            continue
        elif "cr" in prevMov:
            continue
        elif "crc" in prevMov:
            continue
        if comp==0:
            if checkPush(nucleus[index-1],solution):
                check=nucleus[index-1]
                if check not in solution:
                    temporary=(index-1,nucleus[index-1]+solution,comp,'p'+prevMov,counter+1)
                else:
                    temporary=(index-1,solution,comp,'p'+prevMov,counter+1)
                q.append(temporary)
            if checkReverse(prevMov[0]):
                rev=reverse(solution)
                temporary=(index,rev,comp,'r'+prevMov,counter)
                q.append(temporary)
            if checkComp(prevMov[0]):
                temporary=(index,solution,1,'c'+prevMov,counter)
                q.append(temporary)
        elif comp==1 :
            if checkPush(cmpl[index-1],solution):
                if cmpl[index-1] not in solution:
                    temporary=(index-1,cmpl[index-1]+solution,comp,'p'+prevMov,counter+1)
                else:
                    temporary=(index-1,solution,comp,'p'+prevMov,counter+1)
                q.append(temporary)
            if checkReverse(prevMov[0]):
                rev=reverse(solution)
                temporary=(index,rev,comp,'r'+prevMov,counter)
                q.append(temporary)
            if checkComp(prevMov[0]):
                temporary=(index,solution,0,'c'+prevMov,counter)
                q.append(temporary)
    result=currentsol
    return result
with open(filename) as f:
    N=int(f.readline())
    for i in range (N):
        check=f.readline().split('\n')
        print(solve(check[0]))