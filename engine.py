# tiny chess: 0x88 negamax + alpha-beta + TT + quiescence, fixed node budget.
import random
_r=random.Random(0x5EED)
Z=[[_r.getrandbits(64) for _ in range(128)] for _ in range(13)]
ZC=[_r.getrandbits(64) for _ in range(16)]
ZE=[_r.getrandbits(64) for _ in range(8)]
ZT=_r.getrandbits(64)
V=[0,100,320,330,500,900,0]
ND=(31,33,14,18,-14,-18,-31,-33)
KD=(1,15,16,17,-1,-15,-16,-17)
BD=(15,17,-15,-17)
RD=(1,16,-1,-16)
CA=((4,6,7,5,1,(5,6),(4,5,6)),(4,2,0,3,2,(1,2,3),(4,3,2)),(116,118,119,117,4,(117,118),(116,117,118)),(116,114,112,115,8,(113,114,115),(116,115,114)))
CK=[15]*128
for _s,_b in ((4,12),(7,14),(0,13),(116,3),(119,11),(112,7)):CK[_s]=_b

def fen(s):
    p=s.split()
    b=[0]*128
    wk=bk=-1
    r=7
    fl=0
    for c in p[0]:
        if c=='/':r-=1;fl=0
        elif c.isdigit():fl+=int(c)
        else:
            q=r*16+fl
            v='PNBRQK'.index(c.upper())+1
            if c.isupper():
                b[q]=v
                if v==6:wk=q
            else:
                b[q]=-v
                if v==6:bk=q
            fl+=1
    t=1 if p[1]=='w' else -1
    ca=0
    for c in p[2]:ca|={'K':1,'Q':2,'k':4,'q':8}.get(c,0)
    ep=-1
    if p[3]!='-':ep=(ord(p[3][0])-97)+(int(p[3][1])-1)*16
    return b,t,ca,ep,wk,bk,int(p[4])

def atk(b,sq,by):
    if by>0:
        for d in (-15,-17):
            if not (sq+d)&0x88 and b[sq+d]==1:return True
    else:
        for d in (15,17):
            if not (sq+d)&0x88 and b[sq+d]==-1:return True
    for d in ND:
        t=sq+d
        if not t&0x88 and b[t]==2*by:return True
    for d in KD:
        t=sq+d
        if not t&0x88 and b[t]==6*by:return True
    for d in BD:
        t=sq+d
        while not t&0x88:
            p=b[t]
            if p:
                if p==3*by or p==5*by:return True
                break
            t+=d
    for d in RD:
        t=sq+d
        while not t&0x88:
            p=b[t]
            if p:
                if p==4*by or p==5*by:return True
                break
            t+=d
    return False

def gen(b,turn,ca,ep):
    ms=[]
    for sq in range(128):
        if sq&0x88:continue
        p=b[sq]
        if p*turn<=0:continue
        pt=abs(p)
        if pt==1:
            fw=16*turn
            t=sq+fw
            if not t&0x88 and not b[t]:
                if t>>4==(7 if turn>0 else 0):
                    for pr in (5,4,3,2):ms.append(sq+(t<<7)+(pr<<14))
                else:
                    ms.append(sq+(t<<7))
                    t2=sq+2*fw
                    if (sq>>4==(1 if turn>0 else 6)) and not b[t2]:ms.append(sq+(t2<<7))
            for d in (fw-1,fw+1):
                t=sq+d
                if t&0x88:continue
                if b[t]*turn<0 or t==ep:
                    if t>>4==(7 if turn>0 else 0):
                        for pr in (5,4,3,2):ms.append(sq+(t<<7)+(pr<<14))
                    else:ms.append(sq+(t<<7))
        elif pt==2 or pt==6:
            for d in ND if pt==2 else KD:
                t=sq+d
                if not t&0x88 and b[t]*turn<=0:ms.append(sq+(t<<7))
        else:
            ds=BD if pt==3 else RD if pt==4 else BD+RD
            for d in ds:
                t=sq+d
                while not t&0x88:
                    if b[t]*turn>0:break
                    ms.append(sq+(t<<7))
                    if b[t]:break
                    t+=d
    for kf,kt,rf,rt,bit,em,sf in CA:
        if turn>0 and bit>2 or turn<0 and bit<4:continue
        if not ca&bit or b[kf]!=6*turn:continue
        if any(b[s] for s in em):continue
        if any(atk(b,s,-turn) for s in sf):continue
        ms.append(kf+(kt<<7))
    return ms

def hsh(b,turn,ca,ep):
    h=ZC[ca]
    if ep!=-1:h^=ZE[ep&7]
    if turn<0:h^=ZT
    for sq in range(128):
        if not sq&0x88 and b[sq]:h^=Z[b[sq]+6][sq]
    return h

def mk(b,m,turn,ca,ep,wk,bk,hm=0,h=0):
    f=m&127
    t=(m>>7)&127
    pr=m>>14
    nb=b[:]
    p=nb[f]
    nb[f]=0
    h^=Z[p+6][f]
    cp=0
    if p==turn and t==ep:
        cp=1
        cs=t-16*turn
        h^=Z[-turn+6][cs]
        nb[cs]=0
    elif nb[t]:
        cp=abs(nb[t])
        h^=Z[nb[t]+6][t]
    np=pr*turn if pr else p
    nb[t]=np
    h^=Z[np+6][t]
    nc=ca&CK[f]&CK[t]
    if nc!=ca:h^=ZC[ca]^ZC[nc]
    nep=-1
    if ep!=-1:h^=ZE[ep&7]
    if abs(p)==1 and abs(t-f)==32:
        nep=(f+t)//2
        h^=ZE[nep&7]
    if abs(p)==6:
        if t==f+2:
            nb[f+3]=0
            nb[f+1]=4*turn
            h^=Z[4*turn+6][f+3]^Z[4*turn+6][f+1]
        elif t==f-2:
            nb[f-4]=0
            nb[f-1]=4*turn
            h^=Z[4*turn+6][f-4]^Z[4*turn+6][f-1]
        if turn>0:wk=t
        else:bk=t
    h^=ZT
    nh=0 if (abs(p)==1 or cp) else hm+1
    return nb,nc,nep,wk,bk,cp,nh,h

def leg(b,turn,ca,ep,wk,bk,hm=0,h=0):
    out=[]
    for m in gen(b,turn,ca,ep):
        nb,_,_,nw,nk,_,_,_=mk(b,m,turn,ca,ep,wk,bk,hm,h)
        if not atk(nb,nw if turn>0 else nk,-turn):out.append(m)
    return out

def chk(b,turn,wk,bk):
    return atk(b,wk if turn>0 else bk,-turn)

def uc(m):
    f=m&127
    t=(m>>7)&127
    s=chr(97+(f&7))+str((f>>4)+1)+chr(97+(t&7))+str((t>>4)+1)
    pr=m>>14
    return s+('nbrq'[pr-2] if pr else '')

def fu(b,turn,ca,ep,wk,bk,u):
    for m in gen(b,turn,ca,ep):
        if uc(m)==u:return m
    return 0

_P="00000000122,,2211/.00./1000440001125521122466422::::::::00000000"
_N="&(****(&(,0110,(*123321**034430**134431**023320*(,0000,(&(****(&"
_B=",......,.100001..222222..022220..112211..012210..000000.,......,"
_R="00011000/000000//000000//000000//000000//000000/1222222100000000"
_Q=",..//..,.010000..111110.0011110//011110/.011110..000000.,..//..,"
_KM="46200264"+"44000044"+".,,,,,,."+",**((**,"+"".join(chr(x+48) for x in (-6,-8,-8,-10,-10,-8,-8,-6))*4
_KE="&******&**0000***.4664.**.6886.**.6886.**.4664.**,.00.,*&(*,,*(&"

def _tb(s,mat):
    t=[0]*128
    for r in range(8):
        for f in range(8):
            t[r*16+f]=mat+(ord(s[r*8+f])-48)*5
    return t
ET=[None,_tb(_P,100),_tb(_N,320),_tb(_B,330),_tb(_R,500),_tb(_Q,900),_tb(_KM,0)]
EK=_tb(_KE,0)

def ev(b,turn):
    s=0
    wb=bb=0
    pc=[0]*7
    bc=-1
    sm=True
    ws=bs=-1
    for sq in range(128):
        if sq&0x88:continue
        p=b[sq]
        if not p:continue
        pt=abs(p)
        pc[pt]+=1
        if p>0:
            s+=ET[pt][sq]
            if pt==3:
                wb+=1
                c=((sq>>4)+(sq&7))&1
                if bc<0:bc=c
                elif bc!=c:sm=False
            elif pt==6:ws=sq
        else:
            s-=ET[pt][sq^112]
            if pt==3:
                bb+=1
                c=((sq>>4)+(sq&7))&1
                if bc<0:bc=c
                elif bc!=c:sm=False
            elif pt==6:bs=sq
    if pc[1]==0 and pc[4]==0 and pc[5]==0:
        if pc[2]+pc[3]<=1:return 0
        if pc[2]==0 and pc[3]==2 and sm:return 0
    if pc[5]==0 or (pc[5]==1 and pc[4]==0):
        s+=EK[ws]-ET[6][ws]
        s-=EK[bs^112]-ET[6][bs^112]
    if wb>=2:s+=30
    if bb>=2:s-=30
    return s*turn+10

MATE=100000
NODES=[0]
BUDGET=100000
TT=[None]*(1<<17)
KILLER=[[0,0] for _ in range(64)]
HIST=[[0]*128 for _ in range(13)]
class Abort(Exception):pass

def cap(b,m):
    f=m&127
    t=(m>>7)&127
    return b[t]!=0 or (m>>14) or (abs(b[f])==1 and (t&7)!=(f&7))

def oo(b,m,turn,tt,ply):
    if m==tt:return 1<<20
    f=m&127
    t=(m>>7)&127
    v=b[t]
    if not v and abs(b[f])==1 and (t&7)!=(f&7):v=1
    pr=m>>14
    if v or pr:return (1<<18)+10*V[abs(v)]-V[abs(b[f])]//8+pr*200
    if m==KILLER[ply][0]:return 1<<17
    if m==KILLER[ply][1]:return (1<<17)-1
    return HIST[b[f]+6][t]

def hl(b,turn,ca,ep,wk,bk,hm,h):
    for m in gen(b,turn,ca,ep):
        nb,_,_,nw,nk,_,_,_=mk(b,m,turn,ca,ep,wk,bk,hm,h)
        if not atk(nb,nw if turn>0 else nk,-turn):return True
    return False

def qs(b,turn,ca,ep,wk,bk,hm,h,al,be,ply,line,seen):
    NODES[0]+=1
    if NODES[0]>BUDGET:raise Abort
    if seen.get(h,0)+line.get(h,0)>=2 or hm>=100:return 0
    st=ev(b,turn)
    if st>=be:return be
    if st>al:al=st
    ch=chk(b,turn,wk,bk)
    ms=[]
    for m in gen(b,turn,ca,ep):
        if not ch and not cap(b,m):continue
        ms.append(m)
    if not ms and not ch and not hl(b,turn,ca,ep,wk,bk,hm,h):return 0
    ms.sort(key=lambda m:-oo(b,m,turn,0,ply))
    line[h]=line.get(h,0)+1
    try:
        lg=0
        for m in ms:
            nb,nc,nep,nw,nk,_,nhm,nh=mk(b,m,turn,ca,ep,wk,bk,hm,h)
            if atk(nb,nw if turn>0 else nk,-turn):continue
            lg+=1
            s=-qs(nb,-turn,nc,nep,nw,nk,nhm,nh,-be,-al,ply+1,line,seen)
            if s>=be:return be
            if s>al:al=s
        if ch and lg==0:return -MATE+ply
        return al
    finally:line[h]-=1

def sr(b,turn,ca,ep,wk,bk,hm,h,dep,al,be,ply,line,seen):
    NODES[0]+=1
    if NODES[0]>BUDGET:raise Abort
    if seen.get(h,0)+line.get(h,0)>=2 or hm>=100:return 0
    if dep<=0:return qs(b,turn,ca,ep,wk,bk,hm,h,al,be,ply,line,seen)
    ix=h&131071
    tt=TT[ix]
    tm=0
    if tt is not None and tt[0]==h:
        _,td,fl,ts,tm2=tt
        if ts>MATE-1000:ts-=ply
        elif ts<-MATE+1000:ts+=ply
        if td>=dep:
            if fl==0:return ts
            if fl==1 and ts>=be:return ts
            if fl==2 and ts<=al:return ts
        tm=tm2
    line[h]=line.get(h,0)+1
    try:
        ms=gen(b,turn,ca,ep)
        ms.sort(key=lambda m:-oo(b,m,turn,tm,ply))
        best=-MATE
        bm=0
        lg=0
        a=al
        for m in ms:
            nb,nc,nep,nw,nk,_,nhm,nh=mk(b,m,turn,ca,ep,wk,bk,hm,h)
            if atk(nb,nw if turn>0 else nk,-turn):continue
            lg+=1
            s=-sr(nb,-turn,nc,nep,nw,nk,nhm,nh,dep-1,-be,-a,ply+1,line,seen)
            if s>best:best=s;bm=m
            if s>a:
                a=s
                if a>=be:
                    if not cap(b,m):
                        if KILLER[ply][0]!=m:
                            KILLER[ply][1]=KILLER[ply][0]
                            KILLER[ply][0]=m
                        HIST[b[m&127]+6][(m>>7)&127]+=dep*dep
                    break
        if lg==0:return -MATE+ply if chk(b,turn,wk,bk) else 0
        if best<=al:fl=2
        elif best>=be:fl=1
        else:fl=0
        st=best
        if st>MATE-1000:st+=ply
        elif st<-MATE+1000:st-=ply
        TT[ix]=(h,dep,fl,st,bm)
        return best
    finally:line[h]-=1

def root(b,turn,ca,ep,wk,bk,hm,h,seen):
    ms=leg(b,turn,ca,ep,wk,bk,hm,h)
    if not ms:return 0
    if len(ms)==1:return ms[0]
    NODES[0]=0
    best=ms[0]
    line={}
    for dep in range(1,40):
        try:
            cb=best
            cs=-MATE
            al=-MATE
            ms.sort(key=lambda m:-oo(b,m,turn,best,0))
            for m in ms:
                nb,nc,nep,nw,nk,_,nhm,nh=mk(b,m,turn,ca,ep,wk,bk,hm,h)
                s=-sr(nb,-turn,nc,nep,nw,nk,nhm,nh,dep-1,-MATE,-al,1,line,seen)
                if s>cs:cs=s;cb=m
                if s>al:al=s
            best=cb
        except Abort:break
    return best

import json,os,sys,urllib.error,urllib.request
ENV=os.environ.get("MAKEFUN_ENV_URL")
def post(p,bo):
    rq=urllib.request.Request(ENV+p,data=json.dumps(bo).encode(),headers={"content-type":"application/json"})
    try:
        with urllib.request.urlopen(rq) as r:return json.load(r)
    except urllib.error.HTTPError as e:sys.exit(f"{p}: HTTP {e.code} {e.read().decode(errors='replace')}")
def hs(mv,sf):
    b,turn,ca,ep,wk,bk,hm=fen(sf)
    h=hsh(b,turn,ca,ep)
    seen={h:1}
    for u in mv:
        m=fu(b,turn,ca,ep,wk,bk,u)
        if not m:break
        b,ca,ep,wk,bk,_,hm,h=mk(b,m,turn,ca,ep,wk,bk,hm,h)
        turn=-turn
        seen[h]=seen.get(h,0)+1
    return seen
if __name__=="__main__":
    if not ENV:sys.exit("MAKEFUN_ENV_URL is not set")
    seen={}
    gn=-1
    cc=False
    while True:
        g=post("/game",{})
        if g["status"]=="done":break
        if g["game"]!=gn:
            gn=g["game"]
            seen=hs(g["moves"],g["start_fen"])
            cc=True
        while g["status"]=="your_move":
            b,turn,ca,ep,wk,bk,hm=fen(g["fen"])
            h=hsh(b,turn,ca,ep)
            if not cc:seen[h]=seen.get(h,0)+1
            cc=True
            m=root(b,turn,ca,ep,wk,bk,hm,h,seen)
            if not m or uc(m) not in g["legal_moves"]:m=fu(b,turn,ca,ep,wk,bk,g["legal_moves"][0])
            nb,_,_,_,_,_,_,nh=mk(b,m,turn,ca,ep,wk,bk,hm,h)
            seen[nh]=seen.get(nh,0)+1
            g=post("/move",{"game":g["game"],"move":uc(m)})
            cc=False
        print(f"game {g['game']}: {g['outcome']} by {g['reason']}",file=sys.stderr)
    print(f"done: {g.get('wins')} won, {g.get('draws')} drawn, {g.get('losses')} lost",file=sys.stderr)
