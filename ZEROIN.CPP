/* File converted with FORTRAN CONVERTER utility.
   FORTRAN CONVERTER is written by Grigoriev D., 2081/4.*/

#include "forsythe.h"

REAL ZEROIN(REAL AX,REAL BX,REAL (F)(REAL X),REAL TOL)
{
//     HÓËÜ ÔÓHKÖÈÈ F(X) BÛ×ÈCËßETCß B ÈHTEPBAËE AX,BX

//     BXOÄHAß ÈHÔOPMAÖÈß..

//     AX     ËEBÛÉ KOHEÖ ÈCXOÄHOÃO ÈHTEPBAËA
//     BX     ÏPABÛÉ KOHEÖ ÈCXOÄHOÃO ÈHTEPBAËA
//     F      ÏOÄÏPOÃPAMMA-ÔÓHKÖÈß, KOTOPAß BÛ×ÈCËßET F(X)
//            ÄËß ËÞÁOÃO X B ÈHTEPBAËE AX BX
//     TOL    ÆEËAEMAß ÄËÈHA ÈHTEPBAËA HEOÏPEÄEËEHHOCTÈ
//            KOHE×HOÃO PEÇÓËÜTATA

//     BÛXOÄHAß ÈHÔOPMAÖÈß...

//     ZEROIN AÁCÖÈCCA, AÏÏPOKCÈMÈPÓÞÙAß HÓËÜ ÔÓHKÖÈÈ F B
//            ÈHTEPBAËE AX, BX

//        ÁEÇ ÏPOBEPKÈ ÏPEÄÏOËAÃAETCß, ×TO F(AX) È F(BX) ÈMEÞT
//     ÏPOTÈBOÏOËOÆHÛE ÇHAKÈ.
//        ZEROIN BÛ×ÈCËßET HÓËÜ X B ÇAÄAHHOM ÈHTEPBAËE AX, BX
//     B ÏPEÄEËAX ÄOÏÓCKA HA OØÈÁKÓ  4*MACHEPS*ABS(X) + TOL,
//     ÃÄE MACHEPS-OTHOCÈTEËÜHAß MAØÈHHAß TO×HOCTÜ.
//        ÝTA ÏOÄÏPOÃPAMMA-ÔÓHKÖÈß ÏPEÄCTABËßET COÁOÉ CËEÃKA
//     MOÄÈÔÈÖÈPOBAHHÓÞ TPAHCËßÖÈÞ AËÃOË 60-ÏPOÖEÄÓPÛ ZERO,
//     ÏPÈBEÄEHHOÉ B KHÈÃE RICHARD BRENT, ALGORITHMS FOR
//     MINIMIZATION WITHOUT DERIVATIVES,PRENTICE HALL,INC.(1973).

REAL A,B,C,D,E,EPS,FA,FB,FC,TOL1,XM,P,Q,R,S;

//     BÛ×ÈCËÈTÜ EPS,OTHOCÈTEËÜHÓÞ MAØÈHHÓÞ TO×HOCTÜ

EPS=1.0;
_10:;
EPS=EPS/2.0;
TOL1=1.0+EPS;
if(TOL1>1.0)goto _10;

//     ÏPÈCBOEHÈE HA×AËÜHÛX ÇHA×EHÈÉ

A=AX;
B=BX;
FA=F(A);
FB=F(B);

//     HA×ATÜ ØAÃ

_20:;
C=A;
FC=FA;
D=B-A;
E=D;
_30:;
if(ABS(FC)>=ABS(FB))goto _40;
A=B;
B=C;
C=A;
FA=FB;
FB=FC;
FC=FA;

//     ÏPOBEPKA CXOÄÈMOCTÈ

_40:;
TOL1=2.0*EPS*ABS(B)+0.5*TOL;
XM=0.5*(C-B);
if(ABS(XM)<=TOL1)goto _90;

//     HEOÁXOÄÈMA ËÈ ÁÈCEKÖÈß

if(FB==0.0)goto _90;
if(ABS(E)<TOL1)goto _70;
if(ABS(FA)<=ABS(FB))goto _70;

//     BOÇMOÆHA ËÈ KBAÄPATÈ×HAß ÈHTEPÏOËßÖÈß

if(A!=C)goto _50;

//     ËÈHEÉHAß ÈHTEPÏOËßÖÈß

S=FB/FA;
P=2.0*XM*S;
Q=1.0-S;
goto _60;

//     OÁPATHAß KBAÄPATÈ×HAß ÈHTEPÏOËßÖÈß

_50:;
Q=FA/FC;
R=FB/FC;
S=FB/FA;
P=S*(2.0*XM*Q*(Q-R)-(B-A)*(R-1.0));
Q=(Q-1.0)*(R-1.0)*(S-1.0);

//     BÛÁPATÜ ÇHAKÈ

_60:;
if(P>0.0)Q=-Q;
P=ABS(P);

//     ÏPÈEMËEMA ËÈ ÈHTEPÏOËßÖÈß

if((2.0*P)>=(3.0*XM*Q-ABS(TOL1*Q)))goto _70;
if(P>=ABS(0.5*E*Q))goto _70;
E=D;
D=P/Q;
goto _80;

//     ÁÈCEKÖÈß

_70:;
D=XM;
E=D;

//     ÇABEPØÈTÜ ØAÃ

_80:;
A=B;
FA=FB;
if(ABS(D)>TOL1)B=B+D;
if(ABS(D)<=TOL1)B=B+SIGN(TOL1,XM);
FB=F(B);
if((FB*(FC/ABS(FC)))>0.0)goto _20;
goto _30;

//     KOH×EHO

_90:;
return B;
}                         	// END

