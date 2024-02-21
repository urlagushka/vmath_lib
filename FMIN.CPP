/* File converted with FORTRAN CONVERTER utility.
   FORTRAN CONVERTER is written by Grigoriev D., 2081/4.*/

#include "forsythe.h"

REAL FMIN(REAL AX,REAL BX,REAL (F)(REAL X),REAL TOL)
{

//     BÛ×ÈCËßET ÏPÈÁËÈÆEHÈE X K TO×KE, ÃÄE F ÄOCTÈÃAET
//     MÈHÈMÓMA HA ÈHTEPBAËE (AX,BX)

//     BXOÄHAß ÈHÔOPMAÖÈß.

//     AX   ËEBÛÉ KOHEÖ ÈCXOÄHOÃO ÈHTEPBAËA
//     BX   ÏPABÛÉ KOHEÖ ÈCXOÄHOÃO ÈHTEPBAËA
//     F    ÏOÄÏPOÃPAMMA-ÔÓHKÖÈß, KOTOPAß BÛ×ÈCËßET F(X)
//          ÄËß ËÞÁOÃO X B ÈHTEPBAËE (AX,BX)
//    TOL   ÆEËAEMAß ÄËÈHA ÈHTEPBAËA HEOÏPEÄEËEHHOCTÈ
//          KOHE×HOÃO PEÇÓËÜTATA (>=0.0)

//    BÛXOÄHAß ÈHÔOPMAÖÈß.

//    FMIN  AÁCÖÈCCA, AÏÏPOKCÈMÈPÓÞÙAß TO×KÓ,
//          ÃÄE F ÄOCTÈÃAET MÈHÈMÓMA

//       METOÄ ÈCÏOËÜÇÓET KOMÁÈHAÖÈÞ ÏOÈCKA ÇOËOTOÃO CE×EHÈß
//    È ÏOCËEÄOBATEËÜHOÉ ÏAPAÁOËÈ×ECKOÉ ÈHTEPÏOËßÖÈÈ. CXOÄÈ-
//    MOCTÜ HÈKOÃÄA HE ÁÛBAET ÇHA×ÈTEËÜHO XÓÆE, ×EM ÄËß
//    ÏOÈCKA ÔÈÁOHA××È. ECËÈ F ÈMEET HEÏPEPÛBHÓÞ BTOPÓÞ
//    ÏPOÈÇBOÄHÓÞ, ÏOËOÆÈTEËÜHÓÞ B TO×KE MÈHÈMÓMA (HE
//    COBÏAÄAÞÙEÉ HÈ C AX,HÈ C BX), TO CXOÄÈMOCTÜ CBEPX-
//    ËÈHEÉHAß È OÁÛ×HO ÈMEET ÏOPßÄOK ÏPÈMEPHO 1.324...
//       ÔÓHKÖÈß F HÈKOÃÄA HE BÛ×ÈCËßETCß B ÄBÓX TO×KAX,
//    OTCTOßÙÈX ÄPÓÃ OT ÄPÓÃA MEHEE ×EM HA EPS*ABS(X)+(TOL/3),
//    ÃÄE EPS ÏPÈÁËÈÇÈTEËÜHO PABHO KBAÄPATHOMÓ KOPHÞ ÈÇ
//    OTHOCÈTEËÜHOÉ MAØÈHHOÉ TO×HOCTÈ. ECËÈ F-ÓHÈMOÄAËÜHAß
//    ÔÓHKÖÈß È BÛ×ÈCËEHHÛE ÇHA×EHÈß F COXPAHßÞT ÓHÈMOÄAËÜ-
//    HOCTÜ ÏPÈ COÁËÞÄEHÈÈ ÓKAÇAHHOÃO ÓCËOBÈß PAÇÄEËEHHOCTÈ,
//    TO FMIN AÏÏPOKCÈMÈPÓET AÁCÖÈCCÓ ÃËOÁAËÜHOÃO MÈHÈMÓMA F
//    HA ÈHTEPBAËE (AX,BX) C OØÈÁKOÉ, MEHÜØEÉ 3*EPS*ABS(X)+TOL.
//    ECËÈ F HE ßBËßETCß ÓHÈMOÄAËÜHOÉ, TO FMIN MOÆET C TOÉ ÆE
//    TO×HOCTÜÞ AÏÏPOKCÈMÈPOBATÜ ËOKAËÜHÛÉ MÈHÈMÓM, BOÇMOÆHO,
//    HE COBÏAÄAÞÙÈÉ C ÃËOÁAËÜHÛM.
//       ÝTA ÏOÄÏPOÃPAMMA-ÔÓHKÖÈß ßBËßETCß CËEÃKA MOÄÈÔÈÖÈPO-
//    BAHHOÉ BEPCÈEÉ AËÃOË 60-ÏPOÖEÄÓPÛ LOCALMIN, ÏPÈBEÄEHHOÉ
//    B KHÈÃE RICARD BRENT, ALGORITHMS FOR MINIMIZATION
//    WITHOUT DERIVATIVES, PRENTICE-HALL, INC.(1973).

REAL A,B,C,D=0.0,E,EPS,XM,P,Q,R,TOL1,TOL2,U,V,W;
REAL FU,FV,FW,FX,X;

//     C ECTÜ BOÇBEÄEHHAß B KBAÄPAT BEËÈ×ÈHA,
//     OÁPATHAß K ÇOËOTOMÓ CE×EHÈÞ

C=0.5*(3.0-SQRT(5.0));

//     EPS ÏPÈÁËÈÇÈTEËÜHO PABHO KBAÄPATHOMÓ KOPHÞ ÈÇ
//     OTHOCÈTEËÜHOÉ MAØÈHHOÉ TO×HOCTÈ

EPS=1.0;
_10:;
EPS=EPS/2.0;
TOL1=1.0+EPS;
if(TOL1>1.0)goto _10;
EPS=SQRT(EPS);

//     ÏPÈCBOEHÈE HA×AËÜHÛX ÇHA×EHÈÉ

A=AX;
B=BX;
V=A+C*(B-A);
W=V;
X=V;
E=0.0;
FX=F(X);
FV=FX;
FW=FX;

//     ÇÄECÜ HA×ÈHAETCß OCHOBHOÉ ÖÈKË

_20:;
XM=0.5*(A+B);
TOL1=EPS*ABS(X)+TOL/3.0;
TOL2=2.0*TOL1;

//     ÏPOBEPÈTÜ KPÈTEPÈÉ OKOH×AHÈß

if(ABS(X-XM)<=(TOL2-0.5*(B-A)))goto _90;

//     HEOÁXOÄÈMO ËÈ ÇOËOTOE CE×EHÈE

if(ABS(E)<=TOL1)goto _40;

//     ÏOCTPOÈTÜ ÏAPAÁOËÓ

R=(X-W)*(FX-FV);
Q=(X-V)*(FX-FW);
P=(X-V)*Q-(X-W)*R;
Q=2.0*(Q-R);
if(Q>0.0)P=-P;
Q=ABS(Q);
R=E;
E=D;

//     ÏPÈEMËEMA ËÈ ÏAPAÁOËA

//_30:;  //?!!
if(ABS(P)>=ABS(0.5*Q*R))goto _40;
if(P<=Q*(A-X))goto _40;
if(P>=Q*(B-X))goto _40;

//     ØAÃ ÏAPAÁOËÈ×ECKOÉ ÈHTEPÏOËßÖÈÈ

D=P/Q;
U=X+D;

//     F HE CËEÄÓET BÛ×ÈCËßTÜ CËÈØKOM ÁËÈÇKO K AX ÈËÈ BX

if((U-A)<TOL2)D=SIGN(TOL1,XM-X);
if((B-U)<TOL2)D=SIGN(TOL1,XM-X);
goto _50;

//     ØAÃ ÇOËOTOÃO CE×EHÈß

_40:;
if(X>=XM)E=A-X;
if(X<XM)E=B-X;
D=C*E;

//     F HE CËEÄÓET BÛ×ÈCËßTÜ CËÈØKOM ÁËÈÇKO K X

_50:;
if(ABS(D)>=TOL1)U=X+D;
if(ABS(D)<TOL1)U=X+SIGN(TOL1,D);
FU=F(U);

//     ÏPÈCBOÈTÜ HOBÛE ÇHA×EHÈß ÏAPAMETPAM A,B,V,W È X

if(FU>FX)goto _60;
if(U>=X)A=X;
if(U<X)B=X;
V=W;
FV=FW;
W=X;
FW=FX;
X=U;
FX=FU;
goto _20;

_60:;
if(U<X)A=U;
if(U>=X)B=U;
if(FU<=FW)goto _70;
if(W==X)goto _70;
if(FU<=FV)goto _80;
if(V==X)goto _80;
if(V==W)goto _80;
goto _20;

_70:;
V=W;
FV=FW;
W=U;
FW=FU;
goto _20;

_80:;
V=U;
FV=FU;
goto _20;

//     KOHEÖ OCHOBHOÃO ÖÈKËA

_90:;
return X;
}                         	// END
