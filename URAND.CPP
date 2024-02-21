/* File converted with FORTRAN CONVERTER utility.
   FORTRAN CONVERTER is written by Grigoriev D., 2081/4.*/

#include "forsythe.h"

static int IY;
void INIT_URAND(int I)
{
    IY = I;
}

REAL URAND()
{

//       URAND-ÝTO ÄAT×ÈK PABHOMEPHO PACÏPEÄEËEHHÛX CËÓ×AÉHÛX
//     ×ÈCEË, OCHOBAHHÛÉ HA TEOPÈÈ È ÏPEÄËOÆEHÈßX, COÄEPÆAÙÈXCß
//     B KHÈÃE KHÓT (1969),TOM 2.
//       ÏEPEÄ ÏEPBÛM OÁPAÙEHÈEM K URAND ÖEËOÉ ÏEPEMEHHOÉ IY
//     CËEÄÓET ÏPÈCBOÈTÜ ÏPOÈÇBOËÜHOE ÖEËO×ÈCËEHHOE HA×AËÜ-
//     HOE ÇHA×EHÈE. BÛÇÛBAÞÙAß ÏPOÃPAMMA HE ÄOËÆHA ÈÇMEHßTÜ
//     ÇHA×EHÈE IY MEÆÄÓ ÏOCËEÄOBATEËÜHÛMÈ BÛÇOBAMÈ. ÇHA×EHÈß
//     ÔÓHKÖÈÈ URAND ßBËßÞTCß ×ÈCËAMÈ ÈÇ ÈHTEPBAËA (0,1).

static IA,IC,M,MIC;
static double HALFM;
static REAL S;
static M2 = 0, ITWO = 2;
if(M2!=0)goto _20;

//     ECËÈ ÝTO ÏEPBÛÉ BXOÄ, TO BÛ×ÈCËÈTÜ ÄËÈHÓ
//     ÖEËO×ÈCËEHHOÃO MAØÈHHOÃO CËOBA

M=1;
_10:;
M2=M;
M=ITWO*M2;
if(M > M2)goto _10;
HALFM=M2;

//     BÛ×ÈCËÈTÜ MHOÆÈTEËÜ È ÏPÈPAÙEHÈE ËÈHEÉHOÃO
//     KOHÃPÓÝHTHOÃO METOÄA

IA=8*int(HALFM*atan(1.0)/8.0)+5;
IC=2*int(HALFM*(0.5-sqrt(3.0)/6.0))+1;
MIC=(M2-IC)+M2;

//     S-MACØTAÁÈPÓÞÙÈÉ MHOÆÈTEËÜ ÄËß ÏPEOÁPAÇOBAHÈß B ×ÈCËO
//     C ÏËABAÞÙEÉ TO×KOÉ

S=0.5/HALFM;

//     BÛ×ÈCËÈTÜ CËEÄÓÞÙEE CËÓ×AÉHOE ×ÈCËO

_20:;
IY=IY*IA;

//     CËEÄÓÞÙÈÉ OÏEPATOP-ÄËß MAØÈH,KOTOPÛE HE ÄOÏÓCKAÞT
//     ÏEPEÏOËHEHÈß ÖEËÛX ×ÈCEË ÏPÈ CËOÆEHÈÈ

if(IY>MIC)IY=(IY-M2)-M2;
IY=IY+IC;

//     CËEÄÓÞÙÈÉ OÏEPATOP - ÄËß MAØÈH, Ó KOTOPÛX ÄËÈHA CËOBA
//     ÄËß CËOÆEHÈß ÁOËÜØE, ×EM ÄËß ÓMHOÆEHÈß

if(IY/2>M2)IY=(IY-M2)-M2;

//     CËEÄÓÞÙÈÉ OÏEPATOP - ÄËß MAØÈH, Ó KOTOPÛX ÏEPEÏOËHEHÈE
//     ÖEËOÃO ×ÈCËA BËÈßET HA ÇHAKOBÛÉ PAÇPßÄ

if(IY<0)IY=(IY+M2)+M2;
return REAL(IY)*S;
}                         	// END
