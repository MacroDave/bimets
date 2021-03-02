library(bimets)

#define model
MARTINDefinition=
  "MODEL
COMMENT> Test Oz Macro Model in R bimets package

COMMENT> Government Consumption
BEHAVIORAL> GC
TSRANGE 1985 3 2019 3
EQ> TSDELTALOG(GC,1) = c1+c2*(LOG(TSLAG(GC,1))-LOG(TSLAG(Y,1)/(1-2*TSLAG(LURGAP,1)/100)))
                      +c3*TSLAG(TSDELTALOG(GC,1),1) + c4*(TDLLA + TDLLHPP + TDLLPOP)
COEFF> c1 c2 c3 c4
RESTRICT> c3+c4=1

COMMENT> Government Investment
BEHAVIORAL> GI
TSRANGE 1986 1 2019 3
EQ> TSDELTALOG(GI,1)= c1+c2*(TSLAG(LOG(GI,1))-LOG(TSLAG(Y,1)/(1-2*TSLAG(LURGAP,1)/100)))
                        +c3*TSLAG(TSDELTALOG(GI,1),1) + c4*(TDLLA + TDLLHPP + TDLLPOP)
COEFF> c1 c2 c3 c4
RESTRICT> c3+c4=1

COMMENT> Non-mining Investment
BEHAVIORAL> IBN
TSRANGE 1979 4 2019 3
EQ> TSDELTALOG(IBN,1) = c1+c2*(LOG(TSLAG(IBN,1))-LOG(TSLAG(GNE,1)) + 0.4*LOG((TSLAG(IBCR,1)*TSLAG(PGNE,1)/TSLAG(PIBN,1)))+ LOG(TSLAG(PIBN,1)/TSLAG(PGNE,1)) 
- LOG(TSLAG(TDLLA,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLHPP,1) + TSLAG(IBNDR,1)/100))
                      + c3*(LOG(TSLAG(NIBRE,1)/TSLAG(NY,1))) 
                      + c4*LOG(TSLAG(GNE,1)/TSLAG(GNE,3))
                      + c5*D_OLY
COEFF> c1 c2 c3 c4 c5
RESTRICT>c4=0.5

COMMENT> Mining Investment
BEHAVIORAL> IBRE
TSRANGE 1986 3 2019 3
EQ> TSDELTALOG(IBRE,1) = c1 + c2*(LOG(TSLAG(IBRE,1)) - LOG(TSLAG(Y,1)) + LOG(TSLAG(PIBRE,1)) - LOG(TSLAG(PGNE,1)))
                  + c3*(LOG(TSLAG(PXRE,1)) - LOG(TSLAG(PGNE,1))) 
                  + c4*LOG((TSLAG(TDLLA,1) +TSLAG(TDLLPOP,1) + TSLAG(TDLLHPP,1) + TSLAG(IBREDR,1)/100))
                  + c5*(LOG(PXRE)-LOG(TSLAG(PXRE,4)) - LOG(PGNE)+LOG(TSLAG(PGNE,4)))/4  
                  + c6*LOG(TSLAG(IBRE,1)/TSLAG(IBRE,3))/2 
                  + c7*(TSLAG(TDLLA,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLHPP,1) )  
                  + c8*D_IBRE_1 + c9*D_IBRE_2 + c10*D_IBRE_3 + c11*D_IBRE_4 + c12*D_IBRE_5
COEFF> c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12
RESTRICT>c4=-1

COMMENT> Dwelling Investment
BEHAVIORAL> ID
TSRANGE 1985 2 2019 3
EQ> TSDELTALOG(ID,1) = c1 + c2*(LOG(TSLAG(ID,1)) - LOG(TSLAG(RC,1)) + 0.3*(TSLAG(NMR,1)-TSLAG(PI_E,1))/100 + c10*LOG(TSLAG(PID,1)/TSLAG(PC,1))) 
      + c3*D_GSTDEC+c4*D_GSTSEP+c5*D_GSTJUN+c6*D_GSTMAR 
      + c7*(LOG(TSLAG(RPH,1)/TSLAG(RPH,4)))/3 
      + c8*(TSLAG(NMR,1)-TSLAG(NMR,5))/400 
      + c9*(TSLAG(TDLLPOP,1) + TSLAG(TDLLHPP,1) + TSLAG(TDLLA,1))
COEFF> c1 c2 c3 c4 c5 c6 c7 c8 c9
RESTRICT>c9=1

COMMENT> Stocks
BEHAVIORAL> KV
TSRANGE 1959 3 2019 3
EQ> TSDELTALOG(KV,1)=c1+c2*(LOG(TSLAG(KV,1))-LOG(TSLAG(Y,1)))
              +c3*TSLAG(TSDELTALOG(KV,1),1)+c4*(TDLLA + TDLLHPP + TDLLPOP)
              + c5*PC_TREND
COEFF> c1 c2 c3 c4 c5
RESTRICT>c3+c4=1
RESTRICT>c2=-0.001

COMMENT> Employment
BEHAVIORAL> LE
TSRANGE 1980 1 2019 3
EQ> TSDELTALOG(LE,1) = c1 + c2*(LOG(TSLAG(LE,1)) - LOG(TSLAG(Y,1))+0.4*(LOG(TSLAG(RLC,1)) - TSLAG(TLLA,1)) + TSLAG(TLLA,1) + TSLAG(TLLHPP,1) ) 
+ c3*TSLAG(TSDELTALOG(LE,1),1) 
+ c4*(TSLAG(TSDELTALOG(Y,1),1)-(TSLAG(TDLLA,1)+TSLAG(TDLLPOP,1)+TSLAG(TDLLHPP,1)))
+ c5*(TSLAG(TDLLPOP,1)) 
+ c6*(TSDELTALOG(RLC,1) - TSLAG(TDLLA,1)) 
+ c7*(TSDELTALOG(Y,1) - TSLAG(TDLLA,1) - TSLAG(TDLLPOP,1) - TSLAG(TDLLPOP,1)) 
+ c8*D_LE   
COEFF> c1 c2 c3 c4 c5 c6 c7 c8
RESTRICT>c3+c5=1

COMMENT> Employment Rate
BEHAVIORAL> LUR
TSRANGE 1964 1 2019 3
EQ> TSDELTA(LUR,1) = c1*(LOKLAG*(TSDELTA(TSLAG(LUR,1)))- LUR_DUM*0.025*(TSLAG(LUR,2)-TSLAG(TLUR,1))) 
                    + c2*( ((LOG(Y)-LOG(TSLAG(Y,2)))/2) - TY )
                    + c3*( (LOG(TSLAG(RULc,2))-LOG(TSLAG(RULc,4)))/2 )
COEFF> c1 c2 c3
RESTRICT>c1=1

COMMENT> Imports
BEHAVIORAL> M
TSRANGE 1986 2 2019 3
EQ> TSDELTALOG(M,1) = c1+c2*(LOG(TSLAG(M,1))-LOG(TSLAG(IAD,1)))
            +c3*(LOG(TSLAG(PM,1))-LOG(TSLAG(PDFD,1))+LOG(TSLAG(PM,2))-LOG(TSLAG(PDFD,2))+LOG(TSLAG(PM,3))-LOG(TSLAG(PDFD,3))+LOG(TSLAG(PM,4))-LOG(TSLAG(PDFD,4)))
            +c4*TSDELTALOG(IAD,1)
            +c5*TSDELTALOG(TSLAG(IAD,1),1)
            +c6*TSDELTALOG(TSLAG(IAD,2))
            +c7*(TSLAG(TDLLPOP,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLA,1))
            +c8*(LOG(TSLAG(PM,1)/TSLAG(PDFD,1))-LOG(TSLAG(PM,2)/TSLAG(PDFD,2)))
            +c9*PC_TREND
COEFF> c1 c2 c3 c4 c5 c6 c7 c8 c9
RESTRICT> c4 + c5 + c6 + c7 = 1

COMMENT> 10 Year Bond Rate
BEHAVIORAL> N10R
TSRANGE 1993 1 2019 3
EQ> N10R = c1*TSLAG(N10R,1)
            +c2*(RSTAR+PI_E+0.25*(NCR-PI_E-RSTAR))
            +c3
COEFF> c1 c2 c3
RESTRICT> c1+c2 = 1

COMMENT> 2-Year Bond Rate
BEHAVIORAL> N2R
TSRANGE 1993 1 2019 3
EQ> N2R= c1 + c2*(RSTAR+PI_E+0.52*(NCR-RSTAR-PI_E))
            +c3*TSLAG(N2R,1)
COEFF> c1 c2 c3
RESTRICT> c2 + c3 = 1

COMMENT> Nominal business borrowing rate spread to the cash rate
BEHAVIORAL> NBRSP
TSRANGE 1993 1 2019 3
EQ> NBRSP = c1 + c2*TSLAG(NBRSP,1) + c3*LURGAP
COEFF> c1 c2 c3

COMMENT> Nominal household credit
BEHAVIORAL> NHC
TSRANGE 1993 1 2019 3
EQ> TSDELTALOG(NHC,1)=c1+c2*(LOG(TSLAG(NHC,1))-LOG(TSLAG(PH,1))-LOG(TSLAG(KID,2)))
            +c3*TSLAG(RMR,1)
            +c4*TSDELTALOG(TSLAG(NHC,1),1)
            +c5*TSDELTALOG(TSLAG(PH,1),1)
            +c6*TSDELTALOG(TSLAG(KID,2),1)
            +c7*TSDELTA(TSLAG(NMR,1),1)
            +c8*(PI_E/400)
COEFF> c1 c2 c3 c4 c5 c6 c7 c8
RESTRICT> c4+c6=1
RESTRICT> c4+c5+c8=1

COMMENT> Nominal household Financial assets
BEHAVIORAL> NHFA
TSRANGE 1993 1 2019 3
EQ> TSDELTALOG(NHFA,1)=c1+c2*(LOG(TSLAG(NHFA,1))-LOG(TSLAG(NGNE,1)))
                  +c3*TSDELTALOG(NGNE,1)
                  +c4*(TSLAG(TDLLA,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLPOP,1) + PI_E/400)
                  +c5*NHFA_TREND
                  +c6*PID_TREND
COEFF> c1 c2 c3 c4 c5 c6
RESTRICT>c4+c5=1

COMMENT> Household Liabilities
BEHAVIORAL> NHL
TSRANGE 1993 1 2019 3
EQ> TSDELTALOG(NHL,1)= c1+c10*LOG(TSLAG(NHL,1))
              +c2*(-LOG(TSLAG(NHC,1)))+c3*(-LOG(TSLAG(NY,1)))
              +c4*TSLAG(TSDELTALOG(NHL,1),1)
              +c5*TSDELTALOG(NHC,1)
              +c6*TSDELTALOG(NY,1)
COEFF> c1 c10 c2 c3 c4 c5 c6
RESTRICT>c2+c3=1 
RESTRICT>c4+c5+c6=1

COMMENT> Nominal Household Other Income
BEHAVIORAL> NHOY
TSRANGE 1985 1 2019 3
EQ> TSDELTALOG(NHOY,1) = c1 + c2*(LOG(TSLAG(NHOY,1)) - LOG(TSLAG(NYEXCOE,1)))
                    +c3*TSDELTA(NMR,1)/100 
                    +c4*(TSLAG(TDLLA,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLPOP,1) + PI_E/400)
COEFF> c1 c2 c3 c4
RESTRICT>c4=1

COMMENT> Nominal GDP excl COE
IDENTITY> NYEXCOE
EQ> NYEXCOE = NY - NHCOE

COMMENT> Spread Between Nominal Mortgage and Cash Rate
BEHAVIORAL> NSP
TSRANGE 1998 1 2019 3
EQ> NSP = c1 + c2*D_NSP + c3*TSLAG(NSP,1)
COEFF>c1 c2 c3

COMMENT> Ownership Transfer Costs
BEHAVIORAL> OTC
TSRANGE 1983 1 2019 3
EQ> TSDELTALOG(OTC,1) = c1 + c2*(LOG(TSLAG(OTC,1)) - LOG(TSLAG(ID,1)))
              +c3*(LOG(TSLAG(POTC,1)/LOG(TSLAG(PID,1))))
              +c4*TSDELTA(TSLAG(NMR,1),1)/400
              +c5*(TSDELTALOG(PH,1)*100 - PI_TARGET/4)
              +c6*D_GSTSEP
              +c7*(TSLAG(TDLLA,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLPOP,1))
COEFF> c1 c2 c3 c4 c5 c6 c7
RESTRICT>c7=1

COMMENT> P
BEHAVIORAL> P
TSRANGE 1987 1 2019 3
EQ> TSDELTALOG(P,1) = c1*TSDELTALOG(PTM,1) 
              + c2*TSDELTALOG(POIL,1)
              + c3*TSLAG(TSDELTALOG(POIL,1),1)
COEFF> c1 c2 c3
RESTRICT> c1+c2+c3=1

COMMENT> PAE
BEHAVIORAL> PAE
TSRANGE 1997 4 2019 3
EQ> TSDELTALOG(PAE,1) = c1*(TDLLA + PI_E/400 ) 
              + c2*(TSDELTALOG(PW,1)-0.4239*TDLLA - PI_E/400)
COEFF> c1 c2
RESTRICT>c1=1

COMMENT> PC
BEHAVIORAL> PC
TSRANGE 1982 1 2019 3
EQ> TSDELTALOG(PC,1)=c1+c2*(LOG(TSLAG(PC,1))-LOG(TSLAG(PTM,1)))
                 +c3*PC_TREND/100
                 +c4*TSDELTALOG(PTM,1)
                 +c5*TSDELTALOG(PM,1)
                 +c6*D_GSTSEP
COEFF> c1 c2 c3 c4 c5 c6
RESTRICT>c4+c5=1

COMMENT> PG
BEHAVIORAL> PG
TSRANGE 1986 1 2019 3
EQ> TSDELTALOG(PG,1)=c1+c2*(LOG(TSLAG(PG,1))-LOG(TSLAG(PC,1)))
                +c3*(TSLAG(TSDELTALOG(PG,1),1))
                +c4*TSDELTALOG(PC,1)
COEFF> c1 c2 c3 c4
RESTRICT>c3+c4=1

COMMENT> PH
BEHAVIORAL> PH
TSRANGE 1988 3 2019 3
EQ> TSDELTALOG(PH,1) = c1 + c2*(LOG(TSLAG(PH,1)) - LOG(TSLAG(PRT,1)))
              +c3*TSLAG(RMR,1)
              +c4*TSLAG(TSDELTALOG(PH,1),1)
              +c5*PI_E/400
              +c6*TSLAG(TSDELTA(NMR,1),1)
COEFF> c1 c2 c3 c4 c5 c6
RESTRICT>c6=-0.003

COMMENT> PIBN
BEHAVIORAL> PIBN
TSRANGE 1986 1 2019 3
EQ> TSDELTALOG(PIBN,1)=c1+c2*(LOG(TSLAG(PIBN,1))-LOG(TSLAG(PC,1)))
                +c3*PIBN_TREND_1
                +c4*PIBN_TREND_2
                +c5*TSLAG(TSDELTALOG(PIBN,1),1)
                +c6*TSLAG(TSDELTALOG(PIBN,1),2)
                +c7*TSDELTALOG(PM,1)
                +c8*D_GSTSEP
COEFF> c1 c2 c3 c4 c5 c6 c7 c8
RESTRICT>c5+c6+c7=1

COMMENT> PIBRE
BEHAVIORAL> PIBRE
TSRANGE 1990 1 2019 3
EQ> TSDELTALOG(PIBRE,1) = c1+c2*( LOG(TSLAG(PIBRE,1))-LOG(TSLAG(PC,1)) )
                    +c3*TSDELTALOG(PC,1)
                    +c4*TSLAG(TSDELTALOG(PIBRE,1),1)
                    +c5*PIBRE_DUM1 
                    +c6*PIBRE_DUM2 
                    +c7*PIBRE_DUM3
                    +c8*PIBRE_DUM4
COEFF> c1 c2 c3 c4 c5 c6 c7 c8
RESTRICT>c3+c4=1


COMMENT> PMCG
BEHAVIORAL> PMCG
TSRANGE 1984 1 2019 3
EQ> TSDELTALOG(PMCG,1) = c2*TSDELTALOG(PM,1) 
                        + c6*TSLAG(TSDELTALOG(PMCG,1),1) 
                        + c3*TSDELTALOG(POIL,1)
                        + c4*TSLAG(TSDELTALOG(POIL,1),1) 
                        + c5*D_CPMCG
COEFF> c2 c6 c3 c4 c5
RESTRICT>c6+c2+c3+c4=1

COMMENT> POTC
BEHAVIORAL> POTC
TSRANGE 1992 1 2019 3
EQ> TSDELTALOG(POTC,1)= c1+c2*(LOG(TSLAG(POTC,1))-LOG(TSLAG(PC,1)))
                        +c3*POTC_TREND_1
                        +c4*POTC_TREND_2
                        +c5*TSLAG(PI_E,1)/400
COEFF> c1 c2 c3 c4 c5
RESTRICT>c5=1

COMMENT> PRT
BEHAVIORAL> PRT
TSRANGE 1993 1 2019 3
EQ> TSDELTALOG(PRT,1) = c1 + c2*(LOG(TSLAG(PRT,1))-LOG(TSLAG(PC,1))-LOG(TSLAG(HCOE,4))-LOG(TSLAG(KID,5)))
            + c3*TSLAG(TSDELTALOG(PRT,1),1)
            + c4*PI_E/400
            + c5*(LOG(TSLAG(HCOE,1)/TSLAG(HCOE,3)))/2
            + c6*(TSLAG(TDLLA,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLPOP,1))
COEFF> c1 c2 c3 c4 c5 c6
RESTRICT>c5+c6=0
RESTRICT>c3+c4=1

COMMENT> PTM
BEHAVIORAL> PTM
TSRANGE 1993 1 2019 3
EQ> TSDELTALOG(PTM,1) = c1+c2*(LOG(TSLAG(PEX,1)))
        +c3*LOG(TSLAG(NULCBS,1))+c4*LOG(TSLAG(PMCG,1))
        +c5*TSLAG(TSDELTALOG(PTM,1),1)
        +c6*TSLAG(PI_E,1)/400
        +c7*LURGAP
COEFF> c1 c2 c3 c4 c5 c6 c7
RESTRICT>c3+c4=1
RESTRICT>c5+c6=1

COMMENT> PW
BEHAVIORAL> PW
TSRANGE 1998 1 2019 3
EQ> TSDELTALOG(PW,1)=c1*TDLLA
                    +c2*(TSLAG(LURGAP,1)/TSLAG(LUR,1))
                    +c3*TSLAG(TSDELTA(LUR,1),1)
                    +c4*PI_E/400
                    +c5*(LOG(TSLAG(PY,1))-LOG(TSLAG(PY,9)))/8
                    +c6*TSLAG(TSDELTALOG(PW,1))
COEFF> c1 c2 c3 c4 c5 c6
RESTRICT>c4+c5+c6=1

COMMENT> PXAG
BEHAVIORAL> PXAG
TSRANGE 1985 2 2019 3
EQ> TSDELTALOG(PXAG,1)=c1+c2*(LOG(TSLAG(PXAG,1))-LOG(TSLAG(WPAG,1))+LOG(TSLAG(NUSD,1)))
                +c3*TSDELTALOG(NUSD,1)
                +c4*TSDELTALOG(WPAG,1)
                +c5*TSLAG(TSDELTALOG(WPAG,1),1)
                +c6*TSLAG(PI_E,1)/400
COEFF> c1 c2 c3 c4 c5 c6
RESTRICT>c4+c5+c6=1

COMMENT> PXM
BEHAVIORAL> PXM
TSRANGE 1985 1 2019 3
EQ> TSDELTALOG(PXM,1)=c1+c2*(LOG(TSLAG(PXM,1))-LOG(TSLAG(PM,1)))
                      +c3*PXM_TREND/100
                      +c4*TSDELTALOG(PM,1)
                      +c5*TSLAG(PI_E,1)/400
COEFF> c1 c2 c3 c4 c5
RESTRICT>c4+c5=1

COMMENT> PXO
BEHAVIORAL> PXO
TSRANGE 1990 1 2019 3
EQ> TSDELTALOG(PXO,1)=c1+c2*(LOG(TSLAG(PXO,1))-LOG(TSLAG(NUSD,1)))
                +c3*LOG(TSLAG(PC,1))
                +c4*(LOG(TSLAG(WPCOM,1)))
                +c5*TSDELTALOG(PC,1)
                +c6*(TSDELTALOG(WPCOM,1)-TSDELTALOG(NUSD,1))
COEFF> c1 c2 c3 c4 c5 c6
RESTRICT>c3+c4=1
RESTRICT>c5+c6=1

COMMENT> PXRE
BEHAVIORAL> PXRE
TSRANGE 1985 1 2019 3
EQ> TSDELTALOG(PXRE,1)=c1+c2*(LOG(TSLAG(PXRE,1))-LOG(TSLAG(WPCOM,1))+LOG(TSLAG(NUSD,1)))
                      +c3*TSDELTALOG(NUSD,1)
                      +c4*TSDELTALOG(WPCOM,1)
                      +c5*TSLAG(PI_E,1)/400
COEFF> c1 c2 c3 c4 c5

COMMENT> PXS
BEHAVIORAL> PXS
TSRANGE 1985 1 2019 3
EQ> TSDELTALOG(PXS,1)=c1+c2*(LOG(TSLAG(PXS,1))-LOG(TSLAG(PC,1)))
                      +c3*PXS_TREND_1/100
                      +c4*PXS_TREND_2/100
                      +c5*TSDELTALOG(PC,1)
                      +c6*TSLAG(TSDELTALOG(PXS,1),1)
COEFF> c1 c2 c3 c4 c5 c6
RESTRICT>c5+c6=1

COMMENT> RC
BEHAVIORAL> RC
TSRANGE 1989 1 2019 3
EQ> TSDELTALOG(RC,1)=c1+c2*(LOG(TSLAG(RC,1))+0.5/100*(TSLAG(RCR,1)))
              +c3*LOG(TSLAG(HDY,1))+c4*LOG(TSLAG(HNW,1))
              +c5*TSDELTALOG(HCOE,1)
              +c6*TSLAG(TSDELTALOG(HOY,1),2)
              +c7*TSLAG(TSDELTALOG(HNW,1),1)
              +c8*(TSLAG(TDLLA,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLPOP,1))
              +c9*TSDELTA(TSLAG(LUR,2))/100
              +c10*DUM_RC
COEFF> c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
RESTRICT>c5=0.15
RESTRICT>c5+c6+c7+c8=1

COMMENT> RTWI
BEHAVIORAL> RTWI
TSRANGE 1990 1 2019 3
EQ> TSDELTALOG(RTWI,1) = c1+c2*(LOG(TSLAG(RTWI,1))+3.5/100*(TSLAG(WR2SP,1)-WR2R_GAP_AVG))
                  +c3*LOG(TSLAG(TOT,1))
                  +c4*TSDELTALOG(TOT,1)
                  +c5*TSDELTA(WR2SP,1)
COEFF> c1 c2 c3 c4 c5
RESTRICT>c5=-0.05

COMMENT> WPAG
BEHAVIORAL> WPAG
TSRANGE 1985 1 2019 3
EQ> TSDELTALOG(WPAG,1) = c1+c2*(LOG(TSLAG(WPAG,1))-LOG(TSLAG(WP,1)))
                          +c3*TSLAG(TSDELTALOG(WPAG,1),1)
                          +c4*PI_E/400
COEFF> c1 c2 c3 c4
RESTRICT>c3+c4=1

COMMENT> WPOIL
BEHAVIORAL> WPOIL
TSRANGE 1985 1 2019 3
EQ> TSDELTALOG(WPOIL,1) = c1+c2*(LOG(TSLAG(WPOIL,1))-LOG(TSLAG(WP,1)))
                          +c3*TSLAG(TSDELTALOG(WPOIL,1),1)
                          +c4*PI_E/400
COEFF> c1 c2 c3 c4
RESTRICT>c3+c4=1

COMMENT> WPX
BEHAVIORAL> WPX
TSRANGE 1985 1 2019 3
EQ> TSDELTALOG(WPX,1) = c1+c2*(LOG(TSLAG(WPX,1)/TSLAG(WP,1)))
                        +c3*WPX_TREND_1
                        +c4*WPX_TREND_2
                        +c5*TSLAG(TSDELTALOG(WPX,1))
                        +c6*TSDELTALOG(WP,1)
                        +c7*D_2008Q4
COEFF> c1 c2 c3 c4 c5 c6 c7
RESTRICT> c5+c6=1

COMMENT> WR2R
BEHAVIORAL> WR2R
TSRANGE 1993 1 2019 3
EQ> WR2R =  c1*WRR + c2*(TSLAG(WR2R,1)-TSLAG(WRR,1)) + c3*WR2GAP
COEFF> c1 c2 c3
RESTRICT>c2+c3=1
RESTRICT>c1=1

COMMENT> XAG
BEHAVIORAL> XAGEXRAIN
TSRANGE 1985 3 2019 3
EQ> TSDELTALOG(XAGEXRAIN,1) = c1+c2*(LOG(TSLAG(XAGEXRAIN,1))-LOG(TSLAG(WY,1)))
                              +c3*LOG(TSLAG(REWI,1))
                              +c4*TSLAG(TSDELTALOG(XAGEXRAIN,1),4)
                              +c5*(TSLAG(TDLLA,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLPOP,1))
                              +c6*PC_TREND
COEFF> c1 c2 c3 c4 c5 c6
RESTRICT> c4+c5=1

COMMENT> Agricultural Exports
IDENTITY> XAG
EQ> XAG = XAGEXRAIN+RAIN

COMMENT> XM
BEHAVIORAL> XM
TSRANGE 1986 2 2019 3
EQ> TSDELTALOG(XM) = c1+c2*(LOG(TSLAG(XM,1))-LOG(TSLAG(WY,1)))
                    +c3*LOG(TSLAG(REWI,1))
                    +c4*TSLAG(TSDELTALOG(WY,1),1)
                    +c5*TSLAG(TSDELTALOG(WY,1),2)
                    +c6*(TSLAG(TDLLA,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLPOP,1))
                    +c7*XS_TREND
COEFF> c1 c2 c3 c4 c5 c6 c7
RESTRICT>c5+c6=1

COMMENT> XO
BEHAVIORAL> XO
TSRANGE 1986 2 2019 3
EQ> TSDELTALOG(XO,1) = c1+c2*(LOG(TSLAG(XO,1))-LOG(TSLAG(WY,1)))
                        +c3*LOG(TSLAG(REWI,1))
                        +c4*(LOG(TSLAG(XO,1)/TSLAG(XO,3)))/2
                        +c5*(TSLAG(TDLLA,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLPOP,1))
                        +c6*XS_TREND
COEFF> c1 c2 c3 c4 c5 c6
RESTRICT>c4+c5=1

COMMENT> XRE
BEHAVIORAL> XRE
TSRANGE 1959 3 2019 3
EQ> TSDELTALOG(XRE,1) = c1+c2*(LOG(TSLAG(XRE,1))-LOG(TSLAG(KIBRE,6)))
                  +c3*TSLAG(TSDELTALOG(XRE,1),1)
                  +c4*D_RBAGOLD
                  +c5*(TSLAG(TDLLPOP,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLA,1))
COEFF> c1 c2 c3 c4 c5
RESTRICT>c3+c5=1

COMMENT> XS
BEHAVIORAL> XS
TSRANGE 1988 1 2019 3
EQ> TSDELTALOG(XS,1)=c1+c2*(LOG(TSLAG(XS,1))-LOG(TSLAG(WY,1)))
                      +c3*LOG(TSLAG(REWI,1))
                      +c4*D_OLYX
                      +c5*(LOG(WY)-LOG(TSLAG(WY,4)))/4
                      +c6*TSDELTALOG(REWI)
                      +c7*(TSLAG(TDLLPOP,1) + TSLAG(TDLLPOP,1) + TSLAG(TDLLA,1))
                      +c8*XS_TREND
COEFF> c1 c2 c3 c4 c5 c6 c7 c8
RESTRICT>c6+c7=1

COMMENT> tlla
IDENTITY> tlla
EQ> tlla  = TSLAG(tlla,1)  + tdlla

COMMENT> tdlla
IDENTITY> tdlla
EQ> tdlla  = 0.95  * TSLAG(tdlla,1)  + ( 0.05  * aasteady_state_la )

COMMENT> tllhpp
IDENTITY> tllhpp
EQ> tllhpp  = TSLAG(tllhpp,1)  + tdllhpp

COMMENT> tdllhpp
IDENTITY> tdllhpp
EQ> tdllhpp  = 0.95  * TSLAG(tdllhpp,1)

COMMENT> lhpp
IDENTITY> lhpp
EQ> LOG(lhpp)  = tllhpp

COMMENT> tllpop
IDENTITY> tllpop
EQ> tllpop  = TSLAG(tllpop,1) + tdllpop

COMMENT> tdllpop
IDENTITY> tdllpop
EQ> tdllpop  = 0.975*TSLAG(tdllpop,1) + ( 0.025  * aasteady_state_pop )

COMMENT> pi_e
IDENTITY> pi_e
EQ> pi_e  = 0.9  * TSLAG(pi_e,1)  + 0.1  * pi_target

COMMENT> TY_POT
IDENTITY> TY_POT
EQ>  TY_POT  = TDLLA  + TDLLPOP  + TDLLHPP

COMMENT> hoy
IDENTITY> hoy
EQ>  hoy  = nhoy  / pc

COMMENT> nhdy
IDENTITY> nhdy
EQ>  nhdy  = nhcoe  + nhoy

COMMENT> hdy
IDENTITY> hdy
EQ>  hdy  = nhdy  / pc

COMMENT> nhs
IDENTITY> nhs
EQ> nhs  = nhdy  - nc  - kidc

COMMENT> nhsr
IDENTITY> nhsr
EQ>  nhsr  = nhs  / (nhdy  - kidc)

COMMENT> nha
IDENTITY> nha
EQ>  nha  = nhnfa  + nhfa

COMMENT> nhnfa
IDENTITY> nhnfa
EQ> TSDELTALOG(nhnfa)  = TSDELTALOG(kid*ph)

COMMENT> nhnw
IDENTITY> nhnw
EQ>  nhnw  = nha  - nhl

COMMENT> hnw
IDENTITY> hnw
EQ>  hnw  = nhnw  / pc

COMMENT> nhws
IDENTITY> nhws
EQ>  nhws  = nhcoe  / ny

COMMENT> kidc
IDENTITY> kidc
EQ> TSDELTALOG(kidc)  = 0.95*TSLAG(TSDELTALOG(kidc,1),1)  + (1-0.95)*(TSLAG(tdlla,1)  + TSLAG(tdllpop,1)  + TSLAG(tdllhpp,1)  + pi_e  / 400)

COMMENT> rph
IDENTITY> rph
EQ>  rph  = ph  / ptm

COMMENT> iddr
IDENTITY> iddr
EQ> iddr  = TSLAG(iddr,1)

COMMENT> kid
IDENTITY> kid
EQ> kid  = (1  - iddr  / 100)*TSLAG(kid,1) + id

COMMENT> ib
IDENTITY> ib
EQ>  ib  = ibre  + ibn

COMMENT> ibcr
IDENTITY> ibcr
EQ>  ibcr  = (rbr  / 100  + ibndra  / 100)  * ((1  - ibctr  * (ibndra  / 100  * (1  + n10r  / 100)  / (n10r  / 100  + ibndra  / 100)))  / (1  - ibctr))  * (pibn  / pgne)

COMMENT> ibctr
IDENTITY> ibctr
EQ> ibctr  = TSLAG(ibctr,1)

COMMENT> kibn
IDENTITY> kibn
EQ> kibn  = (1  - ibndr  / 100)  * TSLAG(kibn,1)  + ibn

COMMENT> ibndr
IDENTITY> ibndr
EQ> ibndr  = TSLAG(ibndr,1)

COMMENT> ibndra
IDENTITY> ibndra
EQ>  ibndra  = ibndr  + TSLAG(ibndr,1)  + TSLAG(ibndr,2)  + TSLAG(ibndr,3)

COMMENT> kibre
IDENTITY> kibre
EQ> kibre  = (1-ibredr/100)*TSLAG(kibre,1)+ibre

COMMENT> ibredr
IDENTITY> ibredr
EQ> ibredr  = TSLAG(ibredr,1)

COMMENT> V
IDENTITY> V
EQ> V  = TSDELTA(KV,1)

COMMENT> g
IDENTITY> g
EQ> g  = gi  + gc

COMMENT> x
IDENTITY> x
EQ>  x  = xm  + xag  + xo  + xre  + xs

COMMENT> iad
IDENTITY> iad
EQ> iad  = rc^iad_w_c  * ib^iad_w_i  * gi^iad_w_gi  * gc^iad_w_gc  * x^iad_w_x

COMMENT> piad
IDENTITY> piad
EQ>  piad  = pc^iad_w_c  * pib^iad_w_i  * pg^iad_w_gi  * pg^iad_w_gc  * px^iad_w_x

COMMENT> ATS
IDENTITY> ATS
EQ> ATS  = TSLAG(ATS,1)

COMMENT> dfd
IDENTITY> dfd
EQ> dfd  = rc  + id  + ib  + g  + otc  - ats

COMMENT> dfdx
IDENTITY> dfdx
EQ>  dfdx  = dfd  + x

COMMENT> dpfd
IDENTITY> dpfd
EQ>  dpfd  = rc  + id  + ib  + otc  - ats

COMMENT> gne
IDENTITY> gne
EQ> gne  = dfd  + v

COMMENT> y
IDENTITY> y
EQ>  y  = rc  + id  + ib  + g  + otc  - ats  + v  + x  - m  + sd

COMMENT> sd
IDENTITY> sd
EQ> sd  = TSLAG(sd,1)

COMMENT> nc
IDENTITY> nc
EQ>  nc  = pc  * rc  / 100

COMMENT> nid
IDENTITY> nid
EQ>  nid  = pid  * id  / 100

COMMENT> notc
IDENTITY> notc
EQ>  notc  = potc  * otc  / 100

COMMENT> nib
IDENTITY> nib
EQ>  nib  = nibn  + nibre

COMMENT> nibn
IDENTITY> nibn
EQ>  nibn  = pibn  * ibn  / 100

COMMENT> nibre
IDENTITY> nibre
EQ>  nibre  = pibre  * ibre  / 100

COMMENT> ng
IDENTITY> ng
EQ>  ng  = pg  * g  / 100

COMMENT> nv
IDENTITY> nv
EQ> nv  = pg  * v  / 100

COMMENT> nxre
IDENTITY> nxre
EQ>  nxre  = pxre  * xre  / 100

COMMENT> nxm
IDENTITY> nxm
EQ>  nxm  = pxm  * xm  / 100

COMMENT> nxs
IDENTITY> nxs
EQ>  nxs  = pxs  * xs  / 100

COMMENT> nxo
IDENTITY> nxo
EQ>  nxo  = pxo  * xo  / 100

COMMENT> nxag
IDENTITY> nxag
EQ>  nxag  = pxag  * xag  / 100

COMMENT> nx
IDENTITY> nx
EQ>  nx  = nxm  + nxs  + nxo  + nxre  + nxag

COMMENT> nm
IDENTITY> nm
EQ>  nm  = pm  * m  / 100

COMMENT> nats
IDENTITY> nats
EQ> nats  = TSLAG(nats,1)

COMMENT> ndfd
IDENTITY> ndfd
EQ> ndfd  = nc  + nid  + nib  + ng  + notc  - nats

COMMENT> ndpfd
IDENTITY> ndpfd
EQ>  ndpfd  = nc  + nid  + nib  + notc  - nats

COMMENT> ngne
IDENTITY> ngne
EQ> ngne  = ndfd  + nv

COMMENT> nsd
IDENTITY> nsd
EQ> nsd  = TSLAG(nsd,1)

COMMENT> ny
IDENTITY> ny
EQ>  ny  = nc  + nid  + nib  + ng  + notc  + nv  + nx  - nm  - nats  + nsd

COMMENT> nhcoe
IDENTITY> nhcoe
EQ>  TSDELTALOG(nhcoe,1)  = TSDELTALOG(pae,1)+TSDELTALOG(le,1)+TSDELTALOG(lhpp,1)

COMMENT> hcoe
IDENTITY> hcoe
EQ>  hcoe  = nhcoe  / pc

COMMENT> nulc
IDENTITY> nulc
EQ>  TSDELTALOG(nulc,1)  = TSDELTALOG(nhcoe,1)-TSDELTALOG(y,1)

COMMENT> nulcbs
IDENTITY> nulcbs
EQ> nulcbs  = nulc/TSLAG(nulc,1)*TSLAG(nulcbs,1)

COMMENT> rlc
IDENTITY> rlc
EQ>  rlc  = pae  / pgne

COMMENT> rulc
IDENTITY> rulc
EQ>  rulc  = nulc  / pgne

COMMENT> rulcy
IDENTITY> rulcy
EQ>  rulcy  = nulc  / py

COMMENT> pex
IDENTITY> pex
EQ> pex  = TSLAG(pex,1)*(ptm/TSLAG(ptm,1))

COMMENT> poil
IDENTITY> poil
EQ>  poil  = wpoil  / nusd

COMMENT> px
IDENTITY> px
EQ>  px  = nx  * 100  / x

COMMENT> pib
IDENTITY> pib
EQ>  pib  = nib  * 100  / ib

COMMENT> pdfd
IDENTITY> pdfd
EQ>  pdfd  = ndfd  * 100  / dfd

COMMENT> pdpfd
IDENTITY> pdpfd
EQ>  pdpfd  = ndpfd  * 100  / dpfd

COMMENT> pgne
IDENTITY> pgne
EQ>  pgne  = ngne  * 100  / gne

COMMENT> py
IDENTITY> py
EQ>  py  = ny  * 100  / y

COMMENT> tot
IDENTITY> tot
EQ>  tot  = px  / pm  * 100

COMMENT> loklag
IDENTITY> loklag
EQ> loklag  = TSLAG(loklag,1)

COMMENT> ty
IDENTITY> ty
EQ> ty  = tdlla  + tdllpop  + tdllhpp  + 0.95  * (TSLAG(ty,1)-TSLAG(tdlla,1)  - TSLAG(tdllhpp,1)  - TSLAG(tdllpop,1) )

COMMENT> tlur
IDENTITY> tlur
EQ> tlur  = TSLAG(tlur,1)

COMMENT> lurgap
IDENTITY> lurgap
EQ>  lurgap  = lur  - tlur

COMMENT> lf
IDENTITY> lf
EQ>  lf  = le  / (1  - (lur  / 100))

COMMENT> lpr
IDENTITY> lpr
EQ> lpr  = lf  / lpop  * 100

COMMENT> lpop
IDENTITY> lpop
EQ> TSDELTALOG(lpop)  = tdllpop  - 0.005  * (log(TSLAG(lpop,1))-TSLAG(tllpop,1))

COMMENT> ncr
IDENTITY> ncr
EQ> ncr  = 0.7  * TSLAG(ncr,1)  + 0.3  * ( rstar  + (ptm  / TSLAG(ptm,4)  * 100  - 100)  + (tr_ptm  - 1)  * (ptm  / TSLAG(ptm,4)  * 100  - 100  - pi_target)  - tr_lurgap  * lurgap)  - tr_dlur  / 2  * (lur  - TSLAG(lur,2))

COMMENT> rcr
IDENTITY> rcr
EQ>  rcr  = ((1  + ncr  / 100)  / (1  + ptm  / TSLAG(ptm,4)  - 1))  * 100  - 100

COMMENT> r2r
IDENTITY> r2r
EQ>  r2r  = ((1  + n2r  / 100)  / (1  + ptm  / TSLAG(ptm,4)  - 1))  * 100  - 100

COMMENT> nbr
IDENTITY> nbr
EQ>  nbr  = ncr  + nbrsp

COMMENT> rbr
IDENTITY> rbr
EQ>  rbr  = (1  + (1  - ibctr)  * nbr  / 100)  / (TSLAG(ptm,1)/TSLAG(ptm,5))  * 100  - 100

COMMENT> nmr
IDENTITY> nmr
EQ>  nmr  = ncr  + nsp

COMMENT> rmr
IDENTITY> rmr
EQ>  rmr  = ( (1  + nmr  / 100)  / (ptm  / TSLAG(ptm,4))  - 1 )  * 100

COMMENT> rmre
IDENTITY> rmre
EQ>  rmre  = nmr  - pi_e

COMMENT> rstar
IDENTITY> rstar
EQ> rstar  = 0.95  * TSLAG(rstar,1)  + 0.05  * rcr_target

COMMENT> rtwi_const
IDENTITY> rtwi_const
EQ> rtwi_const  = TSLAG(rtwi_const,1)  + (0.0005  * (TSLAG(rcr,1)  - rcr_target))  * rtwi_const_dum

COMMENT> rtwi
IDENTITY> rtwi
EQ> TSDELTALOG(rtwi,1)  = rtwi_const  - 0.218928*(log(TSLAG(rtwi,1))  - 0.519544*log(TSLAG(tot,1))+3.5/100  * (TSLAG(wr2sp,1)  - wr2r_gap_avg))  + 0.231987  * TSDELTALOG(tot,1)  - 5  / 100  * TSDELTA(wr2sp)

COMMENT> ntwi
IDENTITY> ntwi
EQ> TSDELTALOG(ntwi,1)  = TSDELTALOG(rtwi,1)  + TSDELTALOG(wp,1)  - TSDELTALOG(ptm,1)

COMMENT> nusd
IDENTITY> nusd
EQ> TSDELTALOG(nusd,1)  = TSDELTALOG(ntwi,1)

COMMENT> rewi
IDENTITY> rewi
EQ> rewi  = TSLAG(rewi,1) * rtwi  / TSLAG(rtwi,1)

COMMENT> wy
IDENTITY> wy
EQ> TSDELTALOG(wy,1)  = aasteady_state_pop  + aasteady_state_la  + 0.9  * (TSLAG(TSDELTALOG(wy,1),1)  - (aasteady_state_pop  + aasteady_state_la))

COMMENT> wp
IDENTITY> wp
EQ> TSDELTALOG(wp,1)  = 0.9  * TSLAG(TSDELTALOG(wp,1),1)  + 0.1  * pi_target  / 400

COMMENT> wpcom
IDENTITY> wpcom
EQ> LOG(wpcom)  = LOG(TSLAG(wpcom,1))  + 0.2  * LOG(TSLAG(wpcom,1)/TSLAG(wpcom,5))/4  + 0.8  * pi_target  / 400  - 0.05  * ( LOG(TSLAG(wpcom,1))  - LOG(TSLAG(wp,1))  - LOG(rwpcom_lr))

COMMENT> wrr
IDENTITY> wrr
EQ> wrr  = 0.95  * TSLAG(wrr,1)  + 0.05  * wrr_target

COMMENT> wrsp
IDENTITY> wrsp
EQ>  wrsp  = wrr  - rcr

COMMENT> wr2r
IDENTITY> wr2r
EQ> wr2r  = (0.99)  * (TSLAG(wr2r,1)  - TSLAG(wrr,1))  + wrr  + (1  - 0.99)*0.806560

COMMENT> wr2sp
IDENTITY> wr2sp
EQ>  wr2sp  = wr2r  - r2r

END"

library(readxl)
xlsdata <- read_excel("MARTINDATAXLSX.xlsx")
xlsdata$DATE <- as.Date(xlsdata$DATE) 


MARTIN=LOAD_MODEL(modelText=MARTINDefinition)

MARTINDATA <- scan("MARTINDATA.txt", what="", sep="\n")

#load model data
MARTIN=LOAD_MODEL_DATA(MARTIN,MARTINDATA)

#estimate model
MARTIN=ESTIMATE(MARTIN)

