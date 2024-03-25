      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1 RPL,DDSDDT,DRPLDE,DRPLDT,STRAN,DSTRAN,
     2 TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,MATERL,NDI,NSHR,NTENS,
     3 NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,CELENT,
     4 DFGRD0,DFGRD1,NOEL,NPT,KSLAY,KSPT,KSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 MATERL
      DIMENSION STRESS(NTENS),STATEV(NSTATV),
     1 DDSDDE(NTENS,NTENS),DDSDDT(NTENS),DRPLDE(NTENS),
     2 STRAN(NTENS),DSTRAN(NTENS),TIME(2),PREDEF(1),DPRED(1),
     3 PROPS(NPROPS),COORDS(3),DROT(3,3),
     4 DFGRD0(3,3),DFGRD1(3,3),ESTIFNS(NTENS,NTENS)
      
      REAL*8, DIMENSION(NTENS) ::FENZI2nd(NTENS),
     1 ELASTRANinc(NTENS),R1(NTENS),R2(NTENS),
     2 R3(NTENS),R4(NTENS),R5(NTENS),R6(NTENS),FENMU(NTENS),
     3 FENMUi(NTENS)
      REAL*8, DIMENSION(NTENS,NTENS) ::ESTIFNSchuFENMU(NTENS,NTENS)
      
      REAL*8 Tau1, Tau2, Tau3, Tau4, Tau5, Tau6, Fai1, Fai2,
     1 Fai3, Fai4,Fai5,Fai6, FZ1TI,FZ2TI, FZ3TI, FZ4TI, FZ5TI, 
     2 FZ6TI, FZ1E,FZ2E, FZ3E,FZ4E, FZ5E, FZ6E, FZ1, FZ2,FZ3, FZ4,
     3 FZ5,FZ6,FM1TI,FM2TI,FM3TI,FM4TI,FM5TI,FM6TI,FM1FAI,FM2FAI,
     4 FM3FAI,FM4FAI,FM5FAI,FM6FAI,C11,C12,C13,C21,C22,C23,
     5 C31,C32,C33,C44,C55,C66,ENU,EBULK3,EG2,EG3,ELAM,
     6 EMODA,ES,EMODB,
     7 Tau11, Tau12, Tau13, Tau14, Tau15, Tau16, Fai11, Fai12,
     8 Fai13, Fai14,Fai15,Fai16, FZ11TI,FZ12TI, FZ13TI, FZ14TI,
     9 FZ15TI, FZ16TI, FZ11E,FZ12E, FZ13E,FZ14E, FZ15E, FZ16E,
     1 FZ11, FZ12,FZ13, FZ14,FZ15,FZ16,FM11TI,FM12TI,FM13TI,
     2 FM14TI,FM15TI,FM16TI,FM11FAI,FM12FAI,
     3 FM13FAI,FM14FAI,FM15FAI,FM16FAI,
     4 Tau21, Tau22, Tau23, Tau24, Tau25, Tau26, Fai21, Fai22,
     8 Fai23, Fai24,Fai25,Fai26, FZ21TI,FZ22TI, FZ23TI, FZ24TI,
     9 FZ25TI, FZ26TI, FZ21E,FZ22E, FZ23E,FZ24E, FZ25E, FZ26E,
     1 FZ21, FZ22,FZ23, FZ24,FZ25,FZ26,FM21TI,FM22TI,FM23TI,
     2 FM24TI,FM25TI,FM26TI,FM21FAI,FM22FAI,
     3 FM23FAI,FM24FAI,FM25FAI,FM26FAI,
     4 Tau31, Tau32, Tau33, Tau34, Tau35, Tau36, Fai31, Fai32,
     8 Fai33, Fai34,Fai35,Fai36, FZ31TI,FZ32TI, FZ33TI, FZ34TI,
     9 FZ35TI, FZ36TI, FZ31E,FZ32E, FZ33E,FZ34E, FZ35E, FZ36E,
     1 FZ31, FZ32,FZ33, FZ34,FZ35,FZ36,FM31TI,FM32TI,FM33TI,
     2 FM34TI,FM35TI,FM36TI,FM31FAI,FM32FAI,
     3 FM33FAI,FM34FAI,FM35FAI,FM36FAI


C UMAT FOR ISOTROPIC ELASTICITY
C CANNOT BE USED FOR PLANE STRESS
C -----------------------------------------------------------
C PROPS(1)-E
C PROPS(2)-NU
CC
C     IF (NDI.NE.3) THEN
C        WRITE(6,*) 'THIS UMAT MAY ONLY BE USED FOR ELEMENTS
C    1           WITH THREE DIRECT STRESS COMPONENTS'
C        CALL XIT
C     ENDIF
C
C     ELASTIC PROPERTIES
C
      EMODA=PROPS(1)
      EMODB=PROPS(2)
      ENU=PROPS(3)
      ES=PROPS(4)

C     DIRECTION 1-VISCOELASTIC

      Prop1A=PROPS(5)
      Prop1B=PROPS(6)
      Prop1C=PROPS(7)
      Prop1D=PROPS(8)
      Prop1E=PROPS(9)
      Prop1F=PROPS(10)
      Prop1G=PROPS(11)
      Prop1H=PROPS(12)
      Prop1I=PROPS(13)
      Prop1J=PROPS(14)
      Prop1K=PROPS(15)
      Prop1L=PROPS(16)
      Prop1M=PROPS(17)

C     DIRECTION 2-VISCOELASTIC

      Prop2A=PROPS(18)
      Prop2B=PROPS(19)
      Prop2C=PROPS(20)
      Prop2D=PROPS(21)
      Prop2E=PROPS(22)
      Prop2F=PROPS(23)
      Prop2G=PROPS(24)
      Prop2H=PROPS(25)
      Prop2I=PROPS(26)
      Prop2J=PROPS(27)
      Prop2K=PROPS(28)
      Prop2L=PROPS(29)
      Prop2M=PROPS(30)

C     DIRECTION 3-VISCOELASTIC

      Prop3A=PROPS(31)
      Prop3B=PROPS(32)
      Prop3C=PROPS(33)
      Prop3D=PROPS(34)
      Prop3E=PROPS(35)
      Prop3F=PROPS(36)
      Prop3G=PROPS(37)
      Prop3H=PROPS(38)
      Prop3I=PROPS(39)
      Prop3J=PROPS(40)
      Prop3K=PROPS(41)
      Prop3L=PROPS(42)
      Prop3M=PROPS(43)


C
      Tau11=Prop1B
      Tau12=Prop1D
      Tau13=Prop1F
      Tau14=Prop1H
      Tau15=Prop1J
      Tau16=Prop1L

      Tau21=Prop2B
      Tau22=Prop2D
      Tau23=Prop2F
      Tau24=Prop2H
      Tau25=Prop2J
      Tau26=Prop2L

      Tau31=Prop3B
      Tau32=Prop3D
      Tau33=Prop3F
      Tau34=Prop3H
      Tau35=Prop3J
      Tau36=Prop3L
C     



C     Fai(i)=E(0)/E(i)
C     PropA=1/E(1)
C     PropM=1/E(0)
C
      Fai11=Prop1A/Prop1M
      Fai12=Prop1C/Prop1M
      Fai13=Prop1E/Prop1M
      Fai14=Prop1G/Prop1M
      Fai15=Prop1I/Prop1M
      Fai16=Prop1K/Prop1M

      Fai21=Prop2A/Prop2M
      Fai22=Prop2C/Prop2M
      Fai23=Prop2E/Prop2M
      Fai24=Prop2G/Prop2M
      Fai25=Prop2I/Prop2M
      Fai26=Prop2K/Prop2M

      Fai31=Prop3A/Prop3M
      Fai32=Prop3C/Prop3M
      Fai33=Prop3E/Prop3M
      Fai34=Prop3G/Prop3M
      Fai35=Prop3I/Prop3M
      Fai36=Prop3K/Prop3M


C      
      EBULK=1/(1.0-ENU**2*EMODB/EMODA)
      Q11=EMODA*EBULK
      Q12=ENU*EMODB*EBULK
      Q21=ENU*EMODB*EBULK
      Q22=EMODB*EBULK
      Q66=ES


  

********************************************
C
C     Initial Elastic Calculation
C
********************************************
      IF (KSTEP.EQ.1) THEN
      DO K1=1,NTENS
        DO K2=1,NTENS
            DDSDDE(K2,K1)=0
        END DO
      END DO  
C
C     ELASTIC STIFFNESS
      DDSDDE(1,1)=Q11
      DDSDDE(1,2)=Q12
      DDSDDE(2,1)=Q21
      DDSDDE(2,2)=Q22
      DDSDDE(3,3)=Q66
C
C
C     Calculate stress from elastic strains
C
      DO K1=1,NTENS
        DO K2=1,NTENS
           STRESS(K2)=STRESS(K2)+DDSDDE(K2,K1)*DSTRAN(K1)
 60     END DO
      END DO
C
      DO K1=1,NTENS
        DO K2=1,NTENS
           ESTIFNS(K2,K1)=0.0D0
        END DO
      END DO
      
C     DO K1=1,NTENS

C     STATEV(K1+10)=1e-2        
C     STATEV(K1+20)=1e-2
C     STATEV(K1+30)=1e-2
C     STATEV(K1+40)=1e-2
C     STATEV(K1+50)=1e-2
C     STATEV(K1+60)=1e-2

C     END DO

      STATEV(11)=Fai11*DSTRAN(1)
      STATEV(12)=Fai11*DSTRAN(1)
      STATEV(13)=Fai11*DSTRAN(1)
      STATEV(14)=Fai11*DSTRAN(1)
      STATEV(15)=Fai11*DSTRAN(1)
      STATEV(16)=Fai11*DSTRAN(1)

      STATEV(2+10)=Fai21*DSTRAN(2)
      STATEV(2+20)=Fai21*DSTRAN(2)
      STATEV(2+30)=Fai21*DSTRAN(2)
      STATEV(2+40)=Fai21*DSTRAN(2)
      STATEV(2+50)=Fai21*DSTRAN(2)
      STATEV(2+60)=Fai21*DSTRAN(2)

      STATEV(3+10)=Fai31*DSTRAN(3)
      STATEV(3+20)=Fai31*DSTRAN(3)
      STATEV(3+30)=Fai31*DSTRAN(3)
      STATEV(3+40)=Fai31*DSTRAN(3)
      STATEV(3+50)=Fai31*DSTRAN(3)
      STATEV(3+60)=Fai31*DSTRAN(3)










      END IF
C
****************************************
C     Calculate Creep Stress
******************************************
C
      IF (KSTEP.GT.1) THEN 
C
C     Elastic stiffness matrix
      DO K1=1,NTENS
        DO K2=1,NTENS
           ESTIFNS(K2,K1)=0.0D0
        END DO
      END DO

 
      ESTIFNS(1,1)=Q11
      ESTIFNS(1,2)=Q12

      ESTIFNS(2,1)=Q21
      ESTIFNS(2,2)=Q22


      ESTIFNS(3,3)=Q66


C
      C11=Q11
      C12=Q12

      C21=Q21
      C22=Q22

      C33=Q66


C     R1=R1+EXP(TIME(1)/Tau1)*Fai1*ELASTRANinc
C     STATEV(1)=R1 

C     R1=STATEV(1)+EXP(TIME(1)/TAUI)*FAII*ELASTRANinc
C     STATEV(1)=RN 



      DO K1=1,NTENS
C      IF (ELASTRANinc(K1) .eq.0.0d0) THEN 
C      PRINT *, "ELASTRANinc(K1) has been resetted", ELASTRANinc(K1)
C         ELASTRANinc(K1)=1E-30
C         else 
C      end if      
      
      R1(K1)=STATEV(K1+10)
      R2(K1)=STATEV(K1+20)
      R3(K1)=STATEV(K1+30)
      R4(K1)=STATEV(K1+40)
      R5(K1)=STATEV(K1+50)
      R6(K1)=STATEV(K1+60)

*     R1(K1)=STATEV(K1+10)+ (exp(TIME(1)/Tau1))*Fai1*ELASTRANinc(K1)
*     R2(K1)=STATEV(K1+20)+ (exp(TIME(1)/Tau2))*Fai1*ELASTRANinc(K1)
*     R3(K1)=STATEV(K1+30)+ (exp(TIME(1)/Tau3))*Fai1*ELASTRANinc(K1)
*     R4(K1)=STATEV(K1+40)+ (exp(TIME(1)/Tau4))*Fai1*ELASTRANinc(K1)
*     R5(K1)=STATEV(K1+50)+ (exp(TIME(1)/Tau5))*Fai1*ELASTRANinc(K1)
*     R6(K1)=STATEV(K1+60)+ (exp(TIME(1)/Tau6))*Fai1*ELASTRANinc(K1)
      
      END DO


C     FENZI2nd=1/Tau1*exp(-TIME(1)/Tau1)*R1*DTIME+1/Tau2*exp(-TIME(1)/Tau2)*R2*DTIME+...
      



            FZ11TI=1/Tau11
            FZ12TI=1/Tau12
            FZ13TI=1/Tau13
            FZ14TI=1/Tau14
            FZ15TI=1/Tau15
            FZ16TI=1/Tau16

            FZ21TI=1/Tau21
            FZ22TI=1/Tau22
            FZ23TI=1/Tau23
            FZ24TI=1/Tau24
            FZ25TI=1/Tau25
            FZ26TI=1/Tau26

            FZ31TI=1/Tau31
            FZ32TI=1/Tau32
            FZ33TI=1/Tau33
            FZ34TI=1/Tau34
            FZ35TI=1/Tau35
            FZ36TI=1/Tau36


            FZ11E=exp(-TIME(2)*FZ11TI)
            FZ12E=exp(-TIME(2)*FZ12TI)
            FZ13E=exp(-TIME(2)*FZ13TI)
            FZ14E=exp(-TIME(2)*FZ14TI)
            FZ15E=exp(-TIME(2)*FZ15TI)
            FZ16E=exp(-TIME(2)*FZ16TI)

            FZ21E=exp(-TIME(2)*FZ21TI)
            FZ22E=exp(-TIME(2)*FZ22TI)
            FZ23E=exp(-TIME(2)*FZ23TI)
            FZ24E=exp(-TIME(2)*FZ24TI)
            FZ25E=exp(-TIME(2)*FZ25TI)
            FZ26E=exp(-TIME(2)*FZ26TI)

            FZ31E=exp(-TIME(2)*FZ31TI)
            FZ32E=exp(-TIME(2)*FZ32TI)
            FZ33E=exp(-TIME(2)*FZ33TI)
            FZ34E=exp(-TIME(2)*FZ34TI)
            FZ35E=exp(-TIME(2)*FZ35TI)
            FZ36E=exp(-TIME(2)*FZ36TI)



            FZ11=FZ11TI*FZ11E*R1(1)*DTIME
            FZ12=FZ12TI*FZ12E*R2(1)*DTIME
            FZ13=FZ13TI*FZ13E*R3(1)*DTIME
            FZ14=FZ14TI*FZ14E*R4(1)*DTIME
            FZ15=FZ15TI*FZ15E*R5(1)*DTIME
            FZ16=FZ16TI*FZ16E*R6(1)*DTIME

            FZ21=FZ21TI*FZ21E*R1(2)*DTIME
            FZ22=FZ22TI*FZ22E*R2(2)*DTIME
            FZ23=FZ23TI*FZ23E*R3(2)*DTIME
            FZ24=FZ24TI*FZ24E*R4(2)*DTIME
            FZ25=FZ25TI*FZ25E*R5(2)*DTIME
            FZ26=FZ26TI*FZ26E*R6(2)*DTIME

            FZ31=FZ31TI*FZ31E*R1(3)*DTIME
            FZ32=FZ32TI*FZ32E*R2(3)*DTIME
            FZ33=FZ33TI*FZ33E*R3(3)*DTIME
            FZ34=FZ34TI*FZ34E*R4(3)*DTIME
            FZ35=FZ35TI*FZ35E*R5(3)*DTIME
            FZ36=FZ36TI*FZ36E*R6(3)*DTIME
      
      FENZI2nd(1)=FZ11+FZ12+FZ13+FZ14+FZ15+FZ16

      FENZI2nd(2)=FZ21+FZ22+FZ23+FZ24+FZ25+FZ26

      FENZI2nd(3)=FZ31+FZ32+FZ33+FZ34+FZ35+FZ36

C     FENZI2nd(k1)= DTIME*(exp(-TIME(1)/Tau1))*R1(K1)/Tau1
C    2             +DTIME*(exp(-TIME(1)/Tau2))*R2(K1)/Tau2
C    3             +DTIME*(exp(-TIME(1)/Tau3))*R3(K1)/Tau3
C    4             +DTIME*(exp(-TIME(1)/Tau4))*R4(K1)/Tau4
C    5             +DTIME*(exp(-TIME(1)/Tau5))*R5(K1)/Tau5
C    6             +DTIME*(exp(-TIME(1)/Tau6))*R6(K1)/Tau6





C     FENMU=1+1/Tau1*Fai1*DTIME+1/Tau2*Fai2*DTIME+...

      FM11TI=1/Tau11
      FM12TI=1/Tau12
      FM13TI=1/Tau13
      FM14TI=1/Tau14
      FM15TI=1/Tau15
      FM16TI=1/Tau16
      
      FM11FAI=FM11TI*Fai11*DTIME
      FM12FAI=FM12TI*Fai12*DTIME
      FM13FAI=FM13TI*Fai13*DTIME
      FM14FAI=FM14TI*Fai14*DTIME
      FM15FAI=FM15TI*Fai15*DTIME
      FM16FAI=FM16TI*Fai16*DTIME

      FM21TI=1/Tau21
      FM22TI=1/Tau22
      FM23TI=1/Tau23
      FM24TI=1/Tau24
      FM25TI=1/Tau25
      FM26TI=1/Tau26
      
      FM21FAI=FM21TI*Fai21*DTIME
      FM22FAI=FM22TI*Fai22*DTIME
      FM23FAI=FM23TI*Fai23*DTIME
      FM24FAI=FM24TI*Fai24*DTIME
      FM25FAI=FM25TI*Fai25*DTIME
      FM26FAI=FM26TI*Fai26*DTIME

      FM31TI=1/Tau31
      FM32TI=1/Tau32
      FM33TI=1/Tau33
      FM34TI=1/Tau34
      FM35TI=1/Tau35
      FM36TI=1/Tau36
      
      FM31FAI=FM31TI*Fai31*DTIME
      FM32FAI=FM32TI*Fai32*DTIME
      FM33FAI=FM33TI*Fai33*DTIME
      FM34FAI=FM34TI*Fai34*DTIME
      FM35FAI=FM35TI*Fai35*DTIME
      FM36FAI=FM36TI*Fai36*DTIME



      FENMU(1)=1.0D0+FM11FAI+FM12FAI+FM13FAI+FM14FAI+FM15FAI+FM16FAI
      FENMU(2)=1.0D0+FM11FAI+FM12FAI+FM13FAI+FM14FAI+FM15FAI+FM16FAI
      FENMU(3)=1.0D0+FM11FAI+FM12FAI+FM13FAI+FM14FAI+FM15FAI+FM16FAI

      FENMUi(1)=1.0D0/FENMU(1)
      FENMUi(2)=1.0D0/FENMU(2)
      FENMUi(3)=1.0D0/FENMU(3)




C     FENMU=1.0D0+DTIME*Fai1/Tau1+DTIME*Fai2/Tau2*+DTIME*Fai3/Tau3
C    2  +DTIME*Fai4/Tau4+DTIME*Fai5/Tau5+DTIME*Fai6/Tau6

C       IF (isnan(FENMU)) THEN
C     PRINT *, "FENMU is NaN"
C       ELSE

C       END IF


C     Calculate elastic strain increment 
C     ELASTRANinc= (DSTRAN-FENZI2nd)/FENMU
C     
     
c     判断DSTRAN并矫正
c     DO K1=1,NTENS
c       IF (DSTRAN(K1) < 0 ) THEN 
c       PRINT *, "DSTRAN(K1) has been resetted", DSTRAN(K1)
c       DSTRAN(K1)=1E-10


c     
c       ELSE
c       DSTRAN(K1)=DSTRAN(K1)
c       END IF
c     END DO
c       
  


      ELASTRANinc(1)=FENMUi(1)*(DSTRAN(1)-FENZI2nd(1))
      ELASTRANinc(2)=FENMUi(2)*(DSTRAN(2)-FENZI2nd(2))
      ELASTRANinc(3)=FENMUi(3)*(DSTRAN(3)-FENZI2nd(3))



C    CALCULATE new STRESS tensor 
C
      DO K1=1,NTENS
        DO K2=1,NTENS
           ESTIFNSchuFENMU(K2,K1)=0.0D0
        END DO
      END DO
      
      ESTIFNSchuFENMU(1,1)=FENMUi(1)*ESTIFNS(1,1)
      ESTIFNSchuFENMU(1,2)=FENMUi(1)*ESTIFNS(1,2)

      ESTIFNSchuFENMU(2,1)=FENMUi(2)*ESTIFNS(2,1)
      ESTIFNSchuFENMU(2,2)=FENMUi(2)*ESTIFNS(2,2)

      ESTIFNSchuFENMU(3,3)=FENMUi(3)*ESTIFNS(3,3)


C     DO K1=1,NTENS
C       DO K2=1,NTENS
C          STRESS(K2)=STRESS(K2)+ESTIFNSchuFENMU(K2,K1)*DSTRAN(K1)
C    1                -ESTIFNSchuFENMU(K2,K1)*FENZI2nd(K1)
C       END DO
C     END DO
      
      STRESS(1)=STRESS(1)+ESTIFNSchuFENMU(1,1)*(DSTRAN(1)-FENZI2nd(1)) 
     1          + ESTIFNSchuFENMU(1,2)*(DSTRAN(2)-FENZI2nd(2)) 

      
      STRESS(2)=STRESS(2)+ESTIFNSchuFENMU(1,2)*(DSTRAN(1)-FENZI2nd(1)) 
     1          + ESTIFNSchuFENMU(2,2)*(DSTRAN(2)-FENZI2nd(2)) 


      STRESS(3)=STRESS(3)+ ESTIFNSchuFENMU(3,3)*(DSTRAN(3)-FENZI2nd(3)) 



      DO K1=1,NTENS
       
        STATEV(K1+10)=STATEV(K1+10)+ FZ1E*Fai1*ELASTRANinc(K1)
        STATEV(K1+20)=STATEV(K1+20)+ FZ2E*Fai1*ELASTRANinc(K1)
        STATEV(K1+30)=STATEV(K1+30)+ FZ3E*Fai1*ELASTRANinc(K1)
        STATEV(K1+40)=STATEV(K1+40)+ FZ4E*Fai1*ELASTRANinc(K1)
        STATEV(K1+50)=STATEV(K1+50)+ FZ5E*Fai1*ELASTRANinc(K1)
        STATEV(K1+60)=STATEV(K1+60)+ FZ6E*Fai1*ELASTRANinc(K1)
 
      END DO
C     creat new Jacobian matrix 
        DO K1=1,NTENS
        DO K2=1,NTENS
           DDSDDE(K2,K1)=0.0D0
        END DO
      END DO


      DDSDDE(1,1)=C11*FENMUi(1)
      DDSDDE(1,2)=C12*FENMUi(1)

      DDSDDE(2,1)=C21*FENMUi(2)
      DDSDDE(2,2)=C22*FENMUi(2)


      DDSDDE(3,3)=C33*FENMUi(3)


c     DDSDDE(1,1)=C11
c     DDSDDE(1,2)=C12
c     DDSDDE(1,3)=C13
c     DDSDDE(2,1)=C21
c     DDSDDE(2,2)=C22
c     DDSDDE(2,3)=C23
c     DDSDDE(3,1)=C31
c     DDSDDE(3,2)=C32
c     DDSDDE(3,3)=C33
c     DDSDDE(4,4)=C44
c     DDSDDE(5,5)=C55
c     DDSDDE(6,6)=C66

      END IF
 

      RETURN 
      END