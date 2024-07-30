      !
      ! Histograms
      !

      subroutine init_hist
      implicit none
      include 'pwhg_math.h'

      call inihists

      ! ATLAS_8_z3d_CC
      call bookupeqbins("ATLAS_8_z3d_CC",
     &                  1.0d0,0.5d0,504.5d0)

      ! ATLAS_8_z3d_CF
      call bookupeqbins("ATLAS_8_z3d_CF",
     &                  1.0d0,0.5d0,150.5d0)

      ! ATLAS_8_z3dFull_CC
      call bookupeqbins("ATLAS_8_z3dFull_CC",
     &                  1.0d0,0.5d0,504.5d0)

      ! ATLAS_8_z3dFull_CF
      call bookupeqbins("ATLAS_8_z3dFull_CF",
     &                  1.0d0,0.5d0,150.5d0)

      end

      !----------!
      ! Analysis !
      !----------!

      subroutine analysis(dsig0)
      implicit none
      include 'hepevt.h'
      include 'pwhg_weights.h'
      include 'nlegborn.h'
      include 'pwhg_rad.h'
      include 'pwhg_rwl.h'
      include 'LesHouches.h'
      include 'pwhg_kn.h'
      logical ini
      data ini/.true./
      save ini
c     something needed
      complex*16 mw,mz,mh,mh2,mw2,mz2
      common/masses/mw,mz,mh,mw2,mz2,mh2
      integer   maxphot,nphot,maxnumg,maxnumlep
      parameter (maxphot=2048,maxnumlep=100)
      real * 8 pg(4,maxphot)
C     allow multiweights
      real * 8 dsig0,dsig(1:weights_max)
c     tell to the analysis file which program is running it
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/
      integer i,j,k
      real * 8 pz(4),pl1(4),pl2(4)
      real * 8 pl103(0:3),pl203(0:3)
      real * 8 y1,eta1,ptl1,m1
      real * 8 y2,eta2,ptl2,m2
      real * 8 pt,m,mtv,yV
      real * 8 dy,deta,delphi,dr
      real * 8 getpt,getdphi,getmass,geteta,gety
      external getpt,getdphi,getmass,geteta,gety
      integer ihep
      real * 8 powheginput,dotp
      external powheginput,dotp
      integer vdecaytemp
      logical accepted
      real*8 cll
      real*8 getdeta,getdr,dphi,dr1,dr2
      real*8 cstar,phistar,phistar_report
      external cstar,phistar_report
      logical pwhg_isfinite
      external pwhg_isfinite
      integer nl1,nl2,ngamma
      integer mu,nu,jl2,il2,igam,jgam,gam,il1,jl1,lep
      integer lep1vec(maxnumlep),lep2vec(maxnumlep),gammavec(maxphot)
      real*8 dsigfmb
      real*8 p_gamma(0:3)
      real*8 pt_lep2,pt_lep2_max
      real*8 pt_lep1,pt_lep_max
      real*8 pt_gamma,pt_gamma_max
      real*8 logptgamma,logptgammamax
      real*8 logptrel,tmpptrel,tmp1ptrel,tmp2ptrel
      integer tmp,ntmp,ngammaup
      real *8 corrfactor
      logical pass_cuts

      integer mllbin,yllbin,cllbin

      ! BINS CMS 13 and 8 TeV
      real*8 cllmllyllbin_CMS
      real*8 mllbins_CMS(13)
      real*8 yllbins_CMS(11)
      real*8 cllbins_CMS_A(6)
      real*8 cllbins_CMS_B(6)
      real*8 cllbins_CMS_C(6)
      real*8 cllbins_CMS_D(6)

      ! BINS CMS 7 TeV
      real*8 mllyllcllbin_CMS7
      real*8 mllbins_CMS7(11)
      real*8 yllbins_CMS7(5)
      real*8 cllbins_CMS7(3)

      ! BINS ATLAS 7 TeV CC
      real*8 mllyllcllbin_ATLAS7_CC
      real*8 mllbins_ATLAS7_CC(21)
      real*8 yllbins_ATLAS7_CC(2)
      real*8 cllbins_ATLAS7_CC(3)

      ! BINS ATLAS 7 TeV CF
      real*8 mllyllcllbin_ATLAS7_CF
      real*8 mllbins_ATLAS7_CF(19)
      real*8 yllbins_ATLAS7_CF(2)
      real*8 cllbins_ATLAS7_CF(3)

      ! BINS ATLAS 8 TeV
      real*8 mllyllcllbin_ATLAS8
      real*8 mllbins_ATLAS8(4)
      real*8 yllbins_ATLAS8(5)
      real*8 cllbins_ATLAS8(3)

      ! BINS ATLAS 8 TeV z3d_CC
      real*8 mllyllcllbin_ATLAS8_z3d_CC
      real*8 mllbins_ATLAS8_z3d_CC(8)
      real*8 yllbins_ATLAS8_z3d_CC(13)
      real*8 cllbins_ATLAS8_z3d_CC(7)

      ! BINS ATLAS 8 TeV z3d_CF
      real*8 mllyllcllbin_ATLAS8_z3d_CF
      real*8 mllbins_ATLAS8_z3d_CF(6)
      real*8 yllbins_ATLAS8_z3d_CF(6)
      real*8 cllbins_ATLAS8_z3d_CF(7)

      ! BINS LHCb 8 TeV
      real*8 mllyllcllbin_LHCb8
      real*8 mllbins_LHCb8(14)
      real*8 yllbins_LHCb8(2)
      real*8 cllbins_LHCb8(3)

      ! BINS LHCb 13 TeV
      real*8 mllyllcllbin_LHCb13
      real*8 mllbins_LHCb13(8)
      real*8 yllbins_LHCb13(8)
      real*8 cllbins_LHCb13(3)

      ! BINS CMS 13 and 8 TeV
      mllbins_CMS(1)=46.0d0
      mllbins_CMS(2)=54.0d0
      mllbins_CMS(3)=66.0d0
      mllbins_CMS(4)=76.0d0
      mllbins_CMS(5)=82.0d0
      mllbins_CMS(6)=86.0d0
      mllbins_CMS(7)=89.5d0
      mllbins_CMS(8)=92.7d0
      mllbins_CMS(9)=96.0d0
      mllbins_CMS(10)=100.0d0
      mllbins_CMS(11)=106.0d0
      mllbins_CMS(12)=116.0d0
      mllbins_CMS(13)=150.0d0

      yllbins_CMS(1)=0.0d0
      yllbins_CMS(2)=0.4d0
      yllbins_CMS(3)=0.8d0
      yllbins_CMS(4)=1.2d0
      yllbins_CMS(5)=1.6d0
      yllbins_CMS(6)=2.0d0
      yllbins_CMS(7)=2.4d0
      yllbins_CMS(8)=2.7d0
      yllbins_CMS(9)=2.9d0
      yllbins_CMS(10)=3.1d0
      yllbins_CMS(11)=3.4d0

      cllbins_CMS_A(1)=-1.0d0
      cllbins_CMS_A(2)=-0.9d0
      cllbins_CMS_A(3)=-0.8d0
      cllbins_CMS_A(4)=-0.7d0
      cllbins_CMS_A(5)=-0.6d0
      cllbins_CMS_A(6)=-0.5d0
      cllbins_CMS_B(1)=-0.5d0
      cllbins_CMS_B(2)=-0.4d0
      cllbins_CMS_B(3)=-0.3d0
      cllbins_CMS_B(4)=-0.2d0
      cllbins_CMS_B(5)=-0.1d0
      cllbins_CMS_B(6)=0.0d0
      cllbins_CMS_C(1)=0.0d0
      cllbins_CMS_C(2)=0.1d0
      cllbins_CMS_C(3)=0.2d0
      cllbins_CMS_C(4)=0.3d0
      cllbins_CMS_C(5)=0.4d0
      cllbins_CMS_C(6)=0.5d0
      cllbins_CMS_D(1)=0.5d0
      cllbins_CMS_D(2)=0.6d0
      cllbins_CMS_D(3)=0.7d0
      cllbins_CMS_D(4)=0.8d0
      cllbins_CMS_D(5)=0.9d0
      cllbins_CMS_D(6)=1.0d0

      ! BINS CMS 7 TeV
      mllbins_CMS7(1)=40.0d0
      mllbins_CMS7(2)=50.0d0
      mllbins_CMS7(3)=60.0d0
      mllbins_CMS7(4)=76.0d0
      mllbins_CMS7(5)=86.0d0
      mllbins_CMS7(6)=96.0d0
      mllbins_CMS7(7)=106.0d0
      mllbins_CMS7(8)=120.0d0
      mllbins_CMS7(9)=150.0d0
      mllbins_CMS7(10)=200.0d0
      mllbins_CMS7(11)=400.0d0

      yllbins_CMS7(1)=0.0d0
      yllbins_CMS7(2)=1.0d0
      yllbins_CMS7(3)=1.25d0
      yllbins_CMS7(4)=1.5d0
      yllbins_CMS7(5)=2.4d0

      cllbins_CMS7(1)=-1.0d0
      cllbins_CMS7(2)=0.0d0
      cllbins_CMS7(3)=1.0d0

      ! BINS ATLAS 7 TeV CC
      mllbins_ATLAS7_CC(1)=66.0d0
      mllbins_ATLAS7_CC(2)=70.0d0
      mllbins_ATLAS7_CC(3)=76.0d0
      mllbins_ATLAS7_CC(4)=80.0d0
      mllbins_ATLAS7_CC(5)=86.0d0
      mllbins_ATLAS7_CC(6)=88.0d0
      mllbins_ATLAS7_CC(7)=89.0d0
      mllbins_ATLAS7_CC(8)=90.0d0
      mllbins_ATLAS7_CC(9)=91.0d0
      mllbins_ATLAS7_CC(10)=92.0d0
      mllbins_ATLAS7_CC(11)=93.0d0
      mllbins_ATLAS7_CC(12)=94.0d0
      mllbins_ATLAS7_CC(13)=95.0d0
      mllbins_ATLAS7_CC(14)=100.0d0
      mllbins_ATLAS7_CC(15)=105.0d0
      mllbins_ATLAS7_CC(16)=110.0d0
      mllbins_ATLAS7_CC(17)=116.0d0
      mllbins_ATLAS7_CC(18)=125.0d0
      mllbins_ATLAS7_CC(19)=250.0d0
      mllbins_ATLAS7_CC(20)=500.0d0
      mllbins_ATLAS7_CC(21)=1000.0d0

      yllbins_ATLAS7_CC(1)=0.0d0
      yllbins_ATLAS7_CC(2)=2.5d0

      cllbins_ATLAS7_CC(1)=-1.0d0
      cllbins_ATLAS7_CC(2)=0.0d0
      cllbins_ATLAS7_CC(3)=1.0d0

      ! BINS ATLAS 7 TeV CF
      mllbins_ATLAS7_CF(1)=66.0d0
      mllbins_ATLAS7_CF(2)=70.0d0
      mllbins_ATLAS7_CF(3)=76.0d0
      mllbins_ATLAS7_CF(4)=80.0d0
      mllbins_ATLAS7_CF(5)=86.0d0
      mllbins_ATLAS7_CF(6)=88.0d0
      mllbins_ATLAS7_CF(7)=89.0d0
      mllbins_ATLAS7_CF(8)=90.0d0
      mllbins_ATLAS7_CF(9)=91.0d0
      mllbins_ATLAS7_CF(10)=92.0d0
      mllbins_ATLAS7_CF(11)=93.0d0
      mllbins_ATLAS7_CF(12)=94.0d0
      mllbins_ATLAS7_CF(13)=95.0d0
      mllbins_ATLAS7_CF(14)=100.0d0
      mllbins_ATLAS7_CF(15)=105.0d0
      mllbins_ATLAS7_CF(16)=110.0d0
      mllbins_ATLAS7_CF(17)=116.0d0
      mllbins_ATLAS7_CF(18)=125.0d0
      mllbins_ATLAS7_CF(19)=250.0d0

      yllbins_ATLAS7_CF(1)=0.0d0
      yllbins_ATLAS7_CF(2)=4.9d0

      cllbins_ATLAS7_CF(1)=-1.0d0
      cllbins_ATLAS7_CF(2)=0.0d0
      cllbins_ATLAS7_CF(3)=1.0d0

      ! BINS ATLAS 8 TeV
      mllbins_ATLAS8(1)=70.0d0
      mllbins_ATLAS8(2)=80.0d0
      mllbins_ATLAS8(3)=100.0d0
      mllbins_ATLAS8(4)=125.0d0

      yllbins_ATLAS8(1)=0.0d0
      yllbins_ATLAS8(2)=0.8d0
      yllbins_ATLAS8(3)=1.6d0
      yllbins_ATLAS8(4)=2.5d0
      yllbins_ATLAS8(5)=3.6d0

      cllbins_ATLAS8(1)=-1.0d0
      cllbins_ATLAS8(2)=0.0d0
      cllbins_ATLAS8(3)=1.0d0

      ! BINS ATLAS 8 TeV z3d_CC
      mllbins_ATLAS8_z3d_CC(1)=46.0d0
      mllbins_ATLAS8_z3d_CC(2)=66.0d0
      mllbins_ATLAS8_z3d_CC(3)=80.0d0
      mllbins_ATLAS8_z3d_CC(4)=91.0d0
      mllbins_ATLAS8_z3d_CC(5)=102.0d0
      mllbins_ATLAS8_z3d_CC(6)=116.0d0
      mllbins_ATLAS8_z3d_CC(7)=150.0d0
      mllbins_ATLAS8_z3d_CC(8)=200.0d0

      yllbins_ATLAS8_z3d_CC(1)=0.0d0
      yllbins_ATLAS8_z3d_CC(2)=0.2d0
      yllbins_ATLAS8_z3d_CC(3)=0.4d0
      yllbins_ATLAS8_z3d_CC(4)=0.6d0
      yllbins_ATLAS8_z3d_CC(5)=0.8d0
      yllbins_ATLAS8_z3d_CC(6)=1.0d0
      yllbins_ATLAS8_z3d_CC(7)=1.2d0
      yllbins_ATLAS8_z3d_CC(8)=1.4d0
      yllbins_ATLAS8_z3d_CC(9)=1.6d0
      yllbins_ATLAS8_z3d_CC(10)=1.8d0
      yllbins_ATLAS8_z3d_CC(11)=2.0d0
      yllbins_ATLAS8_z3d_CC(12)=2.2d0
      yllbins_ATLAS8_z3d_CC(13)=2.4d0

      cllbins_ATLAS8_z3d_CC(1)=-1.0d0
      cllbins_ATLAS8_z3d_CC(2)=-0.7d0
      cllbins_ATLAS8_z3d_CC(3)=-0.4d0
      cllbins_ATLAS8_z3d_CC(4)=0.0d0
      cllbins_ATLAS8_z3d_CC(5)=0.4d0
      cllbins_ATLAS8_z3d_CC(6)=0.7d0
      cllbins_ATLAS8_z3d_CC(7)=1.0d0

      ! BINS ATLAS 8 TeV z3d_CF
      mllbins_ATLAS8_z3d_CF(1)=66.0d0
      mllbins_ATLAS8_z3d_CF(2)=80.0d0
      mllbins_ATLAS8_z3d_CF(3)=91.0d0
      mllbins_ATLAS8_z3d_CF(4)=102.0d0
      mllbins_ATLAS8_z3d_CF(5)=116.0d0
      mllbins_ATLAS8_z3d_CF(6)=150.0d0

      yllbins_ATLAS8_z3d_CF(1)=1.2d0
      yllbins_ATLAS8_z3d_CF(2)=1.6d0
      yllbins_ATLAS8_z3d_CF(3)=2.0d0
      yllbins_ATLAS8_z3d_CF(4)=2.4d0
      yllbins_ATLAS8_z3d_CF(5)=2.8d0
      yllbins_ATLAS8_z3d_CF(6)=3.6d0

      cllbins_ATLAS8_z3d_CF(1)=-1.0d0
      cllbins_ATLAS8_z3d_CF(2)=-0.7d0
      cllbins_ATLAS8_z3d_CF(3)=-0.4d0
      cllbins_ATLAS8_z3d_CF(4)=0.0d0
      cllbins_ATLAS8_z3d_CF(5)=0.4d0
      cllbins_ATLAS8_z3d_CF(6)=0.7d0
      cllbins_ATLAS8_z3d_CF(7)=1.0d0

      ! BINS LHCb 8 TeV
      mllbins_LHCb8(1)=60.0d0
      mllbins_LHCb8(2)=72.0d0
      mllbins_LHCb8(3)=81.0d0
      mllbins_LHCb8(4)=86.0d0
      mllbins_LHCb8(5)=88.d0
      mllbins_LHCb8(6)=89.0d0
      mllbins_LHCb8(7)=90.0d0
      mllbins_LHCb8(8)=91.0d0
      mllbins_LHCb8(9)=92.0d0
      mllbins_LHCb8(10)=93.0d0
      mllbins_LHCb8(11)=94.0d0
      mllbins_LHCb8(12)=98.0d0
      mllbins_LHCb8(13)=120.0d0
      mllbins_LHCb8(14)=160.0d0

      yllbins_LHCb8(1)=-1000.0d0
      yllbins_LHCb8(2)=1000.0d0

      cllbins_LHCb8(1)=-1.0d0
      cllbins_LHCb8(2)=0.0d0
      cllbins_LHCb8(3)=1.0d0

      ! BINS LHCb 13 TeV
      mllbins_LHCb13(1)=50.0d0
      mllbins_LHCb13(2)=66.0d0
      mllbins_LHCb13(3)=76.0d0
      mllbins_LHCb13(4)=86.0d0
      mllbins_LHCb13(5)=96.0d0
      mllbins_LHCb13(6)=106.0d0
      mllbins_LHCb13(7)=116.0d0
      mllbins_LHCb13(8)=150.0d0

      yllbins_LHCb13(1)=2.0d0
      yllbins_LHCb13(2)=2.4d0
      yllbins_LHCb13(3)=2.8d0
      yllbins_LHCb13(4)=3.2d0
      yllbins_LHCb13(5)=3.6d0
      yllbins_LHCb13(6)=4.0d0
      yllbins_LHCb13(7)=4.4d0
      yllbins_LHCb13(8)=4.8d0

      cllbins_LHCb13(1)=-1.0d0
      cllbins_LHCb13(2)=0.0d0
      cllbins_LHCb13(3)=1.0d0

c rwgt stuff START

      if(whcprg.eq.'NLO') then
c$$$         pub(4,1:10)=phep(4,1:10)
         goto 123
      endif



 123  continue

      corrfactor=1d0
      if(powheginput("#btildeviol").eq.1.and.WHCPRG.ne.'NLO') then
         if(rad_type.eq.1) then
            corrfactor=powheginput('corr_btilde')
         elseif(rad_type.eq.2) then
            corrfactor=powheginput('corr_remnant')
         else
            print*, 'no rew'
         endif
      endif
c      print*, corrfactor,rad_type

      if (ini) then
         write(*,*) '*****************************'
         write(*,*) 'THIS FILE'
         if(whcprg.eq.'NLO') then
            write(*,*) '       NLO ANALYSIS'
            weights_num=0
         elseif(WHCPRG.eq.'LHE   ') then
            write(*,*) '       LHE ANALYSIS'
         elseif(WHCPRG.eq.'HERWIG') then
            write (*,*) '           HERWIG ANALYSIS            '
            stop
         elseif(WHCPRG.eq.'PYTHIA') then
            write (*,*) '           PYTHIA ANALYSIS            '
         elseif(WHCPRG.eq.'PY8   ') then
            write (*,*) '           PYTHIA 8 ANALYSIS        '
         endif
         write(*,*) '*****************************'


c         if(weights_num.eq.0) then
c            call setupmulti(1)
c         else
c            call setupmulti(weights_num)
c         endif

         write(*,*) ''
         write(*,*) '*****************************'
         write(*,*) '** weights_num     = ',weights_num
         write(*,*) '** rwl_num_weights = ',rwl_num_weights
         write(*,*) '** rwl_num_groups = ',rwl_num_groups
         write(*,*) '*****************************'
         write(*,*) ''

         if(weights_num.eq.0.and.rwl_num_weights.eq.0) then
            call setupmulti(1)
         else if(weights_num.ne.0.and.rwl_num_weights.eq.0) then
            call setupmulti(weights_num)
         else if(weights_num.eq.0.and.rwl_num_weights.ne.0) then
            call setupmulti(rwl_num_weights)
         else
            call setupmulti(rwl_num_weights)
         endif

         if(weights_num.eq.0.and.rwl_num_weights.gt.weights_max) then
            write(*,*) 'ERROR:'
            write(*,*) 'incoming number of weights (rwl_num_weights)'
            write(*,*) 'is greater than declared dsig and bWdsig    '
            write(*,*) 'array length.'
            stop
         endif

         ini=.false.
      endif

      dsig=0

      if(weights_num.eq.0.and.rwl_num_weights.eq.0) then
         dsig(1)=dsig0
      else if(weights_num.ne.0.and.rwl_num_weights.eq.0) then
         dsig(1:weights_num)=weights_val(1:weights_num)
      else if(weights_num.eq.0.and.rwl_num_weights.ne.0) then
         dsig(1:rwl_num_weights)=rwl_weights(1:rwl_num_weights)
      else
         dsig(1:rwl_num_weights)=rwl_weights(1:rwl_num_weights)
      endif

      if(sum(abs(dsig)).eq.0) return

      dsig=dsig*corrfactor

      do i=1,rwl_num_weights
         if(abs(dsig(i)) > 1d8 .or. dsig(i)+1 .eq. dsig(i)) then
            write(*,*) "LARGE weight. DISCARDING EVENT, i, weight = ",i, dsig(i)
            return
         endif
      enddo



c rwgt stuff END




c      if(dsig.eq.0) return
c      if(.not.pwhg_isfinite(dsig)) then
c         print*,'dsig in analysis not finite: ',dsig
c         return
c      endif

      pz = (/0,0,0,0/)
      pl1= (/0,0,0,0/)
      pl2= (/0,0,0,0/)
      nphot = 0

      vdecaytemp=lprup(1)-10000 ! Z decay product, with positive id

      nl1=0
      nl2=0
      ngamma= 0
      do i=1,maxnumlep
         lep1vec(i) = 0
         lep2vec(i) = 0
      enddo
      maxnumg=maxphot
      do i=1,maxnumg
         gammavec(i) = 0
      enddo

*
      IF(WHCPRG.ne.'PYTHIA') then
         do ihep=1,nhep
c p_Z = p_l1 + p_l2
            if(idhep(ihep).eq.-vdecaytemp) then
C     with a antilepton
               nl2=nl2+1
               lep2vec(nl2)=ihep
            elseif(idhep(ihep).eq.vdecaytemp) then
c     with a lepton
               nl1=nl1+1
               lep1vec(nl1)=ihep
            elseif( idhep(ihep).eq.22 ) then
               ngamma=ngamma+1
               gammavec(ngamma)=ihep
            endif
         enddo
      ELSE
         do ihep=1,nhep
            if(isthep(ihep).eq.1) then
C     Scan over final state particle and record the entries
               if(idhep(ihep).eq.-vdecaytemp) then
C     with an antilepton
                  nl2=nl2+1
                  lep2vec(nl2)=ihep
               elseif(idhep(ihep).eq.vdecaytemp) then
c     with a lepton
                  nl1=nl1+1
                  lep1vec(nl1)=ihep
               elseif(idhep(ihep).eq.22) then
C     with a gamma
                  ntmp= 0
                  tmp= ihep
                  if(jmohep(1,tmp).le.0) then
                     print*,'jmohep(1,tmp)= ',jmohep(1,tmp)
                  endif
c
c this is to avoid photons from meson and baryon  decays
c
                  if(abs(idhep(jmohep(1,tmp))).gt.100) then
                     goto 1221
                  endif
 1220             if(tmp.gt.0) then
                     if(idhep(jmohep(1,tmp)).eq.23.and.
     +                  jmohep(1,tmp).lt.7) then
                        ntmp= ntmp+1
                     elseif(abs(idhep(jmohep(1,tmp))).lt.6) then
                        ntmp= ntmp+1
                     else
                        tmp= jmohep(1,tmp)
                        go to 1220
                     endif
                     if(ntmp.gt.0) then
                        ngamma=ngamma+1
                        gammavec(ngamma)=ihep
                     endif
                  endif
 1221             continue
               endif
            endif
         enddo
      ENDIF

      if(nl1.eq.0.or.nl2.eq.0) then
c            write(*,*)" not enough leptons or gamma! drop event"
c            call exit(1)
         return
      endif

c hardest antilepton
      pt_lep2_max=0d0
      jl2=0
      do nu=1,nl2
         il2=lep2vec(nu)
         pt_lep2=sqrt(phep(1,il2)**2 + phep(2,il2)**2)
         if (pt_lep2.gt.pt_lep2_max) then
            jl2 = il2
            pt_lep2_max = pt_lep2
         endif
      enddo
c hardest lepton
      pt_lep_max=0d0
      jl1=0
      do lep=1,nl1
         il1=lep1vec(lep)
         pt_lep1=sqrt(phep(1,il1)**2 + phep(2,il1)**2)
         if (pt_lep1.gt.pt_lep_max) then
            jl1 = il1
            pt_lep_max = pt_lep1
         endif
      enddo
      pt_gamma_max= 0.d0
      jgam=0
      do gam=1,ngamma
         igam=gammavec(gam)
         pt_gamma=sqrt(phep(1,igam)**2 + phep(2,igam)**2)
         if (pt_gamma.gt.pt_gamma_max) then
            jgam = igam
            pt_gamma_max = pt_gamma
         endif
      enddo
      pl1 = phep(1:4,jl1)
      pl2 = phep(1:4,jl2)
      pz = pl1 + pl2

      pl103(0)=pl1(4)
      pl103(1:3)=pl1(1:3)
      pl203(0)=pl2(4)
      pl203(1:3)=pl2(1:3)

      call getyetaptmass(pl1,y1,eta1,ptl1,m1)
      call getyetaptmass(pl2,y2,eta2,ptl2,m2)
      delphi = getdphi(pl1,pl2)
      pt=getpt(pz)
      m=getmass(pz)
      mtv = sqrt(2*ptl1*ptl2*(1d0-cos(delphi)))
      yV=gety(pz)

      cll = cstar(pl1,pl2)

      phistar = phistar_report(pl2,pl1)   !pl2 is the negatively charged lepton

      ! mllyllcllbin_CMS7
      mllyllcllbin_CMS7=-1
      call getmllyllcllbin(
     & m,
     & abs(yV),
     & cll,
     & mllbins_CMS7,
     & yllbins_CMS7,
     & cllbins_CMS7,
     & size(mllbins_CMS7),
     & size(yllbins_CMS7),
     & size(cllbins_CMS7),
     & mllyllcllbin_CMS7)

      ! mllyllcllbin_ATLAS8_z3d_CC
      mllyllcllbin_ATLAS8_z3d_CC=-1
      call getmllyllcllbin(
     & m,
     & abs(yV),
     & cll,
     & mllbins_ATLAS8_z3d_CC,
     & yllbins_ATLAS8_z3d_CC,
     & cllbins_ATLAS8_z3d_CC,
     & size(mllbins_ATLAS8_z3d_CC),
     & size(yllbins_ATLAS8_z3d_CC),
     & size(cllbins_ATLAS8_z3d_CC),
     & mllyllcllbin_ATLAS8_z3d_CC)

      if (mllyllcllbin_ATLAS8_z3d_CC.ne.-1) then
        call filld(
     & "ATLAS_8_z3dFull_CC",
     & mllyllcllbin_ATLAS8_z3d_CC,
     & dsig)
        if (ptl1.ge.20.0d0.and.
     &      abs(eta1).le.2.40d0.and.
     &      ptl2.ge.20.0d0.and.
     &      abs(eta2).le.2.4d0) then
          call filld(
     &      "ATLAS_8_z3d_CC",
     &      mllyllcllbin_ATLAS8_z3d_CC,
     &      dsig)
        endif
      endif

      ! mllyllcllbin_ATLAS8_z3d_CF
      mllyllcllbin_ATLAS8_z3d_CF=-1
      call getmllyllcllbin(
     & m,
     & abs(yV),
     & cll,
     & mllbins_ATLAS8_z3d_CF,
     & yllbins_ATLAS8_z3d_CF,
     & cllbins_ATLAS8_z3d_CF,
     & size(mllbins_ATLAS8_z3d_CF),
     & size(yllbins_ATLAS8_z3d_CF),
     & size(cllbins_ATLAS8_z3d_CF),
     & mllyllcllbin_ATLAS8_z3d_CF)

      if (mllyllcllbin_ATLAS8_z3d_CF.ne.-1) then
        call filld(
     & "ATLAS_8_z3dFull_CF",
     & mllyllcllbin_ATLAS8_z3d_CF,
     & dsig)
        if (((ptl1.ge.25.0d0.and.
     &        abs(eta1).le.2.40d0.and.
     &        ptl2.ge.20.0d0.and.
     &        abs(eta2).ge.2.5d0.and.
     &        abs(eta2).le.4.9d0).or.
     &       (ptl2.ge.25.0d0.and.
     &        abs(eta2).le.2.4d0.and.
     &        ptl1.ge.20.0d0.and.
     &        abs(eta1).ge.2.5d0.and.
     &        abs(eta1).le.4.9d0)).and.
     &        m.ge.46.0d0) then
          call filld(
     &      "ATLAS_8_z3d_CF",
     &      mllyllcllbin_ATLAS8_z3d_CF,
     &      dsig)
        endif
      endif

      end

! USER DEFINED SUBROUTINES/FUNCTIONS

      subroutine getmllbinyllbincllbin(mll,yll,cll,
     & mllbins,yllbins,cllbins,
     & nmllbins,nyllbins,ncllbins,
     & mllbin,yllbin,cllbin)
      implicit none
      real*8 mll,yll,cll
      integer nmllbins,nyllbins,ncllbins
      real*8 mllbins(nmllbins),yllbins(nyllbins),cllbins(ncllbins)
      integer i,mllbin,yllbin,cllbin

      mllbin=-1
      do i=1,size(mllbins)-1
        if (mll.ge.mllbins(i).and.mll.lt.mllbins(i+1)) then
          mllbin=i
          exit
        endif
      enddo

      yllbin=-1
      do i=1,size(yllbins)-1
        if (yll.ge.yllbins(i).and.yll.lt.yllbins(i+1)) then
          yllbin=i
          exit
        endif
      enddo

      cllbin=-1
      do i=0,size(cllbins)-1
        if (cll.ge.cllbins(i).and.cll.lt.cllbins(i+1)) then
          cllbin=i
          exit
        endif
      enddo
      end

      subroutine getmllyllcllbin(mll,yll,cll,
     & mllbins,yllbins,cllbins,
     & nmllbins,nyllbins,ncllbins,
     & mllyllcllbin)
      implicit none
      real*8 mll,yll,cll
      integer nmllbins,nyllbins,ncllbins
      real*8 mllbins(nmllbins),yllbins(nyllbins),cllbins(ncllbins)
      integer i,mllbin,yllbin,cllbin
      integer yllshift,mllshift
      real*8 mllyllcllbin

      call getmllbinyllbincllbin(mll,yll,cll,
     & mllbins,yllbins,cllbins,
     & nmllbins,nyllbins,ncllbins,
     & mllbin,yllbin,cllbin)

      if (mllbin.eq.-1.or.yllbin.eq.-1.or.cllbin.eq.-1) then
        mllyllcllbin=-1
      else
        ! nbins = vector length - 1
        yllshift=(yllbin-1)*(size(cllbins)-1)
        mllshift=(mllbin-1)*(size(yllbins)-1)*(size(cllbins)-1)
        mllyllcllbin=mllshift+yllshift+cllbin
      endif
      end

      subroutine getcllmllyllbin(mll,yll,cll,
     & mllbins,yllbins,cllbins,
     & nmllbins,nyllbins,ncllbins,
     & cllmllyllbin)
      implicit none
      real*8 mll,yll,cll
      integer nmllbins,nyllbins,ncllbins
      real*8 mllbins(nmllbins),yllbins(nyllbins),cllbins(ncllbins)
      integer i,mllbin,yllbin,cllbin
      integer mllshift,cllshift
      real*8 cllmllyllbin

      call getmllbinyllbincllbin(mll,yll,cll,
     & mllbins,yllbins,cllbins,
     & nmllbins,nyllbins,ncllbins,
     & mllbin,yllbin,cllbin)

      if (mllbin.eq.-1.or.yllbin.eq.-1.or.cllbin.eq.-1) then
        cllmllyllbin=-1
      else
        ! nbins = vector length - 1
        mllshift=(mllbin-1)*(size(yllbins)-1)
        cllshift=(cllbin-1)*(size(mllbins)-1)*(size(yllbins)-1)
        cllmllyllbin=cllshift+mllshift+yllbin
      endif
      end

      subroutine getyetaptmass(p,y,eta,pt,mass)
      implicit none
      real * 8 p(4),y,eta,pt,mass,pv
      real * 8 gety,getpt,geteta,getmass
      external gety,getpt,geteta,getmass
      y  = gety(p)
      pt = getpt(p)
      eta = geteta(p)
      mass = getmass(p)
      end

      function gety(p)
      implicit none
      real * 8 gety,p(4)
      gety=0.5d0*log((p(4)+p(3))/(p(4)-p(3)))
      end

      function getpt(p)
      implicit none
      real * 8 getpt,p(4)
      getpt = sqrt(p(1)**2+p(2)**2)
      end

      function getmass(p)
      implicit none
      real * 8 getmass,p(4)
      getmass=sqrt(abs(p(4)**2-p(3)**2-p(2)**2-p(1)**2))
      end

      function geteta(p)
      implicit none
      real * 8 geteta,p(4),pv
      real * 8 tiny
      parameter (tiny=1.d-5)
      pv = sqrt(p(1)**2+p(2)**2+p(3)**2)
      if(pv.lt.tiny)then
         geteta=sign(1.d0,p(3))*1.d8
      else
         geteta=0.5d0*log((pv+p(3))/(pv-p(3)))
      endif
      end

      subroutine getdydetadphidr(p1,p2,dy,deta,dphi,dr)
      implicit none
      real * 8 p1(*),p2(*),dy,deta,dphi,dr
      real * 8 getdy,getdeta,getdphi,getdr
      external getdy,getdeta,getdphi,getdr
      dy=getdy(p1,p2)
      deta=getdeta(p1,p2)
      dphi=getdphi(p1,p2)
      dr=getdr(deta,dphi)
      end

      function getdy(p1,p2)
      implicit none
      real*8 p1(*),p2(*),getdy
      real*8 y1,y2
      real*8 gety
      external gety
      y1 = gety(p1)
      y2 = gety(p2)
      getdy = y1-y2
      end

      function getdeta(p1,p2)
      implicit none
      real*8 p1(*),p2(*),getdeta
      real*8 eta1,eta2
      real*8 geteta
      external geteta
      eta1 = geteta(p1)
      eta2 = geteta(p2)
      getdeta = eta1-eta2
      end

      function getdphi(p1,p2)
      implicit none
      include 'pwhg_math.h'
      real*8 p1(*),p2(*),getdphi
      real*8 phi1,phi2
      real*8 geteta
      external geteta
      phi1=atan2(p1(2),p1(1))
      phi2=atan2(p2(2),p2(1))
      getdphi=abs(phi1-phi2)
      getdphi=min(getdphi,2d0*pi-getdphi)
      end

      function getdr(deta,dphi)
      implicit none
      real*8 getdr,deta,dphi
      getdr=sqrt(deta**2+dphi**2)
      end

      function islept(j)
      implicit none
      logical islept
      integer j
      if(abs(j).ge.11.and.abs(j).le.15) then
         islept = .true.
      else
         islept = .false.
      endif
      end

      real*8 function cstar(p1,p2)
      implicit none
      real*8 p1(4),p2(4),psum(4)
*
      real*8 dotp
      external dotp
*
      real*8 rq2,cs1p,cs2p,cs1m,cs2m,qmass,pt2,sig
      integer k
*
      psum = p1 + p2
      rq2 = sqrt(2.d0)
! Collins - Soper momenta for particle 1 and 2
      cs1p = (p1(4) + p1(3))/rq2
      cs2p = (p2(4) + p2(3))/rq2
      cs1m = (p1(4) - p1(3))/rq2
      cs2m = (p2(4) - p2(3))/rq2
      qmass = sqrt(psum(4)**2-psum(1)**2-psum(2)**2-psum(3)**2)
      pt2 = psum(1)**2 + psum(2)**2
      cstar = 2.d0/qmass/sqrt(qmass**2 + pt2)*(cs1p*cs2m - cs1m*cs2p)
! for a ppbar should end here
c      if (hadr1.eq.hadr2) then
         sig = 1.d0
         if (psum(3).ne.0.d0) sig = abs(psum(3))/psum(3)
         cstar = cstar * sig
c      endif
      return
      end
*
      real*8 function phistar_report(p2,p1)
      implicit none
      include 'pwhg_math.h'
      real*8 p1(4),p2(4)
*
      real*8 phim,phip,dphi,dphio2,etam,etap,detao2
      real*8 cthetastar,sthetastar
      real*8 geteta
      external geteta
*
      phim= atan2(p2(2),p2(1))
      phip= atan2(p1(2),p1(1))
      dphi= phim-phip
      dphio2= (pi - dphi)/2.d0
      etam= geteta(p2)
      etap= geteta(p1)
      detao2= (etam-etap)/2.d0
      cthetastar= tanh(detao2)
      sthetastar= sqrt(1.d0-cthetastar**2)
      phistar_report= tan(dphio2) * sthetastar

      return
      end

      subroutine calCSVariables(p1,p2,res,swap)
      implicit none
      include '../include/LesHouches.h'
      include 'nlegborn.h'
      include '../include/pwhg_kn.h'
      real *8 p1(0:3),p2(0:3),res(0:3)
      logical swap
      real *8 Pbeam(0:3),Ptarget(0:3),Q(0:3)
      real *8 p1plus,p1minus,p2plus,p2minus,costheta
      integer nu
      real *8 Qmag,Qpt
      real *8 D(0:3),dt_qt,sin2theta,Dpt
      real *8 R(3),Rmag,Runit(3),Qt(3),Qtunit(3),Dt(3),tanphi,phi
      real *8 dotp,dotp3
      external dotp,dotp3

      do nu=0,3
         Pbeam(nu)=0
         Ptarget(nu)=0
         Q(nu)=p1(nu)+p2(nu)
      enddo
      Pbeam(0)=ebmup(1)
      Ptarget(0)=ebmup(2)
      Pbeam(3)=ebmup(1)
      Ptarget(3)=-ebmup(2)

      Qmag=sqrt(dotp(Q,Q))
      Qpt=sqrt(Q(1)**2+Q(2)**2)
c*********************************************************************
c*
c* 1) cos(theta) = 2 Q^-1 (Q^2+Qt^2)^-1/2 (p1^+ p2^- - p1^- p2^+)
c*
c*
c*********************************************************************

      p1plus=1d0/sqrt(2d0) * (p1(0) + p1(3))
      p1minus = 1d0/sqrt(2d0) * (p1(0) - p1(3))
      p2plus=1d0/sqrt(2d0) * (p2(0) + p2(3))
      p2minus = 1d0/sqrt(2d0) * (p2(0) - p2(3))

      costheta = 2d0 / Qmag / sqrt(Qmag**2 +
     $     Qpt**2) * (p1plus * p2minus - p1minus * p2plus)

      if (swap) costheta = -costheta

c********************************************************************
c*
c* 2) sin2(theta) = Q^-2 Dt^2 - Q^-2 (Q^2 + Qt^2)^-1 * (Dt dot Qt)^2
c*
c********************************************************************
      do nu=0,3
         D(nu)=p1(nu)-p2(nu)
      enddo
      Dpt=sqrt(D(1)**2+D(2)**2)
      dt_qt = D(1)*Q(1) + D(2)*Q(2)
      sin2theta=(DPt/QMag)**2 -1d0/QMag**2/(QMag**2 + QPt**2)*dt_qt**2

c      if (abs(sin2theta+(costheta*costheta)-1d0).gt.1d-6) then
c         write (*,*) "HAHA",abs(sin2theta+(costheta*costheta)-1d0),Qpt
c         stop
c      endif

c********************************************************************
c*
c* 3) tanphi = (Q^2 + Qt^2)^1/2 / Q (Dt dot R unit) /(Dt dot Qt unit)
c*
c*********************************************************************
c// unit vector on R direction

      if(Qpt.gt.0d0) then
         call cross3(pbeam(1),Q(1),R)
         Rmag=sqrt(dotp3(R,R))

         Runit(1)=R(1)/Rmag
         Runit(2)=R(2)/Rmag
         Runit(3)=R(3)/Rmag

         Qt(1)=Q(1)
         Qt(2)=Q(2)
         Qt(3)=0

         Qtunit(1)=Qt(1)/Qpt
         Qtunit(2)=Qt(2)/Qpt
         Qtunit(3)=0


         Dt(1)=D(1)
         Dt(2)=D(2)
         Dt(3)=0


         tanphi=sqrt( Qmag**2 + Qpt**2) / Qmag * dotp3(Dt,Runit)/
     $        dotp3(Dt,Qtunit)

         if (swap) tanphi = -tanphi

         phi=atan2(sqrt(Qmag**2 + Qpt**2 )* dotp3(Dt,Runit),QMag
     $        *dotp3(Dt,Qtunit))


         if (swap) phi = atan2(-sqrt(QMag**2+ QPt**2)*dotp3(Dt,Runit)
     $        ,QMag*dotp3(Dt,Qtunit))

      else
         tanphi=0
         phi=0
      endif

      res(0)=costheta
      res(1)=sin2theta
      res(2)=tanphi
      res(3)=phi
      end

      function dotp3(p1,p2)
      implicit none
      real * 8 dotp3,p1(3),p2(3)
      dotp3 = p1(1)*p2(1) + p1(2)*p2(2) + p1(3)*p2(3)
      end

      subroutine cross3(p1,p2,p3)
      implicit none
      real * 8 p3(3),p1(3),p2(3)
      p3(1) = p1(2)*p2(3)-p1(3)*p2(2)
      p3(2) = p1(3)*p2(1)-p1(1)*p2(3)
      p3(3) = p1(1)*p2(2)-p1(2)*p2(1)
      end
