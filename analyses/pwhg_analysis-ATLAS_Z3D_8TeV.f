      subroutine init_hist
      implicit none
      include 'pwhg_math.h'

      call inihists
      call bookupeqbins('total',1d0,-0.5d0,0.5d0)
      call bookupeqbins('z3d_CC',1d0,0d0,504d0)
      call bookupeqbins('z3d_mass_CC',1d0,0d0,7d0)
      call bookupeqbins('z3d_CF',1d0,0d0,150d0)
      call bookupeqbins('z3dFull_CC',1d0,0d0,504d0)
      call bookupeqbins('z3dFull_CF',1d0,0d0,150d0)

      end
     
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
      real * 8 dsig0
      real * 8, allocatable, save :: dsig(:)
c     tell to the analysis file which program is running it
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/
      integer i,j,k
      real * 8 pz(4),pl1(4),pl2(4)
      real * 8 pl103(0:3),pl203(0:3)
      real * 8 y1,eta1,ptl1,m1
      real * 8 y2,eta2,ptl2,m2
      real * 8 pt,m,y,mtv
      real * 8 pV(4),yV,etaV,ptV,mV
      real * 8 dy,deta,delphi,dr
      real * 8 getpt,getdphi,getmass,geteta
      external getpt,getdphi,getmass,geteta
      integer ihep
      real * 8 powheginput,dotp
      external powheginput,dotp
      integer vdecaytemp
      logical accepted
      real*8 cs
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
c z3d variables
c      integer mbin,ybin,csbin
      real*8 mbin,ybin,csbin
      real*8 mbin_cf,ybin_cf 
      real*8 z3dbin_cc,z3dbin_cf
c spin correlation observables
      real * 8 aspincor(0:7),lcos,genphi
      real * 8 corrfactor,ub_btilde_corr,ub_remn_corr,ub_corr

c rwgt stuff START
      ub_btilde_corr = powheginput('#ub_btilde_corr')
      if (ub_btilde_corr < 0d0) then
         ub_btilde_corr = 1d0
      endif
      ub_remn_corr = powheginput('#ub_remn_corr')
      if (ub_remn_corr < 0d0) then
         ub_remn_corr = 1d0
      endif
      
      corrfactor=1d0
      if(WHCPRG.ne.'NLO') then
c     I use an "OR" between rad_type and rwl_type, although rwl_type
c     should be enough, as it should always be present on a LHE file
c     with multiple weights, whereas rad_type is there only if flg_debug
c     was used.
         if(rad_type.eq.1.or.rwl_type.eq.1) then
            corrfactor=ub_btilde_corr
         elseif(rad_type.eq.2.or.rwl_type.eq.2) then
            corrfactor=ub_remn_corr
         else
            write(*,*) 'pwhg_analysis: rwl_type not found: ',rad_type
     $           ,rwl_type
            stop
         endif
      endif
c      print*, corrfactor
      
      if (ini) then
         write(*,*) '*****************************'
         write(*,*) ' STARTING POWHEG CMS PARTON LEVEL ANALYSIS '
         write(*,*) '*****************************'
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
            
c         if(weights_num.eq.0.and.rwl_num_weights.gt.weights_max) then
c            write(*,*) 'ERROR:'
c            write(*,*) 'incoming number of weights (rwl_num_weights)'
c            write(*,*) 'is greater than declared dsig and bWdsig    '
c            write(*,*) 'array length.'
c            stop
c         endif
         if(.not. allocated(dsig)) then
            allocate(dsig(max(10,rwl_num_weights+1)))
         endif
      
         ini=.false.
      endif
      
      dsig=0
      
      if(weights_num.eq.0.and.rwl_num_weights.eq.0) then
         dsig(1)=dsig0
         write(*,*) "dsig = ", dsig(1)                                                                                                         

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
        if(abs(dsig(i)) > 1d6 .or. dsig(i)+1 .eq. dsig(i)) then
          write(*,*) "LARGE weight. DISCARDING EVENT, i, weight = ",i, dsig(i)
          return
        endif
      enddo
c rwgt stuff END

      pz = (/0,0,0,0/)
      pl1= (/0,0,0,0/)
      pl2= (/0,0,0,0/)
      nphot = 0

      vdecaytemp= 13 !lprup(1)-10000 ! Z decay product, with positive id
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
c     p_Z = p_l1 + p_l2
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
            write(*,*)" not enough leptons or gamma! drop event"
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

      do mu=1,4
         pV(mu)=phep(mu,jl1)+phep(mu,jl2)
      enddo



      pl103(0)=pl1(4)
      pl103(1:3)=pl1(1:3)
      pl203(0)=pl2(4)
      pl203(1:3)=pl2(1:3)

      call getyetaptmass(pl1,y1,eta1,ptl1,m1)
      call getyetaptmass(pl2,y2,eta2,ptl2,m2)
      delphi = getdphi(pl1,pl2)

      call getyetaptmassV(pV,yV,etaV,ptV,mV)
      pt=getpt(pz)
      m=getmass(pz)
      y=yV
      yV=dabs(yV)
c      y=0.5*log((pz(4)+pz(3))/(pz(4)-pz(3)))
cy=0.5d0*log((p(0)+p(3))/(p(0)-p(3)))
c      y=dabs(y)
c      y=gety(pz)
      print*,'y=',y, '   yV=',yV, '   m=', m 
      mtv = sqrt(2*ptl1*ptl2*(1d0-cos(delphi)))

      cs = cstar(pl1,pl2)

      phistar = phistar_report(pl2,pl1)   !pl2 is the negatively charged lepton
      call filld('total',0d0,dsig)



      if ((cs.gt.-1.0d0 ).and.(cs.lt.-0.7d0))  then
         csbin=0
      else if ((cs.gt.-0.7d0 ).and.(cs.lt.-0.4d0))  then
         csbin=1
      else if ((cs.gt.-0.4d0 ).and.(cs.lt.0.0d0))  then
         csbin=2
      else if ((cs.gt.0.0d0 ).and.(cs.lt.0.4d0)) then
         csbin=3
      else if ((cs.gt.0.4d0 ).and.(cs.lt.0.7d0)) then
         csbin=4
      else if ((cs.gt.0.7d0 ).and.(cs.lt.1.0d0)) then
         csbin=5
      endif

      
      mbin = 999
      if ((m.gt.46d0 ).and.(m.lt.66d0)  ) then 
         mbin=0
      else if ((m.gt.66d0 ).and.(m.lt.80d0)  ) then
         mbin=1
      else if ((m.gt.80d0 ).and.(m.lt.91d0)  ) then
         mbin=2
      else if ((m.gt.91d0 ).and.(m.lt.102d0) ) then
         mbin=3
      else if ((m.gt.102d0 ).and.(m.lt.116d0))  then
         mbin=4
      else if ((m.gt.116d0 ).and.(m.lt.150d0))  then
         mbin=5
      else if ((m.gt.150d0 ).and.(m.lt.200d0))  then 
         mbin=6
      endif
      
      ybin=999
      if ((yV.gt.0.0d0 ).and.(yV.lt.0.2d0)) then
         ybin=0
      else if ((yV.gt.0.2d0 ).and.(yV.lt.0.4d0)) then
         ybin=1
      else if ((yV.gt.0.4d0 ).and.(yV.lt.0.6d0)) then
         ybin=2
      else if ((yV.gt.0.6d0 ).and.(yV.lt.0.8d0)) then
         ybin=3
      else if ((yV.gt.0.8d0 ).and.(yV.lt.1.0d0)) then
         ybin=4
      else if ((yV.gt.1.0d0 ).and.(yV.lt.1.2d0)) then
         ybin=5
      else if ((yV.gt.1.2d0 ).and.(yV.lt.1.4d0)) then
         ybin=6
      else if ((yV.gt.1.4d0 ).and.(yV.lt.1.6d0)) then
         ybin=7
      else if ((yV.gt.1.6d0 ).and.(yV.lt.1.8d0)) then
         ybin=8
      else if ((yV.gt.1.8d0 ).and.(yV.lt.2.0d0)) then
         ybin=9
      else if ((yV.gt.2.0d0 ).and.(yV.lt.2.2d0)) then
         ybin=10
      else if ((yV.gt.2.2d0 ).and.(yV.lt.2.4d0)) then
         ybin=11
      endif

      z3dbin_cc= 72*mbin +6*ybin + csbin




      ybin_cf=999
      if ((yV.gt.1.2d0 ).and.(yV.lt.1.6d0)) then
         ybin_cf=0
      else if ((yV.gt.1.6d0 ).and.(yV.lt.2.0d0)) then
         ybin_cf=1
      else if ((yV.gt.2.0d0 ).and.(yV.lt.2.4d0)) then
         ybin_cf=2
      else if ((yV.gt.2.4d0 ).and.(yV.lt.2.8d0)) then
         ybin_cf=3
      else if ((yV.gt.2.8d0 ).and.(yV.lt.3.6d0)) then
         ybin_cf=4
      endif

      mbin_cf = 999
      if ((m.gt.66d0 ).and.(m.lt.80d0)  ) then
         mbin_cf=0
      else if ((m.gt.80d0 ).and.(m.lt.91d0)  ) then
         mbin_cf=1
      else if ((m.gt.91d0 ).and.(m.lt.102d0) ) then
         mbin_cf=2
      else if ((m.gt.102d0 ).and.(m.lt.116d0))  then
         mbin_cf=3
      else if ((m.gt.116d0 ).and.(m.lt.150d0))  then
         mbin_cf=4
      endif

      z3dbin_cf= 30*mbin_cf +6*ybin_cf + csbin

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c distributions without cuts
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      call filld('z3dFull_CC',z3dbin_cc,dsig)
      call filld('z3dFull_CF',z3dbin_cf,dsig)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c distributions with cuts
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      if( ((getpt(pl1).gt.25.and.abs(geteta(pl1))<2.4d0.and.
     1     getpt(pl2).gt.20.and.abs(geteta(pl2))>2.5d0.and.
     2     abs(geteta(pl2))<4.9d0) .or. 
     3     (getpt(pl2).gt.25.and.abs(geteta(pl2))<2.4d0.and.
     4     getpt(pl1).gt.20.and.abs(geteta(pl1))>2.5d0.and.
     5     abs(geteta(pl1))<4.9d0)) .and.
     6     m.gt.46.0d0) then
         call filld('z3d_CF',z3dbin_cf,dsig)
      endif

      if(getpt(pl1).gt.20.and.abs(geteta(pl1))<2.4d0.and.
     1   getpt(pl2).gt.20.and.abs(geteta(pl2))<2.4d0) then
         call filld('z3d_CC',z3dbin_cc,dsig)
         call filld('z3d_mass_CC',mbin,dsig)
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


      subroutine getyetaptmassV(p,y,eta,pt,mass)
      implicit none
      real * 8 p(4),y,eta,pt,mass,pv
      real *8 tiny
      parameter (tiny=1.d-5)
      y=0.5d0*log((p(4)+p(3))/(p(4)-p(3)))
      pt=sqrt(p(1)**2+p(2)**2)
      pv=sqrt(pt**2+p(3)**2)
      if(pt.lt.tiny)then
         eta=sign(1.d0,p(3))*1.d8
      else
         eta=0.5d0*log((pv+p(3))/(pv-p(3)))
      endif
      mass=sqrt(abs(p(4)**2-pv**2))
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
