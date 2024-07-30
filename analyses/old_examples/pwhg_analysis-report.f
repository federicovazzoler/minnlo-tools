c  The next subroutines, open some histograms and prepare them 
c      to receive data 
c  You can substitute these  with your favourite ones
c  init   :  opens the histograms
c  topout :  closes them
c  pwhgfill  :  fills the histograms with data

      subroutine init_hist
      implicit none
      include  'LesHouches.h'
      include 'pwhg_math.h'
      integer j,k
      character * 1 cnum(9)
      data cnum/'1','2','3','4','5','6','7','8','9'/

      call inihists

      call bookupeqbins('total',1d0,-0.5d0,0.5d0)
      call bookupeqbins('total_F-B_nocut',1d0,-0.5d0,0.5d0)
      call bookupeqbins('totalcut',1d0,-0.5d0,0.5d0)
      call bookupeqbins('totalcut_eta2.5only',1d0,-0.5d0,0.5d0)
      call bookupeqbins('totalcut_F-B_eta2.5only',1d0,-0.5d0,0.5d0)

c      call bookupeqbins('Nphot',1d0,-0.5d0,5.5d0)
cccccccccccccccccccccc
c nocut distributions
cccccccccccccccccccccc
      call bookupeqbins('V_mt_nocut',0.5d0,50d0,100d0)     !for W
      call bookupeqbins('V_pt1_report_nocut',0.25d0,0d0,25d0)
      call bookupeqbins('V_pt2_report_nocut',1.d0,0d0,300d0)
c      call bookupeqbins('V_pt2',1d0,0d0,350d0)
      call bookupeqbins('V_m_nocut',10.d0,60d0,120d0)
      call bookupeqbins('V_m_report_nocut',1d0,50d0,200d0)
      call bookupeqbins('forward_nocut',1.d0,50d0,200d0)
      call bookupeqbins('backward_nocut',1.d0,50d0,200d0)
      call bookupeqbins('F-B_nocut',1.d0,50d0,200d0)
      call bookupeqbins('X_m_report_nocut',0.006d0,0.6d0,1.2d0)
      call bookupeqbins('X_p_report_nocut',0.008d0,0.4d0,1.2d0)
      call bookupeqbins('l1_y_nocut',0.2d0,-4d0,4d0)
      call bookupeqbins('l1_eta_nocut',0.2d0,-2.5d0,2.5d0)
      call bookupeqbins('l1_pt_nocut',1.4d0,25d0,60d0)
      call bookupeqbins('l1_pt_report_nocut',0.25d0,25d0,65d0)
      call bookupeqbins('l2_y_nocut',0.2d0,-4d0,4d0)
      call bookupeqbins('l2_eta_nocut',0.2d0,-2.5d0,2.5d0)
      call bookupeqbins('l2_pt_nocut',1.4d0,25d0,60d0)
      call bookupeqbins('l2_pt_report_nocut',0.25d0,25d0,65d0)
      call bookupeqbins('delta_phi_nocut',0.1d0,0d0,3.2d0)
      call bookupeqbins('dr1_nocut',0.1d0,0d0,3.2d0)
      call bookupeqbins('dr2_nocut',0.1d0,0d0,3.2d0)
      call bookupeqbins('phistar_report_nocut',0.01d0,0d0,0.4d0)
      call bookupeqbins('ptgamma_nocut',0.2d0,-7d0,3d0)
      call bookupeqbins('ptgammamax_nocut',0.2d0,-7d0,3d0)
      call bookupeqbins('ptrel_gamma-lep_nocut',0.2d0,-7d0,3d0)

cccccccccccccccccccccc
c cut distributions
cccccccccccccccccccccc
      call bookupeqbins('V_mt',0.5d0,50d0,100d0)     !for W
      call bookupeqbins('V_pt1_report',0.25d0,0d0,25d0)
      call bookupeqbins('V_pt2_report',1.d0,0d0,300d0)
c      call bookupeqbins('V_pt2',1d0,0d0,350d0)

      call bookupeqbins('V_m',1.d0,50d0,200d0)
      call bookupeqbins('V_m_report',1d0,50d0,200d0)

      call bookupeqbins('forward',1.d0,50d0,200d0)
      call bookupeqbins('backward',1.d0,50d0,200d0)
      call bookupeqbins('F-B',1.d0,50d0,200d0)

      call bookupeqbins('X_m_report',0.006d0,0.6d0,1.2d0)
      call bookupeqbins('X_p_report',0.008d0,0.4d0,1.2d0)

      call bookupeqbins('l1_y',0.2d0,-4d0,4d0)
      call bookupeqbins('l1_eta',0.2d0,-2.5d0,2.5d0)
      call bookupeqbins('l1_pt',1.4d0,25d0,60d0)
      call bookupeqbins('l1_pt_report',0.25d0,25d0,65d0)

      call bookupeqbins('l2_y',0.2d0,-4d0,4d0)
      call bookupeqbins('l2_eta',0.2d0,-2.5d0,2.5d0)
      call bookupeqbins('l2_pt',1.4d0,25d0,60d0)
      call bookupeqbins('l2_pt_report',0.25d0,25d0,65d0)


      call bookupeqbins('delta_phi',0.1d0,0d0,3.2d0)
      call bookupeqbins('dr1',0.1d0,0d0,3.2d0)
      call bookupeqbins('dr2',0.1d0,0d0,3.2d0)
      call bookupeqbins('phistar_report',0.01d0,0d0,0.4d0)

      call bookupeqbins('ptgamma_cut',0.2d0,-7d0,3d0)
      call bookupeqbins('ptgammamax_cut',0.2d0,-7d0,3d0)
      call bookupeqbins('ptrel_gamma-lep_cut',0.2d0,-7d0,3d0)

c end of cut distributions

      call bookupeqbins('nophot',1d0,-0.5d0,0.5d0)

      call bookupeqbins('lcos5', 0.01d0,-1d0,1d0)
      call bookupeqbins('lcos10',0.01d0,-1d0,1d0)
      call bookupeqbins('lcos20',0.01d0,-1d0,1d0)
      call bookupeqbins('lcos40',0.01d0,-1d0,1d0)
      call bookupeqbins('lcosg40',0.01d0,-1d0,1d0)

      call bookupeqbins('genphi5',0.02d0,-3.15d0,3.15d0)
      call bookupeqbins('genphi10',0.02d0,-3.15d0,3.15d0)
      call bookupeqbins('genphi20',0.02d0,-3.15d0,3.15d0)
      call bookupeqbins('genphi40',0.02d0,-3.15d0,3.15d0)
      call bookupeqbins('genphig40',0.02d0,-3.15d0,3.15d0)

      call bookupeqbins('a0',1d0,0d0,100d0)
      call bookupeqbins('a1',1d0,0d0,100d0)
      call bookupeqbins('a2',1d0,0d0,100d0)
      call bookupeqbins('a3',1d0,0d0,100d0)
      call bookupeqbins('a4',1d0,0d0,100d0)
      call bookupeqbins('a5',1d0,0d0,100d0)
      call bookupeqbins('a6',1d0,0d0,100d0)
      call bookupeqbins('a7',1d0,0d0,100d0)

      end
     
      subroutine analysis(dsig)
      implicit none
      real * 8 dsig
      include 'hepevt.h'
      include 'pwhg_math.h' 
      include  'LesHouches.h'
      complex*16 mw,mz,mh,mh2,mw2,mz2
      common/masses/mw,mz,mh,mw2,mz2,mh2
      integer   maxphot,nphot,maxnumg,maxnumlep
      parameter (maxphot=2048,maxnumlep=100)
      real * 8 pg(4,maxphot)
      character * 1 cnum(9)
      data cnum/'1','2','3','4','5','6','7','8','9'/
      save cnum
      integer i,j,k
c     we need to tell to this analysis file which program is running it
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
c      data WHCPRG/'PYTHIA'/
      data WHCPRG/'NLO   '/
c      data WHCPRG/'PHOTOS'/
      real * 8 pz(4),pl1(4),pl2(4)
      real * 8 pl103(0:3),pl203(0:3)
      real * 8 y1,eta1,ptl1,m1
      real * 8 y2,eta2,ptl2,m2
      real * 8 pt,m,mtv
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
c spin correlation observables
      real * 8 aspincor(0:7),lcos,genphi
      logical ini
      data ini/.true./
      save ini

      if(dsig.eq.0) return
      if(.not.pwhg_isfinite(dsig)) then 
         print*,'dsig in analysis not finite: ',dsig
         return
      endif

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
      call get_ang_coeffs(pl103,pl203,dsig)



      call getyetaptmass(pl1,y1,eta1,ptl1,m1)
      call getyetaptmass(pl2,y2,eta2,ptl2,m2)
      delphi = getdphi(pl1,pl2)
      pt=getpt(pz)
      m=getmass(pz)
      mtv = sqrt(2*ptl1*ptl2*(1d0-cos(delphi)))

      cs = cstar(pl1,pl2)

      phistar = phistar_report(pl2,pl1)   !pl2 is the negatively charged lepton

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c distributions without cuts
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      call filld('total',0d0,dsig)

c lepton 1
      call filld('l1_y_nocut',    y1, dsig)
      call filld('l1_eta_nocut',eta1, dsig)
      call filld('l1_pt_nocut', ptl1, dsig)
      call filld('l1_pt_report_nocut', ptl1, dsig)
c lepton 2
      call filld('l2_y_nocut',    y2, dsig)
      call filld('l2_eta_nocut',eta2, dsig)
      call filld('l2_pt_nocut', ptl2, dsig)
      call filld('l2_pt_report_nocut', ptl2, dsig)

c azimuthal separation betwen lepton and neutrino
      call filld('delta_phi_nocut',delphi,dsig)
      call filld('dr1_nocut',dr1,dsig)
      call filld('dr2_nocut',dr2,dsig)

c W
c      call filld('V_pt_zoom',pt, dsig)
      call filld('V_pt1_report_nocut',pt, dsig)
      call filld('V_pt2_report_nocut',pt, dsig)
      call filld('V_m_nocut',  m, dsig)
      call filld('V_m_report_nocut',  m, dsig)
c transverse mass of the lep1-lep2 system
      call filld('V_mt_nocut',mtv,dsig)

      call filld('X_m_report_nocut',mtv/mz,dsig)
      call filld('X_p_report_nocut',ptl1*2d0/mz,dsig)

      if (cs.lt.0d0) then
         call filld('backward_nocut', m, dsig)
         dsigfmb= -dsig
      else
         call filld('forward_nocut', m, dsig)
         dsigfmb= dsig
      endif
      call filld('F-B_nocut', m, dsigfmb)
      call filld('phistar_report_nocut',phistar,dsig)
      call filld('total_F-B_nocut',0d0,dsigfmb)

      if(ngamma.gt.0) then
         do gam=1,ngamma
            igam=gammavec(gam)
            pt_gamma=sqrt(phep(1,igam)**2 + phep(2,igam)**2)
            logptgamma= log(pt_gamma)/log(10.d0)
            call filld('ptgamma_nocut',logptgamma,dsig)
c     FSR ptrel (minimum pt between photon and leptpons)
            p_gamma(1:3)=phep(1:3,igam)
            p_gamma(0)=phep(4,igam)
            call get_ptrelFSR(p_gamma,pl103,tmp1ptrel)
            call get_ptrelFSR(p_gamma,pl203,tmp2ptrel)
            tmpptrel= min(tmp1ptrel,tmp2ptrel)
            logptrel= log(tmpptrel)/log(10.d0)
            call filld('ptrel_gamma-lep_nocut',logptrel,dsig)
         enddo
         logptgammamax= log(pt_gamma_max)/log(10.d0)
         call filld('ptgammamax_nocut',logptgammamax,dsig)
      endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c distributions without cuts
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      if(abs(geteta(pl1))<2.5d0.and.
     1   abs(geteta(pl2))<2.5d0.and.
     2   m.gt.80.d0.and.m.lt.100.d0) then
         call filld('totalcut_eta2.5only',0d0,dsig)
         call filld('totalcut_F-B_eta2.5only',0d0,dsigfmb)
      endif
      if(getpt(pl1).gt.25.and.abs(geteta(pl1))<2.4d0.and.
     1   getpt(pl2).gt.25.and.abs(geteta(pl2))<2.4d0.and.
     2   m.gt.50) then
         call filld('totalcut',0d0,dsig)
c lepton 1
         call filld('l1_y',    y1, dsig)
         call filld('l1_eta',eta1, dsig)
         call filld('l1_pt', ptl1, dsig)
         call filld('l1_pt_report', ptl1, dsig)
c lepton 2
         call filld('l2_y',    y2, dsig)
         call filld('l2_eta',eta2, dsig)
         call filld('l2_pt', ptl2, dsig)
         call filld('l2_pt_report', ptl2, dsig)

c azimuthal separation betwen lepton and neutrino
         call filld('delta_phi',delphi,dsig)
         call filld('dr1',dr1,dsig)
         call filld('dr2',dr2,dsig)

c W
c      call filld('V_pt_zoom',pt, dsig)
         call filld('V_pt1_report',pt, dsig)
         call filld('V_pt2_report',pt, dsig)
         call filld('V_m',  m, dsig)
         call filld('V_m_report',  m, dsig)
c transverse mass of the lep1-lep2 system
         call filld('V_mt',mtv,dsig)

         call filld('X_m_report',mtv/mz,dsig)
         call filld('X_p_report',ptl1*2d0/mz,dsig)

         if (cs.lt.0d0) then
            call filld('backward', m, dsig)
            dsigfmb= -dsig
         else
            call filld('forward', m, dsig)
            dsigfmb= dsig
         endif
         call filld('F-B', m, dsigfmb)
         call filld('phistar_report',phistar,dsig)
         if(ngamma.gt.0) then
            do gam=1,ngamma
               igam=gammavec(gam)
               pt_gamma=sqrt(phep(1,igam)**2 + phep(2,igam)**2)
               logptgamma= log(pt_gamma)/log(10.d0)
               call filld('ptgamma_cut',logptgamma,dsig)
c     FSR ptrel (minimum pt between photon and leptons)
               p_gamma(1:3)=phep(1:3,igam)
               p_gamma(0)=phep(4,igam)
               call get_ptrelFSR(p_gamma,pl103,tmp1ptrel)
               call get_ptrelFSR(p_gamma,pl203,tmp2ptrel)
               tmpptrel= min(tmp1ptrel,tmp2ptrel)
               logptrel= log(tmpptrel)/log(10.d0)
               call filld('ptrel_gamma-lep_cut',logptrel,dsig)
            enddo
            logptgammamax= log(pt_gamma_max)/log(10.d0)
            call filld('ptgammamax_cut',logptgammamax,dsig)
         endif
      endif


      end

      subroutine yetaptmassplot(p,dsig,prefix)
      implicit none
      real * 8 p(4),dsig
      character *(*) prefix
      real * 8 y,eta,pt,m
      call getyetaptmass(p,y,eta,pt,m)
      call filld(prefix//'_y',y,dsig)
      call filld(prefix//'_eta',eta,dsig)
      call filld(prefix//'_pt',pt,dsig)
      call filld(prefix//'_m',m,dsig)
      end

      subroutine deltaplot(p1,p2,dsig,prefix)
      implicit none
      real * 8 p1(4),p2(4),dsig
      character *(*) prefix
      real * 8 dy,deta,delphi,dr
      call getdydetadphidr(p1,p2,dy,deta,delphi,dr)
      call filld(prefix//'_dy',dy,dsig)
      call filld(prefix//'_deta',deta,dsig)
      call filld(prefix//'_delphi',delphi,dsig)
      call filld(prefix//'_dr',dr,dsig)
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

*
**
*
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

      subroutine get_ang_coeffs(p1,p2,dsig)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_kn.h'
      real * 8 dsig
      real *8 p1(0:3),p2(0:3),res(0:3)
      real *8 a(0:7)
      real *8 theta,lcos,genphi,ptvb
      real *8 mom0,mom1,mom2,mom3,mom4,mom5,mom6,mom7
c //==========================================================
c     // <m0> = <(1/2)*(1-3cos^2(tjeta))> = (3/20)*(A0 - (2/3))
c     // <m1> = <sin(2*theta)*cos(phi)> = (1/5)*A1
c     // <m2> = <sin^2(theta)*cos(2*phi)> = (1/10)*A2
c     // <m3> = <sin(theta)*cos(phi)> = (1/4)*A3
c     // <m4> = <cos(theta)> - (1/4)*A4
c     // <m5> = <sin^2(theta)*sin(2*phi)> = (1/5)*A5
c     // <m6> = <sin(2*theta)*sin(phi)> = (1/5)*A6
c     // <m7> = <sin(theta)*sin(phi)> = (1/4)*A7
c     //======================================================
      
      call calCSVariables(p1,p2,res,.false.)
      
      theta=dacos(res(0))
c      if(abs(res(0)-kn_cthdec).gt.1d-16) then
c         write(*,*) ' cos theta: ', res(0), kn_cthdec,res(0)/kn_cthdec
c      endif
      lcos=res(0)
      genphi=res(3)

      mom0 = 0.5d0*(1-3*lcos*lcos)
      mom1 = dsin(2d0*theta)*dcos(genphi)
      mom2 = dsin(theta)*dsin(theta)*dcos(2d0*genphi)
      mom3 = dsin(theta)*dcos(genphi)
      mom4 = lcos                 
      mom5 = dsin(theta)*dsin(theta)*dsin(2d0*genphi)
      mom6 = dsin(2d0*theta)*dsin(genphi)
      mom7 = dsin(theta)*dsin(genphi)

      a(0)   = (20d0/3d0)*mom0 + (2d0/3d0)
      a(1)   = 5*mom1
      a(2)   = 10*mom2
      a(3)   = 4*mom3
      a(4)   = 4*mom4
      a(5)   = 5*mom5
      a(6)   = 5*mom6
      a(7)   = 4*mom7



      ptvb = sqrt((p1(1)+p2(1))**2 + (p1(2)+p2(2))**2)

      if (ptvb.le.5d0) call filld('lcos5',lcos,dsig)
      if ((ptvb.gt.5d0).and.(ptvb.le.10d0))  call filld('lcos10',
     $    lcos,dsig)
      if ((ptvb.gt.10d0).and.(ptvb.le.20d0)) call filld('lcos20',
     $     lcos,dsig)
      if ((ptvb.gt.20d0).and.(ptvb.le.40d0)) call filld('lcos40',
     $     lcos,dsig)
      if (ptvb.gt.40d0) call filld('lcosg40',lcos,dsig)
c
      if (ptvb.le.5d0) call filld('genphi5',genphi,dsig)
      if ((ptvb.gt.5d0).and.(ptvb.le.10d0))  call filld('genphi10',
     $     genphi,dsig)
      if ((ptvb.gt.10d0).and.(ptvb.le.20d0)) call filld('genphi20',
     $     genphi,dsig)
      if ((ptvb.gt.20d0).and.(ptvb.le.40d0)) call filld('genphi40',
     $     genphi,dsig)
      if (ptvb.gt.40d0) call filld('genphig40',genphi,dsig)
      

      call filld('a0',ptvb,dsig*a(0))

      call filld('a1',ptvb,dsig*a(1))

      call filld('a2',ptvb,dsig*a(2))

      call filld('a3',ptvb,dsig*a(3))

      call filld('a4',ptvb,dsig*a(4))

      call filld('a5',ptvb,dsig*a(5))

      call filld('a6',ptvb,dsig*a(6))

      call filld('a7',ptvb,dsig*a(7))


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
