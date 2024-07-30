      subroutine init_hist
      implicit none
      include 'pwhg_math.h'
      real * 8 dy,dpt,dptzoom,dphi
      integer icut
      integer, parameter :: ncuts=10
      real *8 jetcut(ncuts),mttcut(ncuts)
      real *8 pt_bins(8)
      character * 10 suffix(ncuts)
      common/jcut/jetcut,mttcut,suffix

      call inihists

      dy=0.1d0
      dpt=5d0
      dptzoom=1d0
      dphi = pi/10
c      suffix(1) = '-ptj20'
c      jetcut(1) = 20d0 
c      suffix(2) = '-ptj30'
c      jetcut(2) = 30d0 
c      suffix(3) = '-ptj60'
c      jetcut(3) = 60d0 
c      suffix(4) = '-ptj120'
c      jetcut(4) = 120d0 
c      suffix(5) = '-ptj200'
c      jetcut(5) = 200d0 
c
c      suffix(6) = '-mtt400'
c      mttcut(6) = 400d0 
c      suffix(7) = '-mtt500'
c      mttcut(7) = 500d0 
c      suffix(8) = '-mtt750'
c      mttcut(8) = 750d0 
c      suffix(9) = '-mtt1000'
c      mttcut(9) = 1000d0
c      suffix(10) = '-mtt1500'
c      mttcut(10) = 1500d0


      call bookupeqbins('total',1d0,0d0,1d0)

c     ================================================
c     Distributions from 1906.06535
c      call bookup('m_ttx_paper',10,
c     &     (/300d0,360d0,430d0,500d0,580d0,680d0,800d0,1000d0,1200d0,1500d0,2500d0/))
c      pt_bins = (/0d0,40d0,80d0,120d0,160d0,200d0,240d0,280d0,330d0,380d0,430d0,500d0,800d0/) 
c      call bookup('pt_t_paper' ,12,pt_bins)
c      call bookup('pt_tx_paper',12,pt_bins)
c      call bookup('pt_t1_paper',12,pt_bins)
c      call bookup('pt_t2_paper',12,pt_bins)
c      call bookup('pt_ttx_paper',9,
c     &     (/0d0,40d0,80d0,150d0,220d0,300d0,380d0,500d0,1000d0,6500d0/))
c      call bookup('absy_ttx_paper',10,
c     &     (/0d0,0.2d0,0.4d0,0.6d0,0.8d0,1.0d0,1.2d0,1.4d0,1.6d0,1.8d0,2.4d0/))
c      call bookup('absy_t_paper',11,
c     &     (/0d0,0.2d0,0.4d0,0.6d0,0.8d0,1.0d0,1.2d0,1.4d0,1.6d0,1.8d0,2.0d0,2.5d0/))
c      call bookup('absy_tx_paper',11,
c     &     (/0d0,0.2d0,0.4d0,0.6d0,0.8d0,1.0d0,1.2d0,1.4d0,1.6d0,1.8d0,2.0d0,2.5d0/))
c     ================================================

c     ================================================
c     Distributions from CMS-TOP-XXX
c     ================================================

c     ================================================
c     1D Distributions
c     ================================================

      pt_bins = (/0d0,55d0,100d0,165d0,240d0,330d0,440d0,600d0/) 
      call bookup('pt_t_paper' ,7,pt_bins)
      call bookup('pt_tx_paper',7,pt_bins)
      call bookup('absy_t_paper',5,
     &     (/0d0,0.45d0,0.9d0,1.35d0,1.8d0,2.6d0/))
      call bookup('absy_tx_paper',5,
     &     (/0d0,0.45d0,0.9d0,1.35d0,1.8d0,2.6d0/))
      call bookup('absy_ttx_paper',6,
     &     (/0d0,0.3d0,0.6d0,0.9d0,1.2d0,1.6d0,2.6d0/))
      call bookup('m_ttx_paper',7,
     &     (/300d0,380d0,470d0,620d0,820d0,1100d0,1500d0,2500d0/))
      call bookup('pt_ttx_paper',7,
     &     (/0d0,40d0,100d0,200d0,310d0,420d0,570d0,1000d0/))
      call bookup('dphi_t_tx_paper', 4, 
     &     (/0d0, 1.57d0, 2.67d0, 3.02d0, pi/))
      call bookup('dy_t_tx_paper', 4, 
     &     (/0d0, 0.40d0, 0.9d0, 1.4d0, 2.6d0/))
      call bookup('ratio_pt_t_mttx_paper',5,
     &     (/0d0, 0.15d0, 0.3d0, 0.4d0, 0.6d0, 1.0d0/))
      call bookup('ratio_pt_ttx_mttx_paper',9,
     &     (/0d0, 0.05d0, 0.1d0, 0.2d0, 0.3d0, 0.4d0, 0.55d0, 0.7d0, 0.9d0, 1.5d0/))
      call bookup('log_xi1_paper',9,
     &     (/-3.0d0, -2.2d0, -2.0d0, -1.8d0, -1.6d0, -1.4d0, -1.2d0, -0.95d0, -0.675d0, 0.0d0/))
      call bookup('log_xi2_paper',9,
     &     (/-3.0d0, -2.2d0, -2.0d0, -1.8d0, -1.6d0, -1.4d0, -1.2d0, -0.95d0, -0.675d0, 0.0d0/))





c     ===============
c     2D distributions
c     ================

c      [m(ttbar), |y(ttbar)|]
c      - bins in m(ttbar) [4 bins]: 300, 400, 500, 650, 1500
c      - bins in |y(ttbar)| [4 bins]: 0.00, 0.35, 0.75, 1.15, 2.50
      call bookup('m_ttx_absyttx_1',4,
     &     (/300d0,400d0,500d0,650d0,1500d0/))
      call bookup('m_ttx_absyttx_2',4,
     &     (/300d0,400d0,500d0,650d0,1500d0/))
      call bookup('m_ttx_absyttx_3',4,
     &     (/300d0,400d0,500d0,650d0,1500d0/))
      call bookup('m_ttx_absyttx_4',4,
     &     (/300d0,400d0,500d0,650d0,1500d0/))


c      [m(ttbar), |y(t)|]                                                                                                
c      - bins in m(ttbar) [3 bins]: 300, 400, 500, 650, 1500                                                            
c      - bins in |y(t)| [4 bins]: 0.00, 0.35, 0.85, 1.45, 2.50                                                           
      call bookup('m_ttx_absyt_1_old',3,
     &     (/300d0,450d0,600d0,1500d0/))
      call bookup('m_ttx_absyt_2_old',3,
     &     (/300d0,450d0,600d0,1500d0/))
      call bookup('m_ttx_absyt_3_old',3,
     &     (/300d0,450d0,600d0,1500d0/))
      call bookup('m_ttx_absyt_4_old',3,
     &     (/300d0,450d0,600d0,1500d0/))


c      [m(ttbar), pT(t)]
c      - bins in m(ttbar) [3 bins]: 300, 450, 600, 1500
c      - bins in pT(t) [3 bins]: 0, 100, 180, 600
      call bookup('mttx_ptt_1',3,
     &     (/300d0,450d0,600d0,1500d0/))
      call bookup('mttx_ptt_2',3,
     &     (/300d0,450d0,600d0,1500d0/))
      call bookup('mttx_ptt_3',3,
     &     (/300d0,450d0,600d0,1500d0/))


c      [m(ttbar), |y(t)|]
c      - bins in m(ttbar) [4 bins]: 300, 400, 500, 650, 1500
c      - bins in |y(t)| [4 bins]: 0.00, 0.35, 0.85, 1.45, 2.50
      call bookup('m_ttx_absyt_1',4,
     &     (/300d0,400d0,500d0,650d0,1500d0/))
      call bookup('m_ttx_absyt_2',4,
     &     (/300d0,400d0,500d0,650d0,1500d0/))
      call bookup('m_ttx_absyt_3',4,
     &     (/300d0,400d0,500d0,650d0,1500d0/))
      call bookup('m_ttx_absyt_4',4,
     &     (/300d0,400d0,500d0,650d0,1500d0/))



c      [|y(t)|, pT(t)]
c      - bins in |y(t)| [4 bins]: 0.00, 0.35, 0.85, 1.45, 2.50
c      - bins in pT(t) [4 bins]: 0, 80, 150, 250, 600
      call bookup('absyt_pt_t_1',4,
     &     (/0.0d0, 0.35d0, 0.85d0, 1.45d0, 2.50d0/))
      call bookup('absyt_pt_t_2',4,
     &     (/0.0d0, 0.35d0, 0.85d0, 1.45d0, 2.50d0/))
      call bookup('absyt_pt_t_3',4,
     &     (/0.0d0, 0.35d0, 0.85d0, 1.45d0, 2.50d0/))
      call bookup('absyt_pt_t_4',4,
     &     (/0.0d0, 0.35d0, 0.85d0, 1.45d0, 2.50d0/))



c      [pT(t), pT(ttbar)]
c      - bins in pT(t) [4 bins]: 0, 80, 150, 250, 600
c      - bins in pT(ttbar) [4 bins]: 0, 30, 75, 150, 500
      call bookup('pt_t_ttx_1',4,
     &     (/0.0d0, 80.0d0, 150.0d0, 250.0d0, 600.0d0/))
      call bookup('pt_t_ttx_2',4,
     &     (/0.0d0, 80.0d0, 150.0d0, 250.0d0, 600.0d0/))
      call bookup('pt_t_ttx_3',4,
     &     (/0.0d0, 80.0d0, 150.0d0, 250.0d0, 600.0d0/))
      call bookup('pt_t_ttx_4',4,
     &     (/0.0d0, 80.0d0, 150.0d0, 250.0d0, 600.0d0/))


c      [|y(ttbar)|, pT(ttbar)]
c      - bins in |y(ttbar)| [4 bins]: 0.00, 0.35, 0.75, 1.15, 2.50
c      - bins in pT(ttbar) [4 bins]: 0, 30, 75, 150, 500
      call bookup('absyttx_pt_ttx_1',4,
     &     (/0.0d0, 0.35d0, 0.75d0, 1.15d0, 2.50d0/))
      call bookup('absyttx_pt_ttx_2',4,
     &     (/0.0d0, 0.35d0, 0.75d0, 1.15d0, 2.50d0/))
      call bookup('absyttx_pt_ttx_3',4,
     &     (/0.0d0, 0.35d0, 0.75d0, 1.15d0, 2.50d0/))
      call bookup('absyttx_pt_ttx_4',4,
     &     (/0.0d0, 0.35d0, 0.75d0, 1.15d0, 2.50d0/))


c      [m(ttbar), pT(ttbar)]
c      - bins in m(ttbar) [4 bins]: 300, 400, 500, 650, 1500
c      - bins in pT(ttbar) [4 bins]: 0, 30, 75, 150, 500
      call bookup('mttx_pt_ttx_1',4,
     &     (/300.0d0, 400.0d0, 500.0d0, 650.0d0, 1500.0d0/))
      call bookup('mttx_pt_ttx_2',4,
     &     (/300.0d0, 400.0d0, 500.0d0, 650.0d0, 1500.0d0/))
      call bookup('mttx_pt_ttx_3',4,
     &     (/300.0d0, 400.0d0, 500.0d0, 650.0d0, 1500.0d0/))
      call bookup('mttx_pt_ttx_4',4,
     &     (/300.0d0, 400.0d0, 500.0d0, 650.0d0, 1500.0d0/))

c      [m(ttbar), |dEta(t,tbar)|]
c      - bins in m(ttbar) [4 bins]: 300, 400, 500, 650, 1500
c      - bins in |dEta(t,tbar)| [3 bins]: 0.0, 0.4, 1.2, 6.0
      call bookup('mttx_deta_ttx_1',4,
     &     (/300.0d0, 400.0d0, 500.0d0, 650.0d0, 1500.0d0/))
      call bookup('mttx_deta_ttx_2',4,
     &     (/300.0d0, 400.0d0, 500.0d0, 650.0d0, 1500.0d0/))
      call bookup('mttx_deta_ttx_3',4,
     &     (/300.0d0, 400.0d0, 500.0d0, 650.0d0, 1500.0d0/))


c      [m(ttbar), |dPhi(t,tbar)|]
c      - bins in m(ttbar) [4 bins]: 300, 400, 500, 650, 1500
c      - bins in |dPhi(t,tbar)| [3 bins]: 0.00, 2.20, 2.95, 3.14
      call bookup('mttx_dphi_ttx_1',4,
     &     (/300.0d0, 400.0d0, 500.0d0, 650.0d0, 1500.0d0/))
      call bookup('mttx_dphi_ttx_2',4,
     &     (/300.0d0, 400.0d0, 500.0d0, 650.0d0, 1500.0d0/))
      call bookup('mttx_dphi_ttx_3',4,
     &     (/300.0d0, 400.0d0, 500.0d0, 650.0d0, 1500.0d0/))


c      [pT(ttbar), m(ttbar), |y(ttbar)|]
c      - bins in pT(ttbar) [3 bins]: 0, 40, 120, 500
c      - bins in m(ttbar) [4 bins]: 340, 400, 500, 650, 1500
c      - bins in |y(ttbar)| [4 bins]: 0.00, 0.35, 0.75, 1.15, 2.50
      call bookupeqbins('pt_ttx_mttx_yttx',1d0, 0d0, 48d0)






c     ================================================

      call bookupeqbins('y_ttx',dy,-8d0,8d0)

      call bookupeqbins('pt_ttx',dpt,0d0,1500d0)
      call bookupeqbins('pt_ttx-zoom',dptzoom,0d0,50d0)

c     call bookupeqbins('m_ttx',10d0,50d0,150d0)
c     call bookupeqbins('m_ttx-zoom',1d0,70d0,110d0)
      call bookupeqbins('m_ttx',10d0,300d0,1000d0)
      call bookupeqbins('m_ttx-zoom',5d0,300d0,500d0)

      call bookupeqbins('y_t',dy,-5d0,5d0)
      call bookupeqbins('y_tx',dy,-5d0,5d0)

      call bookupeqbins('pt_t',dpt,0d0,500d0)
      call bookupeqbins('pt_t-zoom',dptzoom,0d0,100d0)

      call bookupeqbins('pt_tx',dpt,0d0,500d0)
      call bookupeqbins('pt_tx-zoom',dptzoom,0d0,100d0)

      call bookupeqbins('pt_t1',dpt,0d0,500d0)
      call bookupeqbins('pt_t2',dpt,0d0,500d0)
      
      call bookupeqbins('pt_j1',dpt,0d0,500d0)
      call bookupeqbins('pt_j1-zoom',dptzoom,0d0,50d0)

      call bookupeqbins('pt_j2',dpt,0d0,500d0)
      call bookupeqbins('pt_j2-zoom',dptzoom,0d0,50d0)  
      
      call bookupeqbins('dphi-t-tx',dphi,0d0,pi)
      call bookupeqbins('dy-t-tx',dy,-8d0,8d0)     
      
c      do icut=1,ncuts
c         call bookupeqbins('total'//trim(suffix(icut)),1d0,0d0,1d0)
c         call bookupeqbins('y_ttx'//trim(suffix(icut)),dy,-8d0,8d0)
c         call bookupeqbins('y-j1'//trim(suffix(icut)),dy,-8d0,8d0)
c         call bookupeqbins('dy-ttx-j1'//trim(suffix(icut)),dy,-8d0,8d0)
c         call bookupeqbins('dphi-ttx-j1'//trim(suffix(icut)),dphi,0d0,pi)
cc     to check top (and ttx) kinematics with a jet cut
c         call bookupeqbins('pt_ttx'//trim(suffix(icut)),dpt,0d0,1500d0)
c         call bookupeqbins('pt_ttx-zoom'//trim(suffix(icut)),dptzoom,0d0,50d0)
c         call bookup('pt_ttx_paper'//trim(suffix(icut)),9,
c     &     (/0d0,40d0,80d0,150d0,220d0,300d0,380d0,500d0,1000d0,6500d0/))
c         call bookupeqbins('pt_t'//trim(suffix(icut)),dpt,0d0,500d0)
c         call bookupeqbins('pt_tx'//trim(suffix(icut)),dpt,0d0,500d0)
c         call bookupeqbins('pt_t1'//trim(suffix(icut)),dpt,0d0,500d0)
c         call bookupeqbins('pt_t2'//trim(suffix(icut)),dpt,0d0,500d0)
c         call bookupeqbins('dphi-t-tx'//trim(suffix(icut)),dphi,0d0,pi)
c         call bookupeqbins('dy-t-tx'//trim(suffix(icut)),dy,-8d0,8d0)     
c         call bookupeqbins('m_ttx'//trim(suffix(icut)),10d0,300d0,1000d0)
c      enddo

      end

      
     
      subroutine analysis(dsig0)
      implicit none
      include 'hepevt.h'
      include 'pwhg_weights.h'
      include 'nlegborn.h'
      include 'pwhg_rad.h'
      include 'pwhg_rwl.h'
      include 'LesHouches.h'
      logical ini
      data ini/.true./
      save ini
C     allow multiweights 
      real * 8 dsig0
      real * 8, allocatable, save :: dsig(:)
c     tell to the analysis file which program is running it
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/
      integer, parameter :: ncuts=10
      real *8 jetcut(ncuts),mttcut(ncuts)
      character * 10 suffix(ncuts)
      common/jcut/jetcut,mttcut,suffix
C     top-antitop variables
      real*8 p_ttx(4),m_ttx,pt_ttx,y_ttx,eta_ttx
c     arrays to reconstruct jets
      integer maxjet
      parameter (maxjet=2048)
      real *8 ptmin,etamax,ptsave
      real *8  ktj(maxjet),etaj(maxjet),rapj(maxjet), phij(maxjet),pj(4
     $     ,maxjet),rr,ptrel(4), yj1,etaj1,ptj1,mj1,dyttxj1,detattxj1
     $     ,dphittxj1,drttxj1,dyttx,detattx,dphittx,drttx
      integer j1,found,mjets,ihep,icut,i,ihardest,isecond,ijet
      integer maxnumtop
      parameter (maxnumtop=10)
      integer topvec(maxnumtop),atopvec(maxnumtop),iatop,itop,ep,em,mu,natop,ntop
      
      real *8 p_t(4),y_t,eta_t,pt_t,m_t
      real *8 p_tx(4),y_tx,eta_tx,pt_tx,m_tx
      real *8 y_t1,eta_t1,pt_t1,y_t2,eta_t2,pt_t2
      real *8 xi1, xi2
c$$$      real * 8 pub(4,10),yVub,etaVub,ptVub,mVub
      real *8 powheginput
      integer kinreg_to_analyze
      logical isatop,istop,condition
      integer idtop,idatop
      parameter (idtop=6,idatop=-6)
      real * 8 corrfactor,ub_btilde_corr,ub_remn_corr,ub_corr

      real*8 ibin3d, ibin_pt, ibin_m, ibin_y

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
      
c     Loop over final state particles to find top and anti-top
c     JM: do I need to adapt this when the shower is on?
c     ER: adapted
      ntop=0
      natop=0
      topvec=0
      atopvec=0

      do ihep=1,nhep
         istop=idhep(ihep).eq.idtop
         isatop=idhep(ihep).eq.idatop
c$$$c     I require ihep>3 to make sure I don't pick up jmohep(1,ihep)=0, as this
c$$$c     gives a bound violation when computing idhep(jmohep(1,ihep))=idhep(0)...
c$$$         if(whcprg.eq.'PY8'.and.ihep.gt.3) then
c$$$            istop=istop.and.idhep(jmohep(1,ihep)).eq.idtop
c$$$            isatop=isatop.and.idhep(jmohep(1,ihep)).eq.idatop
c$$$            if(istop.or.isatop) print*, '--> ',idhep(ihep)
c$$$          endif
c     isthep = 62 is needed for PY8 at partonic level. It selects the
c     last top quarks in the event record (i.e. after all reshufflings
c     have taken place).

c         if (isthep(ihep).eq.1 .or. isthep(ihep).eq.62) then
            if(istop) then
               ntop=ntop+1
               topvec(ntop)=ihep
            elseif(isatop) then
               natop=natop+1
               atopvec(natop)=ihep
            endif
c         endif         
      enddo
         
      if (ntop.ne.1.or.natop.ne.1) then
         write(*,*) "Too many top quarks found. PROGRAM ABORT ",natop,ntop
         call exit(1)
      else 
         itop=topvec(1)
         iatop=atopvec(1)
      endif

c     ttx momentum
      do mu=1,4
         p_ttx(mu)=phep(mu,itop)+phep(mu,iatop)         
      enddo

c     top momentum
      do mu=1,4
         p_t(mu)=phep(mu,itop)
      enddo
      call getyetaptmass(p_t,y_t,eta_t,pt_t,m_t)

c     anti-top momentum
      do mu=1,4
         p_tx(mu)=phep(mu,iatop)
      enddo
      call getyetaptmass(p_tx,y_tx,eta_tx,pt_tx,m_tx)

c     top variables ordered by pT
      ! JM: do I ever have (systematically) events with t-tx back-to-back?
      if(pt_t.gt.pt_tx)then
        pt_t1  = pt_t
        y_t1   = y_t
        eta_t1 = eta_t
        pt_t2  = pt_tx
        y_t2   = y_tx
        eta_t2 = eta_tx
      else
        pt_t1  = pt_tx
        y_t1   = y_tx
        eta_t1 = eta_tx
        pt_t2  = pt_t
        y_t2   = y_t
        eta_t2 = eta_t
      endif

c     ttx-system histograms                                                                                                                                             
      call getyetaptmass(p_ttx,y_ttx,eta_ttx,pt_ttx,m_ttx)


c     log(ksi 1) = log10 ( ( E(t)-pz(t)+E(tbar)-pz(tbar) ) / (2 * 6500 GeV) ): 
c     log(ksi 2) = log10 ( ( E(t)+pz(t)+E(tbar)+pz(tbar) ) / (2 * 6500 GeV) )
c     log10 of the proton momentum fraction from the incoming parton/anti-parton, leading order QCD
c     For reference: y = 0.5* log((p(4)+p(3))/(p(4)-p(3)))
      xi1 = log10 ( ( p_t(4)-p_t(3)+p_tx(4)-p_tx(3) ) / 13000.0d0 )
      xi2 = log10 ( ( p_t(4)+p_t(3)+p_tx(4)+p_tx(3) ) / 13000.0d0 )

c     total sigma
      call filld('total',0.5d0,dsig)

c     ttx-system histograms 
      call getyetaptmass(p_ttx,y_ttx,eta_ttx,pt_ttx,m_ttx)
      call filld('y_ttx',y_ttx,dsig)
      call filld('m_ttx',     m_ttx,  dsig)
      call filld('m_ttx-zoom',m_ttx,  dsig)
c     t/ttx histograms     
      call filld('y_t',   y_t,  dsig)
      call filld('y_tx',  y_tx,  dsig)
      call filld('pt_ttx',pt_ttx,dsig)
      call filld('pt_t',      pt_t,  dsig)
      call filld('pt_t-zoom', pt_t,  dsig)
c      call filld('pt_t_paper',      pt_t,  dsig)
      call filld('pt_tx',      pt_tx,   dsig)
      call filld('pt_tx-zoom', pt_tx,   dsig)
      call filld('pt_t1',    pt_t1,  dsig)
      call filld('pt_t2',    pt_t2,  dsig)

      call getdydetadphidr(p_t,p_tx,dyttx,detattx,dphittx,drttx)
      call filld('dphi-t-tx',dphittx,dsig)
      call filld('dy-t-tx',dyttx,dsig)     


      call filld('pt_t_paper',    pt_t,  dsig)
      call filld('pt_tx_paper',   pt_tx,   dsig)
      call filld('absy_t_paper',   dabs(y_t),  dsig)
      call filld('absy_tx_paper',   dabs(y_tx),  dsig)
      call filld('absy_ttx_paper',dabs(y_ttx),dsig)     
      call filld('m_ttx_paper',     m_ttx,  dsig)
      call filld('pt_ttx_paper',pt_ttx,dsig)
      call filld('dphi_t_tx_paper',dphittx,dsig)
      call filld('dy_t_tx_paper',dabs(y_t)-dabs(y_tx),dsig)
      call filld('log_xi1_paper',xi1,dsig)
      call filld('log_xi2_paper',xi2,dsig)
      call filld('ratio_pt_t_mttx_paper',pt_t/m_ttx,dsig)
      call filld('ratio_pt_ttx_mttx_paper',pt_ttx/m_ttx,dsig)


c      [m(ttbar), |y(ttbar)|]
c      - bins in m(ttbar) [4 bins]: 300, 400, 500, 650, 1500
c      - bins in |y(ttbar)| [4 bins]: 0.00, 0.35, 0.75, 1.15, 2.50
      if(dabs(y_ttx).gt.0.00d0.and.dabs(y_ttx).lt.0.35d0) then 
         call filld('m_ttx_absyttx_1', m_ttx,  dsig)
      endif
      if(dabs(y_ttx).gt.0.35d0.and.dabs(y_ttx).lt.0.75d0) then
         call filld('m_ttx_absyttx_2', m_ttx,  dsig)
      endif
      if(dabs(y_ttx).gt.0.75d0.and.dabs(y_ttx).lt.1.15d0) then
         call filld('m_ttx_absyttx_3', m_ttx,  dsig)
      endif
      if(dabs(y_ttx).gt.1.15d0.and.dabs(y_ttx).lt.2.50d0) then
         call filld('m_ttx_absyttx_4', m_ttx,  dsig)
      endif

c      [m(ttbar), pT(t)]
c      - bins in m(ttbar) [3 bins]: 300, 450, 600, 1500
c      - bins in pT(t) [3 bins]: 0, 100, 180, 600
      if(pt_t.gt.0.0d0 .and. pt_t.lt.100.0d0) then
         call filld('mttx_ptt_1',m_ttx,  dsig)
      endif
      if(pt_t.gt.100.0d0 .and. pt_t.lt.180.0d0) then
         call filld('mttx_ptt_2',m_ttx,  dsig)
      endif
      if(pt_t.gt.180.00d0 .and. pt_t.lt.600.0d0) then
         call filld('mttx_ptt_3',m_ttx,  dsig)
      endif

c      [m(ttbar), |y(t)|]
c      - bins in m(ttbar) [4 bins]: 300, 400, 500, 650, 1500
c      - bins in |y(t)| [4 bins]: 0.00, 0.35, 0.85, 1.45, 2.50
      if(dabs(y_t).gt.0.00d0.and.dabs(y_t).lt.0.35d0) then
         call filld('m_ttx_absyt_1', m_ttx, dsig)
      endif
      if(dabs(y_t).gt.0.35d0.and.dabs(y_t).lt.0.85d0) then
         call filld('m_ttx_absyt_2', m_ttx, dsig)
      endif
      if(dabs(y_t).gt.0.85d0.and.dabs(y_t).lt.1.45d0) then
         call filld('m_ttx_absyt_3', m_ttx, dsig)
      endif
      if(dabs(y_t).gt.1.45d0.and.dabs(y_t).lt.2.50d0) then
         call filld('m_ttx_absyt_4', m_ttx, dsig)
      endif


c      [m(ttbar), |y(t)|]
c      - bins in m(ttbar) [3 bins]: 300, 400, 500, 650, 1500
c      - bins in |y(t)| [4 bins]: 0.00, 0.35, 0.85, 1.45, 2.50
      if(dabs(y_t).gt.0.00d0.and.dabs(y_t).lt.0.35d0) then
         call filld('m_ttx_absyt_1_old', m_ttx, dsig)
      endif
      if(dabs(y_t).gt.0.35d0.and.dabs(y_t).lt.0.85d0) then
         call filld('m_ttx_absyt_2_old', m_ttx, dsig)
      endif
      if(dabs(y_t).gt.0.85d0.and.dabs(y_t).lt.1.45d0) then
         call filld('m_ttx_absyt_3_old', m_ttx, dsig)
      endif
      if(dabs(y_t).gt.1.45d0.and.dabs(y_t).lt.2.50d0) then
         call filld('m_ttx_absyt_4_old', m_ttx, dsig)
      endif



c      [|y(t)|, pT(t)]
c      - bins in |y(t)| [4 bins]: 0.00, 0.35, 0.85, 1.45, 2.50
c      - bins in pT(t) [4 bins]: 0, 80, 150, 250, 600
      if(pt_t.gt.0.0d0.and.pt_t.lt.80.0d0) then
         call filld('absyt_pt_t_1', y_t, dsig)
      endif
      if(pt_t.gt.80.0d0.and.pt_t.lt.150.0d0) then
         call filld('absyt_pt_t_2', y_t, dsig)
      endif
      if(pt_t.gt.150.0d0.and.pt_t.lt.250.0d0) then
         call filld('absyt_pt_t_3', y_t, dsig)
      endif
      if(pt_t.gt.250.0d0.and.pt_t.lt.600.0d0) then
         call filld('absyt_pt_t_4', y_t, dsig)
      endif


c log(ksi 2) = log10 ( ( E(t)+pz(t)+E(tbar)+pz(tbar) ) / (2 * 6500 GeV) ): log10 of the proton momentum fraction from the incoming anti-parton, leading order QCD
c      [pT(t), pT(ttbar)]
c      - bins in pT(t) [4 bins]: 0, 80, 150, 250, 600
c      - bins in pT(ttbar) [4 bins]: 0, 30, 75, 150, 500
      if(pt_ttx.gt.0.0d0.and.pt_ttx.lt.30.0d0) then
         call filld('pt_t_ttx_1', pt_t, dsig)
      endif
      if(pt_ttx.gt.30.0d0.and.pt_ttx.lt.75.0d0) then
         call filld('pt_t_ttx_2', pt_t, dsig)
      endif
      if(pt_ttx.gt.75.0d0.and.pt_ttx.lt.150.0d0) then
         call filld('pt_t_ttx_3', pt_t, dsig)
      endif
      if(pt_ttx.gt.150.0d0.and.pt_ttx.lt.500.0d0) then
         call filld('pt_t_ttx_4', pt_t, dsig)
      endif



c      [|y(ttbar)|, pT(ttbar)]
c      - bins in |y(ttbar)| [4 bins]: 0.00, 0.35, 0.75, 1.15, 2.50
c      - bins in pT(ttbar) [4 bins]: 0, 30, 75, 150, 500
      if(pt_ttx.gt.0.0d0.and.pt_ttx.lt.30.0d0) then
         call filld('absyttx_pt_ttx_1', dabs(y_ttx), dsig)
      endif
      if(pt_ttx.gt.30.0d0.and.pt_ttx.lt.75.0d0) then
         call filld('absyttx_pt_ttx_2', dabs(y_ttx), dsig)
      endif
      if(pt_ttx.gt.75.0d0.and.pt_ttx.lt.150.0d0) then
         call filld('absyttx_pt_ttx_3', dabs(y_ttx), dsig)
      endif
      if(pt_ttx.gt.150.0d0.and.pt_ttx.lt.500.0d0) then
         call filld('absyttx_pt_ttx_4', dabs(y_ttx), dsig)
      endif

c      [m(ttbar), pT(ttbar)]
c      - bins in m(ttbar) [4 bins]: 300, 400, 500, 650, 1500
c      - bins in pT(ttbar) [4 bins]: 0, 30, 75, 150, 500
      if(pt_ttx.gt.0.0d0.and.pt_ttx.lt.30.0d0) then
         call filld('mttx_pt_ttx_1', m_ttx, dsig)
      endif
      if(pt_ttx.gt.30.0d0.and.pt_ttx.lt.75.0d0) then
         call filld('mttx_pt_ttx_2', m_ttx, dsig)
      endif
      if(pt_ttx.gt.75.0d0.and.pt_ttx.lt.150.0d0) then
         call filld('mttx_pt_ttx_3', m_ttx, dsig)
      endif
      if(pt_ttx.gt.150.0d0.and.pt_ttx.lt.500.0d0) then
         call filld('mttx_pt_ttx_4', m_ttx, dsig)
      endif


c      [m(ttbar), |dEta(t,tbar)|]
c      - bins in m(ttbar) [4 bins]: 300, 400, 500, 650, 1500
c      - bins in |dEta(t,tbar)| [3 bins]: 0.0, 0.4, 1.2, 6.0
      if(detattx.gt.0.0d0.and.detattx.lt.0.4d0) then
         call filld('mttx_deta_ttx_1', m_ttx, dsig)
      endif 
      if(detattx.gt.0.4d0.and.detattx.lt.1.2d0) then
         call filld('mttx_deta_ttx_2', m_ttx, dsig)
      endif
      if(detattx.gt.1.2d0.and.detattx.lt.6.0d0) then
         call filld('mttx_deta_ttx_3', m_ttx, dsig)
      endif

c      [m(ttbar), |dPhi(t,tbar)|]
c      - bins in m(ttbar) [4 bins]: 300, 400, 500, 650, 1500
c      - bins in |dPhi(t,tbar)| [3 bins]: 0.00, 2.20, 2.95, 3.14
      if(dphittx.gt.0.0d0.and.dphittx.lt.2.20d0) then
         call filld('mttx_dphi_ttx_1', m_ttx, dsig)
      endif
      if(dphittx.gt.2.20d0.and.dphittx.lt.2.95d0) then
         call filld('mttx_dphi_ttx_2', m_ttx, dsig)
      endif
      if(dphittx.gt.2.95d0.and.dphittx.lt.3.1415d0) then
         call filld('mttx_dphi_ttx_3', m_ttx, dsig)
      endif


c      [pT(ttbar), m(ttbar), |y(ttbar)|]
c      - bins in pT(ttbar) [3 bins]: 0, 40, 120, 500
c      - bins in m(ttbar) [4 bins]: 340, 400, 500, 650, 1500
c      - bins in |y(ttbar)| [4 bins]: 0.00, 0.35, 0.75, 1.15, 2.50



      ibin_pt = 999
      if((pt_ttx.gt.0.0d0).and.(pt_ttx.lt.40.0d0)) then
         ibin_pt=0
      endif
      if((pt_ttx.gt.40.0d0).and.(pt_ttx.lt.120.0d0)) then
         ibin_pt=1
      endif
      if((pt_ttx.gt.120.0d0).and.(pt_ttx.lt.500.0d0)) then
         ibin_pt=2
      endif

      ibin_m = 999
      if((m_ttx.gt.340.0d0).and.(m_ttx.lt.400.0d0)) then
         ibin_m=0
      endif
      if((m_ttx.gt.400.0d0).and.(m_ttx.lt.500.0d0)) then
         ibin_m=1
      endif
      if((m_ttx.gt.500.0d0).and.(m_ttx.lt.650.0d0)) then
         ibin_m=2
      endif
      if((m_ttx.gt.650.0d0).and.(m_ttx.lt.1500.0d0)) then
         ibin_m=3
      endif

      ibin_y = 999
      if((dabs(y_ttx).gt.0.00d0).and.(dabs(y_ttx).lt.0.35d0)) then
         ibin_y=0
      endif
      if((dabs(y_ttx).gt.0.35d0).and.(dabs(y_ttx).lt.0.75d0)) then
         ibin_y=1
      endif
      if((dabs(y_ttx).gt.0.75d0).and.(dabs(y_ttx).lt.1.15d0)) then
         ibin_y=2
      endif
      if((dabs(y_ttx).gt.1.15d0).and.(dabs(y_ttx).lt.2.50d0)) then
         ibin_y=3
      endif


      ibin3d = 16*ibin_pt + 4*ibin_m + ibin_y
c      write(*,*) 'pt: ', pt_ttx, ' m: ', m_ttx, ' y: ', dabs(y_ttx)
c      write(*,*) 'ibin3d: ', ibin3d, ' ibin_m: ',ibin_m, ' ibin_y: ', ibin_y, ' ibin_pt: ', ibin_pt
      call filld('pt_ttx_mttx_yttx', ibin3d, dsig)


c      do icut = 6,ncuts
c         if(icut .ge. 6 .and. m_ttx .lt. mttcut(icut)) then
cc     print*, 'icut = ',icut
c            call filld('total'//trim(suffix(icut)),0.5d0,dsig)
c            call filld('y_ttx'//trim(suffix(icut)),y_ttx,dsig)
c            call filld('pt_ttx'//trim(suffix(icut)),pt_ttx,dsig)
c            call filld('pt_ttx-zoom'//trim(suffix(icut)),pt_ttx,dsig)
c            call filld('pt_ttx_paper'//trim(suffix(icut)),pt_ttx,dsig)
cc     to check top (and ttx) kinematics with a jet cut
c            call filld('pt_ttx'//trim(suffix(icut)), pt_ttx, dsig)
c            call filld('pt_t'//trim(suffix(icut)),   pt_t,   dsig)
c            call filld('pt_tx'//trim(suffix(icut)),  pt_tx,  dsig)
c            call filld('pt_t1'//trim(suffix(icut)),  pt_t1,  dsig)
c            call filld('pt_t2'//trim(suffix(icut)),  pt_t2,  dsig)
c            call filld('dphi-t-tx'//trim(suffix(icut)),dphittx,dsig)
c            call filld('dy-t-tx'//trim(suffix(icut)),dyttx,dsig)
c            call filld('m_ttx'//trim(suffix(icut)),  m_ttx,  dsig)
c         endif
c      enddo
      
ccc	C     find jets 
ccc	      rr=0.4d0       
ccc	      ptmin=25d0
ccc	      etamax=4.5d0
ccc	      call buildjets(1,rr,ptmin,mjets,ktj,etaj,rapj,phij,ptrel,pj)
ccc	
ccc	      if(mjets.eq.0) goto 666
ccc	
ccc	      ihardest=-1
ccc	      if(mjets.ge.1) then
ccc	c     if at least a jet was found, loop over jets and find the hardest within rapidity cut
ccc	         ptsave=-1d0
ccc	         do ijet=1,mjets
ccc	            condition=(dabs(etaj(ijet)).le.etamax).and.(ktj(ijet).ge.ptsave)
ccc	            if(condition) then
ccc	               ihardest=ijet
ccc	               ptsave=ktj(ihardest)
ccc	            endif
ccc	            if(ktj(ijet).lt.ptmin) then
ccc	               write(*,*) 'ERROR1: this cannot happen'
ccc	            endif
ccc	         enddo
ccc	c     if a good jet is found, fill histograms
ccc	         if(ihardest.ne.-1) then
ccc	c         print*, '1 jet found'
ccc	            call filld('pt_j1',ktj(ihardest),dsig)
ccc	            call filld('pt_j1-zoom',ktj(ihardest),dsig)
ccc	            
ccc	            call getdydetadphidr(p_ttx,pj(:,ihardest),dyttxj1,detattxj1,dphittxj1,drttxj1)
ccc	            
ccc	            do icut = 1,5
ccc	               if(icut.le.5 .and. ktj(ihardest) .gt. jetcut(icut)) then
ccc	c     print*, 'icut = ',icut
ccc	                  call filld('y-j1'//trim(suffix(icut)),rapj(ihardest),dsig)
ccc	                  call filld('dy-ttx-j1'//trim(suffix(icut)),dyttxj1,dsig)
ccc	                  call filld('dphi-ttx-j1'//trim(suffix(icut)),dphittxj1,dsig)
ccc	c     to check top (and ttx) kinematics with a jet cut
ccc	                  call filld('pt_ttx'//trim(suffix(icut)), pt_ttx, dsig)
ccc	                  call filld('pt_t'//trim(suffix(icut)),   pt_t,   dsig)
ccc	                  call filld('pt_tx'//trim(suffix(icut)),  pt_tx,  dsig)
ccc	                  call filld('pt_t1'//trim(suffix(icut)),  pt_t1,  dsig)
ccc	                  call filld('pt_t2'//trim(suffix(icut)),  pt_t2,  dsig)
ccc	                  call filld('dphi-t-tx'//trim(suffix(icut)),dphittx,dsig)
ccc	                  call filld('dy-t-tx'//trim(suffix(icut)),dyttx,dsig)
ccc	               endif
ccc	            enddo         
ccc	         endif
ccc	      endif
ccc	
ccc	      isecond=-1      
ccc	      if(mjets.ge.2) then
ccc	c     if at least 2 jets found, loop over jets, find the second-hardest within rapidity cut
ccc	         ptsave=-1d0
ccc	         do ijet=1,mjets
ccc	            condition=(dabs(etaj(ijet)).le.etamax).and.(ktj(ijet).ge.ptsave).and.(ijet.ne.ihardest)
ccc	            if(condition) then
ccc	               isecond=ijet
ccc	               ptsave=ktj(isecond)
ccc	            endif
ccc	            if(ktj(ijet).lt.ptmin) then
ccc	               write(*,*) 'ERROR2: this cannot happen'
ccc	            endif
ccc	         enddo
ccc	c     if a good jet is found, fill histograms
ccc	         if(isecond.ne.-1) then
ccc	c         print*, '2 jets found'
ccc	            call filld('pt_j2',ktj(isecond),dsig)
ccc	            call filld('pt_j2-zoom',ktj(isecond),dsig)
ccc	         endif
ccc	      endif
ccc	
ccc	      if((isecond.eq.ihardest).and.(isecond.ne.-1)) then
ccc	         write(*,*) 'ERROR3: this cannot happen'
ccc	      endif
 

 666  end
      


      subroutine buildjets(iflag,rr,ptmin,mjets,kt,eta,rap,phi,
     $     ptrel,pjet)
c     arrays to reconstruct jets, radius parameter rr
      implicit none
c     tell to the analysis file which program is running it
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      integer iflag,mjets
      real * 8  rr,ptmin,kt(*),eta(*),rap(*),
     1     phi(*),ptrel(3),pjet(4,*)
      include   'hepevt.h'
      include  'LesHouches.h'
      integer   maxtrack,maxjet
      parameter (maxtrack=2048,maxjet=2048)
      real * 8  ptrack(4,maxtrack),pj(4,maxjet)
      integer   jetvec(maxtrack),itrackhep(maxtrack)
      integer   ntracks,njets
      integer   j,k,mu,i
      real * 8 r,palg,tmp
      real * 8 vec(3),pjetin(0:3),pjetout(0:3),beta,
     $     ptrackin(0:3),ptrackout(0:3)
      real * 8 get_ptrel
      external get_ptrel
C - Initialize arrays and counters for output jets
      do j=1,maxtrack
         do mu=1,4
            ptrack(mu,j)=0d0
         enddo
         jetvec(j)=0
      enddo      
      ntracks=0
      do j=1,maxjet
         do mu=1,4
            pjet(mu,j)=0d0
            pj(mu,j)=0d0
         enddo
      enddo
      if(iflag.eq.1) then
C     - Extract final state particles to feed to jet finder
         if(WHCPRG.eq.'PY8   ') then
            do j=1,nhep
c all but top/anti-top quark
               if((isthep(j).gt.0).and.abs(idhep(j)).ne.6) then
                  if(ntracks.eq.maxtrack) then
                     write(*,*) 'analyze: need to increase maxtrack!'
                     write(*,*) 'ntracks: ',ntracks
                     stop
                  endif
                  ntracks=ntracks+1
                  do mu=1,4
                     ptrack(mu,ntracks)=phep(mu,j)
                  enddo
                  itrackhep(ntracks)=j
               endif
            enddo
         else
           do j=1,nhep
c all but top/anti-top quark
               if(isthep(j).eq.1.and.abs(idhep(j)).ne.6) then
                  if(ntracks.eq.maxtrack) then
                     write(*,*) 'analyze: need to increase maxtrack!'
                     write(*,*) 'ntracks: ',ntracks
                     stop
                  endif
                  ntracks=ntracks+1
                  do mu=1,4
                     ptrack(mu,ntracks)=phep(mu,j)
                  enddo
                  itrackhep(ntracks)=j
               endif
            enddo
         endif
      else
         do j=1,nup
            if (istup(j).eq.1.and.abs(idhep(j)).ne.6) then
               if(ntracks.eq.maxtrack) then
                  write(*,*) 'analyze: need to increase maxtrack!'
                  write(*,*) 'ntracks: ',ntracks
                  stop
               endif
               ntracks=ntracks+1
               do mu=1,4
                  ptrack(mu,ntracks)=pup(mu,j)
               enddo
               itrackhep(ntracks)=j
            endif
         enddo
      endif
      if (ntracks.eq.0) then
         mjets=0
         return
      endif
C --------------------------------------------------------------------- C
C     R = 0.7   radius parameter
c palg=1 is standard kt, -1 is antikt
      palg=-1
      r=rr
c      ptmin=20d0 
      call fastjetppgenkt(ptrack,ntracks,r,palg,ptmin,pjet,njets,
     $                        jetvec)
      mjets=njets
      if(njets.eq.0) return
c check consistency
      do k=1,ntracks
         if(jetvec(k).gt.0) then
            do mu=1,4
               pj(mu,jetvec(k))=pj(mu,jetvec(k))+ptrack(mu,k)
            enddo
         endif
      enddo
      tmp=0
      do j=1,mjets
         do mu=1,4
            tmp=tmp+abs(pj(mu,j)-pjet(mu,j))
         enddo
      enddo
      if(tmp.gt.1d-4) then
         write(*,*) ' bug!'
      endif
C --------------------------------------------------------------------- C
C - Computing arrays of useful kinematics quantities for hardest jets - C
C --------------------------------------------------------------------- C
      do j=1,mjets
         call getyetaptmass(pjet(:,j),rap(j),eta(j),kt(j),tmp)
         phi(j)=atan2(pjet(2,j),pjet(1,j))
      enddo

c     loop over the hardest 3 jets
      do j=1,min(njets,3)
         do mu=1,3
            pjetin(mu) = pjet(mu,j)
         enddo
         pjetin(0) = pjet(4,j)         
         vec(1)=0d0
         vec(2)=0d0
         vec(3)=1d0
         beta = -pjet(3,j)/pjet(4,j)
         call mboost(1,vec,beta,pjetin,pjetout)         
c     write(*,*) pjetout
         ptrel(j) = 0
         do i=1,ntracks
            if (jetvec(i).eq.j) then
               do mu=1,3
                  ptrackin(mu) = ptrack(mu,i)
               enddo
               ptrackin(0) = ptrack(4,i)
               call mboost(1,vec,beta,ptrackin,ptrackout) 
               ptrel(j) = ptrel(j) + get_ptrel(ptrackout,pjetout)
            endif
         enddo
      enddo
      end

C     a number of handy functions used by generic analyses 

      subroutine getyetaptmass(p,y,eta,pt,mass)
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

      subroutine getdydetadphidr(p1,p2,dy,deta,dphi,dr)
      implicit none
      include 'pwhg_math.h' 
      real * 8 p1(*),p2(*),dy,deta,dphi,dr
      real * 8 y1,eta1,pt1,mass1,phi1
      real * 8 y2,eta2,pt2,mass2,phi2
      call getyetaptmass(p1,y1,eta1,pt1,mass1)
      call getyetaptmass(p2,y2,eta2,pt2,mass2)
      dy=y1-y2
      deta=eta1-eta2
      phi1=atan2(p1(1),p1(2))
      phi2=atan2(p2(1),p2(2))
      dphi=abs(phi1-phi2)
      dphi=min(dphi,2d0*pi-dphi)
      dr=sqrt(deta**2+dphi**2)
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

      function get_ptrel(pin,pjet)
      implicit none
      real * 8 get_ptrel,pin(0:3),pjet(0:3)
      real * 8 pin2,pjet2,cth2,scalprod
      pin2  = pin(1)**2 + pin(2)**2 + pin(3)**2
      pjet2 = pjet(1)**2 + pjet(2)**2 + pjet(3)**2
      scalprod = pin(1)*pjet(1) + pin(2)*pjet(2) + pin(3)*pjet(3)
      cth2 = scalprod**2/pin2/pjet2
      get_ptrel = sqrt(pin2*abs(1d0 - cth2))
      end
