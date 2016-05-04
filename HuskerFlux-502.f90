!******************************************************************************
!
!	Program: HuskerFlux
!
!  This is an eddy covariance data collection program designed to run as a
!  QuickWin application under WinNT, Win2k, and WinXP.  The program
!  reads serial data streams from one or more of several different instruments.
!  In it's initial release, it will read data streams from the following sonic
!  anemometers:  ATI, Gill R3, Gill WindMaster Pro, Campbell CSAT3, and an
!  R.M. Young 81000V.  It will also read the following IRGAs:  LiCor LI-7500,
!  LiCor LI-6262, and LiCor LI-7000 and LiCor LI-820.
!
!    8/04/02     The first group of instruments read correctly.  [V-0.0]
!
!    9/24/02     All parts of the program are functional except the flux
!                corrections. [V-0.1]
!
!    1/20/03     The corrections are implemented, but are untested.  Also
!                all instruments read correctly except the R.M. Young
!                sonic and the LI-7000 IRGA.  These will be implemented when
!                instruments become available.  [V-0.2]
!
!    1/24/03     The output files are now written correctly and the time
!                stamps are right.  [V-0.3]
!
!    2/06/03     The LiCor LI-7000 closed path IRGA was added to the
!                program.  [V-0.4]  
!
!    2/23/03     The LiCor LI-820 (Gas Hound) closed path IRGA was added
!                to the program and channel delays were also added.  Also
!                the parameter input routine was modified to read a dummy
!                integer that serves as a place holder for an averaging
!                method selection in the reprocessing routine.  [V-0.50]
!
!    5/11/03     Implemented algorithms to do inter-instrument synchronization
!                [V-0.50a]
!
!    7/20/03     Changed Buld_File_Names routine to force it to wait until
!                midnight before building new files.  This fixes a problem
!                caused by instruments running faster than the computer
!                clock rate.  The problem is manifest by files being over-
!                written because the controlling instrument "finishes" a day
!                before the computer clock "finishes" a day. [V-0.50b]
!
!    12/04/03    Put most of the parameters read from the .par file in new
!                structures.  This should make it easier to change the
!                program to be "com port" based rather than "instrument"
!                based.  [V-0.50c]
!
!    12/16/03    The R.M. Young sonic anemometer is now functional (mostly!).
!                There still seems to be a problem "shifting" it into
!                unprompted binary output mode when it starts out in some
!                other mode.  The simple work-around (at this time) is when
!                the anemometer won't go into binary mode, the user can force
!                it by manually setting it up with HyperTerminal.  [V-0.60]
!
!     4/04/04    Changed a bunch of the data structures to allow more efficient
!                indexing of parameters. Also cleaned up a lot of the CASE
!                statements.  Most were replaced with simple DO loops.
!                [V-0.61]
!
!     4/06/04    Re-structured the program to be "com port" based rather than
!                "instrument" based.  Under the old scheme, only a single
!                instrument of any type could be connected.  Now any
!                combination of instruments can be connected.  This allows the
!                user to do such things as compare several LI-7500's or several
!                WindMaster Pros at the same time.  Note that this does change
!                the definition of one field in the parameter file.  In the
!                "channel" section, the third parameter used to be the
!                instrument type, now it is the instrument line number (from
!                the "serial port" section.  This shouldn't affect the data
!                re-processing program.  [V-0.62]
!
!     4/09/04    Cleaned up the code, added some comments, and re-arranged data
!                items in a couple of the IRGA sections for consistency.  Also
!                finished and enabled "REAL*4" storage option.  This is the
!                first "production" release. [V-1.00]
!
!     7/28/04    Re-did the raw data file build routine to speed things up.
!                MUCH faster now.  This should cause fewer problems on slow
!                machines when passing midnight.  Also cleaned up a few items
!                and deleted some old code lines. [V-1.01]
!
!     8/11/04    Put in a subroutine to re-boot the computer after a specified
!                number of days.  This interval is read in from the setup file
!                along with the rest of the parameters.  If the interval is
!                zero (or negative), no re-boot is done.  Also changed the
!                program to read its wait interval (the time between when the
!                program is started till when it actually trys to talk to the
!                serial ports) from the setup file.  This number is in seconds
!                and is currently the last parameter.  [V-1.02]
!
!      2/8/05    Did some housekeeping and fixed up the "Units", Fluxes", and
!                "Corrections" subroutines to be consistent with HuskerProc.
!                [V-1.03]
!
!     4/27/05    Did some more clean ups and brought all of the code to synch
!                with the current version of HuskerProc.  [V-1.04]
!
!     5/16/06    Added some code to the R3 sonic initialization section to
!                make the system work with a Gill R3-50.   [V-1.05]
!
!     12/22/06   Fixed up the CSAT3 synchronization code.  Cleaned up the
!                synch code for the rest of the "binary" instruments.  re- did
!                the synch check and fix code in the "binary" instruments
!                "Cnvrt" routines for better reliability.  Added "outlines" for
!                future addition of LiCor-LI-7700 CH4 open path IRGA   [V-1.06]
!
!     3/27/07    Modified Write_Averages subroutine to re-sync with computer
!                clock whenever computer clock is ahead of or behind the
!                instrument clock by a time interval, read from the .PAR file
!                Also changed .oxx output to record the instrument End Time
!                instead of instrument Begin Time  [V-1.07]
!
!     1/9/09     Extensively modified Write_Averages, UpdateClock, and a few
!                other parts to allow re-synchronization with the computer
!                clock at the end of each averaging period.  Also created two
!                "clocks" that keep "instrument" and "real" (or computer) time
!                for general use around the program.  These were extensive enough
!                changes that it's time for full version change  [V-2.00]
!
!     1/30/09    Elimininated the need for the comsetup.txt file.  The
!                parameters for this function are now taken from the main setup
!                file (setup.par) and each port used is exercised by the
!                "Verify_Ports" routine. [V-2.10]
!
!     4/5/12     Added support for the Aerodyne TDLS used in the COS experiment.
!                [V-3.00]
!
!     6/27/12    Modified LI-7500 sections to also support the LI-7500A and the
!                LI-7200.  Added support for the LI-7700.  [V-3.01]
!
!     1/22/13    Changed where Inter_Sync is called to fix instrument skew
!                issue.  [V-3.02]
!
!     6/27/13    Added native support for the LI-7500A and LI-7200 IRGAs.  Also
!                changed to have the routine "inter_sync (chop)" handle the
!                synchronization task in "Write_Averages (irec_off)".  Also,
!                Stephen Chan found a glitch in writing the 10 second buffer that
!                was causing the last element to be written 10 seconds back.
!                Fixed this.  [V-4.00]
!
!     8/15/13    Fixed a bug in write_buffer that put a 10 second delay in the 
!                instrument clock for the first averaging period of each new day
!                (found by Stephen Chan) also tuned the clock sync in
!                Write_Averages  [V-4.01]
!
!     9/1/13     Changed the counter variables isec, imsec, insec in the LI-7700
!                data input routine (LI7700AscCnvrt) to integer*8 to stop
!                overflows.  [V-4.0.2]  {note the version number didn't change
!                until 12/19/13}
!
!     1/9/14     re-wrote the initialization routines for LI-7500A and LI-7200
!                to work with firmware revisions 6.0.x, 6.5.x, and 7.0.x  Also
!                patched the LI7200ascCnvrt routine to accomodate the new RSSI
!                variables.  This changes the order of variables a bit.  The
!                LI7500AascCnvrt routine simply replaces the AGC variable with
!                the (single) RSSI variable is the version number is greater
!                than 6.0.x  Also recorded full calibration data for the 
!                LI-7500A and LI-7200 IRGAs.  Also put serial numbers and
!                firmware versions in a data structure (see module
!                Instrument_Info) for use throughout the program.  Other
!                instrument parameters can easily be added to this in the
!                future.  If an instrument can't report a serial no. or
!                Firmware ver., "null" is stored. [V-4.0.3]
!
!     2/20/14    Changing the algorithm to calculate averages and variances/
!                standard deviations. It uses a running method that avoids some
!                floating point precision problems that can happen with very
!                small numbers. Changes written by Gilberto Pastorello (LBNL).
!                [V-4.1.0]
!
!     3/11/14    Fixed a synchronization bug in the LI-7700 routine that caused
!                1 bad line of data to be written whenever LI-7700 packets were
!                dumped to synchronize at the end of an averaging period.  Also
!                moved separator lines in the setup subroutine outputs (.ixx file)
!                to the bottom of the "paragraph".  [V-4.1.1]
!
!     6/9/14     Increased the number of allowable channels, rotations,
!                covariances, and corrections from 32 to 128 and the maximum
!                number of serial channels from 8 to 16.  Also re-wrote most of
!                the graphics routines.  The user must now specify (in a new
!                column of the rot section of the PAR file) which channels
!                (if any) correspond to u (1), v (2), w (3), and Tsonic (4).
!                A third column was also added to the rot section of the PAR file
!                that allows the user to specify which channel gets plotted in
!                the live-time graphs (1 - 6).  Finally if the u channel is set to
!                -1 instead of 1, the total horizontal wind speed is calculated
!                and plotted instead of u [V-4.2.0]
!
!     6/20/14    Added support for the Los Gatos Research 911-0010 TDLAS and the
!                Picarro G2300-f TDLAS  [V-4.3.0]
!
!     7/23/14    Quick and very dirty hack to force running with LiCor 7.1
!                firmware update.  Haven't seen the actual update or instruments
!                so no guarantees that this will work!  [V-4.3.0A]
!
!     8/15/14    Modified the Picarro G2300-f code to work correctly with the
!                analyzer.  The analyzer should be set up to output CH4 data
!                as well as CO2 and H2O  [V-4.3.1]
!
!     9/19/14    Modified the Los Gatos Research code to work with the loaner
!                unit that will be installed in the field at Davis, CA.  This
!                machine does CO2, H2O, and O2  [V-4.3.1a]
!
!     2/6/15     Modified ATI setup routine to deal with V 2.6.1 firmware
!                issues (not tested yet as there is a firmware bug in the ATI
!                [V-4.3.2]
!
!     3/16/15    Added simple (non configuring) code for the Campbell EC-155
!                enclosed-path IRGA and the IRGASON open-path combo [V4.3.3]
!
!     6/4/15     Fixed a problem when plotting an odd number of channels that
!                is less than 6  [V4.4.0]
!
!     12/22/15   This is a major break in the development tree.  HuskerFlux
!                is now developed and built under Intel Visual Studio
!                (Intel Visual Fortran).  Development under the old Compaq
!                Visual Fortran system is done.  In this version, a new file
!                manager has been incorporated into the main code.  This
!                subroutine runs with other routines at the end of the day
!                and facilitates file management.  It takes it's setup
!                commands from files.par  If this file is not present, the
!                file manager will not run, and HuskerFlux behaves like
!                earlier versions.  [V5.0.0]
!
!     4/3/16     Patching initialization routines for LI-7500A and LI-7200
!                and the data convert routine for the LI-7200 to work with
!                the new V8.x.x firmware.  This also makes these instrument
!                types work with the new "RS" series.  [V5.0.1]
!
!     4/8/16     Re-wrote the LI-7000 routines to work with new firmware
!                and hardware versions.  Fixed a bug in the LI-7000 code that
!                put 1 line of bad data in every averaging period.  Added
!                framework for instrument information (to eventually be
!                written to every ".ixx" file  [V5.0.2]
!
!  
!  Dave Billesbach
!  Dept. of Biological Systems Engineering
!  University of Nebraska
!  Lincoln, NE 68583-0726
!  (402)472-7961
!  dbillesbach1@unl.edu
!
!  Copyright D. Billesbach 2002, 2006, 2010, 2014
!
!******************************************************************************
!
!
!==============================================================================
!
	module serial_vars
!
!  This sets the size of the write buffer
!
	integer(kind=4), parameter :: ioutbuflen=1024
!
	end module serial_vars
!
!==============================================================================
!
	module file_names
!
!  These are the unit numbers for the various files created by HuskerFlux.
!  Currently, the following units are used:  iraw_unit=raw data
!  iout_unit=mean and flux outputs  info_unit=information about the system.
!  the ierr_unit is not currently used.  Also defined here are the logical unit
!  numbers for the various windows opened by Huskerflux.
!  
	integer(kind=4), parameter :: iraw_unit=1,iout_unit=2,info_unit=3
	integer(kind=4), parameter :: ierr_unit=4
!
	character(len=120) file_root
	character(len=120) raw_name,out_name,err_name,info_name
	integer(kind=4) len_file_names
	integer(kind=4) idat_win,igrf_win,imsg_win
!
	data idat_win/8/,igrf_win/9/,imsg_win/10/
!
	end module file_names
!
!==============================================================================
!
	module Instrument_Info
!
!  This module passes instrument serial numbers and firmware version numbers to
!  routines that need them
!
    use common_vars
!
    integer(kind=4),parameter :: info_str_len=80,max_info_strings=20
    character(len=info_str_len) info_blank
    character(len=info_str_len) run_start
	type inst_info
		character(len=info_str_len) SerialNo
		integer(kind=4) SerialNo_len
		character(len=info_str_len) Firmware
		integer(kind=4) Firmware_len
        character(len=info_str_len) info_string(max_info_strings)
        integer(kind=4) info_string_len(max_info_strings)
        integer(kind=4) n_info_strings
	end type inst_info
!
	type (inst_info):: Inst_Inf(max_ser)
!
	end module Instrument_Info
!
!==============================================================================
!
	Module File_manager_Vars
!
	integer(kind=4),parameter::str_len=8192
    character(len=18),parameter::log_file='.\File-manager-log.log'
    character(len=20),parameter::file_manage_setup='.\File-manager-setup.par'
    character(len=14),parameter::transfer_command_file='.\transfer.cmd'
!
	character(len=str_len),allocatable::EC_in_folder(:),EC_archive_folder(:)
    character(len=str_len),allocatable::EC_transfer_folder(:),EC_remote_folder(:)
	character(len=str_len),allocatable::Slow_in_folder(:),Slow_archive_folder(:)
	character(len=str_len),allocatable::Slow_file(:),Slow_cumul_folder(:)
    character(len=str_len),allocatable::Slow_transfer_folder(:),Slow_remote_folder(:)
	character(len=str_len),allocatable::Other_in_folder(:),Other_archive_folder(:)
    character(len=str_len),allocatable::Other_transfer_folder(:),Other_remote_folder(:)

	character(len=str_len) USB_folder,cmd_line,blank_line
    character(len=str_len) Local_home_folder,new_line
	character(len=12) date_str,time_str,zone_str,new_dir
	character(len=4) doy_chr,year_chr
!
	integer(kind=4) date_time(8)
	integer(kind=4) isleep
!
	logical(kind=4) USB_exist
!
	end module File_manager_Vars
!
!==============================================================================
!
	module graph_params
!
!  This module defines parameters and variables used by the graphing routines
!
	use ifwin
	use dflib
!
	integer(kind=4), parameter :: icomp_x=100,icomp_y=100,icomp_R=50,iworm=10
	integer(kind=4), parameter :: n_plots=3,n_graphs=2*n_plots
!
	character(len=6) color_name(8)
	logical wind
	integer(kind=4) iytop(n_plots),iybottom(n_plots)
	integer(kind=4) ixtop,ixbottom,iplen,itks
	integer(kind=4) icharw,icharh,icharhw
	integer(kind=4) nxpix,nypix,ngrafcols,ngrafrows
	integer(kind=4) icolors(12,2),ichn(n_graphs)
	integer(kind=4) icolyl,icolyr
	real(kind=8) xmin,xmax
!
	record /xycoord/ ixy
	record /rccoord/ curpos
!
	data ixtop /250/
	data ixbottom /950/
	data iytop /10,120,230/
	data iybottom /110,220,330/
	data icolyl /25/,icolyr/121/
	data color_name /'White ','Red   ','Blue  ','Green ','Violet', &
	                 'Cyan  ','Yellow','Black '/
!
	end module graph_params
!
!==============================================================================
!
	module met_data
!
!  Here, we define some constants used in the various meteorological
!  calculations.  amv=molecular weight of water, amd=molecular weight of dry
!  air,  r=perfect gas constant, vk=vonKarmans constant, cp=specific heat of
!  air, al=latent heat of vaporization of water, press=atmospheric pressure
!  g=gravitational acceleration constant.  The following variables are
!  calculated and made available to other program units:  rhod=density of dry
!  air,  rhoa=density of moist air,  rhov=density of water vapor,
!  Tbar=mean temperature.
!
	real(kind=8), parameter :: amv=18.016D0,amd=28.966D0,amu=amd/amv
	real(kind=8), parameter :: r=0.083143D0,vk=0.4D0,Cp=1.016D0
	real(kind=8), parameter :: al=2435.0D0,press=980.0D0,g=9.8D0
!
	real(kind=8) rhod,rhoa,rhov,Tbar
!
	end module met_data
!
!==============================================================================
!
	module common_vars
!
!  This module defines and makes many of the common variables available to
!  program units.
!
	use ifwin
	use dflib
!
	character(len=8), parameter :: prog_ver='5.0.2'
    character(len=128),parameter:: setup_file='setup.par'
!
!  these parameters define the instruments supported
!
	integer, parameter :: indx_ATI=1						! 1
	integer, parameter :: indx_R3=indx_ATI+1				! 2
	integer, parameter :: indx_WMP=indx_R3+1				! 3
	integer, parameter :: indx_CSAT=indx_WMP+1				! 4
	integer, parameter :: indx_Y81000=indx_CSAT+1			! 5
	integer, parameter :: indx_LI7500=indx_Y81000+1			! 6
	integer, parameter :: indx_LI6262=indx_LI7500+1			! 7
	integer, parameter :: indx_LI7000=indx_LI6262+1			! 8
	integer, parameter :: indx_LI820=indx_LI7000+1			! 9
	integer, parameter :: indx_LI840=indx_LI820+1			! 10
	integer, parameter :: indx_LI7700=indx_LI840+1			! 11
	integer, parameter :: indx_AerodyneTDL=indx_LI7700+1	! 12
	integer, parameter :: indx_LI7500A=indx_AerodyneTDL+1	! 13
	integer, parameter :: indx_LI7200=indx_LI7500A+1		! 14
	integer, parameter :: indx_PicarroTDL=indx_LI7200+1		! 15
	integer, parameter :: indx_LGRTDL=indx_PicarroTDL+1		! 16
	integer, parameter :: indx_IRGASON=indx_LGRTDL+1		! 17
	integer, parameter :: indx_EC155=indx_IRGASON+1			! 18
!
	integer, parameter :: num_insts=indx_EC155
!
	integer, parameter :: nbuf=2**17
	integer, parameter :: num_sections=5
	integer, parameter :: max_ser=16,max_chan=128,max_rot=128,max_cov=128
	integer, parameter :: max_cor=128
	integer, parameter :: ipar_chan=7,ipar_cov=6,ipar_cor=7
	integer, parameter :: maxtrys=5
	integer, parameter :: k_buf_len=10
	integer, parameter :: ig_buf_len=1200
	integer, parameter :: n_Picarro=12
	integer, parameter :: n_IRGASON=16
	integer, parameter :: n_EC155=16
!
	type ser_port
		integer(kind=4) kindx
		character(len=12) name
		integer(kind=4) ktype
		integer(kind=4) kcomm
		integer(kind=4) kbaud
		integer(kind=4) kbits
		integer(kind=4) kstop
		integer(kind=4) kparity
	end type ser_port
!
	type chan_pars
		integer(kind=4) kindx
		character(len=12) name
		integer(kind=4) kinst
		integer(kind=4) kseq
		integer(kind=4) keqn
		integer(kind=4) kdel
		integer(kind=4) kave
		integer(kind=4) kspec
		integer(kind=4) kqc
		real(kind=4) par(ipar_chan)
		real(kind=4) qc_up
		real(kind=4) qc_low
		real(kind=4) qc_spike
	end type chan_pars
!
	type rot_pars
		integer(kind=4) kindx
		character(len=12) name
		integer(kind=4) ktype
		integer(kind=4) krot
		integer(kind=4) kplot
		integer(kind=4) kwind
	end type rot_pars
!
	type cov_pars
		integer(kind=4) kindx
		character(len=12) name
		integer(kind=4) kch1
		integer(kind=4) kch2
		integer(kind=4) keqn
		integer(kind=4) kcospec
		real(kind=4) par(ipar_cov)
	end type cov_pars
!
	type corr_pars
		integer(kind=4) kindx
		character(len=12) name
		integer(kind=4) keqn
		real(kind=4) par(ipar_cor)
	end type corr_pars
!
	type time_struct
		integer(kind=4) kyear,kmon,kday,kdoy,khr,kmin,ksec,kzone
		character(len=8) year_str,mon_str1,mon_str2,day_str,doy_str,hr_str, &
		                 min_str,sec_str,time_str,zone_str
		real(kind=4) toy
	end type time_struct
!
	type (time_struct)::comp,inst
	type (ser_port)::serial(max_ser)
	type (chan_pars)::chan(max_chan)
	type (rot_pars)::rot(max_rot)
	type (cov_pars)::cov(max_cov)
	type (corr_pars)::corr(max_cor)
!
!	type single_moments
!		real(kind=8) rot(max_chan)
!		real(kind=8) rot_fil(max_chan)
!		real(kind=8) urot(max_chan)
!		real(kind=8) urot_fil(max_chan)
!	end type single_moments
!!
!	type mixed_moments
!		real(kind=8) rot(max_cov)
!		real(kind=8) urot(max_cov)
!	end type mixed_moments
!!
!	type (single_moments)::means
!	type (single_moments)::vars
!	type (mixed_moments)::covs
!!
	logical new_data(num_insts),file_manage
	logical first_time,first_pass,started,do_graphs,int_dat
	character(len=nbuf) inbuf(num_insts)
	character(len=16) inst_name(num_insts)
	character(len=8) inst_year_str,inst_mon_str1,inst_mon_str2,inst_day_str
	character(len=8) inst_doy_str,inst_time_str
	character(len=8) real_year_str,real_mon_str1,real_mon_str2,real_day_str
	character(len=8) real_doy_str,real_time_str
	integer(kind=4) hCom(num_insts)
	integer(kind=4) iop(num_insts),inp(num_insts)
	integer(kind=4) npkt(num_insts),npacket(num_insts)
	integer(kind=4) nBadSync(num_insts)
	integer(kind=4) ninst,nchnl,nrot,ncov,ncor,it_err
	integer(kind=4) irec,irecl,imax_rec,irec_div,irec_rem,irec_per,intervals_per
	integer(kind=4) samp_interval,i_1Sec,i_10Sec,i_1Sec_cnt
	integer(kind=4) i_10Sec_cnt,i_plen_cnt,iclk_del
	integer(kind=4) nscans,itoday_doy,idays,i_reboots,isleeptime,i_tim_err
	integer(kind=4) iu,iv,iw,it,n_Aerodyne
	integer(kind=4) inst_year,inst_mon,inst_day,inst_doy,inst_hr,inst_min,inst_sec
	integer(kind=4) ireal_year,ireal_mon,ireal_day,ireal_doy,ireal_hr,ireal_min,ireal_sec
	real(kind=4) samp_f,fnyq,Ftau,tim_err
	real(kind=4) f(max_chan),fil_par1,fil_par2
	real(kind=4) ybuf_old(max_chan+1,ig_buf_len),xbuf(ig_buf_len),w_buf(ig_buf_len) 
	real(kind=8) rot_means(max_chan),urot_means(max_chan)
	real(kind=8) rot_vars(max_chan),urot_vars(max_chan)
	real(kind=8) rot_covars(max_cov),urot_covars(max_cov,max_cov)
	real(kind=8) flux(max_cov),stddev(max_chan),correction(max_cor)
	real(kind=8) sums_1sec(max_chan),sums_1min(max_chan),sums_5min(max_chan)
	real(kind=8) sums_plen(max_chan+1)
	real(kind=8) squares_1sec(max_chan),squares_1min(max_chan)
	real(kind=8) squares_5min(max_chan),squares_plen(max_chan+1)
	real(kind=8) ameans_1sec(max_chan),ameans_1min(max_chan)
	real(kind=8) ameans_5min(max_chan),ameans_plen(max_chan+1)
	real(kind=8) stds_1sec(max_chan),stds_1min(max_chan),stds_5min(max_chan)
	real(kind=8) stds_plen(max_chan+1)
	real(kind=8) amean(max_chan),avar(max_chan)
	real(kind=8) ubar,vbar,wbar,usig,vsig,wsig,theta,eta,wdir,wspeed
	real(kind=8) graf_rnge(max_chan+1,3,2)
!   n_count (entry counter within summary -- e.g., half hour summary)
!   run_avg (running average for n first entries within summary)
!   run_var_n (running variance base - M2 - for n first entries within summary) 
	integer(kind=4) n_count
	real(kind=8) run_avg(max_chan)
	real(kind=8) run_var_n(max_chan)
!
	end module common_vars
!
!==============================================================================
!
!  Set up definitions and data structures for QuickWin console input
!
	module con_inp
!
	use dflib
	use ifmt
	use ifport
!
	logical flag
	character(len=1) ConBuf(0:255)
	integer(kind=4) ibufget,ibufput
	integer(kind=4) con_ThreadHandle
!
	type(RTL_CRITICAL_SECTION) con_char
	integer(kind=4), static ::isav_con_unit
!
	data flag /.false./
!
	contains
!
!=============================================================================+
!
!  Build the console input thread and start it
!
	subroutine PeekStart(iunit)
!
	integer(kind=4) iunit,j,m
!
	if (flag) return
	isav_con_unit=iunit
!
	ibufget=0
	ibufput=0
!
	call InitializeCriticalSection (loc(con_char))
	con_ThreadHandle=CreateThread(0,0,GetConChar,%loc(isav_con_unit), &
	                 CREATE_SUSPENDED,j)
	m=SetThreadPriority(con_ThreadHandle,THREAD_PRIORITY_ABOVE_NORMAL)
	m=ResumeThread(con_ThreadHandle)
!
	call sleepqq(100)
	flag=.true.
!
	return
	end subroutine PeekStart
!
!==============================================================================
!
!  This is the console input routine that runs in the console thread
!
	integer(kind=4) function GetConChar(iunit)
!
	integer(kind=4) j,k,iunit
	character(len=1) ch
!
	GetConChar=1
!
	do while(.true.)
		j=focusqq(iunit)
		ch=getcharqq()
!
		call EnterCriticalSection(loc(con_char))
		ibufput=IAND(ibufput+1,16#000000ff)
		if(ibufput.eq.ibufget) then
			ibufput=IAND(ibufput-1,16#000000ff)
		else
			ConBuf(ibufput)=ch
		end if
!
		call LeaveCriticalSection(loc(con_char))
	end do
!
	end function GetConChar
!
!==============================================================================
!
!  This is the user interface to check if a keyboard character is available
!
	logical(kind=4) function new_peek_char()
!
	call EnterCriticalSection(loc(con_char))
	if(ibufget.eq.ibufput) then
		new_peek_char=.false.
	else
		new_peek_char=.true.
	end if
	call LeaveCriticalSection(loc(con_char))
!
	end function new_peek_char
!
!==============================================================================
!
!  This is the user interface to retrieve a keyboard character from the buffer
!
	character(len=1) function Get_Con_Char()
!
	call EnterCriticalSection(loc(con_char))
	if(ibufget.eq.ibufput) then
		Get_Con_Char=char(0)
	else
		ibufget=IAND(ibufget+1,16#000000ff)
		Get_Con_Char=ConBuf(ibufget)
	end if
	call LeaveCriticalSection(loc(con_char))
!
	end function Get_Con_Char
!
!==============================================================================
!
	end module con_inp
!
!==============================================================================
!
	module comm_inputs
!
	contains
!
!==============================================================================
!
	subroutine Com_Start()
!
!  This routine starts the com port thread for the serial inputs
!
    use ifmt
!
	integer(kind=4) Com_thread
!
	Com_thread= CreateThread(0,0,Com_read,0,CREATE_SUSPENDED,j)
	m=SetThreadPriority(Com_Thread,THREAD_PRIORITY_NORMAL)
	n=ResumeThread(Com_Thread)
!
	call sleepqq(100)
!
	return
	end subroutine Com_Start
!
!==============================================================================
!
	subroutine Com_read ()
!
!  This routine reads the Com ports and puts the data in the ring buffers
!
	use common_vars
	use serial_vars
!
	character(len=nbuf) readbuf(num_insts)
	integer(kind=4) n,event
	integer(kind=4) nbr(num_insts)
	integer(kind=4) nrd(num_insts)
	integer(kind=4) errors(num_insts)
	type (T_COMSTAT)::CommStat(num_insts)
!
5	do i=1,ninst
15		n=ClearCommError(hCom(i),loc(errors(i)),CommStat(i))
		nrd(i)=CommStat(i)%cbInQue
		if (nrd(i).eq.0)then
			cycle
		else
			n=ReadFile(hCom(i),loc(readbuf(i)),nrd(i),loc(nbr(i)), &
			  NULL_OVERLAPPED)
			do j=1,nbr(i)
				inbuf(i)(inp(i):inp(i))=readbuf(i)(j:j)
				call badinc (inp(i),1)
			end do
		end if
		goto 15
	end do
!
	goto 5
!
	return
	end subroutine Com_read
!
!==============================================================================
!
	end module comm_inputs
!
!==============================================================================
!
!  Main routine
!
	program HuskerFlux
!
	use common_vars
	use serial_vars
	use file_names
	use con_inp
	use graph_params
	use comm_inputs
    use Instrument_Info
!
	logical issync(num_insts)
!
	logical data_available
	logical ATIbinSync,R3binSync,WMPbinSync,CSATbinSync,Y81000binSync
	logical LI7500ascSync,LI6262ascSync,LI7000ascSync,LI820ascSync
	logical LI840ascSync,LI7700ascSync,AerodyneTDLascSync,LI7500AascSync
	logical LI7200ascSync,PicarroTDLascSync,LGRTDLascSync,IRGASONascSync
	logical EC155ascSync
!
	character(len=1) key1in,key2in
	character(len=5) portname
	character(len=40) tmpstring,frmat
	character(len=nbuf) readbuf
	integer(kind=2),allocatable :: iraw_buf(:,:)
	integer(kind=4) i,icnt,istat
	real(kind=4) y(max_chan+1)
	real(kind=4),allocatable :: dat_buf(:,:),dat_buf_fil(:,:)
	real(kind=4),allocatable :: dat_buf_del(:,:),araw_buf(:,:)
!
10	format (1X,'Starting at ',A,' on ',A,'/',A,'/',A)
!
!==============================================================================
!
!  initialize the "blank" strings and clear character storage
!
    do i=1,info_str_len
        info_blank(i:i)=' '
    end do
    run_start=info_blank
    do i=1,max_ser
        Inst_Inf(i)%Firmware=info_blank
        Inst_Inf(i)%SerialNo=info_blank
        do j=1,max_info_strings
            Inst_Inf(i)%info_string(j)=info_blank
        end do
    end do
!
!  get the setup parameters
!
    File_manage=.false.
	Call Update_Comp_Time ()
    run_start='Run started on '//comp%mon_str1(1:2)//'/'//comp%day_str(1:2)//'/'//comp%year_str(1:4)//'     '//comp%time_str(1:8)//'     UTC '//comp%zone_str(1:3)//'     DOY '//comp%doy_str(1:3)
	call get_par(setup_file)
	call sleepqq(isleeptime)
	do i=1,ninst
		call Verify_ports (serial(i))
	end do
!
	npkt(indx_ATI)=10
	inst_name(indx_ATI)='ATI'
	npkt(indx_R3)=27
	inst_name(indx_R3)='R3'
	npkt(indx_WMP)=28
	inst_name(indx_WMP)='WMP'
	npkt(indx_CSAT)=12
	inst_name(indx_CSAT)='CSAT'
	npkt(indx_Y81000)=20
	inst_name(indx_Y81000)='Y81000'
	npkt(indx_LI7500)=33
	inst_name(indx_LI7500)='LI7500'
	npkt(indx_LI6262)=38
	inst_name(indx_LI6262)='LI6262'
	npkt(indx_LI7000)=36
	inst_name(indx_LI7000)='LI7000'
	npkt(indx_LI820)=38
	inst_name(indx_LI820)='LI820'
	npkt(indx_LI840)=38
	inst_name(indx_LI840)='LI840'
	npkt(indx_LI7700)=140
	inst_name(indx_LI7700)='LI7700'
	npkt(indx_AerodyneTDL)=16+n_Aerodyne*12+(9-n_Aerodyne)
	inst_name(indx_AerodyneTDL)='AerodyneTDL'
	npkt(indx_LI7500A)=117
	inst_name(indx_LI7500A)='LI7500A'
	npkt(indx_LI7200)=210
	inst_name(indx_LI7200)='LI7200'
	npkt(indx_PicarroTDL)=17+(n_Picarro*11)
	inst_name(indx_PicarroTDL)='PicarroTDL'
	npkt(indx_LGRTDL)=266
	inst_name(indx_LGRTDL)='LGRTDL'
	npkt(indx_IRGASON)=130
	inst_name(indx_IRGASON)='IRGASON'
	npkt(indx_EC155)=130
	inst_name(indx_EC155)='EC155'
	do i=1,num_insts
		npacket(i)=npkt(i)*3
	end do
!
	frmat(1:32)='(xx(1X,A3,"=",F8.2)," sec=",I9)'
	write (frmat(2:3),'(I2.2)') nchnl
!
!  make the frame window fill the screen and build the child windows
!
	call BuildWindows()
	i=SETACTIVEQQ(idat_win)
!
!  Build a new console for this app and start watching for keyboard characters
!
	call PeekStart(idat_win)
!
	idays=0
!
	call Build_File_Names()
	call Build_Files()
	call Prime_Inst_Time ()
	call Update_Comp_Time ()
	if (do_graphs) call UpdateClock()
!
	if (i_tim_err.ne.0) then
		write (imsg_win,*) 'Note:  due to your choice of sample interval,', &
		                   ' your day will be ',tim_err,' seconds short'
	end if
!
!  Allocate the data buffers
!
	allocate (iraw_buf(nchnl,(i_10sec)),stat=ialloc_err)
	allocate (araw_buf(nchnl,(i_10sec)),stat=ialloc_err)
	allocate (dat_buf(nchnl,(i_10sec)),stat=ialloc_err)
	allocate (dat_buf_fil(nchnl,(i_10sec)),stat=ialloc_err)
	allocate (dat_buf_del(nchnl,(i_10sec)),stat=ialloc_err)
!
!  Define the serial ports to use and set them up for read and write
!
	do i=1,ninst
		call setupcom(serial(i),hCom(i))
		iop(i)=1
		inp(i)=iop(i)
	end do
!
!  Start reading the ports
!
	call Com_Start()
!
!  Initialize the instruments and synchronize the data packets
!
!
	do i=1,ninst
		select case (serial(i)%ktype)
			case default
				write (imsg_win,*) 'Bad serial device type. Fix the entry in',&
				                   ' SETUP.PAR'
				call stop_pgm()
!
			case (indx_ATI)								! ATI sonic
				itrys=0
				call InitATI(serial(i)%kbaud,i)
15				issync(i)=ATIbinSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'ATI sonic data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 15
				write (imsg_win,*) 'ATI sonic data stream synchronized'
!
			case (indx_R3)								! Gill R3 sonic
				itrys=0
				call InitR3(serial(i)%kbaud,i)
25				issync(i)=R3binSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'R3 sonic data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 25
				write (imsg_win,*) 'R3 sonic data stream synchronized'
!
			case (indx_WMP)								! Gill WindMaster Pro sonic
				itrys=0
				call InitWMP(serial(i)%kbaud,i)
35				issync(i)=WMPbinSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'WMP sonic data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 35
				write (imsg_win,*) 'WMP sonic data stream synchronized'
!
			case (indx_CSAT)							! Campbell CSAT-3 sonic
				itrys=0
				call InitCSAT(serial(i)%kbaud,i)
45				issync(i)=CSATbinSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'CSAT sonic data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 45
				write (imsg_win,*) 'CSAT sonic data stream synchronized'
!
			case (indx_Y81000)							! R.M. Young 81000 sonic
				itrys=0
				call InitY81000(serial(i)%kbaud,i)
55				issync(i)=Y81000binSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'Y81000 sonic data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 55
				write (imsg_win,*) 'Y81000 sonic data stream synchronized'
!
			case (indx_LI7500)							! LiCor LI-7500 IRGA
				itrys=0
				call InitLI7500(serial(i)%kbaud,i)
65				issync(i)=LI7500ascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'LI-7500 IRGA data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 65
				write (imsg_win,*) 'LI-7500 IRGA data stream synchronized'
!
			case (indx_LI6262)							! LiCor LI-6262 IRGA
				itrys=0
				call InitLI6262(serial(i)%kbaud,i)
75				issync(i)=LI6262ascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'LI-6262 IRGA data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 75
				write (imsg_win,*) 'LI-6262 IRGA data stream synchronized'
!
			case (indx_LI7000)							! LiCor LI-7000 IRGA
				itrys=0
				call InitLI7000(serial(i)%kbaud,i)
85				issync(i)=LI7000ascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'LI-7000 IRGA data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 85
				write (imsg_win,*) 'LI-7000 IRGA data stream synchronized'
!
			case (indx_LI820)							! LiCor LI-820 IRGA
				itrys=0
				call InitLI820(serial(i)%kbaud,i)
95				issync(i)=LI820ascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'LI-820 IRGA data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 95
				write (imsg_win,*) 'LI-820 IRGA data stream synchronized'
!
			case (indx_LI840)							! LiCor LI-840 IRGA
				itrys=0
				call InitLI840(serial(i)%kbaud,i)
105				issync(i)=LI840ascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'LI-840 IRGA data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 105
				write (imsg_win,*) 'LI-840 IRGA data stream synchronized'
!
			case (indx_LI7700)							! LiCor LI-7700 CH4 IRGA
				itrys=0
				call InitLI7700(serial(i)%kbaud,i)
115				issync(i)=LI7700ascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'LI-7700 IRGA data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 115
				write (imsg_win,*) 'LI-7700 IRGA data stream synchronized'
!
			case (indx_AerodyneTDL)							! Aerodyne COS TDLAS
				itrys=0
				call InitAerodyneTDL(serial(i)%kbaud,i)
125				issync(i)=AerodyneTDLascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'Aerodyne TDLAS data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 125
				write (imsg_win,*) 'Aerodyne TDLAS data stream synchronized'
!
			case (indx_LI7500A)							! LiCor LI-7500A IRGA
				itrys=0
				call InitLI7500A(serial(i)%kbaud,i)
135				issync(i)=LI7500AascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'LI-7500A IRGA data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 135
				write (imsg_win,*) 'LI-7500A IRGA data stream synchronized'
!
			case (indx_LI7200)							! LiCor LI-7200 IRGA
				itrys=0
				call InitLI7200(serial(i)%kbaud,i)
145				issync(i)=LI7200ascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'LI-7200 IRGA data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 145
				write (imsg_win,*) 'LI-7200 IRGA data stream synchronized'
!
			case (indx_PicarroTDL)						! Picarro TDLAS
				itrys=0
				call InitPicarroTDL(serial(i)%kbaud,i)
155				issync(i)=PicarroTDLascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'Picarro TDLAS data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 155
				write (imsg_win,*) 'Picarro TDLAS data stream synchronized'
!
			case (indx_LGRTDL)							! Los Gatos TDLAS
				itrys=0
				call InitLGRTDL(serial(i)%kbaud,i)
165				issync(i)=LGRTDLascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'Los Gatos TDLAS data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 165
				write (imsg_win,*) 'Los Gatos TDLAS data stream synchronized'
!
			case (indx_IRGASON)							! IRGASON IRGA/sonic
				itrys=0
				call InitIRGASON(serial(i)%kbaud,i)
175				issync(i)=IRGASONascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'IRGASON IRGA data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 175
				write (imsg_win,*) 'IRGASON IRGA data stream synchronized'
!
			case (indx_EC155)							! EC155 IRGA
				itrys=0
				call InitEC155(serial(i)%kbaud,i)
185				issync(i)=EC155ascSync(i)
				itrys=itrys+1
				if (itrys.gt.5) then
					write (imsg_win,*) 'EC155 IRGA data stream not synchronizing'
					call stop_pgm()
				end if
				if (.not.issync(i)) goto 185
				write (imsg_win,*) 'EC155 data stream synchronized'
!
		end select
	end do
!
!  The instruments are now set up, running, and their input streams are
!  synchronized with their data packet lengths.  Because the setup procedures
!  are sometimes long, we now want to synchronize the data packets in the
!  buffers by just dumping all of the old data up to the current packet.
!
	write (imsg_win,*) ' Doing inter-instrument sync'
!
	do i=1,ninst
		nchr=nbyts(iop(i),inp(i))
!
		if (serial(i)%ktype.eq.indx_LI7700) then
			n=0
			iLI7700cntr=iop(i)
			do j=1,nchr
				if (inbuf(i)(iLI7700cntr:iLI7700cntr).eq.'D') n=n+1
				call badinc(iLI7700cntr,1)
			end do
			ncount=n
			do
				call badinc(iop(i),1)
				if (inbuf(i)(iop(i):iop(i)).eq.'D') ncount=ncount-1
				if (ncount.eq.1) exit
			end do
			call badinc(iop(i),1)
!
		else if ((serial(i)%ktype.eq.indx_LI7500A).or. &
				 (serial(i)%ktype.eq.indx_LI7200)) then
			n=0
			k_indx=iop(i)
			k_indxP1=iop(i)
			i_curptr=iop(i)
			i_prevptr=iop(i)
			call badinc(k_indxP1,1)
			do j=1,nchr
				if ((inbuf(i)(k_indx:k_indx).eq.char(13)).and. &
				   (inbuf(i)(k_indxP1:k_indxP1).eq.char(10))) then
					n=n+1
					i_prevptr=i_curptr
					i_curptr=k_indx
				end if
				call badinc(k_indx,1)
				call badinc(k_indxP1,1)
			end do
			n_skip=nbyts(iop(i),i_prevptr)
			call badinc(iop(i),n_skip)
!
		else
			n=nchr/npkt(serial(i)%ktype)
			nskip=(n-1)*npkt(serial(i)%ktype)
			call badinc(iop(i),nskip)
		end if
		write(imsg_win,*) ' dumped ',n-1,inst_name(serial(i)%ktype),' packets'
	end do
!
!  The system is now set up and ready for data collection.
!
!  Initialize a bunch of things
!
	first_time=.true.
	first_pass=.true.
	started=.false.
	i_1Sec_cnt=0
	i_10Sec_cnt=0
	ipltcnt=1
	icnt=1
!
	do i=1,iplen
		xbuf(i)=float(i-1)/samp_f
		do j=1,nchnl
			ybuf_old(j,i)=0.0
		end do
	end do
!
	do i=1,nchnl
		do j=1,i_10sec
			iraw_buf(i,j)=0
			dat_buf_del(i,j)=0.0
		end do
	end do
	call Init_Statistics()
!
!  Calculate the beginning raw file record and synchronize things with
!  the clock.
!
	call Update_Comp_Time ()
	irec=int(float(comp%khr*3600+comp%kmin*60+comp%ksec)*samp_f)+1
	iwrtbuf=mod(irec,i_10sec)
	irec=irec-iwrtbuf
	iwrtbuf=iwrtbuf+1
	idenom=1000/samp_interval
!
	write (imsg_win,*)
	write (imsg_win,10) comp%time_str(1:8),comp%mon_str1(1:2), &
						comp%day_str(1:2),comp%year_str(1:4)
	write (imsg_win,*)
	write (imsg_win,*) 'Writing raw data to: ',raw_name(1:len_file_names)
!
!  This is the main loop:
!  The data streams are now syncronized and we can start collecting data.
!
	do
		call Get_New_Data(iraw_buf,araw_buf,data_available,iwrtbuf)
		call Check_Key_Press()
		if (data_available) then
!
			call Units (iraw_buf,araw_buf,dat_buf,iwrtbuf)
			call Update_int_stats(dat_buf,ameans_1sec,stds_1sec,sums_1sec, &
			                      squares_1sec,i_1Sec_cnt,i_1Sec,iwrtbuf)
			call Filter(dat_buf,dat_buf_fil,iwrtbuf)
			call Update_Statistics(dat_buf,dat_buf_fil,dat_buf_del,iwrtbuf)
!
!  If this is the end of a 1 second period, update the 1 second stuff
!
			if (i_1sec_cnt.eq.0) then
				iclk_tmp=iwrtbuf
				call Update_Inst_Time (irec+iclk_tmp)
				call Update_Comp_Time ()
				if (icnt/idenom.gt.1) then
					write (idat_win,frmat(1:32)) ((chan(j)%name(1:3), &
					      ameans_1Sec(j)),j=1,nchnl),icnt/idenom
				end if
			end if
!
!  If graphics are turned on, update the screen
!
			if (do_graphs) then
				if (wind) then
					call Update_wind_stats(dat_buf,ameans_plen(max_chan+1), &
										stds_plen(max_chan+1),sums_plen(max_chan+1), &
										squares_plen(max_chan+1),i_plen_cnt, &
										iplen,iwrtbuf)
				end if
				call Update_int_stats(dat_buf,ameans_plen,stds_plen,sums_plen, &
			                      squares_plen,i_plen_cnt,iplen,iwrtbuf)
				if (i_1Sec_cnt.eq.0) then
					call UpdateClock()
				end if
				if (i_plen_cnt.eq.0) then
					call Graph_Range(ameans_plen,stds_plen,first_time)
					first_pass=.false.
				end if
				if (.not.first_pass) then
					do i=1,nchnl
						y(i)=dat_buf(i,iwrtbuf)
					end do
					if (wind) then
						y(max_chan+1)=sqrt(y(iu)**2+y(iv)**2)
					end if
					call plot_data(y,ipltcnt,iwrtbuf)
				end if
				ipltcnt=ipltcnt+1
				if (ipltcnt.gt.iplen) ipltcnt=1
			end if
!
!  If this is the end of an averaging period, update the output file
!
			if (mod(irec+iwrtbuf,irec_per).eq.0) then
				if (started) then
					call Do_Averages()
					call Rotate()
					call fluxes()
					call corrections()
					call Write_Averages(iwrtbuf)
				else
					started=.true.
					call Init_Statistics()
				end if
			end if
!
!  If this is the end of a 10 second period, write the buffer to disk
!
			if (iwrtbuf.eq.i_10sec) then
				call write_buffer (iraw_buf,araw_buf,iwrtbuf)
				irec=irec+iwrtbuf
				iwrtbuf=1
			else
				iwrtbuf=iwrtbuf+1
			end if
			icnt=icnt+1
!
!  Prepare to loop again
!
			first_time=.false.
			do i=1,num_insts
				new_data(i)=.false.
			end do
		end if
	end do
!
!  Deallocate the data buffers
!
	deallocate (iraw_buf,stat=ialloc_err)
	deallocate (araw_buf,stat=ialloc_err)
	deallocate (dat_buf,stat=ialloc_err)
	deallocate (dat_buf_fil,stat=ialloc_err)
	deallocate (dat_buf_del,stat=ialloc_err)
!		
	call stop_pgm()
	end program HuskerFlux
!
!==============================================================================
!
!  SUBROUTINE setupcom (serial,ihandle)
!
!  This subroutine configures and starts I/O on the Com port specified in
!  "iport".  It initializes the port to "ibits" data bits, "istop" stop bits,
!  and the parity defined in "iparity".  "iparity" can take the values:
!  0=none, 1=odd, 2=even, 3=mark, 4=space.  "istop" can take the values:
!  0=1, 1=1.5, 2=2.  Currently, the input and output buffers associated with
!  "iport" are 2^^n (n is defined in common_vars) bytes for received data and
!  1024 bytes for transmitted data.
!  serial is a structure of type ser_port that contains the communication
!  parameters
!
!==============================================================================
!
	subroutine setupcom (sserial,ihandle)
!
	use common_vars
	use serial_vars
	use file_names
!
	type (T_DCB)::dcbCom
	type (T_COMMTIMEOUTS)::Comtos
	type (ser_port)::sserial
!
	integer(kind=4) ihandle,evt_mask,n
	character(len=12) portname
	character(len=4) portnum
!
10	format (I1.1)
20	format (I2.2)
30	format (I3.3)
!
	if ((sserial%kstop.ne.0).and.(sserial%kstop.ne.2)) sserial%kstop=0
!
!  build the COM port name.  The valid port numbers are 1 - 256
!
	iser=sserial%kcomm
	if ((iser.ge.1).and.(iser.lt.10)) then
		write (portnum(1:1),10) iser
		portname(1:9)='\\.\COM'//portnum(1:1)//char(0)
	else if ((iser.ge.10).and.(iser.lt.100)) then
		write (portnum(1:2),20) iser
		portname(1:10)='\\.\COM'//portnum(1:2)//char(0)
	else
		write (portnum(1:3),30) iser
		portname(1:11)='\\.\COM'//portnum(1:3)//char(0)
	end if
!
!  open the COM port and start the set up process
!
	itrys=0
	do
		ihandle=CreateFile (portname,IOR(GENERIC_READ,GENERIC_WRITE),NULL, &
	                  NULL_SECURITY_ATTRIBUTES,OPEN_EXISTING, &
					  FILE_ATTRIBUTE_NORMAL,NULL)
		if (ihandle.eq.INVALID_HANDLE_VALUE) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error opening port ',portname,'  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) stop
		else
			exit
		end if
	end do
!
!  Set the input and output buffer lengths
!
	itrys=0
	do
		n=SetupComm(ihandle,nbuf,ioutbuflen)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error configuring port ',portname,'  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) stop
		else
			exit
		end if
	end do
!
!  Set up the timeouts
!
	Comtos%ReadIntervalTimeout=MAXDWORD
	Comtos%ReadTotalTimeoutMultiplier=0
	Comtos%ReadTotalTimeoutConstant=0
	Comtos%WriteTotalTimeoutMultiplier=0
	Comtos%WriteTotalTimeoutConstant=0
!
	itrys=0
	do
		n=SetCommTimeouts (ihandle,Comtos)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error setting Com Timeouts ',portname, &
			                    '  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) stop
		else
			exit
		end if
	end do
!
!  configure the baudrate, wordlength, stopbits, and parity
!
	itrys=0
	do
		n=GetCommState (ihandle,dcbCom)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error getting CommState ',portname,'  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) stop
		else
			exit
		end if
	end do
!
	dcbCom%BitField=1
	dcbCom%BaudRate=sserial%kbaud
	dcbCom%ByteSize=sserial%kbits
	dcbCom%Parity=sserial%kparity
	dcbCom%StopBits=sserial%kstop
!
	itrys=0
	do
		n=SetCommState (ihandle,dcbCom)
		if (n.eq.0) then
		n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error setting Com State ',portname,'  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) stop
		else
			exit
		end if
	end do
!
	n=PurgeComm (ihandle,IOR(PURGE_TXCLEAR,IOR(PURGE_RXCLEAR, &
	             IOR(PURGE_TXABORT,PURGE_RXABORT))))
!
!  configure the port to respond to recieved characters
!
	evt_mask=EV_RXCHAR
	n=SetCommMask (ihandle,evt_mask)
!
!  the port should be configured and active now
!
	return
	end subroutine setupcom
!
!==============================================================================
!
!  SUBROUTINE Set_Baud (ibaud,iun)
!
!  This routine sets the baud rate for the port selected by iun.
!
!==============================================================================
!
	subroutine Set_Baud (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	integer(kind=4) ibaud,iun,evt_mask
!
	type (T_DCB)::dcbCom
!
	itrys=0
	do
		n=GetCommState (hCom(iun),dcbCom)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error 1 setting baud rate for IUN ',iun,':  error =',n
			write (imsg_win,*) 'Error 1 setting baud rate for IUN ',iun,':  error =',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	dcbCom%BaudRate=ibaud
	itrys=0
	do
		n=SetCommState (hCom(iun),dcbCom)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error 2 setting baud rate for IUN ',iun,':  error =',n
			write (imsg_win,*) 'Error 2 setting baud rate for IUN ',iun,':  error =',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	itrys=0
	do
		n=PurgeComm (hCom(iun),IOR(PURGE_TXCLEAR,IOR(PURGE_RXCLEAR, &
					IOR(PURGE_TXABORT,PURGE_RXABORT))))
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error 3 setting baud rate for IUN ',iun,':  error =',n
			write (imsg_win,*) 'Error 3 setting baud rate for IUN ',iun,':  error =',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	evt_mask=EV_RXCHAR
	itrys=0
	do
		n=SetCommMask (hCom(iun),evt_mask)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error 4 setting baud rate for IUN ',iun,':  error =',n
			write (imsg_win,*) 'Error 4 setting baud rate for IUN ',iun,':  error =',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
!
	end subroutine Set_Baud	
!
!==============================================================================
!
!  SUBROUTINE Close_Port (ihandle)
!
!  This routine closes and releases the port associated with ihandle.
!
!==============================================================================
!
	subroutine Close_Port (ihandle)
!
	use common_vars
!
	integer(kind=4) ihandle,n
!
	n=CloseHandle(ihandle)
!
	return
	end subroutine Close_Port
!
!==============================================================================
!
!  SUBROUTINE Wait (ihs)
!
!  delays execution for ihs hundredths of a second
!
!==============================================================================
!
	subroutine Wait (ihs)
!
	integer(kind=4) istart,istop,inow,irate
!
	call system_clock (istart,irate)
	istop=istart+((ihs*irate)/100)
	do
		call system_clock(inow,irate)
		if (inow.ge.istop) then
			return
		end if
	end do
!
	end subroutine Wait
!
!==============================================================================
!
!  SUBROUTINE badinc (ipoint,inc)
!
!  This routine is used to increment (and decrement) the input buffer pointer.
!  It functions such that the buffer wraps around on itself, making it a ring
!  buffer.  "ipoint" is the name of the pointer to be updated and "inc" is the
!  amount to increment (or decrement) the pointer.
!
!==============================================================================
!
	subroutine badinc (ipoint,inc)
!
	use common_vars
!
	integer(kind=4) ipoint,inc
!
	ipoint=ipoint+inc
	if (ipoint.gt.nbuf) ipoint=ipoint-nbuf
	if (ipoint.le.0) ipoint=nbuf+ipoint
!
	return
	end subroutine badinc
!
!==============================================================================
!
!	integer*FUNCTION nbyts (iop,inp)
!
!  This function returns the number of unprocessed bytes in the buffer
!  associated with the two pointers iop and inp.
!
!==============================================================================
!
	integer(kind=4) function nbyts (ioop,innp)
!
	use common_vars
!
	integer(kind=4) n,itmp
!
	itmp=innp
	n=0
	if ((itmp.lt.0).or.(ioop.lt.0)) then
		innp=0
		ioop=0
		n=0
	else if (itmp.ge.ioop) then
		n=itmp-ioop
	else if (itmp.lt.ioop) then
		n=nbuf-ioop+itmp
	end if
	nbyts=n
	return
	end function nbyts
!
!==============================================================================
!
!	SUBROUTINE Check_Key_Press ()
!
!  This routine checks for a key press and then processes it.
!
!==============================================================================
!
	subroutine Check_Key_Press ()
!
	use con_inp
	use common_vars
	use file_names
!
	character(len=1) key1in,key2in
!
	if(new_peek_char()) then
		Key1In=Get_Con_Char()
		if((key1in.eq.'q').or.(key1in.eq.'Q')) then
			call Stop_pgm()
		end if
	end if
!
	return
	end subroutine Check_Key_Press
!
!==============================================================================
!
!	SUBROUTINE Stop_pgm ()
!
!  This routine closes any open files, releases the COM ports, and stops
!  It is a clean way to exit the program.
!
!==============================================================================
!
	subroutine Stop_pgm ()
!
	use con_inp
	use common_vars
	use file_names
!
	write (imsg_win,*) 'Stopping HuskerFlux'
	close (unit=iraw_unit)
	close (unit=iout_unit)
	close (unit=ierr_unit)
!
	do i=1,num_insts
		call Close_Port(hCom(i))
	end do
	stop
!
	return
	end subroutine Stop_pgm
!
!==============================================================================
!
!	INTEGER FUNCTION StopATI (iun)
!
!  This routine stops the ATI sonic anemometer and puts it into the command
!  mode.
!
!==============================================================================
!
	integer function StopATI (iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=4) cmd1
	character(len=1000) retstring
	integer(kind=4) nchr,n,nwr,nbw
!
	cmd1(1:1)=char(27)
	nwr=1
!
	iop(iun)=inp(iun)
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(cmd1),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error stopping hCom(indx_ATI):  Error = ',n
			write (imsg_win,*) 'Error stopping hCom(indx_ATI):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call wait(60)
	nchr= nbyts (iop(iun),inp(iun))
	nchr=min(nchr,1000)
	do ii=1,nchr
		retstring(ii:ii)=inbuf(iun)(iop(iun):iop(iun))
		call badinc(iop(iun),1)
	end do
	if (index(retstring(1:nchr),'Technologies',BACK=.TRUE.).eq.0) then
		StopATI=1
		return
	end if
	StopATI=0
!
	return
	end function StopATI
!
!==============================================================================
!
!  INTEGER FUNCTION StopR3 (iun)
!
!  This routine stops the Gill R3 sonic anemometer and puts it into the command
!  mode.
!
!==============================================================================
!
	integer function StopR3 (iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=4) cmd1
	character(len=5) resp1
	integer(kind=4) nchr,n,nwr,nbw
!
	cmd1(1:3)='im'//char(13)
	nwr=3
	do i=1,5
		iop(iun)=inp(iun)
		itrys=0
		do
			n=WriteFile (hCom(iun),loc(cmd1),nwr,loc(nbw),NULL_OVERLAPPED)
			if (n.eq.0) then
				n=GetLastError ()
				open (unit=ierr_unit,file=err_name,position='append')
				write (ierr_unit,*) 'Error stopping hCom(indx_R3):  Error = ',n
				write (imsg_win,*) 'Error stopping hCom(indx_R3):  Error = ',n
				close (unit=ierr_unit)
				itrys=itrys+1
				if (itrys.gt.maxtrys) call stop_pgm()
			else
				exit
			end if
		end do
		call wait(20)
		nchr=nbyts(iop(iun),inp(iun))
		iop(iun)=inp(iun)
		ipnt=iop(iun)
		call badinc(ipnt,-4)
		do ii=1,4
			resp1(ii:ii)=inbuf(iun)(ipnt:ipnt)
			call badinc(ipnt,1)
		end do
		if (resp1(1:3).eq.'R3>') then
			iop(iun)=inp(iun)
			StopR3=0
			return
		end if
	end do
	iop(iun)=inp(iun)
	StopR3=1
!
	return
	end function StopR3
!
!==============================================================================
!
!  INTEGER FUNCTION StopWMP (iun)
!
!  This routine stops the Gill WindMaster Pro sonic anemometer and puts it into
!  the command mode.
!
!==============================================================================
!
	integer function StopWMP (iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=4) cmd1,cmd2
	character(len=1000) resp1
	integer(kind=4) nchr,n,nwr,nbw
!
    cmd1(1:4)='Q'//char(13)
	cmd2(1:1)='*'
	nwr1=3
    nwr2=1
	do i=1,5
		iop(iun)=inp(iun)
		itrys=0
		do
			n=WriteFile (hCom(iun),loc(cmd1),nwr1,loc(nbw),NULL_OVERLAPPED)
			if (n.eq.0) then
				n=GetLastError ()
				open (unit=ierr_unit,file=err_name,position='append')
				write (ierr_unit,*) 'Error stopping hCom(indx_WMP):  Error = ',n
				write (imsg_win,*) 'Error stopping hCom(indx_WMP):  Error = ',n
				close (unit=ierr_unit)
				itrys=itrys+1
				if (itrys.gt.maxtrys) call stop_pgm()
			else
				exit
			end if
		end do
		call wait(40)
		do
			n=WriteFile (hCom(iun),loc(cmd2),nwr2,loc(nbw),NULL_OVERLAPPED)
			if (n.eq.0) then
				n=GetLastError ()
				open (unit=ierr_unit,file=err_name,position='append')
				write (ierr_unit,*) 'Error stopping hCom(indx_WMP):  Error = ',n
				write (imsg_win,*) 'Error stopping hCom(indx_WMP):  Error = ',n
				close (unit=ierr_unit)
				itrys=itrys+1
				if (itrys.gt.maxtrys) call stop_pgm()
			else
				exit
			end if
		end do
		call wait(40)
		nchr=nbyts(iop(iun),inp(iun))
		nchr=min(nchr,1000)
		do ii=1,nchr
			resp1(ii:ii)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
		if ((index(resp1(1:nchr),'CONFIG',BACK=.TRUE.).ne.0).or. &
		   (index(resp1(1:nchr),'*',BACK=.TRUE.).ne.0)) then
			iop(iun)=inp(iun)
			StopWMP=0
			return
		end if
	end do
	iop(iun)=inp(iun)
	StopWMP=1
!
	return
	end function StopWMP
!
!==============================================================================
!
!	INTEGER*FUNCTION StopCSAT (iun)
!
!  This routine stops the Campbell CSAT3 sonic anemometer and puts it into the
!  command mode.
!
!==============================================================================
!
	integer function StopCSAT (iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=4) cmd1
	character(len=1000) retstring
	integer(kind=4) nchr,n,nwr,nbw
!
	cmd1(1:1)='T'
	nwr=1
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(cmd1),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error stopping hCom(indx_CSAT):  Error = ',n
			write (imsg_win,*) 'Error stopping hCom(indx_CSAT):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait(40)
	nchr=nbyts (iop(iun),inp(iun))
	nchr=min(nchr,1000)
	do ii=1,nchr
		retstring(ii:ii)=inbuf(iun)(iop(iun):iop(iun))
		call badinc(iop(iun),1)
	end do
	if (index(retstring(1:nchr),'>',BACK=.TRUE.).ne.0) then
		StopCSAT=0
		return
	end if
	StopCSAT=1
!
	return
	end function StopCSAT
!
!==============================================================================
!
!	INTEGER*FUNCTION StopY81000 (iun)
!
!  This routine stops the R.M. Young 81000 sonic anemometer and puts it into
!  the command mode.
!
!==============================================================================
!
	integer function StopY81000 (iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=4) cmd1
	character(len=1000) retstring
	integer(kind=4) nchr,n,nwr,nbw
!
	cmd1(1:1)=char(27)
	nwr=1
!
	do i=1,3
		itrys=0
		do
			n=WriteFile (hCom(iun),loc(cmd1),nwr,loc(nbw),NULL_OVERLAPPED)
			if (n.eq.0) then
				n=GetLastError ()
				open (unit=ierr_unit,file=err_name,position='append')
				write (ierr_unit,*) 'Error stopping hCom(indx_Y81000):  Error = ',n
				write (imsg_win,*) 'Error stopping hCom(indx_Y81000):  Error = ',n
				close (unit=ierr_unit)
				itrys=itrys+1
				if (itrys.gt.maxtrys) call stop_pgm()
			else
				exit
			end if
		end do
		call wait(5)
	end do
	call Wait(40)
	nchr=nbyts (iop(iun),inp(iun))
	nchr=min(nchr,1000)
	do ii=1,nchr
		retstring(ii:ii)=inbuf(iun)(iop(iun):iop(iun))
		call badinc(iop(iun),1)
	end do
	if (index(retstring(1:nchr),'COMMANDS',BACK=.TRUE.).ne.0) then
		StopY81000=0
		return
	end if
	StopY81000=1
!
	return
	end function StopY81000
!
!==============================================================================
!
!	INTEGER*FUNCTION StopLI6262 (iun)
!
!  This routine stops the LiCor LI-6262 IRGA and puts it into the command
!  mode.
!
!==============================================================================
!
	integer function StopLI6262 (iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=8) cmd1
	character(len=1000) retstring
	integer(kind=4) nchr,n,nwr,nbw
!
	cmd1(1:7)='*14 0'//char(13)//char(10)
	nwr=7
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(cmd1),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error stopping hCom(indx_LI6262):  Error = ',n
			write (imsg_win,*) 'Error stopping hCom(indx_LI6262):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait(40)
	nchr=nbyts (iop(iun),inp(iun))
	nchr=min(nchr,1000)
	do ii=1,nchr
		retstring(ii:ii)=inbuf(iun)(iop(iun):iop(iun))
		call badinc(iop(iun),1)
	end do
	if (index(retstring(1:nchr),'>',BACK=.TRUE.).ne.0) then
		StopLI6262=0
		return
	end if
	StopLI6262=1
!
	return
	end function StopLI6262
!
!==============================================================================
!
!	INTEGER*FUNCTION StopLI7000 (iun)
!
!  This routine stops the LiCor LI-7000 IRGA and puts it into the command
!  mode.
!
!==============================================================================
!
	integer function StopLI7000 (iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=22) cmd1
	character(len=1000) retstring
	integer(kind=4) nchr,n,nwr,nbw
!
	cmd1(1:22)='(RS232 (Rate Polled))'//char(10)
	nwr=22
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(cmd1),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error stopping hCom(indx_LI7000):  Error = ',n
			write (imsg_win,*) 'Error stopping hCom(indx_LI7000):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait(40)
	nchr=nbyts (iop(iun),inp(iun))
	nchr=min(nchr,1000)
	do ii=1,nchr
		retstring(ii:ii)=inbuf(iun)(iop(iun):iop(iun))
		call badinc(iop(iun),1)
	end do
	if (index(retstring(1:nchr),'OK',BACK=.TRUE.).ne.0) then
		StopLI7000=0
		return
	end if
	StopLI7000=1
!
	return
	end function StopLI7000
!
!==============================================================================
!
!	INTEGER*FUNCTION StopLI820 (iun)
!
!  This routine stops the LiCor LI-820 IRGA and puts it into the command
!  mode.
!
!==============================================================================
!
	integer function StopLI820 (iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=50) cmd1
	character(len=1000) retstring
	integer(kind=4) nchr,n,nwr,nbw
!
	cmd1(1:47)='<LI820><CFG><OUTRATE>0</OUTRATE></CFG></LI820>'//char(10)
	nwr=47
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(cmd1),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error stopping hCom(indx_LI820):  Error = ',n
			write (imsg_win,*) 'Error stopping hCom(indx_LI820):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait(40)
	nchr=nbyts (iop(iun),inp(iun))
	nchr=min(nchr,1000)
	do ii=1,nchr
		retstring(ii:ii)=inbuf(iun)(iop(iun):iop(iun))
		call badinc(iop(iun),1)
	end do
	if (index(retstring(1:nchr),'<ack>true</ack>',BACK=.TRUE.).ne.0) then
		StopLI820=0
		return
	end if
	StopLI820=1
!
	return
	end function StopLI820
!
!==============================================================================
!
!	SUBROUTINE SendDataATI (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the ATI sonic anemometer.  It also returns
!  any response strings back in retstring.  wrtbuf contains the command to
!  send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataATI (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_ATI):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_ATI):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call wait(50)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		write (imsg_win,*) ' No return from the last command'
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataATI
!
!==============================================================================
!
!  SUBROUTINE SendDataR3 (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the Gill R3 sonic anemometer.  It also
!  returns any response strings back in retstring.  wrtbuf contains the command
!  to send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataR3 (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_R3):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_R3):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call wait(50)
	if (wrbuf(1:nwr-1).eq.'exit') then
		inp(iun)=iop(iun)
		return
	end if
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		write (imsg_win,*) ' No return from the last command'
		return
	else
		ipnt=iop(iun)
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(ipnt:ipnt)
			call badinc(ipnt,1)
		end do
		call badinc(iold,nchr)
	end if
!
	iop(iun)=inp(iun)
	return
	end subroutine SendDataR3
!
!==============================================================================
!
!  SUBROUTINE SendDataWMP (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the Gill WindMaster Pro sonic anemometer.
!  It also returns any response strings back in retstring.  wrtbuf contains
!  the command to send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataWMP (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_WMP):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_WMP):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call wait(50)
	if (wrbuf(1:1).eq.'Q') then
		inp(iun)=iop(iun)
		return
	end if
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		write (imsg_win,*) ' No return from the last command'
		return
	else
		ipnt=iop(iun)
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(ipnt:ipnt)
			call badinc(ipnt,1)
		end do
		call badinc(iold,nchr)
	end if
!
	iop(iun)=inp(iun)
	return
	end subroutine SendDataWMP
!
!==============================================================================
!
!	SUBROUTINE SendDataCSAT (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the Campbell CSAT sonic anemometer.  It also
!  returns any response strings back in retstring.  wrtbuf contains the command
!  to send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataCSAT (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_CSAT):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_CSAT):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (100)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		write (imsg_win,*) ' No return from the last command'
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataCSAT
!
!==============================================================================
!
!	SUBROUTINE SendDataY81000 (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the R.M. Young 81000 sonic anemometer.  It
!  also returns any response strings back in retstring.  wrtbuf contains the
!  command to send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataY81000 (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_Y81000):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_Y81000):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (100)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		write (imsg_win,*) ' No return from the last command'
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataY81000
!
!==============================================================================
!
!	SUBROUTINE SendDataLI7500 (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the LiCor LI-7500 IRGA.  It also returns
!  any response strings back in retstring.  wrtbuf contains the command to
!  send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataLI7500 (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		inp(6)=iop(6)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_LI7500):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_LI7500):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (1000)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataLI7500
!
!==============================================================================
!
!	SUBROUTINE SendDataLI6262 (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the LiCor LI-6262 IRGA.  It also returns
!  any response strings back in retstring.  wrtbuf contains the command to
!  send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataLI6262 (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_LI6262):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_LI6262):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (300)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataLI6262
!
!==============================================================================
!
!	SUBROUTINE SendDataLI7000 (wrbuf,nwr,retstring,ideliun)
!
!  This routine sends a command to the LiCor LI-7000 IRGA.  It also returns
!  any response strings back in retstring.  wrtbuf contains the command to
!  send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataLI7000 (wrbuf,nwr,retstring,idel,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n,idel
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_LI7000):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_LI7000):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (idel)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		write (imsg_win,*) ' No return from the last command'
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataLI7000
!
!==============================================================================
!
!	SUBROUTINE SendDataLI820 (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the LiCor LI-820 IRGA.  It also returns
!  any response strings back in retstring.  wrtbuf contains the command to
!  send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataLI820 (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_LI820):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_LI820):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (100)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		write (imsg_win,*) ' No return from the last command'
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataLI820
!
!==============================================================================
!
!	SUBROUTINE SendDataLI7700 (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the LiCor LI-7700 CH4 IRGA.  It also returns
!  any response strings back in retstring.  wrtbuf contains the command to
!  send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataLI7700 (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_LI7700):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_LI7700):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (100)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		write (imsg_win,*) ' No return from the last command'
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataLI7700
!
!==============================================================================
!
!	SUBROUTINE SendDataLI7500A (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the LiCor LI-7500A IRGA.  It also returns
!  any response strings back in retstring.  wrtbuf contains the command to
!  send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataLI7500A (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		inp(6)=iop(6)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_LI7500A):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_LI7500A):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (1000)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataLI7500A
!
!==============================================================================
!
!	SUBROUTINE SendDataLI7200 (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the LiCor LI-7200 IRGA.  It also returns
!  any response strings back in retstring.  wrtbuf contains the command to
!  send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataLI7200 (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		inp(6)=iop(6)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_LI7200):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_LI7200):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (1000)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataLI7200
!
!==============================================================================
!
!	SUBROUTINE SendDataPicarroTDL (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the Picarro G2311-f TDLAS.  It also returns
!  any response strings back in retstring.  wrtbuf contains the command to
!  send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataPicarroTDL (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		inp(6)=iop(6)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_PicarroTDL):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_PicarroTDL):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (1000)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataPicarroTDL
!
!==============================================================================
!
!	SUBROUTINE SendDataLGRTDL (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the Los Gatos 911-0010 TDLAS. It also returns
!  any response strings back in retstring.  wrtbuf contains the command to
!  send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataLGRTDL (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		inp(6)=iop(6)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_LGRTDL):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_LGRTDL):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (1000)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataLGRTDL
!
!==============================================================================
!
!	SUBROUTINE SendDataIRGASON (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the Campbell IRGASON IRGA.  It also returns
!  any response strings back in retstring.  wrtbuf contains the command to
!  send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataIRGASON (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		inp(6)=iop(6)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_IRGASON):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_IRGASON):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (1000)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataIRGASON
!
!==============================================================================
!
!	SUBROUTINE SendDataEC155 (wrbuf,nwr,retstring,iun)
!
!  This routine sends a command to the Campbell EC155 IRGA.  It also returns
!  any response strings back in retstring.  wrtbuf contains the command to
!  send and nwr is the length of the command.
!
!==============================================================================
!
	subroutine SendDataEC155 (wrbuf,nwr,retstring,iun)
!
	use common_vars
	use serial_vars
	use file_names
!
	character(len=*) wrbuf,retstring
	integer(kind=4) nwr,nbw,n
!
	itrys=0
	do
		n=WriteFile (hCom(iun),loc(wrbuf),nwr,loc(nbw),NULL_OVERLAPPED)
		inp(6)=iop(6)
		if (n.eq.0) then
			n=GetLastError ()
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,*) 'Error sending to hCom(indx_EC155):  Error = ',n
			write (imsg_win,*) 'Error sending to hCom(indx_EC155):  Error = ',n
			close (unit=ierr_unit)
			itrys=itrys+1
			if (itrys.gt.maxtrys) call stop_pgm()
		else
			exit
		end if
	end do
	call Wait (1000)
	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.le.0) then
		return
	else
		do i=1,nchr
			retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
			call badinc(iop(iun),1)
		end do
	end if
!
	return
	end subroutine SendDataEC155
!
!==============================================================================
!
!	SUBROUTINE InitATI (ibaud,iun)
!
!  This routine stops and configures an ATI sonic anemometer.  The routine
!  probes and determines what baud rate the ATI is currently running at.  It
!  then matches the COM port to that baud rate.  Note that there is no way to
!  change the baud rate of the ATI sonic without cycling power to the unit.
!  The routine then steps through the command menu and comfigures the unit.
!  It then writes the configuration information to the information file and
!  re-starts the data stream before returning.
!
!==============================================================================
!
	subroutine InitATI (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=1000
!
	character(len=ret_strng_len) retstring,retstring2,blank
	character(len=5) cmd1,cmd2,cmd3,cmd4, baud_string
	integer(kind=4) ibaud,evt_mask,i_FWversion
	integer(kind=4) ibauds(5)
	integer StopATI
!
	type (T_DCB)::dcbCom
!
	data ibauds /115200,57600,38400,19200,9600/
!
10	format (1X,'Serial.No. ',A,5X,'firmware ',A)
20	format (1X,'Data quality algorithm (On)',/,' Wind speed scale: 0-20', &
            ' m/sec',/,' Sonic Temp. C',/,' Median filter (Off)',/, &
			' Shadow correction algorithm (On)',/,' Remove RH from Temp.', &
			' Calculation (Off)',/, &
			' Sample interval is ',A,' msec',/,' Requested Baud rate = ',I7,' bps', &
			/,' Actual Baud rate = ',I7,' bps')
!
	do i=1,ret_strng_len
		blank(i:i)=' '
	end do
	retstring=blank
	do i=1,5
		baud_string(i:i)=' '
	end do
	cmd2(1:1)=char(27)
!
	write (imsg_win,*)
	write (imsg_win,*) ' Configuring the ATI SATI-3SX anemometer'
!
	ifreq=samp_interval/5
	write (cmd3(1:3),'(I3.3)') ifreq
	write (cmd4(1:4),'(I4.4)') samp_interval
	cmd3(4:5)=char(13)//char(10)
	jbaud=0
	if ((ibaud.ne.115200).and.(ibaud.ne.57600).and.(ibaud.ne.38400).and. &
	   (ibaud.ne.19200).and.(ibaud.ne.9600)) then
		write (imsg_win,*) ' Incorrect baud rate for ATI.  Fix the setup file'
		call stop_pgm()
	end if
!
	if (ibaud.eq.115200) then
		baud_string(1:1)='F'
	else if (ibaud.eq.57600) then
		baud_string(1:1)='E'
	else if (ibaud.eq.38400) then
		baud_string(1:1)='D'
	else if (ibaud.eq.19200) then
		baud_string(1:1)='C'
	else if (ibaud.eq.9600) then
		baud_string(1:1)='B'
	end if
!
!  find the current baud rate
!
	write (imsg_win,*)
	do i=1,5
		write (imsg_win,*) ' Trying ',ibauds(i),' baud'
        call Set_Baud (ibauds(i),iun)
		do ii=1,3
			ifail=StopATI(iun)
			if (ifail.eq.0) then
				jbaud=ibauds(i)
				goto 15
			end if
		end do
	end do
!
15	if (jbaud.eq.0) then
		write (imsg_win,*) ' Could not determine baud rate'
		call stop_pgm()
	end if
	write (imsg_win,*) ' Current baud rate is ',jbaud
!
	i_FWversion=1
	retstring=blank
	call SendDataATI (cmd2,1,retstring,iun)
	if (index(retstring(1:1000),'Version 2.1.',BACK=.TRUE.).ne.0) then
		i_FWversion=2
	end if
!	
!  set up data collection parameters
!
	if (i_FWversion.eq.1) then
!	write (imsg_win,*) ' i_FWversion = ',i_FWversion
		if (jbaud.ne.ibaud) then
			write (imsg_win,*) ' Requested baud rate is ',ibaud
			write (imsg_win,*) ' This firmware does not allow simple'
			write (imsg_win,*) ' baud rate changes.'
			write (imsg_win,*) ' Continuing with ',jbaud,' baud.'
		end if
!
		cmd1(1:1)='A'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
!
		if (index(retstring(1:1000),'Parity (Off)',BACK=.TRUE.).eq.0) then
			cmd1(1:1)='B'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'ASCII Output (Off)',BACK=.TRUE.).eq.0) then
			cmd1(1:1)='C'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Terse ASCII Output (On)',BACK=.TRUE.).eq.0) then
			cmd1(1:1)='D'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Binary Output (On)',BACK=.TRUE.).eq.0) then
			cmd1(1:1)='E'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Break Binary (Off)',BACK=.TRUE.).eq.0) then
			cmd1(1:1)='F'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Output Cs (Off)',BACK=.TRUE.).eq.0) then
			cmd1(1:1)='G'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Output Temp (On)',BACK=.TRUE.).eq.0) then
			cmd1(1:1)='H'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Output Wind Speed & Direction (Off)', &
		    BACK=.TRUE.).eq.0) then
			cmd1(1:1)='I'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		cmd1(1:1)='0'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
		cmd1(1:1)='B'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
		if (index(retstring(1:1000),'Median Filter (Off)',BACK=.TRUE.).eq.0) then
			cmd1(1:1)='A'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Data Quality Algorithm (On)', &
		    BACK=.TRUE.).eq.0) then
			cmd1(1:1)='B'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Shadow Correction Algorithm (On)', &
		    BACK=.TRUE.).eq.0) then
			cmd1(1:1)='C'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Remove RH from Temp Calculation (Off)', &
		    BACK=.TRUE.).eq.0) then
			cmd1(1:1)='D'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		cmd1(1:1)='0'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
		cmd1(1:1)='C'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
		if (index(retstring(1:1000),'External Triggering (Off)', &
		    BACK=.TRUE.).eq.0) then
			cmd1(1:1)='A'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Character Triggering (Off)', &
		    BACK=.TRUE.).eq.0) then
			cmd1(1:1)='B'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
	else if (i_FWversion.eq.2) then
!	write (imsg_win,*) ' i_FWversion = ',i_FWversion
!
!  Now switch the sonic to the selected baud rate
!
		cmd1(1:1)='A'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
		cmd1(1:1)='A'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
		retstring=blank
		call SendDataATI (baud_string(1:1),1,retstring,iun)
		cmd1(1:1)='0'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
		cmd1(1:1)='0'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
		cmd1(1:1)='0'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
!
!  Switch the COM port to the selected baud rate
!
        call Set_Baud (ibaud,iun)
!
!  Stop the sonic and go to setup mode
!
		retstring=blank
		call SendDataATI (cmd2,1,retstring,iun)
!
		cmd1(1:1)='A'
		retstring2=blank
		call SendDataATI (cmd1,1,retstring2,iun)

		if (index(retstring2(1:1000),'Binary Output ................. (On)',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) 'turning Binary Output on'
			cmd1(1:1)='E'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring2(1:1000),'Terse Output .................. (On)',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) 'turning Terse Output on'
			cmd1(1:1)='D'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring2(1:1000),'Output Temp ................... (On)',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) 'turning Temperature Output on'
			cmd1(1:1)='G'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring2(1:1000),'Output Cs ..................... (Off)',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) 'turning SOS Output off'
			cmd1(1:1)='H'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring2(1:1000),'Output Wind Speed & Direction . (Off)',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) 'turning UVW output on'
			cmd1(1:1)='I'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		cmd1(1:1)='0'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
!
		cmd1(1:1)='B'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
		if (index(retstring(1:1000),'Median Filter ................... (Off)',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) 'turning median filter off'
			cmd1(1:1)='A'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Spike Detect Algorithm .......... (On)',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) 'turning spike detect algorithm on'
			cmd1(1:1)='B'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Shadow Correction Algorithm ..... (On)',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) 'turning shadow correction algorithm on'
			cmd1(1:1)='C'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Remove RH from Temp Calculation . (Off)',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) 'turning off fixed RH correction for T'
			cmd1(1:1)='D'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		cmd1(1:1)='0'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
		cmd1(1:1)='C'
		retstring=blank
		call SendDataATI (cmd1,1,retstring,iun)
		if (index(retstring(1:1000),'External Triggering .. (Off)',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) 'turning off external triggering'
			cmd1(1:1)='A'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
		if (index(retstring(1:1000),'Character Triggering . (Off)',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) 'turning off character triggering'
			cmd1(1:1)='B'
			retstring=blank
			call SendDataATI (cmd1,1,retstring2,iun)
		end if
	end if
	cmd1(1:1)='0'
	retstring=blank
	call SendDataATI (cmd1,1,retstring,iun)
	cmd1(1:1)='Z'
	retstring=blank
	call SendDataATI (cmd1,1,retstring,iun)
	cmd1(1:1)='A'
	retstring=blank
	call SendDataATI (cmd1,1,retstring,iun)
	retstring=blank
	call SendDataATI (cmd3,5,retstring,iun)
	cmd1(1:1)='0'
	retstring=blank
	call SendDataATI (cmd1,1,retstring,iun)
	cmd1(1:1)='E'
	retstring=blank
	call SendDataATI (cmd1,1,retstring,iun)
	i1=index(retstring(1:1000),'Version ',BACK=.TRUE.)
	Inst_Inf(iun)%Firmware=retstring(i1:i1+12)
	Inst_Inf(iun)%Firmware_len=len_trim(Inst_Inf(iun)%Firmware)
	if (i_FWversion.eq.1) then
		i1=index(retstring(1:1000),'Serial Number = ',BACK=.TRUE.)
		i1=i1+16
		Inst_Inf(iun)%SerialNo=retstring(i1:i1+5)
		Inst_Inf(iun)%SerialNo_len=len_trim(Inst_Inf(iun)%SerialNo)
	elseif (i_FWversion.eq.2) then
		i1=index(retstring(1:1000),'Serial# ',BACK=.TRUE.)
		i1=i1+8
		Inst_Inf(iun)%SerialNo=retstring(i1:i1+5)
		Inst_Inf(iun)%SerialNo_len=len_trim(Inst_Inf(iun)%SerialNo)
	end if
	cmd1(1:1)='0'
	retstring=blank
	call SendDataATI (cmd1,1,retstring,iun)
!
!  start the data stream
!
	cmd1(1:1)='0'
	retstring=blank
	call SendDataATI (cmd1,1,retstring,iun)
!
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'ATI SATI-3SX sonic anemometer'
	write (info_unit,10) Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
						 Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (imsg_win,10) Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
						Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (info_unit,20),cmd4,ibaud,jbaud
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	write (imsg_win,*) ' ATI sonic is configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitATI
!
!==============================================================================
!
!  SUBROUTINE InitR3 (ibaud,iun)
!
!  This routine stops and configures a Gill R3 sonic anemometer.  The routine
!  probes and determines what baud rate the anemometer is currently running at.
!  It then sets the anemometer to the selected baud rate and matches the COM
!  port to that baud rate.  The routine then sends commands to configure the
!  unit.  It then writes the configuration information to the information file
!  and re-starts the data stream before returning.
!
!==============================================================================
!
	subroutine InitR3 (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=1000
!
	character(len=3) irt
	character(len=40) cmd1,cmd2,cmd3,cmd4
	character(len=ret_strng_len) retstrng,vers
	integer(kind=4) ibaud,evt_mask
	integer(kind=4) jbaud,ibod
	integer(kind=4) ibauds(7)
	integer StopR3
!
	type (T_DCB)::dcbCom
!
	data ibauds /115200,57600,38400,19200,9600,4800,2400/
!
10	format (1X,'Serial.No. ',A,5X,'firmware ',A)
20	format (1X,'polar wrap angle: 540',/,' wind scale: 0-30 m/sec',/, &
            ' sonic temp. C',/,' PRT temp. C',/, &
			' 6 differential input channels, +- 5V, 14-bits',/, &
			' sample interval is ',A,'0 msec',/,1X,'Baud rate: ',I7,' bps')
!
!  check and set up the baud rate and the sampling rate, and build a few
!  other strings
!
	do i=1,istrng_len
		Inst_Inf(iun)%SerialNo(i:i)=' '
		Inst_Inf(iun)%Firmware(i:i)=' '
	end do
!
	write (imsg_win,*)
	write (imsg_win,*) ' Configuring the Gill R3 anemometer'
	write (irt(1:3),'(I3.3)') samp_interval/10
	jbaud=0
	cmd2(1:2)='B'//char(13)
	cmd1(1:5)='baud '
	if (ibaud.ge.100000) then
		write (cmd1(6:12),'(I6)') ibaud
		cmd1(12:12)=char(13)
		ibdig=12
	else if ((ibaud.lt.100000).and.(ibaud.ge.10000)) then
		write (cmd1(6:11),'(I5)') ibaud
		cmd1(11:11)=char(13)
		ibdig=11
	else
		write (cmd1(6:10),'(I4)') ibaud
		ibdig=10
		cmd1(10:10)=char(13)
	end if
!
!  determine what baud rate the anemometer is talking at and change it if
!  necessary
!
	do i=1,7
		write (imsg_win,*) ' Trying ',ibauds(i),' baud'
        call Set_Baud (ibauds(i),iun)
		do ii=1,3
			ifail=StopR3 (iun)
			if (ifail.eq.0) then
				jbaud=ibauds(i)
				goto 15
			end if
		end do
	end do
15	if (jbaud.eq.0) then
		write (imsg_win,*) ' Could not determine BAUD rate'
		call stop_pgm()
	end if
	write (imsg_win,*) ' Current BAUD rate is ',jbaud
!
	write (imsg_win,*) ' Setting Echo Off ---'
	cmd3(1:10)='echo off'//char(13)//char(10)
	call SendDataR3 (cmd3,10,retstrng,iun)
!
	write (imsg_win,*) ' Setting ASCII terminator to <CR> ---'
	cmd3(1:12)='ascterm cr'//char(13)//char(10)
	call SendDataR3 (cmd3,12,retstrng,iun)
!
	if (jbaud.ne.ibaud) then
		write (imsg_win,*) 'Setting BAUD rate to ',ibaud,' ---'
		call SendDataR3 (cmd1,ibdig,retstrng,iun)
		if (retstrng(1:9).ne.'Confirm >') then
			write (imsg_win,*) ' Could not set BAUD rate to ',ibaud
			write (imsg_win,*) ' retstrng = ',retstrng(1:20)
			stop
		end if
        call Set_Baud (ibaud,iun)
		iop(iun)=inp(iun)
		call SendDataR3 (cmd2,2,retstrng,iun)
		if (retstrng(1:5).ne.'VBAUD') then
			write (imsg_win,*) ' Baud rate set failed'
			call stop_pgm()
		end if
	end if
	ifail=StopR3 (iun)
!
!  now set up the anemometer
!
	write (imsg_win,*) ' Setting output format to binary ---'
	cmd3(1:4)='strfmt binary'//char(13)
	call SendDataR3 (cmd3,14,retstrng,iun)
!
	write (imsg_win,*) ' Setting reporting to continuous ---'
	cmd3(1:13)='msgmode cont'//char(13)
	call SendDataR3 (cmd3,13,retstrng,iun)
!
	write (imsg_win,*) ' Setting confidence tone OFF ---'
	cmd3(1:14)='ctone disable'//char(13)
	call SendDataR3 (cmd3,14,retstrng,iun)
!
	write (imsg_win,*) ' Setting sample interval to ',irt,'0 msec ---'
	cmd3(1:12)='average '//irt//char(13)
	call SendDataR3 (cmd3,12,retstrng,iun)
!
	write (imsg_win,*) ' Setting wind speed to UVW cal. mode ---'
	cmd3(1:16)='windrep uvw cal'//char(13)
	call SendDataR3 (cmd3,16,retstrng,iun)
!
	write (imsg_win,*) ' Setting UVW aligment to spar ---'
	cmd3(1:14)='alignuvw spar'//char(13)
	call SendDataR3 (cmd3,14,retstrng,iun)
!
	write (imsg_win,*) ' Setting polar wrap angle to 540 ---'
	cmd3(1:14)='polarwrap 540'//char(13)
	call SendDataR3 (cmd3,14,retstrng,iun)
!
	write (imsg_win,*) ' Setting analog output scale to 30 m/s ---'
	cmd3(1:10)='aopfsd 30'//char(13)
	call SendDataR3 (cmd3,10,retstrng,iun)
!
	write (imsg_win,*) ' Setting sonic temperature to C ---'
	cmd3(1:19)='sosrep sonictemp c'//char(13)
	call SendDataR3 (cmd3,19,retstrng,iun)
!
	write (imsg_win,*) ' Setting PRT element to C ---'
	cmd3(1:13)='abstemp on c'//char(13)
	call SendDataR3 (cmd3,13,retstrng,iun)
!
	write (imsg_win,*) ' Setting 6 analog input channels ---'
	cmd3(1:20)='anaip 1,2,3,4,5,6 e'//char(13)
	call SendDataR3 (cmd3,20,retstrng,iun)
!
	do i=1,ret_strng_len
		retstrng(i:i)=' '
	end do
	write (imsg_win,*) ' Getting the firmware version ---'
	cmd3(1:4)='ver'//char(13)
	call SendDataR3 (cmd3,4,retstrng,iun)
	vers(1:10)=retstrng(1:71)
	i0=index(retstrng(1:71),'Version',BACK=.FALSE.)
	i0=i0+8
	Inst_Inf(iun)%Firmware=retstrng(i0:i0+3)
	Inst_Inf(iun)%Firmware_len=len_trim(Inst_Inf(iun)%Firmware)
!
	do i=1,ret_strng_len
		retstrng(i:i)=' '
	end do
	write (imsg_win,*) ' Getting the serial number ---'
	cmd3(1:3)='sn'//char(13)
	call SendDataR3 (cmd3,3,retstrng,iun)
	Inst_Inf(iun)%SerialNo=retstrng(1:7)
	Inst_Inf(iun)%SerialNo_len=len_trim(Inst_Inf(iun)%SerialNo)
!
	cmd3(1:5)='exit'//char(13)
	call SendDataR3 (cmd3,5,retstrng,iun)
!
!  clear out the receiver buffer, write the info file, and return
!
	iop(iun)=1
	inp(iun)=iop(iun)
!
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
!
	if (vers(4:7).eq.'50Hz') then
		write (info_unit,*) 'Gill-Solent R3-50 Sonic Anemometer'
	else
		write (info_unit,*) 'Gill-Solent R3 Sonic Anemometer'
	end if
	write (info_unit,10) Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
						 Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (imsg_win,10) Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
						Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (info_unit,20),irt,ibaud
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	if (vers(4:7).eq.'50Hz') then
		write (imsg_win,*) ' R3-50 sonic is configured and active'
	else
		write (imsg_win,*) ' R3 sonic is configured and active'
	end if
	write (imsg_win,*)
!
	return
	end subroutine InitR3
!
!==============================================================================
!
!  SUBROUTINE InitWMP (ibaud,iun)
!
!  This routine stops and configures a Gill WindMaster Pro sonic anemometer.
!  The routine probes and determines what baud rate the anemometer is currently
!  running at.  It then sets the anemometer to the selected baud rate and
!  matches the COM port to that baud rate.  The routine then sends commands to
!  configure the unit.  It then writes the configuration information to the
!  information file and re-starts the data stream before returning.
!
!==============================================================================
!
	subroutine InitWMP (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
    use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=1000
!
	character(len=4) irt
	character(len=40) psvolt,cmd1,cmd2,cmd3,cmd4
	character(len=ret_strng_len) retstrng
	integer(kind=4) ibaud,evt_mask
	integer(kind=4) jbaud,ibod
	integer(kind=4) ibauds(5)
	integer StopWMP
!
	type (T_DCB)::dcbCom
!
	data ibauds /38400,19200,9600,4800,2400/
!
10	format (1X,'Serial.No. ',A,5X,'firmware version ',A)
20	format (1X,'measurement averaging: off',/,' wind scale: 0-60 m/sec',/, &
            ' sonic temp. C',/,' 4 differential input channels, +- 5V, 14-bits', &
			/,' sample interval is ',I4,' msec',/,1X,'Baud rate: ',I6,' bps',/, &
			' Power supply: ',A,' volts')
!
!  check and set up the baud rate and the sampling rate, and build a few
!  other strings
!
	do i=1,istrng_len
		Inst_Inf(iun)%SerialNo(i:i)=' '
		Inst_Inf(iun)%Firmware(i:i)=' '
	end do
!
	write (imsg_win,*)
	write (imsg_win,*) ' Configuring the Gill WindMaster Pro anemometer'
	jbaud=0
	cmd2(1:3)='B'//char(13)//char(10)
	if (ibaud.eq.38400) then
		cmd1(1:4)='B5'//char(13)//char(10)
	else if (ibaud.eq.19200) then
		cmd1(1:4)='B4'//char(13)//char(10)
	else if (ibaud.eq.9600) then
		cmd1(1:4)='B3'//char(13)//char(10)
	else if (ibaud.eq.4800) then
		write (imsg_win,*) ' Warning:  4800 baud might be too slow for', &
		            ' the WMP.'
		cmd1(1:4)='B2'//char(13)//char(10)
	else if (ibaud.eq.2400) then
		write (imsg_win,*) ' Warning:  2400 baud might be too slow for', &
		            ' the WMP.'
		cmd1(1:4)='B1'//char(13)//char(10)
	else
		write (imsg_win,*) ' Invalid baud rate for WMP.  Fix', &
		            ' the setup file'
		call stop_pgm()
	end if
!
	if (samp_interval.ge.1000) then
		cmd3(1:4)='P1'//char(13)//char(10)
		itmp=1000
	else if ((samp_interval.le.500).and.(samp_interval.gt.333)) then
		cmd3(1:4)='P3'//char(13)//char(10)
		itmp=500
	else if ((samp_interval.le.333).and.(samp_interval.gt.250)) then
		cmd3(1:4)='P5'//char(13)//char(10)
		itmp=333
	else if ((samp_interval.le.250).and.(samp_interval.gt.200)) then
		cmd3(1:4)='P2'//char(13)//char(10)
		itmp=250
	else if ((samp_interval.le.200).and.(samp_interval.gt.100)) then
		cmd3(1:4)='P4'//char(13)//char(10)
		itmp=200
	else if (samp_interval.le.100) then
		cmd3(1:4)='P6'//char(13)//char(10)
		itmp=100
	end if
!
!  determine what baud rate the anemometer is talking at and change it if
!  necessary
!
	do i=1,7
		write (imsg_win,*) ' Trying ',ibauds(i),' baud'
        call Set_Baud (ibauds(i),iun)
		do ii=1,3
			ifail=StopWMP (iun)
			if (ifail.eq.0) then
				jbaud=ibauds(i)
				goto 15
			end if
		end do
	end do
15	if (jbaud.eq.0) then
		write (imsg_win,*) ' Could not determine baud rate'
		call stop_pgm()
	end if
!
! set to proper baud rate if not there already
!
	write (imsg_win,*) ' Current baud rate is ',jbaud
!
	if (jbaud.ne.ibaud) then
		write (imsg_win,*) ' Setting baud rate to ',ibaud
		call SendDataWMP (cmd1,4,retstrng,iun)
		call wait (20)
		if (index(retstrng(1:ret_strng_len),'confirm',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) ' Could not set baud rate to ',ibaud
			call stop_pgm()
		end if
        call Set_Baud (ibaud,iun)
		iop(iun)=inp(iun)
		call SendDataWMP (cmd2,3,retstrng,iun)
		call wait (20)
		if (index(retstrng(1:ret_strng_len),'B',BACK=.TRUE.).eq.0) then
			write (imsg_win,*) ' Baud rate set failed'
			call stop_pgm()
		end if
	end if
	ifail=StopWMP (iun)
	if (ifail.ne.0) then
		write (imsg_win,*) ' Could not stop the anemometer'
		call stop_pgm()
	end if
!
!  now set up the anemometer
!
	write (imsg_win,*) ' Setting UVW Binary Mode ---'
	cmd1(1:4)='M7'//char(13)//char(10)
	call SendDataWMP (cmd1,4,retstrng,iun)
!
	write (imsg_win,*) ' Setting units to m/s ---'
	cmd1(1:4)='U1'//char(13)//char(10)
	call SendDataWMP (cmd1,4,retstrng,iun)
!
	write (imsg_win,*) ' Setting fixed field ---'
	cmd1(1:4)='O2'//char(13)//char(10)
	call SendDataWMP (cmd1,4,retstrng,iun)
!
	write (imsg_win,*) ' Setting message terminator to <CR><LF> ---'
	cmd1(1:4)='L1'//char(13)//char(10)
	call SendDataWMP (cmd1,4,retstrng,iun)
!
	write (imsg_win,*) ' Setting sample interval to ',itmp,' msec ---'
	call SendDataWMP (cmd3,4,retstrng,iun)
!
	write (imsg_win,*) ' Setting measurement averaging off ---'
	cmd1(1:4)='G1'//char(13)//char(10)
	call SendDataWMP (cmd1,4,retstrng,iun)
!
	write (imsg_win,*) ' Setting UVW aligment to spar ---'
	cmd1(1:4)='X2'//char(13)//char(10)
	call SendDataWMP (cmd1,4,retstrng,iun)
!
	write (imsg_win,*) ' Enabling 4 differential analog inputs ---'
	cmd1(1:4)='J4'//char(13)//char(10)
	call SendDataWMP (cmd1,4,retstrng,iun)
!
	write (imsg_win,*) ' Getting the Gill firmware version ---'
	cmd1(1:4)='D4'//char(13)//char(10)
	call SendDataWMP (cmd1,4,retstrng,iun)
	Inst_Inf(iun)%Firmware=retstrng(5:10)
	Inst_Inf(iun)%Firmware_len=len_trim(Inst_Inf(iun)%Firmware)
!
	write (imsg_win,*) ' Getting the Gill serial number ---'
	cmd1(1:4)='D1'//char(13)//char(10)
	call SendDataWMP (cmd1,4,retstrng,iun)
	Inst_Inf(iun)%SerialNo=retstrng(5:11)
	Inst_Inf(iun)%SerialNo_len=len_trim(Inst_Inf(iun)%SerialNo)
!
	write (imsg_win,*) ' Getting the Power supply voltage ---'
	cmd1(1:4)='D5'//char(13)//char(10)
	call SendDataWMP (cmd1,4,retstrng,iun)
	psvolt(1:5)=retstrng(21:25)
!
	write (imsg_win,*) ' Starting data'
	cmd1(1:3)='Q'//char(13)//char(10)
	call SendDataWMP (cmd1,3,retstrng,iun)
!
!  clear out the receiver buffer, write the info file, and return
!
	iop(iun)=1
	inp(iun)=iop(iun)
!
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'Gill-Solent WindMaster Pro sonic anemometer'
	write (info_unit,10) Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
						 Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (imsg_win,10) Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
						Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (info_unit,20),itmp,ibaud,psvolt(1:5)
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	write (imsg_win,*) ' WindMaster Pro sonic is configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitWMP
!
!==============================================================================
!
!	SUBROUTINE InitCSAT (ibaud,iun)
!
!  This routine stops and configures a Campbell CSAT3 sonic anemometer.  The 
!  routine probes and determines what baud rate the CSAT3 is currently running
!  at.  It then matches the COM port to that baud rate.  Note that there is no
!  way to change the baud rate of the CSAT3 sonic without cycling power to the
!  unit.  The routine then steps through the command menu and comfigures the
!  unit.  It then writes the configuration information to the information file
!  and re-starts the data stream before returning.
!
!==============================================================================
!
	subroutine InitCSAT (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=1000
!
	character(len=ret_strng_len) retstring
	character(len=5) cmd1,cmd2,cmd3
	integer(kind=4) ibaud,ifreq,evt_mask
	integer(kind=4) ibauds(2)
	integer StopCSAT
!
	type (T_DCB)::dcbCom
!
	data ibauds /19200,9600/
!
10	format (1X,'Serial. No. ',A,5X,'Code revision No. ',A)
20	format (1X,'Analog outputs: off',/,' Sample interval is ',I5,' msec', &
			/, 'Baud rate: ',I6,' bps')
!
!  check and set up the baud rate and sampling rate, and build a few
!  other strings
!
	do i=1,istrng_len
		Inst_Inf(iun)%SerialNo(i:i)=' '
		Inst_Inf(iun)%FirmWare(i:i)=' '
	end do
!
	write (imsg_win,*)
	write (imsg_win,*) ' Configuring the Campbell Scientific CSAT3 anemometer'
	jbaud=0
	ifreq=samp_interval
	cmd1(1:3)='??'//char(13)
	if ((ibaud.ne.19200).and.(ibaud.ne.9600)) then
		write (imsg_win,*) ' Incorrect baud rate for CSAT.  Fix the setup file.'
		call stop_pgm()
	end if
!
	if (ifreq.le.17) then
		jfreq=17
		cmd2(1:3)='Ae'//char(13)
	else if ((ifreq.gt.17).and.(ifreq.le.33)) then
		jfreq=33
		cmd2(1:3)='Ad'//char(13)
	else if ((ifreq.gt.33).and.(ifreq.le.50)) then
		jfreq=50
		cmd2(1:3)='Ac'//char(13)
	else if ((ifreq.gt.50).and.(ifreq.le.67)) then
		jfreq=67
		cmd2(1:3)='Ab'//char(13)
	else if ((ifreq.gt.67).and.(ifreq.le.83)) then
		jfreq=83
		cmd2(1:3)='Aa'//char(13)
	else if ((ifreq.gt.83).and.(ifreq.le.100)) then
		jfreq=100
		cmd2(1:3)='A9'//char(13)
	else if ((ifreq.gt.100).and.(ifreq.le.167)) then
		jfreq=167
		cmd2(1:3)='A8'//char(13)
	else if ((ifreq.gt.167).and.(ifreq.le.200)) then
		jfreq=200
		cmd2(1:3)='A7'//char(13)
	else if ((ifreq.gt.200).and.(ifreq.le.333)) then
		jfreq=333
		cmd2(1:3)='A6'//char(13)
	else if ((ifreq.gt.333).and.(ifreq.le.500)) then
		jfreq=500
		cmd2(1:3)='A5'//char(13)
	else if (ifreq.gt.500) then
		jfreq=1000
		cmd2(1:3)='A2'//char(13)
	end if
!
!  find the current baud rate and change if necessary
!  NOTE:  as of 9/8/02, the baud rate couldn't be changed over a 3-wire
!         connection.  If new information comes up, this will change.
!
	write (imsg_win,*)
	do i=1,2
		write (imsg_win,*) ' Trying ',ibauds(i),' baud'
        call Set_Baud (ibauds(i),iun)
		do ii=1,3
			ifail=StopCSAT(iun)
			if (ifail.eq.0) then
				jbaud=ibauds(i)
				goto 15
			end if
		end do
	end do
!
15	if (jbaud.eq.0) then
		write (imsg_win,*) ' Could not determine the baud rate.'
		call stop_pgm()
	end if
	write (imsg_win,*) ' Current baud rate is ',jbaud
!
!  now set up the anemometer
!
	write (imsg_win,*)
	write (imsg_win,*) ' Setting sample freqency to',jfreq,' msec'
	write (imsg_win,*) ' This will take about 10 seconds to complete'
	call SendDataCSAT (cmd2,2,retstring,iun)
	call wait(1000)
	cmd3(1:1)=char(13)
	call sendDataCSAT (cmd3,1,retstring,iun)
!
!  turn off the analog outputs
!
	write (imsg_win,*)
	write (imsg_win,*) ' Turning off the analog outputs'
	cmd3(1:2)='O'//char(13)
	call SendDataCSAT(cmd3,2,retstring,iun)
!
!  get the current set up information
!
	call SendDataCSAT (cmd1,3,retstring,iun)
	itmp1=index(retstring(1:500),'SN',BACK=.TRUE.)
	itmp1=itmp1+2
	Inst_Inf(iun)%SerialNo=retstring(itmp1:itmp1+6)//' '//retstring(itmp1+7:itmp1+9) &
	            //' '//retstring(itmp1+10:itmp1+11)
	Inst_Inf(iun)%SerialNo_len=len_trim(Inst_Inf(iun)%SerialNo)
	itmp1=index(retstring(1:500),'RNA=',BACK=.TRUE.)
	itmp1=itmp1+10
	Inst_Inf(iun)%Firmware=retstring(itmp1:itmp1+3)
	Inst_Inf(iun)%Firmware_len=len_trim(Inst_Inf(iun)%Firmware)
	write (imsg_win,*)
!
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'Campbell CSAT3 sonic anemometer'
	write (info_unit,10) Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
						 Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (imsg_win,*) 'Campbell CSAT3 sonic anemometer'
	write (imsg_win,10) Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
						Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (info_unit,20) jfreq,jbaud
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	cmd3(1:1)=char(13)
	call SendDataCSAT(cmd3,1,retstring,iun)
	cmd3(1:1)='D'
	call SendDataCSAT(cmd3,1,retstring,iun)
!
	write (imsg_win,*) ' CSAT sonic is configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitCSAT
!
!==============================================================================
!
!	SUBROUTINE InitY81000 (ibaud,iun)
!
!==============================================================================
!
	subroutine InitY81000 (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	type (T_DCB)::dcbCom
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=1000
!
	character(len=ret_strng_len) retstring
	character(len=7) string_cmd
	character(len=5) cmd1,cmd2,cmd3,crlf
	integer(kind=4) ibaud,ifreq,evt_mask,ibaud_indx,ifreq_indx
	integer(kind=4) ibauds(6)
	integer StopY81000
!
	data ibauds /1200,2400,4800,9600,19200,38400/
!
10	format (1X,'Firmware revision No. ',A)
20	format (1X,'Analog outputs: off',/,' Sample interval is ',I4,' msec', &
			/, 'Baud rate: ',I6,' bps')
!
	string_cmd(1:7)='ABCDEFG'
	ifreq=samp_interval
	write (imsg_win,*)
	write (imsg_win,*) '  Configuring the R.M. Young Y81000-V anemometer'
!
!  Determine if the requested baud rate is legal, and if it is, set up the
!  proper baud rate command
!
	do i=1,istrng_len
		Inst_Inf(iun)%SerialNo(i:i)=' '
		Inst_Inf(iun)%Firmware(i:i)=' '
	end do
!
	jbaud=0
	do i=1,6
			if (ibaud.eq.ibauds(i)) then
			jbaud=ibaud
			ibaud_indx=i
		end if
	end do
	if (jbaud.eq.0) then
		write (imsg_win,*) '  Incorrect baud rate in .par file'
		call stop_pgm()
	end if
!
!  from the chosen output frequency, set up the command for the closest
!  available anemometer frequency
!
	iaver=0
	if (ifreq.le.31) ifreq_indx=7							! 32 Hz
	if ((ifreq.gt.31).and.(ifreq.le.50)) ifreq_indx=6		! 20 Hz
	if ((ifreq.gt.50).and.(ifreq.le.63)) ifreq_indx=5		! 16 Hz
	if ((ifreq.gt.63).and.(ifreq.le.100)) ifreq_indx=4		! 10 Hz
	if ((ifreq.gt.100).and.(ifreq.le.125)) ifreq_indx=3		! 8 Hz
	if ((ifreq.gt.125).and.(ifreq.le.200)) ifreq_indx=2		! 5 Hz
	if (ifreq.gt.200) then
		ifreq_indx=1										! 4 Hz
		if ((ifreq.gt.250).and.(ifreq.le.500)) iaver=2		! 2 Hz
		if ((ifreq.gt.500).and.(ifreq.le.1000)) iaver=4		! 1 Hz
		if ((ifreq.gt.1000).and.(ifreq.le.2000)) iaver=8	! 0.5 Hz
		if ((ifreq.gt.2000).and.(ifreq.le.5000)) iaver=20	! 0.2 Hz
		if ((ifreq.gt.5000).and.(ifreq.le.10000)) iaver=40	! 0.1 Hz
		if (ifreq.gt.10000) then
			write (imsg_win,*) '  Incorrect output frequency in .par file'
			stop
		end if
	end if
!
	write (cmd1(1:2),'(I2.2)') iaver
	cmd1(3:4)=char(13)//char(10)
	cmd2(1:3)='0'//char(13)//char(10)
	crlf(1:2)=char(13)//char(10)
!
!  determine the current baud rate of the Y81000
!
	jbaud=0
	write (imsg_win,*)
	do i=6,1,-1
		write (imsg_win,*) ' Trying ',ibauds(i),' baud'
        call Set_Baud (ibauds(i),iun)
		do ii=1,3
			ifail=StopY81000 (iun)
			if (ifail.eq.0) then
				jbaud=ibauds(i)
				goto 15
			end if
		end do
	end do
!
15	if (jbaud.eq.0) then
		write (imsg_win,*) ' Could not determine baud rate'
		call stop_pgm()
	end if
	write (imsg_win,*) ' Current Baud rate is ',jbaud
!
!  if the current baud rate isn't the requested one, change the baud rate
!  of the Y81000
!
	if (jbaud.ne.ibaud) then
		write (imsg_win,*) ' Setting baud rate to ',ibaud
!
		call SendDataY81000 ('S',1,retstring,iun)
		call SendDataY81000 ('B',1,retstring,iun)
		call SendDataY81000 (string_cmd(ibaud_indx:ibaud_indx),1,retstring,iun)
!
        call Set_Baud (ibaud,iun)
		call SendDataY81000 ('X',1,retstring,iun)
	end if
!
	call SendDataY81000 (char(27),1,retstring,iun)
	Inst_Inf(iun)%Firmware=retstring(13:26)
	Inst_Inf(iun)%Firmware_len=len_trim(Inst_Inf(iun)%Firmware)
	Inst_Inf(iun)%SerialNo='null'
	Inst_Inf(iun)%SerialNo_len=4
!
!  put the anemometer into the setup menu
!
	call SendDataY81000 ('S',1,retstring,iun)
!
!  set the output interval and the averaging length (these determine the output
!  frequency)
!
	write (imsg_win,*) ' Setting output interval to ',ifreq,' milliseconds'
	call SendDataY81000 ('O',1,retstring,iun)
	call SendDataY81000 (string_cmd(ifreq_indx:ifreq_indx),1,retstring,iun)
!
	call SendDataY81000 ('A',1,retstring,iun)
	call SendDataY81000 (cmd1(1:1),1,retstring,iun)
	call SendDataY81000 (cmd1(2:2),1,retstring,iun)
	call SendDataY81000 (crlf(1:1),1,retstring,iun)
!
!  set error handling to send invalid data
!
	write (imsg_win,*) ' Setting error handling to include invalid data'
	call SendDataY81000 ('E',1,retstring,iun)
	call SendDataY81000 ('1',1,retstring,iun)
!
!  set the output format to unprompted binary (NOTE: as of 12/16/03, there
!  were some problems actually getting the anemometer to shift into binary
!  mode)
!
	write (imsg_win,*) ' Setting output mode to unprompted binary'
	call SendDataY81000 ('S',1,retstring,iun)
	call SendDataY81000 ('5',1,retstring,iun)
!
!  set the units to m s-1
!
	write (imsg_win,*) ' Setting output units to m s-1'
	call SendDataY81000 ('U',1,retstring,iun)
	call SendDataY81000 ('2',1,retstring,iun)
!
!  turn on the wake correction
!
	write (imsg_win,*) ' Turning on wake correction'
	call SendDataY81000 ('W',1,retstring,iun)
	call SendDataY81000 ('Y',1,retstring,iun)
!
!  set the threshold wind speed (to report) to zero
!
	write (imsg_win,*) ' Setting threshold wind speed to zero'
	call SendDataY81000 ('T',1,retstring,iun)
	call SendDataY81000 ('0',1,retstring,iun)
	call SendDataY81000 (crlf(1:2),2,retstring,iun)
!
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'R M Young 81000V sonic anemometer'
	write (info_unit,10) Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (imsg_win,*)
	write (imsg_win,10) Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (imsg_win,*)
	write (info_unit,20) ifreq,jbaud
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	call SendDataY81000 ('X',1,retstring,iun)
	write (imsg_win,*)
	write (imsg_win,*) ' Y81000V sonic configured and active'
	call SendDataY81000 ('X',1,retstring,iun)
!
	return
	end subroutine InitY81000
!
!==============================================================================
!
!	SUBROUTINE InitLI7500 (ibaud,iun)
!
!==============================================================================
!
	subroutine InitLI7500 (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=7000
	type(T_DCB)::dcbcom
!
	character(len=ret_strng_len) retstring
	character(len=225) cmd3
	character(len=istrng_len) blank_strng,CO2caldate,H2Ocaldate
	character(len=36) mon_string
	character(len=5) cmd1,cmd2
	integer(kind=4) ibaud,jbaud,nwr,nbw,nbytes,inyq,i1,i2,iwcnt,evt_mask,imon
!
10	format (1X,'Serial.No. ',A,' Firmware ',A, &
            /,1X,'Last CO2 calibration: ',A,/,1X,'Last H2O calibration: ',A)
20	format (1X,'Baud rate = ',I7,' bps',/,1X,'Sampling rate = ',I3,'Hz',/,1X, &
            'Bandwidth = ',I3,'Hz')
30	format (I)
!
	mon_string(1:36)='JanFebMarAprMayJunJulAugSepOctNovDec'
	do i=1,istrng_len
		blank_strng(i:i)=' '
	end do
!
!  if we've already done this a few times, stop the program and flag an IRGA error
!
	iloop=1
5	if (iloop.gt.5) then
		open (unit=ierr_unit,file=err_name,position='append')
		write (ierr_unit,*) 'There was a problem initializing the LI-7500.  Check it'
		write (imsg_win,*) 'There was a problem initializing the LI-7500.  Check it'
		close (unit=ierr_unit)
		call stop_pgm()
	end if
!
	Inst_Inf(iun)%SerialNo=blank_strng
	Inst_Inf(iun)%Firmware=blank_strng
	CO2CalDate=blank_strng
	H2OCalDate=blank_strng
!
	write (imsg_win,*)
	write (imsg_win,*) '            Configuring the LI-7500'
	ifreq=int(samp_f)
!
!  check the baud rate
!
	if ((ibaud.ne.9600).and.(ibaud.ne.19200).and.(ibaud.ne.38400).and.(ibaud.ne.57600).and.(ibaud.ne.115200)) then
		write (imsg_win,*) ' Incorrect baud rate for LI-7500.  Edit your setup file'
		call stop_pgm()
	end if
	if ((ifreq.gt.10).and.(ibaud.ne.38400)) then
		ibaud=38400
	else if (((ifreq.le.10).and.(ifreq.gt.5)).and.(ibaud.lt.19200)) then
		ibaud=19200
	end if
!
!  check the sampling frequency, and figure out the best bandwidth
!
	if (ifreq.gt.20) then
		write (imsg_win,*) ' Incorrect sampling frequency.  Edit your setup file'
		call stop_pgm()
	end if
	if (ifreq.eq.20) then
		inyq=10
	else
		inyq=5
	end if
!
!  build the LI-7500 setup command strings
!
	cmd1(1:1)=char(6)
	cmd2(1:3)=')'//char(13)//char(10)
	cmd3(1:219)='(Outputs (BW xx)(Delay 0)(RS232 (Baud xxxxx)(Freq xx)'// &
	             '(Pres TRUE)(Temp TRUE)(Aux FALSE)(Cooler FALSE)'// &
				 '(CO2Raw FALSE)(CO2D TRUE)(H2ORaw FALSE)(H2OD TRUE)'// &
				 '(Ndx FALSE)(DiagVal TRUE)(DiagRec FALSE)(Labels FALSE)'// &
				 '(EOL "0D0A")))'//char(10)
	write (cmd3(14:15),'(I2)') inyq
	write (cmd3(39:43),'(I5)') ibaud
	write (cmd3(51:52),'(I2)') ifreq
!
!  set the serial port baud rate to 9600 (default known state for LI-7500)
!
	jbaud=9600
    call Set_Baud (jbaud,iun)
	iop(iun)=inp(iun)
!
!  now send a 500 msec BREAK to the IRGA, wait for an ENQ, and send an ACK
!
	write (imsg_win,*) ' Sending BREAK to IRGA'
!
	i1=0
15	iret=SetCommBreak(hCom(iun))
	call Wait (50)
	iret=ClearCommBreak(hCom(iun))
	iwcnt=1
!
	inp(iun)=iop(iun)
25	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.eq.0) then
		call Wait (100)
		if(iwcnt.gt.5) then
			write (imsg_win,*) 'LI-7500 not returning ENQ.  Check it.'
			call stop_pgm()
		end if
		iwcnt=iwcnt+1
		goto 25
	end if
	do i=1,nchr
		retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
		call badinc(iop(iun),1)
	end do
!
	if (index(retstring(1:nchr),char(5)).eq.0) then
		i1=i1+1
		if (i1.gt.5) then
			write (imsg_win,*) ' LI-7500 not responding.  Check it.'
			call stop_pgm()
		else
			goto 15
		end if
	end if
!
	do i=1,7000
		retstring(i:i)=' '
	end do
!
	call SendDataLI7500 (cmd1,1,retstring,iun)
	nchr=len_trim(retstring)
!
!  get the serial number and firmware version number
!
		i1=index(retstring(1:nchr),'(SerialNo',BACK=.TRUE.)
		Inst_Inf(iun)%SerialNo=retstring(i1+11:i1+18)
		Inst_Inf(iun)%SerialNo_len=len_trim(Inst_Inf(iun)%SerialNo)
		i1=index(retstring(1:nchr),'(Version',BACK=.TRUE.)
		Inst_Inf(iun)%Firmware=retstring(i1+1:i1+15)
		Inst_Inf(iun)%Firmware_len=len_trim(Inst_Inf(iun)%Firmware)
!
!  get the H2O and CO2 calibration dates
!
		i1=index(retstring(1:nchr),'(SpanH2O',BACK=.TRUE.)
		i2=index(retstring(i1:nchr),'"',BACK=.FALSE.)
		i1=i1+i2
		i2=index(retstring(i1:nchr),'"))(',BACK=.TRUE.)
		i2=i1+i2-1
		H2OCalDate(1:i2-i1)=retstring(i1:i2)
		do i=1,i2-i1
			if (H2OCalDate(i:i).eq.char(9)) H2OCalDate(i:i)=' '
		end do
!
		i1=index(retstring(1:nchr),'(SpanCO2',BACK=.TRUE.)
		i2=index(retstring(i1:nchr),'"',BACK=.FALSE.)
		i1=i1+i2
		i2=index(retstring(i1:nchr),'")))',BACK=.TRUE.)
		i2=i1+i2-1
		CO2CalDate(1:i2-i1)=retstring(i1:i2)
		do i=1,i2-i1
			if (CO2CalDate(i:i).eq.char(9)) CO2CalDate(i:i)=' '
		end do
!
!  write the information to the info file
!
		open (unit=info_unit,file=info_name, position='append')
		write (info_unit,*)
		write (info_unit,*) 'LiCor LI-7500 open path IRGA'
		write (info_unit,10) Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
							 Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len), &
							 CO2CalDate(1:25),H2OCalDate(1:25)
		write (info_unit,20) ibaud,ifreq,inyq
		write (info_unit,*)
		write (info_unit,*) '*******************************************************************************'
		close (unit=info_unit)
		write (imsg_win,*)
		write (imsg_win,*) 'LI-7500 Serial no. ',Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len)
		write (imsg_win,*) 'firmware ',Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
		write (imsg_win,*)
		write (imsg_win,*) 'baudrate: ',ibaud
		write (imsg_win,*) 'last CO2 cal.: ',CO2CalDate(1:25)
		write (imsg_win,*) 'last H2O cal.: ',H2OCalDate(1:25)
		write (imsg_win,*)
!
!  now set up the IRGA for data collection
!
	call SendDataLI7500 (cmd3,219,retstring,iun)
!
	call wait (200)
    call Set_Baud (ibaud,iun)
!
	if (samp_f.lt.0.1) then
		iwait=1000
	else
		iwait=int4(1000.0/samp_f)
	end if
	call wait (iwait)
	inum=nbyts(iop(iun),inp(iun))
	if (inum.lt.npacket(iun)) then
		iloop=iloop+1
		goto 5
	end if
!
	iop(iun)=inp(iun)
!
	write (imsg_win,*)
	write (imsg_win,*) ' LI-7500 IRGA is configured and active'
	write (imsg_win,*)
!
	iop(iun)=1
	inp(iun)=iop(iun)
!
	return
	end subroutine InitLI7500
!
!==============================================================================
!
!	SUBROUTINE InitLI7500A (ibaud,iun)
!
!==============================================================================
!
	subroutine InitLI7500A (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30, ret_strng_len=7000
!
	type(T_DCB)::dcbcom
!
	character(len=ret_strng_len) retstring
	character(len=1000) cmd3
	character(len=istrng_len) CO2ZeroDate,CO2SpanDate,H2OZeroDate,H2OSpanDate
	character(len=istrng_len) CO2ZerVal,CO2SpanVal,CO2SpanTar,H2OZerVal,H2OSpanVal,H2OSpanTar
	character(len=istrng_len) CXVal,WXVal,CO2A,CO2B,CO2C,CO2D,CO2E,CO2XS,CO2Z
	character(len=istrng_len) H2OA,H2OB,H2OC,H2OXS,H2OZ,blank_strng
	character(len=36) mon_string
	character(len=5) cmd1,cmd2
	integer(kind=4) ibaud,jbaud,nwr,nbw,nbytes,inyq,i0,i1,i2,iwcnt,evt_mask,imon
!
10	format (1X,'Serial.No. ',A,5X,'Firmware Version ',A,/,1X,'Last CO2 zero: ',A,5X, &
			'Last CO2 span: ',A,/,1X,'Last H2O zero: ',A,5X,'Last H2O span: ',A, &
			/,1X,'CO2 zero value ',A,3X,'CO2 span value ',A,3X,'CO2 span target ', &
			A,' umol/mol',/,1X,'H2O zero value ',A,3X,'H2O span value ',A,3X, &
			'H2O span target ',A,' C d.p.',/,1X,'CO2 A = ',A,3X,'CO2 B = ',A,3X, &
			'CO2 C = ',A,3X,'CO2 D = ',A,3X,'CO2 E = ',A,3X,'CO2 XS = ',A,3X, &
			'CO2 Z = ',A,/,1X,'H2O A = ',A,3X,'H2O B = ',A,3X,'H2O C = ', &
			A,3X,'H2O XS = ',A,3X,'H2O Z = ',A)
20	format (1X,'Baud rate = ',I7,' bps',/,1X,'Sampling rate = ',I3,'Hz',/,1X, &
            'Bandwidth = ',I3,'Hz')
30	format (I)
40	format (1X,'CO2 SSI = ',A,5X,'H2O SSI = ',A)
!
!  initialize some strings
!
	mon_string(1:36)='JanFebMarAprMayJunJulAugSepOctNovDec'
	do i=1,istrng_len
		blank_strng(i:i)=' '
	end do
!
!  if we've already done this a few times, stop the program and flag an IRGA error
!
	iloop=1
5	if (iloop.gt.5) then
		open (unit=ierr_unit,file=err_name,position='append')
		write (ierr_unit,*) 'There was a problem initializing the LI-7500A.  Check it'
		write (imsg_win,*) 'There was a problem initializing the LI-7500A.  Check it'
		close (unit=ierr_unit)
		call stop_pgm()
	end if
!
	Inst_Inf(iun)%SerialNo=blank_strng
	Inst_Inf(iun)%Firmware=blank_strng
	CO2ZeroDate=blank_strng
	CO2SpanDate=blank_strng
	H2OZeroDate=blank_strng
	H2OSpanDate=blank_strng
	CO2ZerVal=blank_strng
	CO2SpanVal=blank_strng
	CO2SpanTar=blank_strng
	H2OZerVal=blank_strng
	H2OSpanVal=blank_strng
	H2OSpanTar=blank_strng
	CO2A=blank_strng
	CO2B=blank_strng
	CO2C=blank_strng
	CO2D=blank_strng
	CO2E=blank_strng
	CO2XS=blank_strng
	CO2Z=blank_strng
	H2OA=blank_strng
	H2OB=blank_strng
	H2OC=blank_strng
	H2OXS=blank_strng
	H2OZ=blank_strng
	CXval=blank_strng
	WXval=blank_strng
!
	write (imsg_win,*)
	write (imsg_win,*) '            Configuring the LI-7500A'
	ifreq=int(samp_f)
!
!  check the baud rate
!
	if ((ibaud.ne.19200).and.(ibaud.ne.38400).and.(ibaud.ne.57600).and.(ibaud.ne.115200)) then
		write (imsg_win,*) ' Incorrect baud rate for LI-7500A.  Edit your setup file'
		call stop_pgm()
	end if
!
!  check the sampling frequency, and figure out the best bandwidth
!
	if (ifreq.gt.20) then
		write (imsg_win,*) ' Incorrect sampling frequency.  Edit your setup file'
		call stop_pgm()
	end if
	if (ifreq.eq.20) then
		inyq=10
	else
		inyq=5
	end if
!
!  build some of the LI-7500A setup command strings
!
	cmd1(1:1)=char(6)
	cmd2(1:3)=')'//char(13)//char(10)
!
!  set the serial port baud rate to 9600 (default known state for LI-7500A)
!
	jbaud=9600
    call Set_Baud (jbaud,iun)
	iop(iun)=inp(iun)
!
!  now send a 500 msec BREAK to the IRGA, wait for an ENQ, and send an ACK
!
	write (imsg_win,*) ' Sending BREAK to IRGA'
!
	i1=0
15	iret=SetCommBreak(hCom(iun))
	call Wait (50)
	iret=ClearCommBreak(hCom(iun))
	iwcnt=1
!
	inp(iun)=iop(iun)
25	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.eq.0) then
		call Wait (100)
		if(iwcnt.gt.5) then
			write (imsg_win,*) 'LI-7500A not returning ENQ.  Check it.'
			call stop_pgm()
		end if
		iwcnt=iwcnt+1
		goto 25
	end if
	do i=1,nchr
		retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
		call badinc(iop(iun),1)
	end do
!
	if (index(retstring(1:nchr),char(5)).eq.0) then
		i1=i1+1
		if (i1.gt.5) then
			write (imsg_win,*) ' LI-7500A not responding.  Check it.'
			call stop_pgm()
		else
			goto 15
		end if
	end if
!
	do i=1,ret_strng_len
		retstring(i:i)=' '
	end do
!
	call SendDataLI7500A (cmd1,1,retstring,iun)
	nchr=len_trim(retstring)
!
!  get the serial number and firmware version number
!
	i1=index(retstring(1:nchr),'(SerialNo',BACK=.FALSE.)
	i1=i1+10
	i2=index(retstring(i1:nchr),')',BACK=.FALSE.)
	i2=i2-1
	Inst_Inf(iun)%SerialNo=retstring(i1:i1+i2-1)
	Inst_Inf(iun)%SerialNo_len=len_trim(Inst_Inf(iun)%SerialNo)
	i1=index(retstring(1:nchr),'EmbeddedSW (Version',BACK=.FALSE.)
	i1=i1+20
	i2=index(retstring(i1:nchr),')',BACK=.FALSE.)
	i2=i2-1
	Inst_Inf(iun)%Firmware=retstring(i1:i1+i2-1)
	Inst_Inf(iun)%Firmware_len=len_trim(Inst_Inf(iun)%Firmware)
!
!  figure out which firmwar version this is and build the
!  setup command string
!
	do i=1,1000
		cmd3(i:i)=' '
	end do
!
!  note that a 25 unit (or 166.7 msec) delay has been added to the LI-7500A processing
!  stream.  This makes no difference to HuskerFlux though since all this delay does
!  is synchronize the output from the LI-7500A to it's own internal clock which we
!  don't use.  This will only matter when trying to synchronize internal clocks between
!  the LI-7500A and an external sonic anemometer.
!
	if (Inst_Inf(iun)%Firmware(1:3).eq.'6.0') then
		cmd3='(Outputs (BW 00)(Delay 25)(RS232 (Baud 00000)(Freq 00)'// &
			 '(Ndx FALSE)(Time FALSE)(Date FALSE)(CO2Raw FALSE)'//&
			 '(H2ORaw FALSE)(DiagVal TRUE)(DiagVal2 FALSE)(CO2D TRUE)'//&
			 '(H2OD TRUE)(Temp TRUE)(Pres TRUE)(Aux TRUE)(Aux2 TRUE)'//&
			 '(Aux3 TRUE)(Aux4 TRUE)(Cooler FALSE)(CO2MF TRUE)(H2OMF TRUE)'//&
		     '(DewPt TRUE)(H2OAW FALSE)(H2OAWO FALSE)(CO2AW FALSE)'//&
		     '(CO2AWO FALSE)(DiagRec FALSE)(AGC TRUE)(Labels FALSE)'//&
		     '(EOL "0D0A")))'//char(10)
		n_cmd3=len_trim(cmd3)
		write (cmd3(14:15),'(I2.2)') inyq
		write (cmd3(40:44),'(I5.5)') ibaud
		write (cmd3(52:53),'(I2.2)') ifreq
!!	else if ((Inst_Inf(iun)%Firmware(1:3).eq.'6.5').or.(Inst_Inf(iun)%Firmware(1:3).eq.'7.0')) then
!	else if ((Inst_Inf(iun)%Firmware(1:3).eq.'6.5').or.(Inst_Inf(iun)%Firmware(1:2).eq.'7.')) then
	else if ((Inst_Inf(iun)%Firmware(1:3).eq.'6.5').or.(Inst_Inf(iun)%Firmware(1:2).eq.'7.').or.(Inst_Inf(iun)%Firmware(1:2).eq.'8.')) then
		cmd3='(Outputs (BW 00)(Delay 25)(RS232 (Baud 00000)(Freq 00)'// &
			 '(Ndx FALSE)(Time FALSE)(Date FALSE)(CO2Raw FALSE)'//&
			 '(H2ORaw FALSE)(DiagVal TRUE)(DiagVal2 FALSE)(CO2D TRUE)'//&
			 '(H2OD TRUE)(Temp TRUE)(Pres TRUE)(Aux TRUE)(Aux2 TRUE)'//&
			 '(Aux3 TRUE)(Aux4 TRUE)(Cooler FALSE)(CO2MF TRUE)(H2OMF TRUE)'//&
			 '(DewPt TRUE)(H2OAW FALSE)(H2OAWO FALSE)(CO2AW FALSE)'//&
			 '(CO2AWO FALSE)(DiagRec FALSE)(CO2SS TRUE)(Labels FALSE)'//&
			 '(EOL "0D0A")))'//char(10)
		n_cmd3=len_trim(cmd3)
		write (cmd3(14:15),'(I2.2)') inyq
		write (cmd3(40:44),'(I5.5)') ibaud
		write (cmd3(52:53),'(I2.2)') ifreq
!
		i0=index(retstring(1:nchr),'(MaxRef (CX ',BACK=.FALSE.)
		i0=i0+12
		i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
		i1=i1-1
		CXval=retstring(i0:i0+i1-1)
		CXVal_len=len_trim(CXval)
		i0=index(retstring(1:nchr),'(WX ',BACK=.FALSE.)
		i0=i0+4
		i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
		i1=i1-1
		WXval=retstring(i0:i0+i1-1)
		WXVal_len=len_trim(WXval)
	else
		write (imsg_win,*) 'The firmware version of your LI7500A is not currently supported.'
!		write (imsg_win,*) 'Please update your firmware to V6.0.x, 6.5.x or 7.x.x'
		write (imsg_win,*) 'Please update your firmware to V6.0.x, 6.5.x, 7.x.x, or 8.x.x'
		call stop_pgm()
	end if
!
!  get the H2O and CO2 calibration dates and other info
!
	i0=index(retstring(1:nchr),'(ZeroCO2',BACK=.FALSE.)
	i1=index(retstring(i0:nchr),'(Date',BACK=.FALSE.)
	i1=i0+i1+5
	i2=index(retstring(i1:nchr),')',BACK=.FALSE.)
	i2=i2-1
	CO2ZeroDate=retstring(i1:i1+i2-1)
	CO2ZeroDate_len=len_trim(CO2ZeroDate)
	i0=index(retstring(1:nchr),'(SpanCO2',BACK=.FALSE.)
	i1=index(retstring(i0:nchr),'(Date',BACK=.FALSE.)
	i1=i0+i1+5
	i2=index(retstring(i1:nchr),')',BACK=.FALSE.)
	i2=i2-1
	CO2spandate=retstring(i1:i1+i2-1)
	CO2SpanDate_len=len_trim(CO2SpanDate)
!
	i0=index(retstring(1:nchr),'(ZeroH2O',BACK=.FALSE.)
	i1=index(retstring(i0:nchr),'(Date',BACK=.FALSE.)
	i1=i0+i1+5
	i2=index(retstring(i1:nchr),')',BACK=.FALSE.)
	i2=i2-1
	H2OZeroDate=retstring(i1:i1+i2-1)
	H2OZeroDate_len=len_trim(H2OZeroDate)
	i0=index(retstring(1:nchr),'(SpanH2O',BACK=.FALSE.)
	i1=index(retstring(i0:nchr),'(Date',BACK=.FALSE.)
	i1=i0+i1+5
	i2=index(retstring(i1:nchr),')',BACK=.FALSE.)
	i2=i2-1
	H2OSpanDate=retstring(i1:i1+i2-1)
	H2OSpanDate_len=len_trim(H2OSpanDate)
!
	i0=index(retstring(1:nchr),'(ZeroCO2 (Val ',BACK=.FALSE.)
	i0=i0+14
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	CO2ZerVal=retstring(i0:i0+i1-1)
	CO2ZerVal_len=len_trim(CO2ZerVal)
!
	i0=index(retstring(1:nchr),'(ZeroH2O (Val ',BACK=.FALSE.)
	i0=i0+14
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	H2OZerVal=retstring(i0:i0+i1-1)
	H2OZerVal_len=len_trim(H2OZerVal)
!
	i0=index(retstring(1:nchr),'(SpanCO2 (Val ',BACK=.FALSE.)
	i0=i0+14
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2SpanVal=retstring(i0:i2)
	CO2SpanVal_len=len_trim(CO2SpanVal)
	i0=index(retstring(i2:nchr),'(Target ',BACK=.FALSE.)
	i0=i0+i2+8
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	CO2SpanTar=retstring(i0-1:i0+i1-1)
	CO2SpanTar_len=len_trim(CO2SpanTar)
!
	i0=index(retstring(1:nchr),'(SpanH2O (Val ',BACK=.FALSE.)
	i0=i0+14
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	H2OSpanVal=retstring(i0:i2)
	H2OSpanVal_len=len_trim(H2OSpanVal)
	i0=index(retstring(i2:nchr),'(Target ',BACK=.FALSE.)
	i0=i0+i2+8
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	H2OSpanTar=retstring(i0-1:i0+i1-1)
	H2OSpanTar_len=len_trim(H2OSpanTar)
!
	i0=index(retstring(1:nchr),'(CO2 (A ',BACK=.FALSE.)
	i0=i0+8
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2A=retstring(i0:i2)
	CO2A_len=len_trim(CO2A)
	i0=index(retstring(i2:nchr),'(B ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2B=retstring(i0-1:i2)
	CO2B_len=len_trim(CO2B)
	i0=index(retstring(i2:nchr),'(C ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2C=retstring(i0-1:i2)
	CO2C_len=len_trim(CO2C)
	i0=index(retstring(i2:nchr),'(D ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2D=retstring(i0-1:i2)
	CO2D_len=len_trim(CO2D)
	i0=index(retstring(i2:nchr),'(E ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2E=retstring(i0-1:i2)
	CO2E_len=len_trim(CO2E)
	i0=index(retstring(i2:nchr),'(XS ',BACK=.FALSE.)
	i0=i0+i2+4
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2XS=retstring(i0-1:i2)
	CO2XS_len=len_trim(CO2XS)
	i0=index(retstring(i2:nchr),'(Z ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2Z=retstring(i0-1:i2)
	CO2Z_len=len_trim(CO2Z)
!
	i0=index(retstring(1:nchr),'(H2O (A ',BACK=.FALSE.)
	i0=i0+8
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	H2OA=retstring(i0:i0+i1-1)
	H2OA_len=len_trim(H2OA)
	i0=index(retstring(i2:nchr),'(B ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	H2OB=retstring(i0-1:i2)
	H2OB_len=len_trim(H2OB)
	i0=index(retstring(i2:nchr),'(C ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	H2OC=retstring(i0-1:i2)
	H2OC_len=len_trim(H2OC)
	i0=index(retstring(i2:nchr),'(XS ',BACK=.FALSE.)
	i0=i0+i2+4
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	H2OXS=retstring(i0-1:i2)
	H2OXS_len=len_trim(H2OXS)
	i0=index(retstring(i2:nchr),'(Z ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	H2OZ=retstring(i0-1:i2)
	H2OZ_len=len_trim(H2OZ)
!
!  write the information to the info file
!
	open (unit=info_unit,file=info_name, position='append')
	write (info_unit,*)
	write (info_unit,*) 'LiCor LI-7500A open path IRGA'
	write (info_unit,10) Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
		  Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len),CO2ZeroDate(1:CO2ZeroDate_len), &
	      CO2SpanDate(1:CO2SpanDate_len),H2OZeroDate(1:H2OZeroDate_len),H2OSpanDate(1:H2OSpanDate_len), &
		  CO2ZerVal(1:CO2ZerVal_len),CO2SpanVal(1:CO2SpanVal_len),CO2SpanTar(1:CO2SpanTar_len), &
		  H2OZerVal(1:H2OZerVal_len),H2OSpanVal(1:H2OSpanVal_len),H2OSpanTar(1:H2OSpanTar_len), &
		  CO2A(1:CO2A_len),CO2B(1:CO2B_len),CO2C(1:CO2C_len),CO2D(1:CO2D_len),CO2E(1:CO2E_len), &
		  CO2XS(1:CO2XS_len),CO2Z(1:CO2Z_len),H2OA(1:H2OA_len),H2OB(1:H2OB_len), &
		  H2OC(1:H2OC_len),H2OXS(1:H2OXS_len),H2OZ(1:H2OZ_len)
	if (Inst_Inf(iun)%firmware(1:3).NE.'6.0') then
		write (info_unit,40) CXVal(1:CXVal_len),WXVal(1:WXVal_len)
	end if
	write (info_unit,20) ibaud,ifreq,inyq
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
	write (imsg_win,*)
	write (imsg_win,*) 'LI-7500A Serial no. ',Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
					   '   Firmware Version ',Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (imsg_win,*)
	write (imsg_win,*) 'last CO2 zero: ',CO2ZeroDate(1:CO2ZeroDate_len)
	write (imsg_win,*) 'last CO2 span: ',CO2SpanDate(1:CO2SpanDate_len)
	write (imsg_win,*) 'last H2O zero: ',H2OZeroDate(1:H2OZeroDate_len)
	write (imsg_win,*) 'last H2O span: ',H2OSpanDate(1:H2OSpanDate_len)
	write (imsg_win,*)
!
!  now set up the IRGA for data collection
!
	call SendDataLI7500A (cmd3,n_cmd3,retstring,iun)
!
	call wait (200)
    call Set_Baud (ibaud,iun)
!
!  check to make sure that the IRGA is talking.  If it isn't, try again.
!
	if (samp_f.lt.0.1) then
		iwait=1000
	else
		iwait=int4(1000.0/samp_f)
	end if
	call wait (iwait)
	inum=nbyts(iop(iun),inp(iun))
	if (inum.lt.npacket(iun)) then
		iloop=iloop+1
		goto 5
	end if
!
	iop(iun)=inp(iun)
!
	write (imsg_win,*)
	write (imsg_win,*) ' LI-7500A IRGA is configured and active'
	write (imsg_win,*)
!
	iop(iun)=1
	inp(iun)=iop(iun)
!
	return
	end subroutine InitLI7500A
!
!==============================================================================
!
!	SUBROUTINE InitLI7200 (ibaud,iun)
!
!==============================================================================
!
	subroutine InitLI7200 (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30, ret_strng_len=7000
!
	type(T_DCB)::dcbcom
!
	character(len=ret_strng_len) retstring
	character(len=1000) cmd3
	character(len=istrng_len) CO2ZeroDate,CO2SpanDate,H2OZeroDate,H2OSpanDate
	character(len=istrng_len) CO2ZerVal,CO2SpanVal,CO2SpanTar,H2OZerVal,H2OSpanVal
	character(len=istrng_len) H2OSpanTar,CXVal,WXVal,CO2A,CO2B,CO2C,CO2D,CO2E
	character(len=istrng_len) CO2XS,CO2Z,H2OA,H2OB,H2OC,H2OXS,H2OZ,blank_strng
	character(len=36) mon_string
	character(len=5) cmd1,cmd2
	integer(kind=4) ibaud,jbaud,nwr,nbw,nbytes,inyq,i0,i1,i2,iwcnt,evt_mask,imon
!
10	format (1X,'Serial.No. ',A,5X,'Firmware Version ',A,/,1X,'Last CO2 zero: ',A,5X, &
			'Last CO2 span: ',A,/,1X,'Last H2O zero: ',A,5X,'Last H2O span: ',A, &
			/,1X,'CO2 zero value ',A,3X,'CO2 span value ',A,3X,'CO2 span target ', &
			A,' umol/mol',/,1X,'H2O zero value ',A,3X,'H2O span value ',A,3X, &
			'H2O span target ',A,' C d.p.',/,1X,'CO2 A = ',A,3X,'CO2 B = ',A,3X, &
			'CO2 C = ',A,3X,'CO2 D = ',A,3X,'CO2 E = ',A,3X,'CO2 XS = ',A,3X, &
			'CO2 Z = ',A,/,1X,'H2O A = ',A,3X,'H2O B = ',A,3X,'H2O C = ', &
			A,3X,'H2O XS = ',A,3X,'H2O Z = ',A)
20	format (1X,'Baud rate = ',I7,' bps',/,1X,'Sampling rate = ',I3,'Hz',/,1X, &
            'Bandwidth = ',I3,'Hz')
30	format (I)
40	format (1X,'CO2 SSI = ',A,5X,'H2O SSI = ',A)
!
	mon_string(1:36)='JanFebMarAprMayJunJulAugSepOctNovDec'
	do i=1,istrng_len
		blank_strng(i:i)=' '
	end do
!
!  if we've already done this a few times, stop the program and flag an IRGA error
!
	iloop=1
5	if (iloop.gt.5) then
		open (unit=ierr_unit,file=err_name,position='append')
		write (ierr_unit,*) 'There was a problem initializing the LI-7200.  Check it'
		write (imsg_win,*) 'There was a problem initializing the LI-7200.  Check it'
		close (unit=ierr_unit)
		call stop_pgm()
	end if
!
	Inst_Inf(iun)%SerialNo=blank_strng
	Inst_Inf(iun)%Firmware=blank_strng
	CO2ZeroDate=blank_strng
	CO2SpanDate=blank_strng
	H2OZeroDate=blank_strng
	H2OSpanDate=blank_strng
	CO2ZerVal=blank_strng
	CO2SpanVal=blank_strng
	CO2SpanTar=blank_strng
	H2OZerVal=blank_strng
	H2OSpanVal=blank_strng
	H2OSpanTar=blank_strng
	CO2A=blank_strng
	CO2B=blank_strng
	CO2C=blank_strng
	CO2D=blank_strng
	CO2E=blank_strng
	CO2XS=blank_strng
	CO2Z=blank_strng
	H2OA=blank_strng
	H2OB=blank_strng
	H2OC=blank_strng
	H2OXS=blank_strng
	H2OZ=blank_strng
	CXval=blank_strng
	WXval=blank_strng
!
	write (imsg_win,*)
	write (imsg_win,*) '            Configuring the LI-7200'
	ifreq=int(samp_f)
!
!  check the baud rate
!
	if ((ibaud.ne.38400).and.(ibaud.ne.57600).and.(ibaud.ne.115200)) then
		write (imsg_win,*) ' Incorrect baud rate for LI-7200.  Edit your setup file'
		call stop_pgm()
	end if
!
!  check the sampling frequency, and figure out the best bandwidth
!
	if (ifreq.gt.20) then
		write (imsg_win,*) ' Incorrect sampling frequency.  Edit your setup file'
		call stop_pgm()
	end if
	if (ifreq.eq.20) then
		inyq=10
	else
		inyq=5
	end if
!
!  build some of the LI-7200 setup command strings
!
	cmd1(1:1)=char(6)
	cmd2(1:3)=')'//char(13)//char(10)
!
!  set the serial port baud rate to 9600 (default known state for LI-7200)
!
	jbaud=9600
    call Set_Baud (jbaud,iun)
	iop(iun)=inp(iun)
!
!  now send a 500 msec BREAK to the IRGA, wait for an ENQ, and send an ACK
!
	write (imsg_win,*) ' Sending BREAK to IRGA'
!
	i1=0
15	iret=SetCommBreak(hCom(iun))
	call Wait (50)
	iret=ClearCommBreak(hCom(iun))
	iwcnt=1
!
	inp(iun)=iop(iun)
25	nchr=nbyts(iop(iun),inp(iun))
	if (nchr.eq.0) then
		call Wait (100)
		if(iwcnt.gt.5) then
			write (imsg_win,*) 'LI-7200 not returning ENQ.  Check it.'
			call stop_pgm()
		end if
		iwcnt=iwcnt+1
		goto 25
	end if
	do i=1,nchr
		retstring(i:i)=inbuf(iun)(iop(iun):iop(iun))
		call badinc(iop(iun),1)
	end do
!
	if (index(retstring(1:nchr),char(5)).eq.0) then
		i1=i1+1
		if (i1.gt.5) then
			write (imsg_win,*) ' LI-7200 not responding.  Check it.'
			call stop_pgm()
		else
			goto 15
		end if
	end if
!
	do i=1,ret_strng_len
		retstring(i:i)=' '
	end do
!
	call SendDataLI7200 (cmd1,1,retstring,iun)
	nchr=len_trim(retstring)
!
!  get the serial number and firmware version number
!
	i1=index(retstring(1:nchr),'(SerialNo',BACK=.FALSE.)
	i1=i1+10
	i2=index(retstring(i1:nchr),')',BACK=.FALSE.)
	i2=i2-1
	Inst_Inf(iun)%SerialNo=retstring(i1:i1+i2-1)
	Inst_Inf(iun)%SerialNo_len=len_trim(Inst_Inf(iun)%SerialNo)
	i1=index(retstring(1:nchr),'EmbeddedSW (Version',BACK=.FALSE.)
	i1=i1+20
	i2=index(retstring(i1:nchr),')',BACK=.FALSE.)
	i2=i2-1
	Inst_Inf(iun)%Firmware=retstring(i1:i1+i2-1)
	Inst_Inf(iun)%Firmware_len=len_trim(Inst_Inf(iun)%Firmware)
!
!  figure out which firmware version this is and build the
! setup command string
!
	do i=1,1000
		cmd3(i:i)=' '
	end do
!
!  note that a 25 unit (or 166.7 msec) delay has been added to the LI-7200 processing
!  stream.  This makes no difference to HuskerFlux though since all this delay does
!  is synchronize the output from the LI-7200 to it's own internal clock which we
!  don't use.  This will only matter when trying to synchronize internal clocks between
!  the LI-7500A and an external sonic anemometer.
!
	if (Inst_Inf(iun)%Firmware(1:3).eq.'6.0') then
		cmd3='(Outputs (BW 00)(Delay 25)(RS232 (Baud 00000)(Freq 00)'// &
	         '(Ndx FALSE)(Time FALSE)(Date FALSE)(CO2Raw FALSE)'//&
			 '(H2ORaw FALSE)(DiagVal TRUE)(DiagVal2 FALSE)(CO2D TRUE)'//&
			 '(H2OD TRUE)(Temp TRUE)(AvgTemp TRUE)(TempIn TRUE)'//&
			 '(TempOut TRUE)(Pres TRUE)(APres TRUE)(DPres TRUE)'//&
			 '(FlowPressure TRUE)(MeasFlowRate TRUE)(VolFlowRate TRUE)'//&
			 '(FlowPower TRUE)(FlowDrive TRUE)(Aux TRUE)(Aux2 TRUE)'//&
			 '(Aux3 TRUE)(Aux4 TRUE)(Cooler FALSE)(CO2MF TRUE)(CO2MFd TRUE)'//&
			 '(H2OMF TRUE)(H2OMFd TRUE)(DewPt TRUE)(H2OAW FALSE)'//&
			 '(H2OAWO FALSE)(CO2AW FALSE)(CO2AWO FALSE)(DiagRec FALSE)'//&
			 '(Labels FALSE)(AGC TRUE)(EOL "0D0A")))'//char(10)
		n_cmd3=len_trim(cmd3)
		write (cmd3(14:15),'(I2.2)') inyq
		write (cmd3(40:44),'(I5.5)') ibaud
		write (cmd3(52:53),'(I2.2)') ifreq
!!	else if ((Inst_Inf(iun)%Firmware(1:3).eq.'6.5').or.(Inst_Inf(iun)%Firmware(1:3).eq.'7.0')) then
!	else if ((Inst_Inf(iun)%Firmware(1:3).eq.'6.5').or.(Inst_Inf(iun)%Firmware(1:2).eq.'7.')) then
	else if ((Inst_Inf(iun)%Firmware(1:3).eq.'6.5').or.(Inst_Inf(iun)%Firmware(1:2).eq.'7.').or.(Inst_Inf(iun)%Firmware(1:2).eq.'8.')) then
		cmd3='(Outputs (BW 00)(Delay 25)(RS232 (Baud 00000)(Freq 00)'// &
	         '(Ndx FALSE)(Time FALSE)(Date FALSE)(CO2Raw FALSE)'//&
			 '(H2ORaw FALSE)(DiagVal TRUE)(DiagVal2 FALSE)(CO2D TRUE)'//&
			 '(H2OD TRUE)(Temp TRUE)(AvgTemp TRUE)(TempIn TRUE)'//&
			 '(TempOut TRUE)(Pres TRUE)(APres TRUE)(DPres TRUE)'//&
			 '(FlowPressure TRUE)(MeasFlowRate TRUE)(VolFlowRate TRUE)'//&
			 '(FlowPower TRUE)(FlowDrive TRUE)(Aux TRUE)(Aux2 TRUE)'//&
			 '(Aux3 TRUE)(Aux4 TRUE)(Cooler FALSE)(CO2MF TRUE)(CO2MFd TRUE)'//&
			 '(H2OMF TRUE)(H2OMFd TRUE)(DewPt TRUE)(H2OAW FALSE)'//&
			 '(H2OAWO FALSE)(CO2AW FALSE)(CO2AWO FALSE)(DiagRec FALSE)'//&
			 '(Labels FALSE)(CO2SS TRUE)(H2OSS TRUE)(EOL "0D0A")))'//char(10)
		n_cmd3=len_trim(cmd3)
		write (cmd3(14:15),'(I2.2)') inyq
		write (cmd3(40:44),'(I5.5)') ibaud
		write (cmd3(52:53),'(I2.2)') ifreq
!
		i0=index(retstring(1:nchr),'(MaxRef (CX ',BACK=.FALSE.)
		i0=i0+12
		i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
		i1=i1-1
		CXval=retstring(i0:i0+i1-1)
		CXVal_len=len_trim(CXval)
		i0=index(retstring(1:nchr),'(WX ',BACK=.FALSE.)
		i0=i0+4
		i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
		i1=i1-1
		WXval=retstring(i0:i0+i1-1)
		WXVal_len=len_trim(WXval)
	else
		write (imsg_win,*) 'The firmware version of your LI7200 is not currently supported.'
!		write (imsg_win,*) 'Please update your firmware to V6.0.x, 6.5.x or 7.x.x'
		write (imsg_win,*) 'Please update your firmware to V6.0.x, 6.5.x, 7.x.x, or 8.x.x'
		call stop_pgm()
	end if
!
!  get the H2O and CO2 calibration dates
!
	i0=index(retstring(1:nchr),'(ZeroCO2',BACK=.FALSE.)
	i1=index(retstring(i0:nchr),'(Date',BACK=.FALSE.)
	i1=i0+i1+5
	i2=index(retstring(i1:nchr),')',BACK=.FALSE.)
	i2=i2-1
	CO2ZeroDate=retstring(i1:i1+i2-1)
	CO2ZeroDate_len=len_trim(CO2ZeroDate)
	i0=index(retstring(1:nchr),'(SpanCO2',BACK=.FALSE.)
	i1=index(retstring(i0:nchr),'(Date',BACK=.FALSE.)
	i1=i0+i1+5
	i2=index(retstring(i1:nchr),')',BACK=.FALSE.)
	i2=i2-1
	CO2SpanDate=retstring(i1:i1+i2-1)
	CO2SpanDate_len=len_trim(CO2SpanDate)
!
	i0=index(retstring(1:nchr),'(ZeroH2O',BACK=.FALSE.)
	i1=index(retstring(i0:nchr),'(Date',BACK=.FALSE.)
	i1=i0+i1+5
	i2=index(retstring(i1:nchr),')',BACK=.FALSE.)
	i2=i2-1
	H2OZeroDate=retstring(i1:i1+i2-1)
	H2OZeroDate_len=len_trim(H2OZeroDate)
	i0=index(retstring(1:nchr),'(SpanH2O',BACK=.FALSE.)
	i1=index(retstring(i0:nchr),'(Date',BACK=.FALSE.)
	i1=i0+i1+5
	i2=index(retstring(i1:nchr),')',BACK=.FALSE.)
	i2=i2-1
	H2OSpanDate=retstring(i1:i1+i2-1)
	H2OSpanDate_len=len_trim(H2OSpanDate)
!
	i0=index(retstring(1:nchr),'(ZeroCO2 (Val ',BACK=.FALSE.)
	i0=i0+14
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	CO2ZerVal=retstring(i0:i0+i1-1)
	CO2ZerVal_len=len_trim(CO2ZerVal)
!
	i0=index(retstring(1:nchr),'(ZeroH2O (Val ',BACK=.FALSE.)
	i0=i0+14
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	H2OZerVal=retstring(i0:i0+i1-1)
	H2OZerVal_len=len_trim(H2OZerVal)
!
	i0=index(retstring(1:nchr),'(SpanCO2 (Val ',BACK=.FALSE.)
	i0=i0+14
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2SpanVal=retstring(i0:i2)
	CO2SpanVal_len=len_trim(CO2SpanVal)
	i0=index(retstring(i2:nchr),'(Target ',BACK=.FALSE.)
	i0=i0+i2+8
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	CO2SpanTar=retstring(i0-1:i0+i1-1)
	CO2SpanTar_len=len_trim(CO2SpanTar)
!
	i0=index(retstring(1:nchr),'(SpanH2O (Val ',BACK=.FALSE.)
	i0=i0+14
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	H2OSpanVal=retstring(i0:i2)
	H2OSpanVal_len=len_trim(H2OSpanVal)
	i0=index(retstring(i2:nchr),'(Target ',BACK=.FALSE.)
	i0=i0+i2+8
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	H2OSpanTar=retstring(i0-1:i0+i1-1)
	H2OSpanTar_len=len_trim(H2OSpanTar)
!
	i0=index(retstring(1:nchr),'(CO2 (A ',BACK=.FALSE.)
	i0=i0+8
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2A=retstring(i0:i2)
	CO2A_len=len_trim(CO2A)
	i0=index(retstring(i2:nchr),'(B ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2B=retstring(i0-1:i2)
	CO2B_len=len_trim(CO2B)
	i0=index(retstring(i2:nchr),'(C ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2C=retstring(i0-1:i2)
	CO2C_len=len_trim(CO2C)
	i0=index(retstring(i2:nchr),'(D ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2D=retstring(i0-1:i2)
	CO2D_len=len_trim(CO2D)
	i0=index(retstring(i2:nchr),'(E ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2E=retstring(i0-1:i2)
	CO2E_len=len_trim(CO2E)
	i0=index(retstring(i2:nchr),'(XS ',BACK=.FALSE.)
	i0=i0+i2+4
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2XS=retstring(i0-1:i2)
	CO2XS_len=len_trim(CO2XS)
	i0=index(retstring(i2:nchr),'(Z ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	CO2Z=retstring(i0-1:i2)
	CO2Z_len=len_trim(CO2Z)
!
	i0=index(retstring(1:nchr),'(H2O (A ',BACK=.FALSE.)
	i0=i0+8
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	H2OA=retstring(i0:i0+i1-1)
	H2OA_len=len_trim(H2OA)
	i0=index(retstring(i2:nchr),'(B ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	H2OB=retstring(i0-1:i2)
	H2OB_len=len_trim(H2OB)
	i0=index(retstring(i2:nchr),'(C ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	H2OC=retstring(i0-1:i2)
	H2OC_len=len_trim(H2OC)
	i0=index(retstring(i2:nchr),'(XS ',BACK=.FALSE.)
	i0=i0+i2+4
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	H2OXS=retstring(i0-1:i2)
	H2OXS_len=len_trim(H2OXS)
	i0=index(retstring(i2:nchr),'(Z ',BACK=.FALSE.)
	i0=i0+i2+3
	i1=index(retstring(i0:nchr),')',BACK=.FALSE.)
	i1=i1-1
	i2=i0+i1-1
	H2OZ=retstring(i0-1:i2)
	H2OZ_len=len_trim(H2OZ)
!
!  write the information to the info file
!
	open (unit=info_unit,file=info_name, position='append')
	write (info_unit,*)
	write (info_unit,*) 'LiCor LI-7200 closed-path IRGA'
	write (info_unit,10) Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
		  Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len),CO2ZeroDate(1:CO2ZeroDate_len), &
	      CO2SpanDate(1:CO2SpanDate_len),H2OZeroDate(1:H2OZeroDate_len),H2OSpanDate(1:H2OSpanDate_len), &
		  CO2ZerVal(1:CO2ZerVal_len),CO2SpanVal(1:CO2SpanVal_len),CO2SpanTar(1:CO2SpanTar_len), &
		  H2OZerVal(1:H2OZerVal_len),H2OSpanVal(1:H2OSpanVal_len),H2OSpanTar(1:H2OSpanTar_len), &
		  CO2A(1:CO2A_len),CO2B(1:CO2B_len),CO2C(1:CO2C_len),CO2D(1:CO2D_len),CO2E(1:CO2E_len), &
		  CO2XS(1:CO2XS_len),CO2Z(1:CO2Z_len),H2OA(1:H2OA_len),H2OB(1:H2OB_len), &
		  H2OC(1:H2OC_len),H2OXS(1:H2OXS_len),H2OZ(1:H2OZ_len)
	write (info_unit,20) ibaud,ifreq,inyq
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
	write (imsg_win,*)
	write (imsg_win,*) 'LI-7200 Serial no. ',Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
					   '   Firmware Version ',Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (imsg_win,*)
	write (imsg_win,*) 'baudrate: ',ibaud
	write (imsg_win,*) 'last CO2 zero: ',CO2ZeroDate(1:CO2ZeroDate_len)
	write (imsg_win,*) 'last CO2 span: ',CO2SpanDate(1:CO2SpanDate_len)
	write (imsg_win,*) 'last H2O zero: ',H2OZeroDate(1:H2OZeroDate_len)
	write (imsg_win,*) 'last H2O span: ',H2OSpanDate(1:H2OSpanDate_len)
	write (imsg_win,*)
!
!  now set up the IRGA for data collection
!
	call SendDataLI7200 (cmd3,n_cmd3,retstring,iun)
!
	call wait (200)
    call Set_Baud (ibaud,iun)
!
!  check to make sure that the IRGA is talking.  If it isn't, try again.
!
	if (samp_f.lt.0.1) then
		iwait=1000
	else
		iwait=int4(1000.0/samp_f)
	end if
	call wait (iwait)
	inum=nbyts(iop(iun),inp(iun))
	if (inum.lt.npacket(iun)) then
		iloop=iloop+1
		goto 5
	end if
!
	iop(iun)=inp(iun)
!
	write (imsg_win,*)
	write (imsg_win,*) ' LI-7200 IRGA is configured and active'
	write (imsg_win,*)
!
	iop(iun)=1
	inp(iun)=iop(iun)
!
	return
	end subroutine InitLI7200
!
!==============================================================================
!
!	SUBROUTINE InitLI6262 (ibaud,iun)
!
!==============================================================================
!
	subroutine InitLI6262 (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=1000
!
	logical	lastline
	character(len=ret_strng_len) retstring
	character(len=40) cmd1,cmd2,line1
	integer(kind=4) ibaud,icnt1,icnt2,evt_mask
!
	type(T_DCB)::dcbCom
!
20	format (1X,'Baud=9600 bps  8-bits  no parity')
30	format (1X,'The current calibration data are:')
40	format (1X,A)
50	format (1X,'You chose ',I5.5,' baud.',/,1X, &
            'Only 9600 is allowed for the LI-6262.',/,1X, &
			'Defaulting to 9600 baud.')
!
	do i=1,istrng_len
		Inst_Inf(iun)%SerialNo(i:i)=' '
		Inst_Inf(iun)%Firmware(i:i)=' '
	end do
!
	Inst_Inf(iun)%SerialNo='null'
	Inst_Inf(iun)%SerialNo_len=4
	Inst_Inf(iun)%Firmware='null'
	Inst_Inf(iun)%Firmware_len=4
!
	write(imsg_win,*)
	write(imsg_win,*) ' Configuring the LI6262 IRGA'
!
	if (ibaud.ne.9600) then
		write(imsg_win,50) ibaud
	end if
	jbaud=9600
    call Set_Baud (jbaud,iun)
	iop(iun)=inp(iun)
!
	if ((samp_interval.lt.200).or.(samp_interval.gt.36000)) then
		write (imsg_win,*) 'The sampling frequency is wrong.  The LI-6262'
		write (imsg_win,*) 'only supports 5 Hz, 2Hz, or multiples of 1 second'
		write (imsg_win,*) 'up to 36.  Please correct this in the setup file.'
		call stop_pgm()
	end if
	if (samp_interval.eq.200) then
		cmd2(1:9)='*14 0.2'//char(13)//char(10)
		ncmd=9
	else if (samp_interval.eq.500) then
		cmd2(1:9)='*14 0.5'//char(13)//char(10)
		ncmd=9
	else if ((samp_interval.ge.1000).and.(samp_interval.le.36000)) then
		itmp=samp_interval/1000
		cmd2(1:8)='*14 xx'//char(13)//char(10)
		write (cmd2(5:6),'(I2)') itmp
		ncmd=8
	end if
!		
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'LiCor LI-6262 closed path IRGA'
	write (info_unit,20)
	write (info_unit,30)
	write (imsg_win,30)
	iop(iun)=inp(iun)
	cmd1(1:7)='*14 0'//char(13)//char(10)
	call SendDataLI6262 (cmd1,7,retstring,iun)
	iop(iun)=inp(iun)
!
	call wait (100)
	call SendDataLI6262 (cmd1,7,retstring,iun)
	iop(iun)=inp(iun)
!
	cmd1(1:5)='*16'//char(13)//char(10)
	call SendDataLI6262 (cmd1,5,retstring,iun)
!
	lastline=.false.
	icnt1=1
	icnt2=1
	do
		if((retstring(icnt1:icnt1).eq.char(13)).and.lastline) goto 15
		if(retstring(icnt1:icnt1).eq.char(13)) then
			icnt1=icnt1+2
			write (imsg_win,40) line1(1:icnt2-1)
			write (info_unit,40) line1(1:icnt2-1)
			icnt2=1
			cycle
		end if
		if(retstring(icnt1:icnt1+6).eq.'VP CORR') lastline=.true.
		line1(icnt2:icnt2)=retstring(icnt1:icnt1)
		icnt1=icnt1+1
		icnt2=icnt2+1
	end do
15	write (imsg_win,40) line1(1:icnt2-1)
	write (info_unit,40) line1(1:icnt2-1)
!
	cmd1(1:16)='*1322,32,42,43'//char(13)//char(10)
	call SendDataLI6262 (cmd1,16,retstring,iun)
	call SendDataLI6262 (cmd2,ncmd,retstring,iun)
!
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	iop(iun)=inp(iun)
!
	write (imsg_win,*) ' LI-6262 closed path IRGA configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitLI6262
!
!==============================================================================
!
!	SUBROUTINE InitLI7000 (ibaud,iun)
!
!==============================================================================
!
	subroutine InitLI7000 (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: ret_strng_len=5000,cmd_len=1000
	character(len=ret_strng_len) retstring,ret_blank
	character(len=cmd_len) cmd1,cmd2,cmd3,tmp_1,tmp_2,tmp_3,tmp_4,cmd_blank
	character(len=info_str_len) CO2CalDate,H2OCalDate
	integer(kind=4) evt_mask,ibaud,jbaud,nwr,nbw,nbytes,inyq,i1,i2
    integer(kind=4) ibauds(5)
    integer stopLI7000
!
	type(T_DCB)::dcbcom
!
    data ibauds /115200,57600,38400,19200,9600/
!
!
20	format (1X,'Baud=9600 bps  8-bits  no parity')
30	format (1X,A)
40	format (1X,'You chose ',I5.5,' baud.',/,1X, &
            'Only 9600 is allowed for the LI-7000.',/,1X, &
			'Defaulting to 9600 baud.')
!
	do i=1,info_str_len
		CO2CalDate(i:i)=' '
		H2OCalDate(i:i)=' '
	end do
    do i=1,cmd_len
        cmd_blank(i:i)=' '
    end do
    do i=1,ret_strng_len
        ret_blank(i:i)=' '
    end do
    cmd1=cmd_blank
    cmd2=cmd_blank
    cmd3=cmd_blank
    tmp_1=cmd_blank
    tmp_2=cmd_blank
    tmp_3=cmd_blank
    tmp_4=cmd_blank
!
    Inst_Inf(iun)%n_info_strings=12
!
    Inst_Inf(iun)%info_string(1)='LiCor LI-7000 closed-path IRGA'
    Inst_Inf(iun)%info_string_len(1)=len_trim(Inst_Inf(iun)%info_string(1))
!
!  check and set the baud rate
!
    if (ibaud.ne.9600) then
        write (imsg_win,*) ' Incorrect baud rate for LI-7000.  Edit your setup file'
        call stop_pgm()
    end if
    call Set_Baud (ibaud,iun)
!
	cmd3='(About !)'//char(10)
	ncmd3=len_trim(cmd3)
    retstring=ret_blank
	call SendDataLI7000 (cmd3,ncmd3,retstring,50,iun)
    cmd3=cmd_blank
   	iop(iun)=inp(iun)
	i1=index(retstring,'Version ',BACK=.FALSE.)
	i1=i1+8
	do i=1,cmd_len
		if(retstring(i1:i1).ne.char(09)) then
			Inst_Inf(iun)%Firmware(i:i)=retstring(i1:i1)
			i1=i1+1
		else
			exit
		end if
	end do	
	Inst_Inf(iun)%Firmware_len=len_trim(Inst_Inf(iun)%Firmware)
    Inst_Inf(iun)%info_string(2)='Serial number N/A     Firmware version '//Inst_Inf(iun)%Firmware(1:len_trim(Inst_Inf(iun)%Firmware))
!
	cmd1='(RS232 (Rate Polled) (Sources ("CO2B um/m" "H2OB mm/m" "P kPa" "T C" "Diag")) (Timestamp None)) (Diagnostics 0)'//char(10)
	ncmd1=len_trim(cmd1)
!
	if (samp_interval.lt.100) then
		cmd2='(RS232 (Rate 20Hz)) (Filter (Time 0.0))'//char(10)
		ncmd2=len_trim(cmd2)
        Inst_Inf(iun)%info_string(12)='Baud rate= 9600  sample rate= 20Hz  Filter= 0.0 sec'
	else if ((samp_interval.lt.200).and.(samp_interval.ge.100)) then
		cmd2='(RS232 (Rate 10Hz)) (Filter (Time 0.05))'//char(10)
		ncmd2=len_trim(cmd2)
        Inst_Inf(iun)%info_string(12)='Baud rate= 9600  sample rate= 10Hz  Filter= 0.05 sec'
	else if ((samp_interval.lt.500).and.(samp_interval.ge.200)) then
		cmd2='(RS232 (Rate 5Hz)) (Filter (Time 0.1))'//char(10)
		ncmd2=len_trim(cmd2)
        Inst_Inf(iun)%info_string(12)='Baud rate= 9600  sample rate= 5Hz  Filter= 0.1 sec'
	else if ((samp_interval.lt.1000).and.(samp_interval.ge.500)) then
		cmd2='(RS232 (Rate 2Hz)) (Filter (Time 0.25))'//char(10)
		ncmd2=len_trim(cmd2)
        Inst_Inf(iun)%info_string(12)='Baud rate= 9600  sample rate= 2Hz  Filter= 0.25 sec'
	else if ((samp_interval.lt.2000).and.(samp_interval.ge.1000)) then
		cmd2='(RS232 (Rate 1Hz)) (Filter (Time 0.5))'//char(10)
		ncmd2=len_trim(cmd2)
        Inst_Inf(iun)%info_string(12)='Baud rate= 9600  sample rate= 1Hz  Filter= 0.5 sec'
	else if ((samp_interval.lt.5000).and.(samp_interval.ge.2000)) then
		cmd2='(RS232 (Rate 2s)) (Filter (Time 1.0))'//char(10)
		ncmd2=len_trim(cmd2)
        Inst_Inf(iun)%info_string(12)='Baud rate= 9600  sample rate= 0.5Hz  Filter= 1.0 sec'
	else if ((samp_interval.lt.10000).and.(samp_interval.ge.5000)) then
		cmd2='(RS232 (Rate 5s)) (Filter (Time 2.5))'//char(10)
		ncmd2=len_trim(cmd2)
        Inst_Inf(iun)%info_string(12)='Baud rate= 9600  sample rate= 0.2Hz  Filter= 2.5 sec'
	end if
!
	write(imsg_win,*)
	write(imsg_win,*) ' Configuring the LI7000 IRGA'
!
    retstring=ret_blank
	call SendDataLI7000 (cmd1,ncmd1,retstring,50,iun)
    call wait (200)
	iop(iun)=inp(iun)
!
    cmd3='?'//char(10)
    ncmd3=len_trim(cmd3)
    retstring=ret_blank
	call SendDataLI7000 (cmd3,ncmd3,retstring,500,iun)
    cmd3=cmd_blank
!
	iop(iun)=inp(iun)
!
	i1=index(retstring(1:len_trim(retstring)),'(CO2(CellA(Time ',BACK=.FALSE.)
	i1=i1+17
    tmp_1=cmd_blank
	do i=1,cmd_len
		if(retstring(i1:i1).ne.'"') then
			tmp_1(i:i)=retstring(i1:i1)
			i1=i1+1
		else
			exit
		end if
	end do
	i2=index(retstring(i1:len_trim(retstring)),'(CellB(Time ',BACK=.FALSE.)
	i2=i1+i2+12
    close (unit=40)
    tmp_2=cmd_blank
	do i=1,cmd_len
		if(retstring(i2:i2).ne.'"') then
			tmp_2(i:i)=retstring(i2:i2)
			i2=i2+1
		else
			exit
		end if
	end do
	Inst_Inf(iun)%info_string(3)='Last CO2 cal.: cell A  '//tmp_1(1:len_trim(tmp_1))//'  cell B  '//tmp_2(1:len_trim(tmp_2))
!
	i1=index(retstring(1:len_trim(retstring)),'(H2O(CellA(Time ',BACK=.FALSE.)
	i1=i1+17
    tmp_1=cmd_blank
	do i=1,cmd_len
		if(retstring(i1:i1).ne.'"') then
			tmp_1(i:i)=retstring(i1:i1)
			i1=i1+1
		else
			exit
		end if
	end do
	i2=index(retstring(i1:len_trim(retstring)),'(CellB(Time ',BACK=.FALSE.)
	i2=i1+i2+12
    tmp_2=cmd_blank
	do i=1,cmd_len
		if(retstring(i2:i2).ne.'"') then
			tmp_2(i:i)=retstring(i2:i2)
			i2=i2+1
		else
			exit
		end if
	end do
	Inst_Inf(iun)%info_string(8)='Last H2O cal.: cell A  '//tmp_1(1:len_trim(tmp_1))//'  cell B  '//tmp_2(1:len_trim(tmp_2))
!
    tmp_1=cmd_blank
	i1=index(retstring(1:len_trim(retstring)),'(CO2(a1 ',BACK=.FALSE.)
	i1=i1+8
    tmp_1=cmd_blank
	do i=1,cmd_len
		if(retstring(i1:i1).ne.')') then
			tmp_1(i:i)=retstring(i1:i1)
			i1=i1+1
		else
			exit
		end if
	end do
!
    tmp_2=cmd_blank
	i2=index(retstring(i1:len_trim(retstring)),'(a2 ',BACK=.FALSE.)
	i2=i1+i2+3
    tmp_2=cmd_blank
	do i=1,cmd_len
		if(retstring(i2:i2).ne.')') then
			tmp_2(i:i)=retstring(i2:i2)
			i2=i2+1
		else
			exit
		end if
    end do
!
    tmp_3=cmd_blank
	i3=index(retstring(i2:len_trim(retstring)),'(a3 ',BACK=.FALSE.)
	i3=i2+i3+3
    tmp_3=cmd_blank
	do i=1,cmd_len
		if(retstring(i3:i3).ne.')') then
			tmp_3(i:i)=retstring(i3:i3)
			i3=i3+1
		else
			exit
		end if
	end do
!
    tmp_4=cmd_blank
	i4=index(retstring(i3:len_trim(retstring)),'(a4 ',BACK=.FALSE.)
	i4=i3+i4+3
    tmp_4=cmd_blank
	do i=1,cmd_len
		if(retstring(i4:i4).ne.')') then
			tmp_4(i:i)=retstring(i4:i4)
			i4=i4+1
		else
			exit
		end if
	end do
	Inst_Inf(iun)%info_string(4)='CO2 coeffs:  a1= '//tmp_1(1:len_trim(tmp_1))//' a2= '//tmp_2(1:len_trim(tmp_2))//' a3= '//tmp_3(1:len_trim(tmp_3))//' a4= '//tmp_4(1:len_trim(tmp_4))
!
    tmp_1=cmd_blank
	i1=index(retstring(i4:len_trim(retstring)),'(a5 ',BACK=.FALSE.)
	i1=i1+i4+4
    tmp_1=cmd_blank
	do i=1,cmd_len
		if(retstring(i1:i1).ne.')') then
			tmp_1(i:i)=retstring(i1:i1)
			i1=i1+1
		else
			exit
		end if
	end do
!
    tmp_2=cmd_blank
	i2=index(retstring(i1:len_trim(retstring)),'(gamma ',BACK=.FALSE.)
	i2=i1+i2+6
    tmp_2=cmd_blank
	do i=1,cmd_len
		if(retstring(i2:i2).ne.')') then
			tmp_2(i:i)=retstring(i2:i2)
			i2=i2+1
		else
			exit
		end if
    end do
!
    tmp_3=cmd_blank
	i3=index(retstring(i2:len_trim(retstring)),'(Z ',BACK=.FALSE.)
	i3=i2+i3+2
    tmp_3=cmd_blank
	do i=1,cmd_len
		if(retstring(i3:i3).ne.')') then
			tmp_3(i:i)=retstring(i3:i3)
			i3=i3+1
		else
			exit
		end if
	end do
!
    tmp_4=cmd_blank
	i4=index(retstring(i3:len_trim(retstring)),'(Zt ',BACK=.FALSE.)
	i4=i3+i4+3
    tmp_4=cmd_blank
	do i=1,cmd_len
		if(retstring(i4:i4).ne.')') then
			tmp_4(i:i)=retstring(i4:i4)
			i4=i4+1
		else
			exit
		end if
	end do
	Inst_Inf(iun)%info_string(5)='CO2 coeffs:  a5= '//tmp_1(1:len_trim(tmp_1))//' gamma= '//tmp_2(1:len_trim(tmp_2))//' Z= '//tmp_3(1:len_trim(tmp_3))//' Zt= '//tmp_4(1:len_trim(tmp_4))
!
    tmp_1=cmd_blank
	i1=index(retstring(i4:len_trim(retstring)),'(Zm ',BACK=.FALSE.)
	i1=i1+i4+4
    tmp_1=cmd_blank
	do i=1,cmd_len
		if(retstring(i1:i1).ne.')') then
			tmp_1(i:i)=retstring(i1:i1)
			i1=i1+1
		else
			exit
		end if
	end do
!
    tmp_2=cmd_blank
	i2=index(retstring(i1:len_trim(retstring)),'(S ',BACK=.FALSE.)
	i2=i1+i2+2
    tmp_2=cmd_blank
	do i=1,cmd_len
		if(retstring(i2:i2).ne.')') then
			tmp_2(i:i)=retstring(i2:i2)
			i2=i2+1
		else
			exit
		end if
    end do
!
    tmp_3=cmd_blank
	i3=index(retstring(i2:len_trim(retstring)),'(W0',BACK=.FALSE.)
	i3=i2+i3+4
    tmp_3=cmd_blank
	do i=1,cmd_len
		if(retstring(i3:i3).ne.')') then
			tmp_3(i:i)=retstring(i3:i3)
			i3=i3+1
		else
			exit
		end if
	end do
!
    tmp_4=cmd_blank
	i4=index(retstring(i3:len_trim(retstring)),'(W0',BACK=.FALSE.)
	i4=i3+i4+6
    tmp_4=cmd_blank
	do i=1,cmd_len
		if(retstring(i4:i4).ne.')') then
			tmp_4(i:i)=retstring(i4:i4)
			i4=i4+1
		else
			exit
		end if
	end do
	Inst_Inf(iun)%info_string(6)='CO2 coeffs:  Zm= '//tmp_1(1:len_trim(tmp_1))//' S= '//tmp_2(1:len_trim(tmp_2))//' W0= '//tmp_3(1:len_trim(tmp_3))//' W0_d= '//tmp_4(1:len_trim(tmp_4))
!
    tmp_1=cmd_blank
	i1=index(retstring(i4:len_trim(retstring)),'(AGC ',BACK=.FALSE.)
	i1=i1+i4+5
    tmp_1=cmd_blank
	do i=1,cmd_len
		if(retstring(i1:i1).ne.')') then
			tmp_1(i:i)=retstring(i1:i1)
			i1=i1+1
		else
			exit
		end if
	end do
!
    tmp_2=cmd_blank
	i2=index(retstring(i1:len_trim(retstring)),'(VpCrr ',BACK=.FALSE.)
	i2=i1+i2+6
    tmp_2=cmd_blank
	do i=1,cmd_len
		if(retstring(i2:i2).ne.')') then
			tmp_2(i:i)=retstring(i2:i2)
			i2=i2+1
		else
			exit
		end if
    end do
	Inst_Inf(iun)%info_string(7)='CO2 coeffs:  AGC= '//tmp_1(1:len_trim(tmp_1))//' VpCrr= '//tmp_2(1:len_trim(tmp_2))
!
    tmp_1=cmd_blank
	i1=index(retstring(1:len_trim(retstring)),'(H2O(a1 ',BACK=.FALSE.)
	i1=i1+8
    tmp_1=cmd_blank
	do i=1,cmd_len
		if(retstring(i1:i1).ne.')') then
			tmp_1(i:i)=retstring(i1:i1)
			i1=i1+1
		else
			exit
		end if
	end do
!
    tmp_2=cmd_blank
	i2=index(retstring(i1:len_trim(retstring)),'(a2 ',BACK=.FALSE.)
	i2=i1+i2+3
    tmp_2=cmd_blank
	do i=1,cmd_len
		if(retstring(i2:i2).ne.')') then
			tmp_2(i:i)=retstring(i2:i2)
			i2=i2+1
		else
			exit
		end if
    end do
!
    tmp_3=cmd_blank
	i3=index(retstring(i2:len_trim(retstring)),'(a3 ',BACK=.FALSE.)
	i3=i2+i3+3
    tmp_3=cmd_blank
	do i=1,cmd_len
		if(retstring(i3:i3).ne.')') then
			tmp_3(i:i)=retstring(i3:i3)
			i3=i3+1
		else
			exit
		end if
	end do
!
    tmp_4=cmd_blank
	i4=index(retstring(i3:len_trim(retstring)),'(gamma ',BACK=.FALSE.)
	i4=i3+i4+6
    tmp_4=cmd_blank
	do i=1,cmd_len
		if(retstring(i4:i4).ne.')') then
			tmp_4(i:i)=retstring(i4:i4)
			i4=i4+1
		else
			exit
		end if
	end do
	Inst_Inf(iun)%info_string(9)='H2O coeffs:  a1= '//tmp_1(1:len_trim(tmp_1))//' a2= '//tmp_2(1:len_trim(tmp_2))//' a3= '//tmp_3(1:len_trim(tmp_3))//' gamma= '//tmp_4(1:len_trim(tmp_4))
!
    tmp_1=cmd_blank
	i1=index(retstring(i4:len_trim(retstring)),'(Z ',BACK=.FALSE.)
	i1=i1+i4+3
    tmp_1=cmd_blank
	do i=1,cmd_len
		if(retstring(i1:i1).ne.')') then
			tmp_1(i:i)=retstring(i1:i1)
			i1=i1+1
		else
			exit
		end if
	end do
!
    tmp_2=cmd_blank
	i2=index(retstring(i1:len_trim(retstring)),'(Zt ',BACK=.FALSE.)
	i2=i1+i2+3
    tmp_2=cmd_blank
	do i=1,cmd_len
		if(retstring(i2:i2).ne.')') then
			tmp_2(i:i)=retstring(i2:i2)
			i2=i2+1
		else
			exit
		end if
    end do
!
    tmp_3=cmd_blank
	i3=index(retstring(i2:len_trim(retstring)),'(Zm ',BACK=.FALSE.)
	i3=i2+i3+3
    tmp_3=cmd_blank
	do i=1,cmd_len
		if(retstring(i3:i3).ne.')') then
			tmp_3(i:i)=retstring(i3:i3)
			i3=i3+1
		else
			exit
		end if
	end do
!
    tmp_4=cmd_blank
	i4=index(retstring(i3:len_trim(retstring)),'(S ',BACK=.FALSE.)
	i4=i3+i4+2
    tmp_4=cmd_blank
	do i=1,cmd_len
		if(retstring(i4:i4).ne.')') then
			tmp_4(i:i)=retstring(i4:i4)
			i4=i4+1
		else
			exit
		end if
	end do
	Inst_Inf(iun)%info_string(10)='H2O coeffs:  Z= '//tmp_1(1:len_trim(tmp_1))//' Zt= '//tmp_2(1:len_trim(tmp_2))//' Zm= '//tmp_3(1:len_trim(tmp_3))//' S= '//tmp_4(1:len_trim(tmp_4))
!
    tmp_1=cmd_blank
	i1=index(retstring(i4:len_trim(retstring)),'(W0',BACK=.FALSE.)
	i1=i1+i4+5
    tmp_1=cmd_blank
	do i=1,cmd_len
		if(retstring(i1:i1).ne.')') then
			tmp_1(i:i)=retstring(i1:i1)
			i1=i1+1
		else
			exit
		end if
	end do
!
    tmp_2=cmd_blank
	i2=index(retstring(i1:len_trim(retstring)),'(W0 ',BACK=.FALSE.)
	i2=i1+i2+6
    tmp_2=cmd_blank
	do i=1,cmd_len
		if(retstring(i2:i2).ne.')') then
			tmp_2(i:i)=retstring(i2:i2)
			i2=i2+1
		else
			exit
		end if
    end do
!
    tmp_3=cmd_blank
	i3=index(retstring(i2:len_trim(retstring)),'(AGC ',BACK=.FALSE.)
	i3=i2+i3+4
    tmp_3=cmd_blank
	do i=1,cmd_len
		if(retstring(i3:i3).ne.')') then
			tmp_3(i:i)=retstring(i3:i3)
			i3=i3+1
		else
			exit
		end if
	end do
	Inst_Inf(iun)%info_string(11)='H2O coeffs:  W0= '//tmp_1(1:len_trim(tmp_1))//' W0_d= '//tmp_2(1:len_trim(tmp_2))//' AGC= '//tmp_3(1:len_trim(tmp_3))
!
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
    do i=1,Inst_Inf(iun)%n_info_strings
        Inst_Inf(iun)%info_string_len(i)=len_trim(Inst_Inf(iun)%info_string(i))
        write (info_unit,30) Inst_Inf(iun)%info_string(i)(1:len_trim(Inst_Inf(iun)%info_string(i)))
    end do
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
    write (imsg_win,30) Inst_inf(iun)%info_string(1)(1:len_trim(Inst_Inf(iun)%info_string(1)))
    write (imsg_win,30) Inst_inf(iun)%info_string(2)(1:len_trim(Inst_Inf(iun)%info_string(2)))
    write (imsg_win,30) Inst_inf(iun)%info_string(3)(1:len_trim(Inst_Inf(iun)%info_string(3)))
    write (imsg_win,30) Inst_inf(iun)%info_string(12)(1:len_trim(Inst_Inf(iun)%info_string(12)))
!
    iop(iun)=inp(iun)
    retstring=ret_blank
	call SendDataLI7000 (cmd2,ncmd2,retstring,50,iun)
!
	write (imsg_win,*) ' LI-7000 is configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitLI7000
!
!==============================================================================
!
!	SUBROUTINE InitLI820 (ibaud,iun)
!
!==============================================================================
!
	subroutine InitLI820 (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=5000
!
	type(T_DCB)::dcbcom
!
	character(len=ret_strng_len) retstring
	character(len=200) cmd1,cmd2,cmd3,cmd4
	character(len=istrng_len) benln,span,CO2ZeroDate,CO2SpanDate,hstat,pstat
	integer(kind=4) ibaud,jbaud,nwr,nbw,nbytes,inyq,i1,i2,evt_mask
!
20	format (1X,'Baud=9600 bps  8-bits  no parity')
30	format (1X,'The firmware revision number is: ',A)
40	format (1X,'You chose ',I5.5,' baud.',/,1X, &
            'Only 9600 is allowed for the LI-820.',/,1X, &
			'Defaulting to 9600 baud.')
50	format (1X,'Heater is ',A,/1X,'Pressure compensation is ',A)
60	format (1X,A,'Bench is installed',/1X,'Span is ',A)
70	format (1X,'The analyzer zero was last set on ',A,/,1X, &
            'The analyzer span was last set on ',A)
!
	do i=1,istrng_len
		Inst_Inf(iun)%SerialNo(i:i)=' '
		Inst_Inf(iun)%Firmware(i:i)=' '
		benln(i:i)=' '
		span(i:i)=' '
		CO2ZeroDate(i:i)=' '
		CO2SpanDate(i:i)=' '
		hstat(i:i)=' '
		pstat(i:i)=' '
	end do
!
	cmd1(1:51)='<li820><rs232><co2>true</co2><co2abs>false</co2abs>'
	cmd1(52:103)='<celltemp>true</celltemp><cellpress>true</cellpress>'
	cmd1(104:157)='<ivolt>false</ivolt><raw>false</raw><echo>false</echo>'
	cmd1(158:192)='<strip>true</strip></rs232><li820>'//char(10)
	ncmd1=192
	cmd2(1:53)='<li820><cfg><pcomp>true</pcomp><alarms><enabled>false'
	cmd2(54:87)='</enabled></alarms></cfg></li820>'//char(10)
	ncmd2=87
	cmd3(1:17)='<li820>?</li820>'//char(10)
	ncmd3=17
	cmd4(1:48)='<li820><cfg><outrate>00</outrate></cfg></li820>'//char(10)
	ncmd4=48
!
	write(imsg_win,*)
	write(imsg_win,*) ' Configuring the LI820 IRGA'
!
!  set up the baud rate
!
	if (ibaud.ne.9600) then
		write(imsg_win,40) ibaud
		jbaud=9600
	end if
    call Set_Baud (jbaud,iun)
	iop(iun)=inp(iun)
!
!  set up the sampling interval
!
	if (samp_interval.le.500) then
		cmd4(22:23)='01'
	else if ((samp_interval.le.1000).and.(samp_interval.gt.500)) then
		cmd4(22:23)='02'
	else if ((samp_interval.le.1500).and.(samp_interval.gt.1000)) then
		cmd4(22:23)='03'
	else if ((samp_interval.le.2000).and.(samp_interval.gt.1500)) then
		cmd4(22:23)='04'
	else if ((samp_interval.le.2500).and.(samp_interval.gt.2000)) then
		cmd4(22:23)='05'
	else if ((samp_interval.le.3000).and.(samp_interval.gt.2500)) then
		cmd4(22:23)='06'
	else if ((samp_interval.le.3500).and.(samp_interval.gt.3000)) then
		cmd4(22:23)='07'
	else if ((samp_interval.le.4000).and.(samp_interval.gt.3500)) then
		cmd4(22:23)='08'
	else if ((samp_interval.le.4500).and.(samp_interval.gt.4000)) then
		cmd4(22:23)='09'
	else if ((samp_interval.le.5000).and.(samp_interval.gt.4500)) then
		cmd4(22:23)='10'
	end if
!
	call SendDataLI820 (cmd1,ncmd1,retstring,iun)
	call SendDataLi820 (cmd2,ncmd2,retstring,iun)
	call SendDataLI820 (cmd3,ncmd3,retstring,iun)
	iend=index(retstring(1:ret_strng_len),'</li820>',BACK=.TRUE.)
	if (iend.le.10) then
		write (imsg_win,*) 'error from LI-820 status inquery'
		call stop_pgm()
	end if
	i1=index(retstring(1:iend),'<heater>',BACK=.TRUE.)
	i1=i1+8
	if (retstring(i1:i1).eq.'t') then
		hstat='on'
	else
		hstat='off'
	end if
	i1=index(retstring(1:iend),'<pcomp>',BACK=.TRUE.)
	i1=i1+7
	if (retstring(i1:i1).eq.'t') then
		pstat='on'
	else
		pstat='off'
	end if
	i1=index(retstring(1:iend),'<bench>',BACK=.TRUE.)
	i1=i1+7
	if (retstring(i1:i1).eq.'1') then
		benln='14 cm'
	else
		benln='5.5 cm'
	end if
	i1=index(retstring(1:iend),'<span>',BACK=.TRUE.)
	i1=i1+5
	do i=1,5
		if(retstring(i1+i:i1+i).eq.'<') exit
		span(i:i)=retstring(i1+i:i1+i)
		ispan=i
	end do
	i1=index(retstring(1:iend),'<co2lastzero>',BACK=.TRUE.)
	i1=i1+12
	do i=1,20
		if(retstring(i1+i:i1+i).eq.'<') exit
		CO2ZeroDate(i:i)=retstring(i1+i:i1+i)
		izer=i
	end do
	i1=index(retstring(1:iend),'<co2lastspan>',BACK=.TRUE.)
	i1=i1+12
	do i=1,20
		if(retstring(i1+i:i1+i).eq.'<') exit
		CO2SpanDate(i:i)=retstring(i1+i:i1+i)
		ispandt=i
	end do
	i1=index(retstring(1:iend),'<ver>',BACK=.TRUE.)
	i1=i1+4
	do i=1,10
		if(retstring(i1+i:i1+i).eq.'<') exit
		Inst_Inf(iun)%Firmware(i:i)=retstring(i1+i:i1+i)
	end do
	Inst_Inf(iun)%Firmware_len=len_trim(Inst_Inf(iun)%Firmware)
	Inst_Inf(iun)%SerialNo='null'
	Inst_Inf(iun)%SerialNo_len=4
!
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'LiCor LI-820 closed path IRGA'
	write (info_unit,20)
	write (info_unit,30) Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (info_unit,50) hstat(1:3),pstat(1:3)
	write (info_unit,60) benln(1:6),span(1:ispan)
	write (info_unit,70) CO2ZeroDate(1:izer),CO2SpanDate(1:ispandt)
!
	call SendDataLI820 (cmd4,ncmd4,retstring,iun)
!
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	iop(iun)=inp(iun)
!
	write (imsg_win,*) ' LI-820 is configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitLI820
!!
!==============================================================================
!
!	SUBROUTINE InitLI840 (ibaud,iun)
!
!==============================================================================
!
	subroutine InitLI840 (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=5000
!
	type(T_DCB)::dcbcom
!
	character(len=ret_strng_len) retstring
	character(len=200) cmd1,cmd2,cmd3,cmd4
	character(len=istrng_len) benln,span,CO2ZeroDate,CO2SpanDate,hstat,pstat
	integer(kind=4) ibaud,jbaud,nwr,nbw,nbytes,inyq,i1,i2,evt_mask
!
20	format (1X,'Baud=9600 bps  8-bits  no parity')
30	format (1X,'The firmware revision number is: ',A)
40	format (1X,'You chose ',I5.5,' baud.',/,1X, &
            'Only 9600 is allowed for the LI-840.',/,1X, &
			'Defaulting to 9600 baud.')
50	format (1X,'Heater is ',A,/1X,'Pressure compensation is ',A)
60	format (1X,A,'Bench is installed',/1X,'Span is ',A)
70	format (1X,'The analyzer zero was last set on ',A,/,1X, &
            'The analyzer span was last set on ',A)
!
	do i=1,istrng_len
		Inst_Inf(iun)%SerialNo(i:i)=' '
		Inst_Inf(iun)%Firmware(i:i)=' '
		benln(i:i)=' '
		span(i:i)=' '
		CO2ZeroDate(i:i)=' '
		CO2SpanDate(i:i)=' '
		hstat(i:i)=' '
		pstat(i:i)=' '
	end do
!
	cmd1(1:51)='<li840><rs232><co2>true</co2><co2abs>false</co2abs>'
	cmd1(52:103)='<celltemp>true</celltemp><cellpress>true</cellpress>'
	cmd1(104:157)='<ivolt>false</ivolt><raw>false</raw><echo>false</echo>'
	cmd1(158:192)='<strip>true</strip></rs232><li840>'//char(10)
	ncmd1=192
	cmd2(1:53)='<li840><cfg><pcomp>true</pcomp><alarms><enabled>false'
	cmd2(54:87)='</enabled></alarms></cfg></li840>'//char(10)
	ncmd2=87
	cmd3(1:17)='<li840>?</li840>'//char(10)
	ncmd3=17
	cmd4(1:48)='<li840><cfg><outrate>00</outrate></cfg></li840>'//char(10)
	ncmd4=48
!
	write(imsg_win,*)
	write(imsg_win,*) ' Configuring the LI840 IRGA'
!
!  set up the baud rate
!
	if (ibaud.ne.9600) then
		write(imsg_win,40) ibaud
		jbaud=9600
	end if
    call Set_Baud (jbaud,iun)
	iop(iun)=inp(iun)
!
!  set up the sampling interval
!
	if (samp_interval.le.500) then
		cmd4(22:23)='01'
	else if ((samp_interval.le.1000).and.(samp_interval.gt.500)) then
		cmd4(22:23)='02'
	else if ((samp_interval.le.1500).and.(samp_interval.gt.1000)) then
		cmd4(22:23)='03'
	else if ((samp_interval.le.2000).and.(samp_interval.gt.1500)) then
		cmd4(22:23)='04'
	else if ((samp_interval.le.2500).and.(samp_interval.gt.2000)) then
		cmd4(22:23)='05'
	else if ((samp_interval.le.3000).and.(samp_interval.gt.2500)) then
		cmd4(22:23)='06'
	else if ((samp_interval.le.3500).and.(samp_interval.gt.3000)) then
		cmd4(22:23)='07'
	else if ((samp_interval.le.4000).and.(samp_interval.gt.3500)) then
		cmd4(22:23)='08'
	else if ((samp_interval.le.4500).and.(samp_interval.gt.4000)) then
		cmd4(22:23)='09'
	else if ((samp_interval.le.5000).and.(samp_interval.gt.4500)) then
		cmd4(22:23)='10'
	end if
!
	call SendDataLI820 (cmd1,ncmd1,retstring,iun)
	call SendDataLi820 (cmd2,ncmd2,retstring,iun)
	call SendDataLI820 (cmd3,ncmd3,retstring,iun)
	iend=index(retstring(1:5000),'</li840>',BACK=.TRUE.)
	if (iend.le.10) then
		write (imsg_win,*) 'error from LI-840 status inquery'
		call stop_pgm()
	end if
	i1=index(retstring(1:iend),'<heater>',BACK=.TRUE.)
	i1=i1+8
	if (retstring(i1:i1).eq.'t') then
		hstat='on'
	else
		hstat='off'
	end if
	i1=index(retstring(1:iend),'<pcomp>',BACK=.TRUE.)
	i1=i1+7
	if (retstring(i1:i1).eq.'t') then
		pstat='on'
	else
		pstat='off'
	end if
	i1=index(retstring(1:iend),'<bench>',BACK=.TRUE.)
	i1=i1+7
	if (retstring(i1:i1).eq.'1') then
		benln='14 cm'
	else
		benln='5.5 cm'
	end if
	i1=index(retstring(1:iend),'<span>',BACK=.TRUE.)
	i1=i1+5
	do i=1,5
		if(retstring(i1+i:i1+i).eq.'<') exit
		span(i:i)=retstring(i1+i:i1+i)
		ispan=i
	end do
	i1=index(retstring(1:iend),'<co2lastzero>',BACK=.TRUE.)
	i1=i1+12
	do i=1,20
		if(retstring(i1+i:i1+i).eq.'<') exit
		CO2ZeroDate(i:i)=retstring(i1+i:i1+i)
		izer=i
	end do
	i1=index(retstring(1:iend),'<co2lastspan>',BACK=.TRUE.)
	i1=i1+12
	do i=1,20
		if(retstring(i1+i:i1+i).eq.'<') exit
		CO2SpanDate(i:i)=retstring(i1+i:i1+i)
		ispandt=i
	end do
	i1=index(retstring(1:iend),'<ver>',BACK=.TRUE.)
	i1=i1+4
	do i=1,10
		if(retstring(i1+i:i1+i).eq.'<') exit
		Inst_Inf(iun)%Firmware(i:i)=retstring(i1+i:i1+i)
	end do
	Inst_Inf(iun)%Firmware_len=len_trim(Inst_Inf(iun)%Firmware)
	Inst_Inf(iun)%SerialNo='null'
	Inst_Inf(iun)%SerialNo_len=4
!
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'LiCor LI-840 closed path IRGA'
	write (info_unit,20)
	write (info_unit,30) Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
	write (info_unit,50) hstat(1:3),pstat(1:3)
	write (info_unit,60) benln(1:6),span(1:ispan)
	write (info_unit,70) CO2ZeroDate(1:izer),CO2SpanDate(1:ispandt)
!
	call SendDataLI820 (cmd4,ncmd4,retstring,iun)
!
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	iop(iun)=inp(iun)
!
	write (imsg_win,*) ' LI-840 is configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitLI840
!
!==============================================================================
!
!	SUBROUTINE InitLI7700 (ibaud,iun)
!
!==============================================================================
!
	subroutine InitLI7700 (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=5000
	type(T_DCB)::dcbcom
!
	character(len=ret_strng_len) retstring
	character(len=100) cmd1,cmd2,cmd3,cmd4,cmd5,cmd6,cmd7
	character(len=istrng_len) CH4ZeroDate,CH4SpanDate,hstat
	character(len=istrng_len) net_name,net_addr
	character(len=36) mon_string
	integer(kind=4) ibaud,jbaud,nwr,nbw,nbytes,inyq,i1,i2,evt_mask
!
10	format (1X,'You chose ',I5.5,' baud.',/,1X, &
            'Only 115200 baud is allowed for the LI-7700.',/,1X, &
			'Defaulting to 115200 baud.')
20	format (I)
30	format (1X,'Serial No.: ',A,/,1X,'Firmware version: ',A,1X,'Last CH4 zero: ',A,/,1X,'Last CH4 span: ',A)
40	format (1X,'Baud rate: ',I7,' bps',/,1X,'Network name: ',A,/,1X,'Instrument network ip address: ',A)
!
	mon_string(1:36)='JanFebMarAprMayJunJulAugSepOctNovDec'
!
	do i=1,istrng_len
		cmd1(i:i)=' '
		cmd2(i:i)=' '
		cmd3(i:i)=' '
		cmd4(i:i)=' '
		cmd5(i:i)=' '
		cmd6(i:i)=' '
		cmd7(i:i)=' '
		Inst_Inf(iun)%SerialNo(i:i)=' '
		Inst_Inf(iun)%Firmware(i:i)=' '
	end do
	cmd1='<licor><li7700><output><rate>0</rate></output></li7700></licor>'//char(10)
	ncmd1=len_trim(cmd1)
	cmd2='<licor><li7700><serialnumber>&query;</serialnumber></li7700></licor>'//char(10)
	ncmd2=len_trim(cmd2)
	cmd3='<licor><li7700><cfg>&query;</cfg></li7700></licor>'//char(10)
	ncmd3=len_trim(cmd3)
	cmd4='<licor><li7700><cal>&query;</cal></li7700></licor>'//char(10)
	ncmd4=len_trim(cmd4)
	cmd5=char(13)//char(10)
	ncmd5=len_trim(cmd5)
	cmd7='<licor><li7700><ver>&query;</ver></li7700></licor>'//char(10)
	ncmd7=len_trim(cmd7)
	write(imsg_win,*)
	write(imsg_win,*) '      Configuring the LI7700 CH4 TDLS'
!
!  set up the baud rate
!
	if (ibaud.ne.115200) then
		write(imsg_win,10) ibaud
	end if
	jbaud=115200
    call Set_Baud (jbaud,iun)
	iop(iun)=inp(iun)
!
!  set up the sampling interval
!
	if (samp_interval.eq.1000) then
		cmd6='<licor><li7700><output><rate>1</rate></output></li7700></licor>'//char(10)
	else if (samp_interval.eq.500) then
		cmd6='<licor><li7700><output><rate>2</rate></output></li7700></licor>'//char(10)
	else if (samp_interval.eq.200) then
		cmd6='<licor><li7700><output><rate>5</rate></output></li7700></licor>'//char(10)
	else if (samp_interval.eq.100) then
		cmd6='<licor><li7700><output><rate>10</rate></output></li7700></licor>'//char(10)
	else if (samp_interval.eq.50) then
		cmd6='<licor><li7700><output><rate>20</rate></output></li7700></licor>'//char(10)
	else if (samp_interval.eq.25) then
		cmd6='<licor><li7700><output><rate>40</rate></output></li7700></licor>'//char(10)
	else
		write (imsg_win,*)'Error: bad LI-7700 output rate ',samp_interval
		call stop_pgm()
	end if
	ncmd6=len_trim(cmd6)
!
!  wake up the ethernet to serial converter
!
	call SendDataLI7700 (cmd5,ncmd5,retstring,iun)
	call wait (200)
	do i=1,ret_strng_len
		retstring(i:i)=' '
	end do
!
!  stop the data output
!
	call SendDataLI7700 (cmd1,ncmd1,retstring,iun)
!
!  erase the return string
!
	do i=1,ret_strng_len
		retstring(i:i)=' '
	end do
!
!  get the serial number
!
	call SendDataLI7700 (cmd2,ncmd2,retstring,iun)
	itmp1=index (retstring(1:len(retstring)),'</serial',BACK=.FALSE.)
	Inst_Inf(iun)%SerialNo=retstring(30:itmp1-1)
	Inst_Inf(iun)%SerialNo_len=len_trim(Inst_Inf(iun)%SerialNo)
	write (imsg_win,*) 'serial number: ',Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len)
!
!  erase the return string
!
	do i=1,ret_strng_len
		retstring(i:i)=' '
	end do
!
!  get the version number
!
	call SendDataLI7700 (cmd7,ncmd7,retstring,iun)
	itmp1=index (retstring(1:len(retstring)),'</ver',BACK=.FALSE.)
	Inst_Inf(iun)%Firmware=retstring(21:itmp1-1)
	Inst_Inf(iun)%Firmware_len=len_trim(Inst_Inf(iun)%Firmware)
	write (imsg_win,*) 'firmware version: ',Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len)
!
!  erase the return string
!
	do i=1,ret_strng_len
		retstring(i:i)=' '
	end do
!
!  get the network information
!
	call SendDataLI7700 (cmd3,ncmd3,retstring,iun)
	itmp1=index (retstring(1:len(retstring)),'<name>',BACK=.FALSE.)
	itmp2=index (retstring(1:len(retstring)),'</name>',BACK=.FALSE.)
	itmp1=itmp1+6
	inamelen=itmp2-itmp1
	net_name(1:inamelen)=retstring(itmp1:itmp2-1)
	write (imsg_win,*) 'network name: ',net_name(1:inamelen)
	itmp1=index (retstring(1:len(retstring)),'<ipaddress>',BACK=.FALSE.)
	itmp2=index (retstring(1:len(retstring)),'</ipaddress>',BACK=.FALSE.)
	itmp1=itmp1+11
	iipaddrlen=itmp2-itmp1
	net_addr(1:iipaddrlen)=retstring(itmp1:itmp2-1)
	write (imsg_win,*) 'instrument network ip address: ',net_addr(1:iipaddrlen)
!
!  erase the return string
!
	do i=1,ret_strng_len
		retstring(i:i)=' '
	end do
!
!  get the calibration information
!
	call SendDataLI7700 (cmd4,ncmd4,retstring,iun)
	itmp1=index(retstring(1:len(retstring)),'<ch4lastzero>',BACK=.FALSE.)
	itmp1=itmp1+13
	read(retstring(itmp1+5:itmp1+6),20) imon
	imon=(imon-1)*3
	CH4ZeroDate(1:3)=mon_string(imon+1:imon+3)
	CH4ZeroDate(4:4)=' '
	CH4ZeroDate(5:6)=retstring(itmp1+8:itmp1+9)
	CH4ZeroDate(7:8)=', '
	CH4ZeroDate(9:12)=retstring(itmp1:itmp1+3)
	CH4ZeroDate(13:18)=retstring(itmp1+10:itmp1+15)
	write (imsg_win,*) 'last zero date: ',CH4ZeroDate(1:18)
!
	itmp1=index(retstring(1:len(retstring)),'<ch4lastspan>',BACK=.FALSE.)
	itmp1=itmp1+13
	read(retstring(itmp1+5:itmp1+6),20) imon
	imon=(imon-1)*3
	CH4SpanDate(1:3)=mon_string(imon+1:imon+3)
	CH4SpanDate(4:4)=' '
	CH4SpanDate(5:6)=retstring(itmp1+8:itmp1+9)
	CH4SpanDate(7:8)=', '
	CH4SpanDate(9:12)=retstring(itmp1:itmp1+3)
	CH4SpanDate(13:18)=retstring(itmp1+10:itmp1+15)
	write (imsg_win,*) 'last span date: ',CH4SpanDate(1:18)
!
!  start the data strean
!
	call SendDataLI7700 (cmd6,ncmd6,retstring,iun)
!
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'LiCor LI-7700 open path CH4 TDLS'
	write (info_unit,30) Inst_Inf(iun)%SerialNo(1:Inst_Inf(iun)%SerialNo_len), &
						 Inst_Inf(iun)%Firmware(1:Inst_Inf(iun)%Firmware_len),CH4ZeroDate(1:18), &
						 CH4SpanDate(1:18)
	write (info_unit,40) jbaud,Net_name(1:inamelen),Net_addr(1:iipaddrlen)
!
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	iop(iun)=inp(iun)
!
	write (imsg_win,*) ' LI-7700 is configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitLI7700
!
!==============================================================================
!
!	SUBROUTINE InitAerodyneTDL (ibaud,iun)
!
!==============================================================================
!
	subroutine InitAerodyneTDL (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=5000
!
	integer(kind=4) ibaud,jbaud,nwr,nbw,nbytes,inyq,i1,i2
!
	Inst_Inf(iun)%SerialNo='null'
	Inst_Inf(iun)%SerialNo_len=4
	Inst_Inf(iun)%Firmware='null'
	Inst_Inf(iun)%Firmware_len=4
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'Aerodyne COS closed-path TDLAS'
!
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	iop(iun)=inp(iun)
!
	write (imsg_win,*) ' Aerodyne TDLAS is configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitAerodyneTDL
!
!==============================================================================
!
!	SUBROUTINE InitPicarroTDL (ibaud,iun)
!
!==============================================================================
!
	subroutine InitPicarroTDL (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=5000
!
	integer(kind=4) ibaud,jbaud,nwr,nbw,nbytes,inyq,i1,i2
!
	Inst_Inf(iun)%SerialNo='null'
	Inst_Inf(iun)%SerialNo_len=4
	Inst_Inf(iun)%Firmware='null'
	Inst_Inf(iun)%Firmware_len=4
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'Picarro 2311-f CRD TDLAS'
!
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	iop(iun)=inp(iun)
!
	write (imsg_win,*) ' Picarro TDLAS is configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitPicarroTDL
!
!==============================================================================
!
!	SUBROUTINE InitLGRTDL (ibaud,iun)
!
!==============================================================================
!
	subroutine InitLGRTDL (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=5000
!
	integer(kind=4) ibaud,jbaud,nwr,nbw,nbytes,inyq,i1,i2
!
	Inst_Inf(iun)%SerialNo='null'
	Inst_Inf(iun)%SerialNo_len=4
	Inst_Inf(iun)%Firmware='null'
	Inst_Inf(iun)%Firmware_len=4
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'Los Gatos 911-0010 OA-ICOS TDLAS'
!
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	iop(iun)=inp(iun)
!
	write (imsg_win,*) ' Los Gatos TDLAS is configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitLGRTDL
!
!==============================================================================
!
!	SUBROUTINE InitIRGASON (ibaud,iun)
!
!==============================================================================
!
	subroutine InitIRGASON (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=5000
!
	integer(kind=4) ibaud,jbaud,nwr,nbw,nbytes,inyq,i1,i2
!
	Inst_Inf(iun)%SerialNo='null'
	Inst_Inf(iun)%SerialNo_len=4
	Inst_Inf(iun)%Firmware='null'
	Inst_Inf(iun)%Firmware_len=4
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'Campbell Scientific IRGASON IRGA'
!
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	iop(iun)=inp(iun)
!
	write (imsg_win,*) ' IRGASON IRGA is configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitIRGASON
!
!==============================================================================
!
!	SUBROUTINE InitEC155 (ibaud,iun)
!
!==============================================================================
!
	subroutine InitEC155 (ibaud,iun)
!
	use common_vars
	use serial_vars
	use file_names
	use Instrument_Info
!
	integer(kind=4),parameter:: istrng_len=30,ret_strng_len=5000
!
	integer(kind=4) ibaud,jbaud,nwr,nbw,nbytes,inyq,i1,i2
!
	Inst_Inf(iun)%SerialNo='null'
	Inst_Inf(iun)%SerialNo_len=4
	Inst_Inf(iun)%Firmware='null'
	Inst_Inf(iun)%Firmware_len=4
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*)
	write (info_unit,*) 'Campbell Scientific EC155 IRGA'
!
	write (info_unit,*)
	write (info_unit,*) '*******************************************************************************'
	close (unit=info_unit)
!
	iop(iun)=inp(iun)
!
	write (imsg_win,*) ' EC155 IRGA is configured and active'
	write (imsg_win,*)
!
	return
	end subroutine InitEC155
!
!==============================================================================
!
!	logical*FUNCTION ATIbinSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function ATIbinSync (iun)
!
	use common_vars
!
	character(len=4) tststr,terms
	integer(kind=4) inum,i,ip,ip1,ip10,ip11
!
	terms(1:4)=char(128)//char(0)//char(128)//char(0)
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
	ip1=ip
	ip10=ip
	ip11=ip
	call badinc(ip1,1)
	call badinc(ip10,10)
	call badinc(ip11,11)
!
	do i=1,inum
		tststr(1:4)=inbuf(iun)(ip:ip)//inbuf(iun)(ip1:ip1)// &
		            inbuf(iun)(ip10:ip10)//inbuf(iun)(ip11:ip11)
		if (tststr(1:4).eq.terms(1:4)) then
			call badinc(ip,2)
			iop(iun)=ip
			ATIbinSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
		call badinc(ip10,1)
		call badinc(ip11,1)
	end do
	inp(iun)=iop(iun)
	ATIbinSync=.false.
!
	return
	end function ATIbinSync
!
!==============================================================================
!
!	logical*FUNCTION R3binSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function R3binSync (iun)
!
	use common_vars
!
	character(len=4) tststr,terms
	integer(kind=4) inum,i,ip,ip1,ip27,ip28
!
	terms(1:4)=char(186)//char(186)//char(186)//char(186)
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
	ip1=ip
	ip27=ip
	ip28=ip
	call badinc(ip1,1)
	call badinc(ip27,27)
	call badinc(ip28,28)
!
	do i=1,inum
		tststr(1:4)=inbuf(iun)(ip:ip)//inbuf(iun)(ip1:ip1)// &
		            inbuf(iun)(ip27:ip27)//inbuf(iun)(ip28:ip28)
		if (tststr(1:4).eq.terms(1:4)) then
			call badinc(ip,2)
			iop(iun)=ip
			R3binSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
		call badinc(ip27,1)
		call badinc(ip28,1)
	end do
	inp(iun)=iop(iun)
	R3binSync=.false.
!
	return
	end function R3binSync
!
!==============================================================================
!
!	logical*FUNCTION WMPbinSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function WMPbinSync (iun)
!
	use common_vars
!
	character(len=4) tststr,terms
	integer(kind=4) inum,i,ip,ip1,ip28,ip29
!
	terms(1:4)=char(129)//char(129)//char(129)//char(129)
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
	ip1=ip
	ip28=ip
	ip29=ip
	call badinc(ip1,1)
	call badinc(ip28,28)
	call badinc(ip29,29)
!
	do i=1,inum
		tststr(1:4)=inbuf(iun)(ip:ip)//inbuf(iun)(ip1:ip1)// &
		            inbuf(iun)(ip28:ip28)//inbuf(iun)(ip29:ip29)
		if (tststr(1:4).eq.terms(1:4)) then
			call badinc(ip,2)
			iop(iun)=ip
			WMPbinSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
		call badinc(ip28,1)
		call badinc(ip29,1)
	end do
	inp(iun)=iop(iun)
	WMPbinSync=.false.
!
	return
	end function WMPbinSync
!
!
!==============================================================================
!
!	logical*FUNCTION CSATbinSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function CSATbinSync (iun)
!
	use common_vars
!
	character(len=4) tststr,terms
	integer(kind=4) inum,i,ip,ip1,ip12,ip13
!
	terms(1:4)=char(85)//char(170)//char(85)//char(170)
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
	ip1=ip
	ip12=ip
	ip13=ip
	call badinc(ip1,1)
	call badinc(ip12,12)
	call badinc(ip13,13)
!
	do i=1,inum
		tststr(1:4)=inbuf(iun)(ip:ip)//inbuf(iun)(ip1:ip1)// &
		            inbuf(iun)(ip12:ip12)//inbuf(iun)(ip13:ip13)
		if (tststr(1:4).eq.terms(1:4)) then
			call badinc(ip,-10)
			iop(iun)=ip
			CSATbinSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
		call badinc(ip12,1)
		call badinc(ip13,1)
	end do
	inp(iun)=iop(iun)
	CSATbinSync=.false.
!
	return
	end function CSATbinSync
!
!==============================================================================
!
!	logical*FUNCTION Y81000binSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function Y81000binSync (iun)
!
	use common_vars
!
	character(len=4)tststr,terms
	integer(kind=4) inum,i,ip,ip1,ip20,ip21
!
	terms(1:4)=char(171)//char(205)//char(171)//char(205)
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
	ip1=ip
	ip20=ip
	ip21=ip
	call badinc(ip1,1)
	call badinc(ip20,20)
	call badinc(ip21,21)
!
	do i=1,inum
		tststr(1:4)=inbuf(iun)(ip:ip)//inbuf(iun)(ip1:ip1)// &
		            inbuf(iun)(ip20:ip20)//inbuf(iun)(ip21:ip21)
		if (tststr(1:4).eq.terms(1:4)) then
			call badinc(ip,2)
			iop(iun)=ip
			Y81000binSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
		call badinc(ip20,1)
		call badinc(ip21,1)
	end do
	inp(iun)=iop(iun)
	Y81000binSync=.false.
!
	return
	end function Y81000binSync
!
!==============================================================================
!
!	logical*FUNCTION LI7500ascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function LI7500ascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip,ip1
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
	ip1=ip
	call badinc(ip1,1)
!
	do i=1,inum
		if ((inbuf(iun)(ip:ip).eq.char(13)).and.(inbuf(iun)(ip1:ip1) &
		    .eq.char(10))) then
			call badinc(ip,2)
			iop(iun)=ip
			LI7500ascSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
	end do
	inp(iun)=iop(iun)
	LI7500ascSync=.false.
!
	return
	end function LI7500ascSync
!
!==============================================================================
!
!	logical*FUNCTION LI7500AascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function LI7500AascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip,ip1
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
	ip1=ip
	call badinc(ip1,1)
!
	do i=1,inum
		if ((inbuf(iun)(ip:ip).eq.char(13)).and.(inbuf(iun)(ip1:ip1) &
		    .eq.char(10))) then
			call badinc(ip,2)
			iop(iun)=ip
			LI7500AascSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
	end do
	inp(iun)=iop(iun)
	LI7500AascSync=.false.
!
	return
	end function LI7500AascSync
!
!==============================================================================
!
!	logical*FUNCTION LI7200ascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function LI7200ascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip,ip1
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
	ip1=ip
	call badinc(ip1,1)
!
	do i=1,inum
		if ((inbuf(iun)(ip:ip).eq.char(13)).and.(inbuf(iun)(ip1:ip1) &
		    .eq.char(10))) then
			call badinc(ip,2)
			iop(iun)=ip
			LI7200ascSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
	end do
	inp(iun)=iop(iun)
	LI7200ascSync=.false.
!
	return
	end function LI7200ascSync
!
!==============================================================================
!
!	logical*FUNCTION LI6262ascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function LI6262ascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip,ip1
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
	ip1=ip
	call badinc(ip1,1)
!
	do i=1,inum
		if ((inbuf(iun)(ip:ip).eq.char(13)).and.(inbuf(iun)(ip1:ip1) &
		    .eq.char(10))) then
			call badinc(ip,2)
			iop(iun)=ip
			LI6262ascSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
	end do
	inp(iun)=iop(iun)
	LI6262ascSync=.false.
!
	return
	end function LI6262ascSync
!
!==============================================================================
!
!	logical*FUNCTION LI7000ascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function LI7000ascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip,ip1
	character(len=6) tststrng,header
!
	header(1:5)='DATA'//char(9)
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
!
	do i=1,inum
		ip1=ip
		do j=1,5
			tststrng(j:j)=inbuf(iun)(ip1:ip1)
			call badinc(ip1,1)
		end do
		if (tststrng(1:5).eq.header(1:5)) then
!
            call badinc(ip1,1)
!
			iop(iun)=ip1
			LI7000ascSync=.true.
			return
		end if
		call badinc(ip,1)
	end do
	inp(iun)=iop(iun)
	LI7000ascSync=.false.
!
	return
	end function LI7000ascSync
!
!==============================================================================
!
!	logical*FUNCTION LI820ascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function LI820ascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip,ip1
	character(len=6) tststrng,header
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
!
	ip1=ip
	call badinc(ip1,1)
!
	do i=1,inum
		if ((inbuf(iun)(ip:ip).eq.char(13)).and.(inbuf(iun)(ip1:ip1) &
			.eq.char(10))) then
			call badinc(ip,2)
			iop(iun)=ip
			LI820ascSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
	end do
	inp(iun)=iop(iun)
	LI820ascSync=.false.
!
	return
	end function LI820ascSync
!
!==============================================================================
!
!	logical*FUNCTION LI840ascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function LI840ascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip,ip1
	character(len=6) tststrng,header
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
!
	ip1=ip
	call badinc(ip1,1)
!
	do i=1,inum
		if ((inbuf(iun)(ip:ip).eq.char(13)).and.(inbuf(iun)(ip1:ip1) &
			.eq.char(10))) then
			call badinc(ip,2)
			iop(iun)=ip
			LI840ascSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
	end do
	inp(iun)=iop(iun)
	LI840ascSync=.false.
!
	return
	end function LI840ascSync
!
!==============================================================================
!
!	logical*FUNCTION LI7700ascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function LI7700ascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip
	character(len=6) tststrng,header
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
!
	ip1=ip
!
	do i=1,inum
		if (inbuf(iun)(ip:ip).eq.'D') then
			call badinc(ip,5)
			iop(iun)=ip
			LI7700ascSync=.true.
			return
		end if
		call badinc(ip,1)
	end do
	inp(iun)=iop(iun)
	LI7700ascSync=.false.
!
	return
	end function LI7700ascSync
!
!==============================================================================
!
!	logical*FUNCTION AerodyneTDLascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function AerodyneTDLascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
!
	do i=1,inum
		if (inbuf(iun)(ip:ip).eq.char(97)) then
			iop(iun)=ip
			AerodyneTDLascSync=.true.
			return
		end if
		call badinc(ip,1)
	end do
	inp(iun)=iop(iun)
	AerodyneTDLascSync=.false.
!
	return
	end function AerodyneTDLascSync
!
!==============================================================================
!
!	logical*FUNCTION PicarroTDLascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function PicarroTDLascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip,ip1
	character(len=2) terms
!
	terms(1:2)=char(13)//char(10)
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
	ip1=ip
	call badinc(ip1,1)
!
	do i=1,inum
		if (inbuf(iun)(ip:ip1).eq.terms(1:2)) then
			call badinc(ip,3)
			iop(iun)=ip
			PicarroTDLascSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
	end do
	inp(iun)=iop(iun)
	PicarroTDLascSync=.false.
!
	return
	end function PicarroTDLascSync
!
!==============================================================================
!
!	logical*FUNCTION LGRTDLascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function LGRTDLascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip,ip1
	character(len=2) terms
!
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
!
	do i=1,inum
		if (inbuf(iun)(ip:ip).eq.char(10)) then
			call badinc(ip,1)
			iop(iun)=ip
			LGRTDLascSync=.true.
			return
		end if
		call badinc(ip,1)
	end do
	inp(iun)=iop(iun)
	LGRTDLascSync=.false.
!
	return
	end function LGRTDLascSync
!
!==============================================================================
!
!	logical*FUNCTION IRGASONascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function IRGASONascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip,ip1
	character(len=2) terms
!
	terms(1:2)=char(13)//char(10)
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
	ip1=ip
	call badinc(ip1,1)
!
	do i=1,inum
		if (inbuf(iun)(ip:ip1).eq.terms(1:2)) then
			call badinc(ip,2)
			iop(iun)=ip
			IRGASONascSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
	end do
	inp(iun)=iop(iun)
	IRGASONascSync=.false.
!
	return
	end function IRGASONascSync
!
!==============================================================================
!
!	logical*FUNCTION EC155ascSync (iun)
!
!  This function synchronizes the input buffer pointer to the beginning of a
!  data packet.  It returns as either .TRUE. or .FALSE.
!
!==============================================================================
!
	logical function EC155ascSync (iun)
!
	use common_vars
!
	integer(kind=4) inum,i,ip,ip1
	character(len=2) terms
!
	terms(1:2)=char(13)//char(10)
	do
		inum=nbyts(iop(iun),inp(iun))
		if (inum.ge.npacket(serial(iun)%ktype)) goto 15
	end do
!
15	ip=iop(iun)
	ip1=ip
	call badinc(ip1,1)
!
	do i=1,inum
		if (inbuf(iun)(ip:ip1).eq.terms(1:2)) then
			call badinc(ip,2)
			iop(iun)=ip
			EC155ascSync=.true.
			return
		end if
		call badinc(ip,1)
		call badinc(ip1,1)
	end do
	inp(iun)=iop(iun)
	EC155ascSync=.false.
!
	return
	end function EC155ascSync
!
!==============================================================================
!
!	SUBROUTINE ATIbinCnvrt (iraw,araw,iun)
!
!  This subroutine converts synchronized, binary input data from an ATI sonic
!  anemometer into a string of 4 2-byte integers arranged as: u, v, w, T.  It
!  returns them in the order specified in the SETUP.PAR file
!
!  The 10 byte packet output by the ATI sonic is:
!
!  <80h>,<00h>,u1,u2,v1,v2,w1,w2,T1,T2
!
!==============================================================================
!
	subroutine ATIbinCnvrt (iraw,araw,iun)
!
	use common_vars
!
	character(len=2) chu,chv,chw,cht
	character(len=4) tststr,terms
	integer(kind=2) ju,jv,jw,jt
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
!
	equivalence(ju,chu)
	equivalence(jv,chv)
	equivalence(jw,chw)
	equivalence(jt,cht)
!
	terms(1:4)=char(128)//char(0)//char(128)//char(0)
	ip=iop(iun)
	ipm1=iop(iun)
	ipm2=iop(iun)
	ip9=iop(iun)
	ip8=iop(iun)
	call badinc(ip9,9)
	call badinc(ip8,8)
	call badinc(ipm2,-2)
	call badinc(ipm1,-1)
	tststr(1:4)=inbuf(iun)(ipm2:ipm2)//inbuf(iun)(ipm1:ipm1)// &
	            inbuf(iun)(ip8:ip8)//inbuf(iun)(ip9:ip9)
!
	if (tststr(1:4).ne.terms(1:4)) then
		inum=nbyts(iop(iun),inp(iun))
		do i=1,inum
			call badinc(ip9,1)
			call badinc(ip8,1)
			call badinc(ipm2,1)
			call badinc(ipm1,1)
			tststr(1:4)=inbuf(iun)(ipm2:ipm2)//inbuf(iun)(ipm1:ipm1)// &
				        inbuf(iun)(ip8:ip8)//inbuf(iun)(ip9:ip9)
			if (tststr(1:4).eq.terms(1:4)) then
				call badinc(iop(iun),i)
				exit
			end if
		end do
		nBadSync(iun)=nBadSync(iun)+1
		do i=1,4
			iraw(i,iun)=-9999
			araw(i,iun)=-9999.0
		end do
		return
	end if
!
	chu(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chu(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chv(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chv(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chw(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chw(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cht(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cht(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),3)
!
	iraw(1,iun)=ju										! u (m s-1 x100)
	iraw(2,iun)=jv										! v (m s-1 x100)
	iraw(3,iun)=jw										! w (m s-1 x100)
	iraw(4,iun)=jt										! Tsonic (C x100)
!
	araw(1,iun)=float(ju)*0.01							! u (m s-1)
	araw(2,iun)=float(jv)*0.01							! v (m s-1)
	araw(3,iun)=float(jw)*0.01							! w (m s-1)
	araw(4,iun)=float(jt)*0.01							! Tsonic (C)
!
	return
	end subroutine ATIbinCnvrt
!
!==============================================================================
!
!	SUBROUTINE R3binCnvrt (iraw,araw,iun)
!
!  This subroutine converts synchronized, binary input data from a Gill R3 
!  sonic anemometer into a string of 11 2-byte integers arranged as: u, v, w,
!  Ts, Tprt, Ain1, Ain2, Ain3, Ain4, Ain5, Ain6.  The analog inputs are returned
!  as millivolts.  The routine returns them in the order specified in the
!  SETUP.PAR file.  This also applies to the Gill R3-50
!
!  The 27 byte packet output by the Gill R3 (R3-50) sonic is:
!
!  <BAh>,<BAh>,statA,statD,u1,u2,v1,v2,w1,w2,C1,C2,T1,T2,A11,A12,A21,A22,
!  A31,A32,A41,A42,A51,A52,A61,A62,<chk sum>
!
!==============================================================================
!
	subroutine R3binCnvrt (iraw,araw,iun)
!
	use common_vars
!
	character(len=2) chu,chv,chw,cht,chprt,cha1,cha2,cha3,cha4,cha5,cha6
	character(len=4) tststr,terms
	integer(kind=2) ju,jv,jw,jt,jprt,ja1,ja2,ja3,ja4,ja5,ja6
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
!
	equivalence(ju,chu)
	equivalence(jv,chv)
	equivalence(jw,chw)
	equivalence(jt,cht)
	equivalence(jprt,chprt)
	equivalence(ja1,cha1)
	equivalence(ja2,cha2)
	equivalence(ja3,cha3)
	equivalence(ja4,cha4)
	equivalence(ja5,cha5)
	equivalence(ja6,cha6)
!
	terms(1:4)=char(186)//char(186)//char(186)//char(186)
	ip=iop(iun)
	ipm1=iop(iun)
	ipm2=iop(iun)
	ip25=iop(iun)
	ip26=iop(iun)
	call badinc(ip25,25)
	call badinc(ip26,26)
	call badinc(ipm1,-1)
	call badinc(ipm2,-2)
	tststr(1:4)=inbuf(iun)(ipm2:ipm2)//inbuf(iun)(ipm1:ipm1)// &
	            inbuf(iun)(ip25:ip25)//inbuf(iun)(ip26:ip26)
!
	if (tststr(1:4).ne.terms(1:4)) then
		inum=nbyts(iop(iun),inp(iun))
		do i=1,inum
			call badinc(ip25,1)
			call badinc(ip26,1)
			call badinc(ipm1,1)
			call badinc(ipm2,1)
			tststr(1:4)=inbuf(iun)(ipm2:ipm2)//inbuf(iun)(ipm1:ipm1)// &
				        inbuf(iun)(ip25:ip25)//inbuf(iun)(ip26:ip26)
			if (tststr(1:4).eq.terms(1:4)) then
				call badinc(iop(iun),i)
				exit
			end if
		end do
		nBadSync(iun)=nBadSync(iun)+1
		do i=1,11
			iraw(i,iun)=-9999
			araw(i,iun)=-9999.0
		end do
		return
	end if
!
	call badinc(iop(iun),2)
	chu(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chu(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chv(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chv(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chw(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chw(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cht(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cht(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chprt(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chprt(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha1(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha1(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha2(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha2(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha3(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha3(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha4(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha4(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha5(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha5(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha6(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha6(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),4)
!
	iraw(1,iun)=ju										! u (m s-1 x100)
	iraw(2,iun)=jv										! v (m s-1 x100)
	iraw(3,iun)=jw										! w (m s-1 x100)
	iraw(4,iun)=jt										! Tsonic (C x100)
	iraw(5,iun)=jprt									! Tprt (C x100)
!
	araw(1,iun)=float(ju)*0.01							! u (m s-1)
	araw(2,iun)=float(jv)*0.01							! v (m s-1)
	araw(3,iun)=float(jw)*0.01							! w (m s-1)
	araw(4,iun)=float(jt)*0.01							! Tsonic (C)
	araw(5,iun)=float(jprt)*0.01						! Tprt (C)
	araw(6,iun)=float(ja1)*0.61043						! Analog(1) (mv)
	araw(7,iun)=float(ja2)*0.61043						! Analog(2) (mv)
	araw(8,iun)=float(ja3)*0.61043						! Analog(3) (mv)
	araw(9,iun)=float(ja4)*0.61043						! Analog(4) (mv)
	araw(10,iun)=float(ja5)*0.61043						! Analog(5) (mv)
	araw(11,iun)=float(ja6)*0.61043						! Analog(6) (mv)
!
	iraw(6,iun)=int2(araw(6,iun))						! Analog(1) (mv)
	iraw(7,iun)=int2(araw(7,iun))						! Analog(2) (mv)
	iraw(8,iun)=int2(araw(8,iun))						! Analog(3) (mv)
	iraw(9,iun)=int2(araw(9,iun))						! Analog(4) (mv)
	iraw(10,iun)=int2(araw(10,iun))						! Analog(5) (mv)
	iraw(11,iun)=int2(araw(11,iun))						! Analog(6) (mv)
!
	return
	end subroutine R3binCnvrt
!
!==============================================================================
!
!	SUBROUTINE WMPbinCnvrt (iraw,araw,iun)
!
!  This subroutine converts synchronized, binary input data from a Gill WMP
!  sonic anemometer into a string of 8 2-byte integers arranged as: u, v, w,
!  Ts, Ain1, Ain2, Ain3, Ain4.  The analog inputs are returned as millivolts.
!  It returns them in the order specified in the SETUP.PAR file
!
!  The 28 byte packet output by the Gill WindMaster Pro sonic is:
!
!  <81h>,<81h>,u1,u2,v1,v2,w1,w2,C1,C2,A11,A12,A13,A14,A21,A22,A23,A24,
!  A31,A32,A33,A34,A41,A42,A43,A44,<stat1>,<stat2>
!
!==============================================================================
!
	subroutine WMPbinCnvrt (iraw,araw,iun)
!
	use common_vars
!
	character(len=2) chu,chv,chw,cht
	character(len=4) cha1,cha2,cha3,cha4,tststr,terms
	character(len=1000) tmpstr
	integer(kind=2) ju,jv,jw,jt
	integer(kind=4) ja1,ja2,ja3,ja4
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) c
!
	equivalence(ju,chu)
	equivalence(jv,chv)
	equivalence(jw,chw)
	equivalence(jt,cht)
	equivalence(ja1,cha1)
	equivalence(ja2,cha2)
	equivalence(ja3,cha3)
	equivalence(ja4,cha4)
!
	terms(1:4) = char(129)//char(129)//char(129)//char(129)
	ip=iop(iun)
	ipm1=iop(iun)
	ipm2=iop(iun)
	ip26=iop(iun)
	ip27=iop(iun)
	call badinc(ip26,26)
	call badinc(ip27,27)
	call badinc(ipm1,-1)
	call badinc(ipm2,-2)
	tststr(1:4)=inbuf(iun)(ipm2:ipm2)//inbuf(iun)(ipm1:ipm1)// &
	            inbuf(iun)(ip26:ip26)//inbuf(iun)(ip27:ip27)
	if (tststr(1:4).ne.terms(1:4)) then
		inum=nbyts(iop(iun),inp(iun))
		do i=1,inum
			call badinc(ip26,1)
			call badinc(ip27,1)
			call badinc(ipm1,1)
			call badinc(ipm2,1)
			tststr(1:4)=inbuf(iun)(ipm2:ipm2)//inbuf(iun)(ipm1:ipm1)// &
				        inbuf(iun)(ip26:ip26)//inbuf(iun)(ip27:ip27)
			if (tststr(1:4).eq.terms(1:4)) then
				call badinc(iop(iun),i)
				exit
			end if
		end do
		nBadSync(iun)=nBadSync(iun)+1
		do i=1,8
			iraw(i,iun)=-9999
			araw(i,iun)=-9999.0
		end do
		return
	end if
!
15	chu(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chu(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chv(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chv(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chw(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chw(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cht(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cht(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha1(4:4)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha1(3:3)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha1(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha1(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha2(4:4)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha2(3:3)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha2(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha2(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha3(4:4)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha3(3:3)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha3(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha3(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha4(4:4)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha4(3:3)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha4(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha4(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),5)
!
	if (jt.lt.0) then
		c=float(65536+jt)*0.01
	else
		c=float(jt)*0.01
	end if
	jt=int2(((c*c/401.856)-273.15)*100.0)
!
	iraw(1,iun)=ju										! u (m s-1 x100)
	iraw(2,iun)=jv										! v (m s-1 x100)
	iraw(3,iun)=jw										! w (m s-1 x100)
	iraw(4,iun)=jt										! Tsonic (C x100)
!
	araw(1,iun)=float(ju)*0.01							! u (m s-1)
	araw(2,iun)=float(jv)*0.01							! v (m s-1)
	araw(3,iun)=float(jw)*0.01							! w (m s-1)
	araw(4,iun)=float(jt)*0.01							! Tsonic (C)
	araw(5,iun)=float(ja1)*1.0E-3						! analog(1) (mv)
	araw(6,iun)=float(ja2)*1.0E-3						! analog(2) (mv)
	araw(7,iun)=float(ja3)*1.0E-3						! analog(3) (mv)
	araw(8,iun)=float(ja4)*1.0E-3						! analog(4) (mv)
!
	iraw(5,iun)=int2(araw(5,iun))						! analog(1) (mv)
	iraw(6,iun)=int2(araw(6,iun))						! analog(2) (mv)
	iraw(7,iun)=int2(araw(7,iun))						! analog(3) (mv)
	iraw(8,iun)=int2(araw(8,iun))						! analog(4) (mv)
!
	return
	end subroutine WMPbinCnvrt
!
!==============================================================================
!
!	SUBROUTINE CSATbinCnvrt (iraw,araw,iun)
!
!  This subroutine converts synchronized, binary input data from a CSAT3 sonic
!  anemometer into a string of 4 2-byte integers arranged as: u, v, w, T.
!  It returns them in the order specified in the SETUP.PAR file
!
!  The 12 byte packet output by the Campbell CSAT3 sonic is:
!
!  u1,u2,v1,v2,w1,w2,C1,C2,<stat1>,<stat2>,<55h>,<AAh>
!
!==============================================================================
!
	subroutine CSATbinCnvrt (iraw,araw,iun)
!
	use common_vars
!
	character(len=2) chu,chv,chw,cht,stat
	character(len=4) tststr,terms
	integer(kind=2) ju,jv,jw,jt,jstat,jjstat
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) u,v,w,T
!
	equivalence(ju,chu)
	equivalence(jv,chv)
	equivalence(jw,chw)
	equivalence(jt,cht)
	equivalence(jstat,stat)
!
	terms(1:4)=char(85)//char(170)//char(85)//char(170)
	ip=iop(iun)
	ipm1=iop(iun)
	ipm2=iop(iun)
	ip10=iop(iun)
	ip11=iop(iun)
	call badinc(ip11,11)
	call badinc(ip10,10)
	call badinc(ipm1,-1)
	call badinc(ipm2,-2)
	tststr(1:4)=inbuf(iun)(ipm2:ipm2)//inbuf(iun)(ipm1:ipm1)// &
	            inbuf(iun)(ip10:ip10)//inbuf(iun)(ip11:ip11)
	if (tststr(1:4).ne.terms(1:4)) then
		inum=nbyts(iop(iun),inp(iun))
		do i=1,inum
			call badinc(ip11,1)
			call badinc(ip10,1)
			call badinc(ipm1,1)
			call badinc(ipm2,1)
			tststr(1:4)=inbuf(iun)(ipm2:ipm2)//inbuf(iun)(ipm1:ipm1)// &
				        inbuf(iun)(ip10:ip10)//inbuf(iun)(ip11:ip11)
			if (tststr(1:4).eq.terms(1:4)) then
				call badinc(iop(iun),i)
				exit
			end if
		end do
		nBadSync(iun)=nBadSync(iun)+1
		do i=1,5
			iraw(i,iun)=-9999
			araw(i,iun)=-9999.0
		end do
		return
	end if
!
	chu(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chu(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chv(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chv(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chw(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chw(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cht(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cht(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	stat(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	stat(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),3)
!
	if ((jstat.eq.-4033).or.(jstat.eq.-4096)) then
		do i=1,4
			iraw(i,iun)=-9999
			araw(i,iun)=-9999.0
		end do
		iraw(5,iun)=jstat
		return
	end if
!
	iuscale=ibits(jstat,10,2)
	ivscale=ibits(jstat,8,2)
	iwscale=ibits(jstat,6,2)
	inctr=ibits(jstat,0,6)
	jjstat=ibits(jstat,12,4)
!
	select case (iuscale)
		case default
			u=-9999.0
		case (0)
			u=float(ju)*0.002
		case (1)
			u=float(ju)*0.001
		case (2)
			u=float(ju)*0.0005
		case (3)
			u=float(ju)*0.00025
	end select
!
	select case (ivscale)
		case default
			v=-9999.0
		case (0)
			v=float(jv)*0.002
		case (1)
			v=float(jv)*0.001
		case (2)
			v=float(jv)*0.0005
		case (3)
			v=float(jv)*0.00025
	end select
!
	select case (iwscale)
		case default
			w=-9999.0
		case (0)
			w=float(jw)*0.002
		case (1)
			w=float(jw)*0.001
		case (2)
			w=float(jw)*0.0005
		case (3)
			w=float(jw)*0.00025
	end select
!
	T=((((float(jt)*0.001)+340.0)**2)/401.856)-273.15
	iraw(1,iun)=ifix(u*100.0)							! u (m s-1 x100)
	iraw(2,iun)=ifix(v*100.0)							! v (m s-1 x100)
	iraw(3,iun)=ifix(w*100.0)							! w (m s-1 x100)
	iraw(4,iun)=ifix(T*100.0)							! Tsonic (C x100)
!	iraw(5,iun)=jstat									! full status word
	iraw(5,iun)=jjstat									! upper 4 bits of status word
!
	araw(1,iun)=u										! u (m s-1)
	araw(2,iun)=v										! v (m s-1)
	araw(3,iun)=w										! w (m s-1)
	araw(4,iun)=T										! Tsonic (C)
!	araw(5,iun)=float(jstat)							! full status word
	araw(5,iun)=float(jjstat)							! upper 4 bits of status word
	return
	end subroutine CSATbinCnvrt
!
!==============================================================================
!
!	SUBROUTINE Y81000binCnvrt (iraw,araw,iun)
!
!  This subroutine converts synchronized, binary input data from an RM Young
!  81000V sonic anemometer into a string of 5 2-byte integers arranged as:
!  u, v, w, T, stat.
!  It returns them in the order specified in the SETUP.PAR file
!
!  The 20 byte packet output by the RM Young 81000V sonic is:
!
!  <ABh>,<CDh>,<poll>,<err>,u1,u2,v1,v2,w1,w2,T1,T2,A11,A12,A21,A22,A31,A32,A41,A42
!
!==============================================================================
!
	subroutine Y81000binCnvrt (iraw,araw,iun)
!
	use common_vars
!
	character(len=2) chu,chv,chw,cht,cha1,cha2,cha3,cha4,stat
	character(len=4) tststr,terms
	integer(kind=2) ju,jv,jw,jt,ja1,ja2,ja3,ja4,jstat
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) u,v,w,T,A1,A2,A3,A4
!
	equivalence(ju,chu)
	equivalence(jv,chv)
	equivalence(jw,chw)
	equivalence(jt,cht)
	equivalence(ja1,cha1)
	equivalence(ja2,cha2)
	equivalence(ja3,cha3)
	equivalence(ja4,cha4)
	equivalence(jstat,stat)
!
	terms(1:4)=char(171)//char(205)//char(171)//char(205)
	ip=iop(iun)
	ipm1=iop(iun)
	ipm2=iop(iun)
	ip18=iop(iun)
	ip19=iop(iun)
	call badinc(ip19,19)
	call badinc(ip18,18)
	call badinc(ipm2,-2)
	call badinc(ipm1,-1)
	tststr=inbuf(iun)(ipm2:ipm2)//inbuf(iun)(ipm1:ipm1)// &
	       inbuf(iun)(ip18:ip18)//inbuf(iun)(ip19:ip19)
	if (tststr(1:4).ne.terms(1:4)) then
		inum=nbyts(iop(iun),inp(iun))
		do i=1,inum
			call badinc(ip19,1)
			call badinc(ip18,1)
			call badinc(ipm1,1)
			call badinc(ipm2,1)
			tststr=inbuf(iun)(ipm2:ipm2)//inbuf(iun)(ipm1:ipm1)// &
				   inbuf(iun)(ip18:ip18)//inbuf(iun)(ip19:ip19)
			if (tststr(1:4).eq.terms(1:4)) then
				call badinc(iop(iun),i)
				exit
			end if
		end do
		nBadSync(iun)=nBadSync(iun)+1
		do i=1,9
			iraw(i,iun)=-9999
			araw(i,iun)=-9999.0
		end do
		return
	end if
!
	stat(2:2)=char(0)
	call badinc(iop(iun),1)
	stat(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chv(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chv(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chu(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chu(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chw(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	chw(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cht(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cht(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha1(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha1(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha2(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha2(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha3(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha3(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha4(2:2)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),1)
	cha4(1:1)=inbuf(iun)(iop(iun):iop(iun))
	call badinc(iop(iun),3)
!
	iraw(1,iun)=-ju										! u (m s-1 x100)
	iraw(2,iun)=jv										! v (m s-1 x100)
	iraw(3,iun)=jw										! w (m s-1 x100)
	iraw(4,iun)=jt-27315								! Tsonic (C x100)
!
	araw(1,iun)=float(-ju)*0.01							! u (m s-1)
	araw(2,iun)=float(jv)*0.01							! v (m s-1)
	araw(3,iun)=float(jw)*0.01							! w (m s-1)
	araw(4,iun)=(float(jt)*0.01)-273.15					! Tsonic (C)
!
	araw(5,iun)=float(ja1)*1.25							! analog(1) (mv)
	araw(6,iun)=float(ja2)*1.25							! analog(2) (mv)
	araw(7,iun)=float(ja3)*0.25							! analog(3) (mv)
	araw(8,iun)=float(ja4)*0.25							! analog(4) (mv)
	araw(9,iun)=float(jstat)							! status word
!
	iraw(5,iun)=int2(araw(5,iun))						! analog(1) (mv)
	iraw(6,iun)=int2(araw(6,iun))						! analog(2) (mv)
	iraw(7,iun)=int2(araw(7,iun))						! analog(3) (mv)
	iraw(8,iun)=int2(araw(8,iun))						! analog(4) (mv)
	iraw(9,iun)=jstat									! status word
!
	return
	end subroutine Y81000binCnvrt
!
!==============================================================================
!
!	SUBROUTINE LI7500ascCnvrt (iraw,araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from a LiCor LI-7500
!  IRGA into an a string of 5 2-byte integers, arranged as: CO2, H2O, T, P,
!  Status.  It returns them in the order specified in the SETUP.PAR file
!
!==============================================================================
!
	subroutine LI7500ascCnvrt (iraw,araw,iun)
!
	use common_vars
	use Instrument_Info
!
	character(len=100) strng
	integer(kind=4) itmp
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) a1,a2,a3,a4
!
10	format (5G25.0)
!
	n=nbyts(iop(iun),inp(iun))
	itmp=1
!
	do
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(13)) goto 25
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(9)) then
			strng(itmp:itmp)=','
		else
			strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		end if
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
25	call badinc(iop(iun),2)
	itmp=itmp-1
!
	read (strng(1:itmp),10,ERR=35) iraw(5,iun),a1,a2,a3,a4
!
	iraw(1,iun)=int2(a1*100.0+0.5)						! CO2 (mmol m-3 x100)
	iraw(2,iun)=int2(a2*10.0+0.5)						! H2O (mmol m-3 x10)
	iraw(3,iun)=int2(a3*100.0+0.5)						! T (C x 100)
	iraw(4,iun)=int2(a4*10.0+0.5)						! P (kPa x10)
!	iraw(5,iun)											! status
!
	araw(1,iun)=a1										! CO2 (mmol m-3)
	araw(2,iun)=a2										! H2O (mmol m-3)
	araw(3,iun)=a3										! T (C)
	araw(4,iun)=a4										! P (kPa)
	araw(5,iun)=float(iraw(5,iun))						! status
!
	return
!
35	iraw(1,iun)=-9999
	iraw(2,iun)=-9999
	iraw(3,iun)=-9999
	iraw(4,iun)=-9999
	iraw(5,iun)=-9999
!
	araw(1,iun)=-9999.0
	araw(2,iun)=-9999.0
	araw(3,iun)=-9999.0
	araw(4,iun)=-9999.0
	araw(5,iun)=-9999.0
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine LI7500ascCnvrt
!
!==============================================================================
!
!	SUBROUTINE LI7500AascCnvrt (iraw,araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from a LiCor LI-7500A
!  IRGA into an a string of 13 2-byte integers or 4-byte reals, arranged
!  as: Status, CO2 density, H2O density, T, P, Aux1, Aux2, Aux3, Aux4, 
!  CO2 mixing ratio, H2O mixing ratio, dew point, and AGC.  It returns them in
!  the order specified in the SETUP.PAR file.
!
!==============================================================================
!
	subroutine LI7500AascCnvrt (iraw,araw,iun)
!
	use common_vars
	use Instrument_Info
!
	character(len=1000) strng
	integer(kind=4) itmp
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) a1,a2,a3,a4,a5,a6,a7
!
10	format (13G25.0)
!
	n=nbyts(iop(iun),inp(iun))
	itmp=1
!
	do
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(13)) goto 25
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(9)) then
			strng(itmp:itmp)=','
		else
			strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		end if
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
25	call badinc(iop(iun),2)
	itmp=itmp-1
!
	read (strng(1:itmp),10,ERR=35) iraw(13,iun),a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12
!
	iraw(1,iun)=int2(a1*100.0+0.5)						! CO2 density (mmol m-3 x100)
	iraw(2,iun)=int2(a2*10.0+0.5)						! H2O density (mmol m-3 x10)
	iraw(3,iun)=int2(a3*100.0+0.5)						! T (C x100)
	iraw(4,iun)=int2(a4*10.0+0.5)						! P (kPa x10)
	iraw(5,iun)=int2(a5*1000.0+0.5)						! analog input 1 (Volts x1000)
	iraw(6,iun)=int2(a6*1000.0+0.5)						! analog input 2 (Volts x1000)
	iraw(7,iun)=int2(a7*1000.0+0.5)						! analog input 3 (Volts x1000)
	iraw(8,iun)=int2(a8*1000.0+0.5)						! analog input 4 (Volts x1000)
	iraw(9,iun)=int2(a9*10.0+0.5)						! CO2 mixing ratio (umol mol-1 x10)
	iraw(10,iun)=int2(a10*100.0+0.5)					! H2O mixing ratio (mmol mol-1 x100)
	iraw(11,iun)=int2(a11*100.0+0.5)					! dew point (C x100)
	iraw(12,iun)=int2(a12*10.0+0.5)						! AGC (% x10) for V6.0.0 or CO2 RSSI V6.5.5 and higher
!	iraw(13,iun)										! status (x1)
!
	araw(1,iun)=a1										! CO2 (mmol m-3)
	araw(2,iun)=a2										! H2O (mmol m-3)
	araw(3,iun)=a3										! T (C)
	araw(4,iun)=a4										! P (kPa)
	araw(5,iun)=a5										! analog input 1 (Volts)
	araw(6,iun)=a6										! analog input 2 (Volts)
	araw(7,iun)=a7										! analog input 3 (Volts)
	araw(8,iun)=a8										! analog input 4 (Volts)
	araw(9,iun)=a9										! [CO2] (umol mol-1)
	araw(10,iun)=a10									! [H2O] (mmol mol-1)
	araw(11,iun)=a11									! dew point (C)
	araw(12,iun)=a12									! AGC (%) for V6.0.0 or CO2 RSSI V6.5.5 and higher
	araw(13,iun)=float(iraw(13,iun))					! status
!
	return
!
35	iraw(1,iun)=-9999
	iraw(2,iun)=-9999
	iraw(3,iun)=-9999
	iraw(4,iun)=-9999
	iraw(5,iun)=-9999
	iraw(6,iun)=-9999
	iraw(7,iun)=-9999
	iraw(8,iun)=-9999
	iraw(9,iun)=-9999
	iraw(10,iun)=-9999
	iraw(11,iun)=-9999
	iraw(12,iun)=-9999
	iraw(13,iun)=-9999
!
	araw(1,iun)=-9999.0
	araw(2,iun)=-9999.0
	araw(3,iun)=-9999.0
	araw(4,iun)=-9999.0
	araw(5,iun)=-9999.0
	araw(6,iun)=-9999.0
	araw(7,iun)=-9999.0
	araw(8,iun)=-9999.0
	araw(9,iun)=-9999.0
	araw(10,iun)=-9999.0
	araw(11,iun)=-9999.0
	araw(12,iun)=-9999.0
	araw(13,iun)=-9999.0
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine LI7500AascCnvrt
!
!==============================================================================
!
!	SUBROUTINE LI7200ascCnvrt (iraw,araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from a LiCor LI-7500A
!  IRGA into an a string of 13 2-byte integers or 4-byte reals, arranged
!  as: CO2 density, H2O density, T, P, dry CO2 mixing ratio, wet CO2 mixing
!  ratio, dry H2O mixing ratio, wet H2O mixing ratio, dew point, flow rate, Aux1,
!  Aux2, Aux3, Aux4, and Status.  It returns them in the order specified in the 
!  SETUP.PAR file
!
!==============================================================================
!
	subroutine LI7200ascCnvrt (iraw,araw,iun)
!
	use common_vars
	use Instrument_Info
!
	character(len=1000) strng
	integer(kind=4) itmp
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18
	real(kind=4) a19,a20,a21,a22,a23,a24,a25
!
10	format (26G25.0)
!
	n=nbyts(iop(iun),inp(iun))
	itmp=1
!
	do
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(13)) goto 25
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(9)) then
			strng(itmp:itmp)=','
		else
			strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		end if
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
25	call badinc(iop(iun),2)
	itmp=itmp-1
!
	if (Inst_Inf(iun)%Firmware(1:3).eq.'6.0') then
		read (strng(1:itmp),10,ERR=35) iraw(26,iun),a1,a2,a3,a4,a5,a6,a7,a8,a9,&
		                               a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,&
									   a20,a21,a22,a23,a24
!
		iraw(1,iun)=int2(a1*100.0+0.5)						! CO2 (mmol m-3 x100)
		iraw(2,iun)=int2(a2*10.0+0.5)						! H2O (mmol m-3 x10)
		iraw(3,iun)=int2(a3*100.0+0.5)						! T block (C x100)
		iraw(4,iun)=int2(a4*10.0+0.5)						! P (cell) (kPa x10)
		iraw(5,iun)=int2(a5*10.0+0.5)						! P (box) (kPa x10)
		iraw(6,iun)=int2(a6*10.0+0.5)						! P (diff... cell-box) (kPa x10)
		iraw(7,iun)=int2(a7*1000.0+0.5)						! Analog input 1 (Volts x1000)
		iraw(8,iun)=int2(a8*1000.0+0.5)						! Analog input 2 (Volts x1000)
		iraw(9,iun)=int2(a9*1000.0+0.5)						! Analog input 3 (Volts x1000)
		iraw(10,iun)=int2(a10*1000.0+0.5)					! Analog input 4 (Volts x1000)
		iraw(11,iun)=int2(a11*10.0+0.5)						! [CO2]ambient (umol mol-1 x10)
		iraw(12,iun)=int2(a12*10.0+0.5)						! [CO2]dry (umol mol-1 x10)
		iraw(13,iun)=int2(a13*100.0+0.5)					! [H2O]ambient (mmol mol-1 x100)
		iraw(14,iun)=int2(a14*100.0+0.5)					! [H2O]dry (mmol mol-1 x100)
		iraw(15,iun)=int2(a15*100.0+0.5)					! dew point (C x100)
		iraw(16,iun)=int2(a16*100.0+0.5)					! T average (C x100)
		iraw(17,iun)=int2(a17*100.0+0.5)					! T inlet (C x100)
		iraw(18,iun)=int2(a18*100.0+0.5)					! T outlet (C x100)
		iraw(19,iun)=int2(a19*10.0+0.5)						! AGC (% x10)
		iraw(20,iun)=-9999
		iraw(21,iun)=int2(a20*100.0+0.5)					! measured Flow rate (SLM x100)
		iraw(22,iun)=int2(a21*100.0+0.5)					! Volumetric Flow rate (LM x100)
		iraw(23,iun)=int2(a22*100.0+0.5)					! Flow Pressure (kPa x100)
		iraw(24,iun)=int2(a23*100.0+0.5)					! Flow Power (V x100)
		iraw(25,iun)=int2(a24*100.0+0.5)					! Flow Drive (% x100)
!		iraw(26,iun)										! status (x1)
!
		araw(1,iun)=a1										! CO2 (mmol m-3)
		araw(2,iun)=a2										! H2O (mmol m-3)
		araw(3,iun)=a3										! T block (C)
		araw(4,iun)=a4										! P (box) (kPa)
		araw(5,iun)=a5										! P (cell) (kPa)
		araw(6,iun)=a6										! P (diff  cell-box) (kPa)
		araw(7,iun)=a7										! Analog input 1 (Volts)
		araw(8,iun)=a8										! Analog input 2 (Volts)
		araw(9,iun)=a9										! Analog input 3 (Volts)
		araw(10,iun)=a10									! Analog input 4 (Volts)
		araw(11,iun)=a11									! [CO2]ambient (umol mol-1)
		araw(12,iun)=a12									! [CO2]dry (umol mol-1)
		araw(13,iun)=a13									! [H2O]ambient (mmol mol-1)
		araw(14,iun)=a14									! [H2O]dry (mmol mol-1)
		araw(15,iun)=a15									! dew point (C)
		araw(16,iun)=a16									! T average (C)
		araw(17,iun)=a17									! T inlet (C)
		araw(18,iun)=a18									! T outlet (C)
		araw(19,iun)=a19									! AGC (%)
		araw(20,iun)=-9999.0
		araw(21,iun)=a20									! measured Flow rate (SLM)
		araw(22,iun)=a21									! Volumetric Flow rate (LM)
		araw(23,iun)=a22									! Flow Pressure (kPa)
		araw(24,iun)=a23									! Flow Power (V)
		araw(25,iun)=a24									! Flow Drive (%)
		araw(26,iun)=float(iraw(26,iun))					! status ()
!
!	else if ((Inst_Inf(iun)%Firmware(1:3).eq.'6.5').or.(Inst_Inf(iun)%Firmware(1:2).eq.'7.')) then
	else if ((Inst_Inf(iun)%Firmware(1:3).eq.'6.5').or.(Inst_Inf(iun)%Firmware(1:2).eq.'7.').or.(Inst_Inf(iun)%Firmware(1:2).eq.'8.')) then
		read (strng(1:itmp),10,ERR=35) iraw(26,iun),a1,a2,a3,a4,a5,a6,a7,a8,a9,&
		                               a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,&
									   a20,a21,a22,a23,a24,a25
!
		iraw(1,iun)=int2(a1*100.0+0.5)						! CO2 (mmol m-3 x100)
		iraw(2,iun)=int2(a2*10.0+0.5)						! H2O (mmol m-3 x10)
		iraw(3,iun)=int2(a3*100.0+0.5)						! T block (C x100)
		iraw(4,iun)=int2(a4*10.0+0.5)						! P (cell) (kPa x10)
		iraw(5,iun)=int2(a5*10.0+0.5)						! P (box) (kPa x10)
		iraw(6,iun)=int2(a6*10.0+0.5)						! P (diff... cell-box) (kPa x10)
		iraw(7,iun)=int2(a7*1000.0+0.5)						! Analog input 1 (Volts x1000)
		iraw(8,iun)=int2(a8*1000.0+0.5)						! Analog input 2 (Volts x1000)
		iraw(9,iun)=int2(a9*1000.0+0.5)						! Analog input 3 (Volts x1000)
		iraw(10,iun)=int2(a10*1000.0+0.5)					! Analog input 4 (Volts x1000)
		iraw(11,iun)=int2(a11*10.0+0.5)						! [CO2]ambient (umol mol-1 x10)
		iraw(12,iun)=int2(a12*10.0+0.5)						! [CO2]dry (umol mol-1 x10)
		iraw(13,iun)=int2(a13*100.0+0.5)					! [H2O]ambient (mmol mol-1 x100)
		iraw(14,iun)=int2(a14*100.0+0.5)					! [H2O]dry (mmol mol-1 x100)
		iraw(15,iun)=int2(a15*100.0+0.5)					! dew point (C x100)
		iraw(16,iun)=int2(a16*100.0+0.5)					! T average (C x100)
		iraw(17,iun)=int2(a17*100.0+0.5)					! T inlet (C x100)
		iraw(18,iun)=int2(a18*100.0+0.5)					! T outlet (C x100)
		iraw(19,iun)=int2(a19*10.0+0.5)						! CO2 RSSI (% x10)
		iraw(20,iun)=int2(a20*10.0+0.5)						! H2O RSSI (% x10)
		iraw(21,iun)=int2(a21*100.0+0.5)					! measured Flow rate (SLM x100)
		iraw(22,iun)=int2(a22*100.0+0.5)					! Volumetric Flow rate (LM x100)
		iraw(23,iun)=int2(a23*100.0+0.5)					! Flow Pressure (kPa x100)
		iraw(24,iun)=int2(a24*100.0+0.5)					! Flow Power (V x100)
		iraw(25,iun)=int2(a25*100.0+0.5)					! Flow Drive (% x100)
!		iraw(26,iun)										! status (x1)
!
		araw(1,iun)=a1										! CO2 (mmol m-3)
		araw(2,iun)=a2										! H2O (mmol m-3)
		araw(3,iun)=a3										! T block (C)
		araw(4,iun)=a4										! P (box) (kPa)
		araw(5,iun)=a5										! P (cell) (kPa)
		araw(6,iun)=a6										! P (diff  cell-box) (kPa)
		araw(7,iun)=a7										! Analog input 1 (Volts)
		araw(8,iun)=a8										! Analog input 2 (Volts)
		araw(9,iun)=a9										! Analog input 3 (Volts)
		araw(10,iun)=a10									! Analog input 4 (Volts)
		araw(11,iun)=a11									! [CO2]ambient (umol mol-1)
		araw(12,iun)=a12									! [CO2]dry (umol mol-1)
		araw(13,iun)=a13									! [H2O]ambient (mmol mol-1)
		araw(14,iun)=a14									! [H2O]dry (mmol mol-1)
		araw(15,iun)=a15									! dew point (C)
		araw(16,iun)=a16									! T average (C)
		araw(17,iun)=a17									! T inlet (C)
		araw(18,iun)=a18									! T outlet (C)
		araw(19,iun)=a19									! CO2 RSSI (%)
		araw(20,iun)=a20									! H2O RSSI (%)
		araw(21,iun)=a21									! measured Flow rate (SLM)
		araw(22,iun)=a22									! Volumetric Flow rate (LM)
		araw(23,iun)=a23									! Flow Pressure (kPa)
		araw(24,iun)=a24									! Flow Power (V)
		araw(25,iun)=a25									! Flow Drive (%)
		araw(26,iun)=float(iraw(26,iun))					! status ()
	else
		call stop_pgm()
	end if
!
	return
!
35	iraw(1,iun)=-9999
	iraw(2,iun)=-9999
	iraw(3,iun)=-9999
	iraw(4,iun)=-9999
	iraw(5,iun)=-9999
	iraw(6,iun)=-9999
	iraw(7,iun)=-9999
	iraw(8,iun)=-9999
	iraw(9,iun)=-9999
	iraw(10,iun)=-9999
	iraw(11,iun)=-9999
	iraw(12,iun)=-9999
	iraw(13,iun)=-9999
	iraw(14,iun)=-9999
	iraw(15,iun)=-9999
	iraw(16,iun)=-9999
	iraw(17,iun)=-9999
	iraw(18,iun)=-9999
	iraw(19,iun)=-9999
	iraw(20,iun)=-9999
	iraw(21,iun)=-9999
	iraw(22,iun)=-9999
	iraw(23,iun)=-9999
	iraw(24,iun)=-9999
	iraw(25,iun)=-9999
	iraw(26,iun)=-9999
!
	araw(1,iun)=-9999.0
	araw(2,iun)=-9999.0
	araw(3,iun)=-9999.0
	araw(4,iun)=-9999.0
	araw(5,iun)=-9999.0
	araw(6,iun)=-9999.0
	araw(7,iun)=-9999.0
	araw(8,iun)=-9999.0
	araw(9,iun)=-9999.0
	araw(10,iun)=-9999.0
	araw(11,iun)=-9999.0
	araw(12,iun)=-9999.0
	araw(13,iun)=-9999.0
	araw(14,iun)=-9999.0
	araw(15,iun)=-9999.0
	araw(16,iun)=-9999.0
	araw(17,iun)=-9999.0
	araw(18,iun)=-9999.0
	araw(19,iun)=-9999.0
	araw(20,iun)=-9999.0
	araw(21,iun)=-9999.0
	araw(22,iun)=-9999.0
	araw(23,iun)=-9999.0
	araw(24,iun)=-9999.0
	araw(25,iun)=-9999.0
	araw(26,iun)=-9999.0
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine LI7200ascCnvrt
!
!==============================================================================
!
!	SUBROUTINE LI6262ascCnvrt (iraw,araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from a LiCor LI-6262
!  IRGA into an a string of 5 2-byte integers, arranged as: CO2, H2O, T, P,
!  Status.  It returns them in the order specified in the SETUP.PAR file
!
!==============================================================================
!
	subroutine LI6262ascCnvrt (iraw,araw,iun)
!
	use common_vars
!
	integer(kind=4),parameter :: istrln=400
!
	logical LI6262ascSync
	character(len=istrln) strng
	character(len=12) year,tim,zone
	integer(kind=4) itim(8)
	integer(kind=4) i,ipm1,iptst,itst
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) a1,a2,a3,a4
!
10	format (4G25.0)
!
	ipm1=iop(iun)
	call badinc(ipm1,-1)
	n=nbyts(iop(iun),inp(iun))
!
	if(n.ge.20*npacket(indx_LI6262)) then
		iop(iun)=inp(iun)
		do
			n=nbyts(iop(iun),inp(iun))
			if(n.ge.npacket(indx_LI6262)) exit
		end do
	end if
!
	nloop=min(istrln,n)
	itmp=1
!
	if(inbuf(iun)(ipm1:ipm1).ne.char(10)) goto 25
	do i=1,nloop
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(13)) goto 15
		strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
15	call badinc(iop(iun),2)
	itmp=itmp-1
	if (itmp.ne.36) goto 25
!
	read (strng(1:itmp),10,ERR=25) a1,a2,a3,a4
!
	iraw(1,iun)=int2(a1*10.0+0.5)						! CO2 (umol mol-1 x10)
	iraw(2,iun)=int2(a2*100.0+0.5)						! H2O (mmol mol-1 x100)
	iraw(3,iun)=int2(a3*100.0+0.5)						! Tcell (C x100)
	iraw(4,iun)=int2(a4*100.0+0.5)						! Pcell (kPa x100)
!
	araw(1,iun)=a1										! CO2 (umol mol-1)
	araw(2,iun)=a2										! H2O (mmol mol-1)
	araw(3,iun)=a3										! Tcell (C)
	araw(4,iun)=a4										! Pcell (kPa)
!
	return
!
25	ii=0
	ip=iop(iun)
	do i=1,nloop
		ii=ii+1
		strng(i:i)=inbuf(iun)(ip:ip)
		call badinc(ip,1)
		if(strng(i:i).eq.char(10)) exit
	end do
	call badinc(iop(iun),ii)
!
	iraw(1,iun)=-9999
	iraw(2,iun)=-9999
	iraw(3,iun)=-9999
	iraw(4,iun)=-9999
!
	araw(1,iun)=-9999.0
	araw(2,iun)=-9999.0
	araw(3,iun)=-9999.0
	araw(4,iun)=-9999.0
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine LI6262ascCnvrt
!
!==============================================================================
!
!	SUBROUTINE LI7000ascCnvrt (iraw,araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from a LiCor LI-7000
!  IRGA into an a string of 5 2-byte integers, arranged as: CO2, H2O, T, P,
!  Status.  It returns them in the order specified in the SETUP.PAR file
!
!==============================================================================
!
	subroutine LI7000ascCnvrt (iraw,araw,iun)
!
	use common_vars
!
	logical LI7000ascSync
	character(len=256) strng
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) a1,a2,a3,a4
!
10	format (G25.0,G25.0,G25.0,G25.0,G25.0)
!
	n=nbyts(iop(iun),inp(iun))
	itmp=1
!
	do
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(10)) goto 25
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(9)) then
			strng(itmp:itmp)=','
		else
			strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		end if
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
25	call badinc(iop(iun),6)
	itmp=itmp-1
!
	read (strng(1:itmp),10,ERR=35) a1,a2,a3,a4,iraw(5,iun)
!
	iraw(1,iun)=int2(a1*10.0+0.5)						! CO2 (umol mol-1 x10)
	iraw(2,iun)=int2(a2*100.0+0.5)						! H2O (mmol mol-1 x100)
	iraw(3,iun)=int2(a4*100.0+0.5)						! Tcell (C x100)
	iraw(4,iun)=int2(a3*100.0+0.5)						! Pcell (kPa x100)
!	iraw(5,iun)											! status
!
	araw(1,iun)=a1										! CO2 (umol mol-1)
	araw(2,iun)=a2										! H2O (mmol mol-1)
	araw(3,iun)=a4										! Tcell (C)
	araw(4,iun)=a3										! Pcell (kPa)
	araw(5,iun)=float(iraw(5,iun))						! status
!
	return
!
35	iraw(1,iun)=-9999
	iraw(2,iun)=-9999
	iraw(3,iun)=-9999
	iraw(4,iun)=-9999
	iraw(5,iun)=-9999
!
	araw(1,iun)=-9999.0
	araw(2,iun)=-9999.0
	araw(3,iun)=-9999.0
	araw(4,iun)=-9999.0
	araw(5,iun)=-9999.0
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine LI7000ascCnvrt
!
!==============================================================================
!
!	SUBROUTINE LI820ascCnvrt (iraw.araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from a LiCor LI-820
!  IRGA into an a string of 5 2-byte integers, arranged as: CO2, H2O, T, P,
!  Status.  It returns them in the order specified in the SETUP.PAR file
!
!==============================================================================
!
	subroutine LI820ascCnvrt (iraw,araw,iun)
!
	use common_vars
!
	logical LI820ascSync
	character(len=40) strng
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) a1,a2,a3,a4
!
10	format (G25.0,G25.0,G25.0)
!
	n=nbyts(iop(iun),inp(iun))
	itmp=1
!
	do
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(13)) goto 25
		if(inbuf(iun)(iop(iun):iop(iun)).eq.' ') then
			strng(itmp:itmp)=','
		else
			strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		end if
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
25	call badinc(iop(iun),2)
	itmp=itmp-1
!
	read (strng(1:itmp),10,ERR=35) a1,a2,a3
!
	iraw(1,iun)=int2(a3*10.0+0.5)						! CO2 (umol mol-1 x10)
	iraw(2,iun)=int2(a1*100.0+0.5)						! Tcell (C x100)
	iraw(3,iun)=int2(a2*100.0+0.5)						! Pcell (kPa x100)
!
	araw(1,iun)=a3										! CO2 (umol mol-1)
	araw(2,iun)=a1										! Tcell (C)
	araw(3,iun)=a2										! Pcell (kPa)
!
	return
!
35	iraw(1,iun)=-9999
	iraw(2,iun)=-9999
	iraw(3,iun)=-9999
!
	araw(1,iun)=-9999.0
	araw(2,iun)=-9999.0
	araw(3,iun)=-9999.0
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine LI820ascCnvrt
!
!==============================================================================
!
!	SUBROUTINE LI840ascCnvrt (iraw.araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from a LiCor LI-840
!  IRGA into an a string of 5 2-byte integers, arranged as: CO2, H2O, T, P,
!  Status.  It returns them in the order specified in the SETUP.PAR file
!
!==============================================================================
!
	subroutine LI840ascCnvrt (iraw,araw,iun)
!
	use common_vars
!
	logical LI840ascSync
	character(len=40) strng
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) a1,a2,a3,a4
!
10	format (3G25.0)
!
	n=nbyts(iop(iun),inp(iun))
	itmp=1
!
	do
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(13)) goto 25
		if(inbuf(iun)(iop(iun):iop(iun)).eq.' ') then
			strng(itmp:itmp)=','
		else
			strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		end if
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
25	call badinc(iop(iun),2)
	itmp=itmp-1
!
	read (strng(1:itmp),10,ERR=35) a1,a2,a3
!
	iraw(1,iun)=int2(a3*10.0+0.5)						! CO2 (umol mol-1 x10)
	iraw(2,iun)=int2(a1*100.0+0.5)						! Tcell (C x100)
	iraw(3,iun)=int2(a2*100.0+0.5)						! Pcell (kPa x100)
!
	araw(1,iun)=a3										! CO2 (umol mol-1)
	araw(2,iun)=a1										! Tcell (C)
	araw(3,iun)=a2										! Pcell (kPa)
!
	return
!
35	iraw(1,iun)=-9999
	iraw(2,iun)=-9999
	iraw(3,iun)=-9999
!
	araw(1,iun)=-9999.0
	araw(2,iun)=-9999.0
	araw(3,iun)=-9999.0
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine LI840ascCnvrt
!
!==============================================================================
!
!	SUBROUTINE LI7700ascCnvrt (iraw.araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from a LiCor LI-7700
!  CH4 IRGA into an a string of 5 2-byte integers, arranged as: CO2, H2O, T, P,
!  Status.  It returns them in the order specified in the SETUP.PAR file
!
!==============================================================================
!
	subroutine LI7700ascCnvrt (iraw,araw,iun)
!
	use common_vars
	use Instrument_Info
!
	logical LI7700ascSync
	character(len=200) strng
	integer(kind=8) imsec,isec,insec
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) CH4D,CH4,TDL_T,TDL_P,RSSI
	real(kind=4) a1,a2,a3,a4,a5,a6,a7,a8,TC1,TC2,TC3
!
10	format (21G25.0)
!
	n=nbyts(iop(iun),inp(iun))
	itmp=1
!
	do
		if (inbuf(iun)(iop(iun):iop(iun)).eq.'D') goto 25
		if (inbuf(iun)(iop(iun):iop(iun)).eq.char(9)) then
			strng(itmp:itmp)=','
		else
			strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		end if
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
25	call badinc(iop(iun),5)
	itmp=itmp-1
!
	read (strng(1:itmp),10,ERR=35) imsec,isec,insec,idiag,CH4D,CH4,TDL_T,TDL_P,RSSI,idrop, &
								   a1,a2,a3,a4,a5,a6,a7,a8,TC1,TC2,TC3
!
	iraw(1,iun)=int2(CH4D*100000.0+0.5)					! CH4 density (mmol m-3 x100000)
	iraw(2,iun)=int2(CH4*1000.0+0.5)					! CH4 mixing ratio (umol mol-1 x1000)
	iraw(3,iun)=int2(TDL_T*100.0+0.5)					! T (C x100)
	iraw(4,iun)=int2(TDL_P*100.0+0.5)					! P (kPa x100)
	iraw(5,iun)=int2(RSSI*1000.0+0.5)					! RSSI (% x1000)
	iraw(6,iun)=int2(a1*1000.0+0.5)						! analog 1 (Volts x1000)
	iraw(7,iun)=int2(a2*1000.0+0.5)						! analog 2 (Volts x1000)
	iraw(8,iun)=int2(a3*1000.0+0.5)						! analog 3 (Volts x1000)
	iraw(9,iun)=int2(a4*1000.0+0.5)						! analog 4 (Volts x1000)
	iraw(10,iun)=int2(TC1*100.0+0.5)					! TC 1 (C x100)
	iraw(11,iun)=int2(TC2*100.0+0.5)					! TC 2 (C x100)
	iraw(12,iun)=int2(TC3*100.0+0.5)					! TC 3 (Ca x100)
	iraw(13,iun)=idiag									! diagnostic word)
	iraw(14,iun)=idrop									! drop rate
!
	araw(1,iun)=CH4D									! CH4 density(mmol m-3)
	araw(2,iun)=CH4										! CH4 mixing ratio (umol mol-1)
	araw(3,iun)=TDL_T									! T (C)
	araw(4,iun)=TDL_P									! P (kPa)
	araw(5,iun)=RSSI									! RSSI (%)
	araw(6,iun)=a1										! analog 1 (Volts)
	araw(7,iun)=a2										! analog 2 (Volts)
	araw(8,iun)=a3										! analog 3 (Volts)
	araw(9,iun)=a4										! analog 4 (Volts)
	araw(10,iun)=TC1									! TC 1 (C)
	araw(11,iun)=TC2									! TC 2 (C)
	araw(12,iun)=TC3									! TC 3 (C)
	araw(13,iun)=float(idiag)							! diagnostic word
	araw(14,iun)=float(idrop)							! drop rate
!
	return
!
35	iraw(1,iun)=-9999
	iraw(2,iun)=-9999
	iraw(3,iun)=-9999
	iraw(4,iun)=-9999
	iraw(5,iun)=-9999
	iraw(6,iun)=-9999
	iraw(7,iun)=-9999
	iraw(8,iun)=-9999
	iraw(9,iun)=-9999
	iraw(10,iun)=-9999
	iraw(11,iun)=-9999
	iraw(12,iun)=-9999
	iraw(13,iun)=-9999
	iraw(14,iun)=-9999
!
	araw(1,iun)=-9999.0
	araw(2,iun)=-9999.0
	araw(3,iun)=-9999.0
	araw(4,iun)=-9999.0
	araw(5,iun)=-9999.0
	araw(6,iun)=-9999.0
	araw(7,iun)=-9999.0
	araw(8,iun)=-9999.0
	araw(9,iun)=-9999.0
	araw(10,iun)=-9999.0
	araw(11,iun)=-9999.0
	araw(12,iun)=-9999.0
	araw(13,iun)=-9999.0
	araw(14,iun)=-9999.0
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine LI7700ascCnvrt
!
!==============================================================================
!
!	SUBROUTINE AerodyneTDLascCnvrt (iraw.araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from an Aerodyne
!  TDLAS into a string of 3 2-byte integers, arranged as: CO2, H2O, COS.
!  It returns them in the order specified in the SETUP.PAR file
!
!==============================================================================
!
	subroutine AerodyneTDLascCnvrt (iraw,araw,iun)
!
	use common_vars
!
	logical AerodyneTDLascSync
	character(len=40) strng
	character(len=50) tmp
	integer(kind=4) i,ipm1,iptst,itst
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) a(8),fact(8)
!
10	format (8G25.0)
!
	n=nbyts(iop(iun),inp(iun))
	itmp=1
	do i=1,8
		fact(i)=0.0
	end do
!
	do i=1,nchnl
		if(serial(chan(i)%kinst)%ktype.eq.indx_AerodyneTDL) then
			fact(chan(i)%kseq)=chan(i)%par(ipar_chan)
		end if
	end do
!
	do
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(10)) goto 25
		strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
25	call badinc(iop(iun),1)
	itmp=itmp-1
!
	read (strng(17:itmp),10,ERR=35) a
!
	do i=1,8
		araw(i,iun)=a(i)
		iraw(i,iun)=int2(a(i)*fact(i)+0.5)
	end do
!
	return
!
35	iraw(1,iun)=-9999
	iraw(2,iun)=-9999
	iraw(3,iun)=-9999
!
	araw(1,iun)=-9999.0
	araw(2,iun)=-9999.0
	araw(3,iun)=-9999.0
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine AerodyneTDLascCnvrt
!
!==============================================================================
!
!	SUBROUTINE PicarroTDLascCnvrt (iraw.araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from a Picarro G2311-f
!  CRD TDLAS into an a string of 3 2-byte integers, arranged as: CO2, H2O, CH4.
!  It returns them in the order specified in the SETUP.PAR file
!
!==============================================================================
!
	subroutine PicarroTDLascCnvrt (iraw,araw,iun)
!
	use common_vars
!
	logical PicarroTDLascSync
	character(len=512) strng,frmat
	integer(kind=4) i,itmp
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) a(n_Picarro)
!
	frmat(1:9)='(xxG11.0)'
	write (frmat(2:3),'(I2.2)') n_Picarro
!
	n=nbyts(iop(iun),inp(iun))
	itmp=1
!
	do
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(13)) goto 25
		strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
25	call badinc(iop(iun),3)
	itmp=itmp-1
!
	read (strng(16:itmp),frmat(1:9),ERR=35) a
!
	do i=1,n_picarro-2
		araw(i,iun)=a(i)
		iraw(i,iun)=-9999.0
	end do
!
	return
!
35	do i=1,n_Picarro-1
		araw(i,iun)=-9999.0
		iraw(i,iun)=-9999
end do

!	araw(1,iun)=-9999.0									! P cavity (Torr)
!	araw(2,iun)=-9999.0									! T cavity (C)
!	araw(3,iun)=-9999.0									! T Das (C)
!	araw(4,iun)=-9999.0									! T etalon (C)
!	araw(5,iun)=-9999.0									! T warm box (C)
!	araw(6,iun)=-9999.0									! [CH4] ambient (ppmv)
!	araw(7,iun)=-9999.0									! [CH4] dry (ppmv)
!	araw(8,iun)=-9999.0									! [CO2] ambient (ppmv)
!	araw(9,iun)=-9999.0									! [CO2] dry (ppmv)
!	araw(10,iun)=-9999.0								! H2O (%)
!	araw(11,iun)=-9999.0								! ????
!	araw(12,iun)=-9999.0								! ????
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine PicarroTDLascCnvrt
!
!==============================================================================
!
!	SUBROUTINE LGRTDLascCnvrt (iraw.araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from Los Gatos
!  911-0010 OA ICOS TDLAS into an a string of 3 2-byte integers, arranged
!  as: CO2, H2O, CH4.  It returns them in the order specified in the SETUP.PAR file
!
!==============================================================================
!
	subroutine LGRTDLascCnvrt (iraw,araw,iun)
!
	use common_vars
!
	logical LGRTDLascSync
	character(len=512) strng
	integer(kind=4) i,itmp
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) a(15)
!
10	format (15G25.0)
!
	n=nbyts(iop(iun),inp(iun))
	itmp=1
!
	do
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(10)) goto 25
		strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
25	call badinc(iop(iun),1)
	itmp=itmp-1
	read (strng(27:itmp),10,ERR=35) a
!
	do i=1,14
		araw(i,iun)=a(i)
		iraw(i,iun)=-9999
	end do
!
	return
!
35	araw(1,iun)=-9999.0									! [O2] ambient mean (ppmv)
	araw(2,iun)=-9999.0									! [O2] ambient std dev. (ppmv)
	araw(3,iun)=-9999.0									! [H2O] ambient mean (ppmv)
	araw(4,iun)=-9999.0									! [H2O] ambient std. dev. (ppmv)
	araw(5,iun)=-9999.0									! [CO2] ambient mean (ppmv)
	araw(6,iun)=-9999.0									! [CO2] ambient std. dev. (ppmv)
	araw(7,iun)=-9999.0									! P cell mean (Torr)
	araw(8,iun)=-9999.0									! P cell std. dev. (Torr)
	araw(9,iun)=-9999.0									! T cell mean (C)
	araw(10,iun)=-9999.0								! T cell std. dev. (C)
	araw(11,iun)=-9999.0								! Laser A tau mean (usec)
	araw(12,iun)=-9999.0								! Laser A tau std. dev. (usec)
	araw(13,iun)=-9999.0								! Laser B tau mean (usec)
	araw(14,iun)=-9999.0								! Laser B tau std. dev. (usec)
!
	iraw(1,iun)=-9999
	iraw(2,iun)=-9999
	iraw(3,iun)=-9999
	iraw(4,iun)=-9999
	iraw(5,iun)=-9999
	iraw(6,iun)=-9999
	iraw(7,iun)=-9999
	iraw(8,iun)=-9999
	iraw(9,iun)=-9999
	iraw(10,iun)=-9999
	iraw(11,iun)=-9999
	iraw(12,iun)=-9999
	iraw(13,iun)=-9999
	iraw(14,iun)=-9999
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine LGRTDLascCnvrt
!
!==============================================================================
!
!	SUBROUTINE IRGASONascCnvrt (iraw.araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from a Campbell IRGASON
!  IRGA into an a string of real numbers.  
!  It returns them in the order specified in the SETUP.PAR file
!
!==============================================================================
!
	subroutine IRGASONascCnvrt (iraw,araw,iun)
!
	use common_vars
!
	logical IRGASONascSync
	character(len=512) strng,frmat
	integer(kind=4) i,itmp
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) a(n_IRGASON)
!
	frmat(1:9)='(xxG11.0)'
	write (frmat(2:3),'(I2.2)') n_IRGASON
!
	n=nbyts(iop(iun),inp(iun))
	itmp=1
!
	do
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(13)) goto 25
		strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
25	call badinc(iop(iun),2)
	itmp=itmp-1
!
	read (strng(1:itmp),frmat(1:9),ERR=35) a
!
	do i=1,n_IRGASON
		araw(i,iun)=a(i)
		iraw(i,iun)=-9999.0
	end do
!
	return
!
35	do i=1,n_IRGASON
		araw(i,iun)=-9999.0
		iraw(i,iun)=-9999
end do

!	araw(1,iun)=-9999.0									! u (m s-1)
!	araw(2,iun)=-9999.0									! v (m s-1)
!	araw(3,iun)=-9999.0									! w (m s-1)
!	araw(4,iun)=-9999.0									! Tsonic (C)
!	araw(5,iun)=-9999.0									! sonic diagnostic (1-byte integer)
!	araw(6,iun)=-9999.0									! rho CO2 (mg m-3)
!	araw(7,iun)=-9999.0									! rho H2O (g m-3)
!	araw(8,iun)=-9999.0									! IRGA diagnostic (4-byte integer)
!	araw(9,iun)=-9999.0									! Tair (C) slow
!	araw(10,iun)=-9999.0								! Pair (kPa) slow
!	araw(11,iun)=-9999.0								! CO2 signal strength (arb. units)
!	araw(12,iun)=-9999.0								! H2O signal strength (arb. units)
!	araw(13,iun)=-9999.0								! not used
!	araw(14,iun)=-9999.0								! ????
!	araw(15,iun)=-9999.0								! ????
!	araw(16,iun)=-9999.0								! counter
!
! Note that the CRC word is not reported.
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine IRGASONascCnvrt
!
!==============================================================================
!
!	SUBROUTINE EC155ascCnvrt (iraw.araw,iun)
!
!  This subroutine converts synchronized, ASCII input data from a Campbell EC155
!  IRGA into an a string of real numbers.
!  It returns them in the order specified in the SETUP.PAR file
!
!==============================================================================
!
	subroutine EC155ascCnvrt (iraw,araw,iun)
!
	use common_vars
!
	logical EC155ascSync
	character(len=512) strng,frmat
	integer(kind=4) i,itmp
	integer(kind=2) iraw(max_chan,max_ser)
	real(kind=4) araw(max_chan,max_ser)
	real(kind=4) a(n_EC155)
!
	frmat(1:9)='(xxG11.0)'
	write (frmat(2:3),'(I2.2)') n_EC155
!
	n=nbyts(iop(iun),inp(iun))
	itmp=1
!
	do
		if(inbuf(iun)(iop(iun):iop(iun)).eq.char(13)) goto 25
		strng(itmp:itmp)=inbuf(iun)(iop(iun):iop(iun))
		itmp=itmp+1
		call badinc(iop(iun),1)
	end do
!
25	call badinc(iop(iun),2)
	itmp=itmp-1
!
	read (strng(1:itmp),frmat(1:9),ERR=35) a
!
	do i=1,n_EC155
		araw(i,iun)=a(i)
		iraw(i,iun)=-9999.0
	end do
!
	return
!
35	do i=1,n_EC155
		araw(i,iun)=-9999.0
		iraw(i,iun)=-9999
end do

!	araw(1,iun)=-9999.0									! u (m s-1)
!	araw(2,iun)=-9999.0									! v (m s-1)
!	araw(3,iun)=-9999.0									! w (m s-1)
!	araw(4,iun)=-9999.0									! Tsonic (C)
!	araw(5,iun)=-9999.0									! sonic diagnostic (1-byte integer)
!	araw(6,iun)=-9999.0									! rho CO2 (mg m-3)
!	araw(7,iun)=-9999.0									! rho H2O (g m-3)
!	araw(8,iun)=-9999.0									! IRGA diagnostic (4-byte integer)
!	araw(9,iun)=-9999.0									! Tair (or Tcell) (C) slow
!	araw(10,iun)=-9999.0								! Pair (kPa) slow
!	araw(11,iun)=-9999.0								! CO2 signal strength (arb. units)
!	araw(12,iun)=-9999.0								! H2O signal strength (arb. units)
!	araw(13,iun)=-9999.0								! ambient-cell pressure (kPa)
!	araw(14,iun)=-9999.0								! ????
!	araw(15,iun)=-9999.0								! ????
!	araw(16,iun)=-9999.0								! counter
!
!  note that the CRC word is not reported.
!
	nBadSync(iun)=nBadSync(iun)+1
!
	return
	end subroutine EC155ascCnvrt
!
!==============================================================================
!
!  SUBROUTINE Get_Par (setup_name)
!
!  Subroutine to read SETUP.PAR
!  This subroutine reads the program configuration information from SETUP.PAR
!
!==============================================================================
!
	subroutine get_par (setup_name)
!
	use common_vars
	use file_names
	use graph_params
    use File_manager_Vars
!
	character(len=*) setup_name
	character(len=1) label
	logical cmnt,exists
	integer(kind=4) icnt(num_sections)
!
10	format (A)
20	format (I10,A10,50G30.0)
30	format (20X,A)
40	format (20X,G25.0)
100 format (I)
!
	ninst=1
	nchnl=1
	nrot=1
	ncov=1
	ncor=1
	j=0
	do i=1,num_sections
		icnt(i)=0
	end do
	cmnt=.true.
!
!  open the PAR file
!
	inquire (FILE=setup_name,EXIST=exists)
	if (exists) then
		open (UNIT=1,FILE=setup_name)
	else
		write (*,*) 'Cant find the SETUP file.  Fix the problem and try again.'
		call stop_pgm()
	end if
!
!  determine how many "data" lines are in each of the first 5 sections
!
15	read (1,10,END=25) label
		if ((label.eq.'#').or.(label.eq.'*')) then
			cmnt=.true.
			goto 15
		else if (cmnt) then
			j=j+1
			if(j.gt.num_sections) goto 25
			icnt(j)=icnt(j)+1
			cmnt=.false.
		else
			icnt(j)=icnt(j)+1
		end if
	goto 15
!
25	ninst=icnt(1)
	nchnl=icnt(2)
	nrot=icnt(3)
	ncov=icnt(4)
	ncor=icnt(5)
	n_Aerodyne=0
!
!  make sure that we won't over run any of the arrays
!
	if ((ninst.gt.max_ser).or.(nchnl.gt.max_chan).or. &
	   (nrot.gt.max_rot).or.(ncov.gt.max_cov).or. &
	   (ncor.gt.max_cor)) return
!
	rewind 1
!
!  get the parameters for each serial instrument
!
	label = '#'
	do while ((label.eq.'#').or.(label.eq.'*'))
		read (1,10,end=35) label
	end do
	backspace 1
	do i=1,ninst
		read (1,20) serial(i)%kindx,serial(i)%name,serial(i)%ktype, &
		            serial(i)%kcomm,serial(i)%kbaud,serial(i)%kbits, &
					serial(i)%kstop,serial(i)%kparity
	end do
!
!  get the parameters for changing the raw data to real units
!
	label = '#'
	do while ((label.eq.'#').or.(label.eq.'*'))
		read (1,10,end=35) label
	end do
	backspace 1
	do i=1,nchnl
		read (1,20) chan(i)%kindx,chan(i)%name,chan(i)%kinst,chan(i)%kseq, &
		            chan(i)%keqn,chan(i)%kdel,chan(i)%kave,chan(i)%kspec, &
					chan(i)%kqc,(chan(i)%par(j),j=1,ipar_chan),chan(i)%qc_up, &
					chan(i)%qc_low,chan(i)%qc_spike
	end do
!
!  is there a OCS TDLAS?  If yes, figure out how many species
!
	do i=1,nchnl
		if (serial(chan(i)%kinst)%ktype.eq.indx_AerodyneTDL) then
			n_Aerodyne=n_Aerodyne+1
		end if
	end do
!
!  get the parameters for calculating the rotations
!
	label = '#'
	do while ((label.eq.'#').or.(label.eq.'*'))
		read (1,10,end=35) label
	end do
	backspace 1
	do i=1,nrot
		read (1,20) rot(i)%kindx,rot(i)%name,rot(i)%ktype,rot(i)%krot, &
					rot(i)%kplot,rot(i)%kwind
	end do
!
!  get the parameters for calculating covariances and fluxes
!
	label = '#'
	do while ((label.eq.'#').or.(label.eq.'*'))
		read (1,10,end=35) label
	end do
	backspace 1
	do i=1,ncov
		read (1,20) cov(i)%kindx,cov(i)%name,cov(i)%kch1,cov(i)%kch2, &
		            cov(i)%keqn,cov(i)%kcospec,(cov(i)%par(j),j=1,ipar_cov)
	end do
!
!  get the parameters for calculating corrections and other parameters
!
	label = '#'
	do while ((label.eq.'#').or.(label.eq.'*'))
		read (1,10,end=35) label
	end do
	backspace 1
	do i=1,ncor
		read (1,20) corr(i)%kindx,corr(i)%name,corr(i)%keqn, &
					(corr(i)%par(j),j=1,ipar_cor)
	end do
!
!  read in other misc. information
!
	label='#'
	do while ((label.eq.'#').or.(label.eq.'*'))
		read (1,10,end=35) label
	end do
	backspace 1
	read (1,30) file_root
	read (1,40) run_length
	read (1,40) samp_interval
	read (1,40) Ftau
	read (1,40) ii
	do_graphs=.true.
	if (ii.eq.0) do_graphs=.false.
	read (1,40) ii
	int_dat=.true.
	if (ii.eq.1) int_dat=.false.
	read (1,40) i_time_stamp
	read (1,40) i_reboots
	read (1,40) isleeptime
	isleeptime=isleeptime*1000
	read (1,40) it_err
!
35	close (UNIT=1)
!
!  calculate some program parameters from the information read in from
!  SETUP.PAR
!
	imax_rec=86400000/int4(samp_interval)
!
!  check to see if this is a "commensurate" day, and calculate a file size divisor
!
	i_tim_err=86400000-(imax_rec*int4(samp_interval))
	tim_err=float(i_tim_err)/1000.0
!
	irec_div=1
	irec_rem=0
	if ((imax_rec.ge.8640).and.(imax_rec.lt.86400)) then
		irec_div=60
		irec_rem=mod(imax_rec,60)
	else if (imax_rec.ge.86400) then
		irec_div=600
		irec_rem=mod(imax_rec,600)
	end if
!
	irec_per=1000*int4(run_length)/int4(samp_interval)
	i_1Sec=1000/int4(samp_interval)
	i_10Sec=(k_buf_len*1000)/int4(samp_interval)
	intervals_per=86400/int4(run_length)
	irecl=2*nchnl
	if (.not.int_dat) irecl=4*nchnl
	samp_f=1000.0/float(samp_interval)
	fnyq=samp_f*0.5
!
!  initialize pointers to u,v,w, and T and determine
!  if we should plot u and v or horizontal wind speed
!
	wind=.false.
	do i=1,nchnl
		if (iabs(rot(i)%kwind).eq.1) then
			iu=rot(i)%ktype
		end if
		if (rot(i)%kwind.eq.2) then
			iv=rot(i)%ktype
		end if
		if (rot(i)%kwind.eq.3) then
			iw=rot(i)%ktype
		end if
		if (rot(i)%kwind.eq.4) then
			it=rot(i)%ktype
		end if
		if (rot(i)%kwind.eq.-1) then
			wind=.true.
		end if
	end do
!
!  initialize the channel pointers for plotting
!
	do i=1,n_graphs
		ichn(i)=0
	end do
	do i=1,nchnl
		if (rot(i)%kplot.ne.0) then
			ichn(rot(i)%kplot)=rot(i)%ktype
		end if
	end do
!
!  initialize the filter parameters
!
	fil_par1=exp(-(1/samp_f)/Ftau)
	fil_par2=1.0-fil_par1
!
!  get the file manager information
!
    inquire (file=file_manage_setup,EXIST=file_manage)
    if (file_manage) then
        n_transfers=0
    	do i=1,str_len
    		blank_line(i:i)=' '
    	end do
    	USB_folder(1:str_len)=blank_line(1:str_len)
    	new_line(1:str_len)=blank_line(1:str_len)
    	cmd_line(1:str_len)=blank_line(1:str_len)
    	Local_home_folder(1:str_len)=blank_line(1:str_len)
    	USB_exist=.false.
!
!  get the file data from the archiver_setup.txt file
!
    	open (unit=1,file=file_manage_setup(1:len_trim(file_manage_setup)))
    	do
    		read (1,10,end=45) new_line
    		if (new_line(1:1).eq.'#') cycle
    		backspace (unit=1)
    		read (1,100) n_EC
    		read (1,10) new_line
    		if (n_EC.gt.0) then
    			allocate (EC_in_folder(n_EC),stat=ialloc_err)
    			allocate (EC_archive_folder(n_EC),stat=ialloc_err)
                allocate (EC_transfer_folder(n_EC),stat=ialloc_err)
                allocate (EC_remote_folder(n_EC),stat=ialloc_err)
    		end if
    		do i=1,n_EC
    			EC_in_folder(i)(1:str_len)=blank_line(1:str_len)
    			EC_archive_folder(i)(1:str_len)=blank_line(1:str_len)
                EC_transfer_folder(i)(1:str_len)='#####'//blank_line(1:str_len-5)
                EC_remote_folder(i)(1:str_len)='#####'//blank_line(1:str_len-5)
    		end do
    		do i=1,n_EC
    			new_line(1:str_len)=blank_line(1:str_len)
    			read (1,10) new_line
    			call parse_folder (new_line,EC_in_folder(i))
    			new_line(1:str_len)=blank_line(1:str_len)
    			read (1,10) new_line
    			call parse_folder (new_line,EC_archive_folder(i))
                new_line(1:str_len)=blank_line(1:str_len)
                read (1,10) new_line
                if (new_line(1:1).ne.'#') then
                    call parse_folder (new_line,EC_transfer_folder(i))
                end if
                new_line(1:str_len)=blank_line(1:str_len)
                read (1,10) new_line
                if (new_line(1:1).ne.'#') then
                    call parse_folder (new_line,EC_remote_folder(i))
                    n_transfers=n_transfers+1
                end if
                read (1,10,end=45) new_line
            end do
    		read(1,100) n_slow
    		read (1,10) new_line
    		if (n_slow.gt.0) then
    			allocate (Slow_in_folder(n_slow),stat=ialloc_err)
    			allocate (Slow_archive_folder(n_slow),stat=ialloc_err)
    			allocate (Slow_cumul_folder(n_slow),stat=ialloc_err)
                allocate (Slow_transfer_folder(n_slow),stat=ialloc_err)
                allocate (Slow_remote_folder(n_slow),stat=ialloc_err)
    			allocate (Slow_file(n_slow),stat=ialloc_err)
    		end if
    		do i=1,n_slow
    			Slow_in_folder(i)(1:str_len)=blank_line(1:str_len)
    			Slow_archive_folder(i)(1:str_len)=blank_line(1:str_len)
    			Slow_cumul_folder(i)(1:str_len)='#####'//blank_line(1:str_len-5)
                Slow_transfer_folder(i)(1:str_len)='#####'//blank_line(1:str_len-5)
                Slow_remote_folder(i)(1:str_len)='#####'//blank_line(1:str_len-5)
    			Slow_file(i)(1:str_len)=blank_line(1:str_len)
    		end do
    		do i=1,n_slow
    			new_line(1:str_len)=blank_line(1:str_len)
    			read (1,10) new_line
    			call parse_folder (new_line,Slow_in_folder(i))
    			new_line(1:str_len)=blank_line(1:str_len)
    			read (1,10) new_line
    			call parse_folder (new_line,Slow_archive_folder(i))
                new_line(1:str_len)=blank_line(1:str_len)
                read (1,10) new_line
                if (new_line(1:1).ne.'#') then
                    call parse_folder (new_line,Slow_transfer_folder(i))
                end if
    			new_line(1:str_len)=blank_line(1:str_len)
    			read (1,10) new_line
    			call parse_folder (new_line,Slow_cumul_folder(i))
    			new_line(1:str_len)=blank_line(1:str_len)
    			read (1,10) new_line
    			call parse_folder (new_line,Slow_file(i))
                new_line(1:str_len)=blank_line(1:str_len)
                read (1,10) new_line
                if (new_line(1:1).ne.'#') then
                    call parse_folder (new_line,Slow_remote_folder(i))
                    n_transfers=n_transfers+1
                end if
                read (1,10,end=45) new_line
            end do
    		read (1,100) n_other
    		read (1,10) new_line
    		if (n_other.gt.0) then
    			allocate (Other_in_folder(n_other),stat=ialloc_err)
    			allocate (Other_archive_folder(n_other),stat=ialloc_err)
                allocate (Other_transfer_folder(n_other),stat=ialloc_err)
                allocate (Other_remote_folder(n_other),stat=ialloc_err)
    		end if
    		do i=1,n_other
    			Other_in_folder(i)(1:str_len)=blank_line(1:str_len)
    			Other_archive_folder(i)(1:str_len)=blank_line(1:str_len)
                Other_transfer_folder(i)(1:str_len)='#####'//blank_line(1:str_len-5)
                Other_remote_folder(i)(1:str_len)='#####'//blank_line(1:str_len-5)
    		end do
    		do i=1,n_other
    			new_line(1:str_len)=blank_line(1:str_len)
    			read (1,10) new_line
    			call parse_folder (new_line,Other_in_folder(i))
    			new_line(1:str_len)=blank_line(1:str_len)
    			read (1,10) new_line
    			call parse_folder (new_line,Other_archive_folder(i))
                new_line(1:str_len)=blank_line(1:str_len)
                read (1,10) new_line
                if (new_line(1:1).ne.'#') then
                    call parse_folder (new_line,Other_transfer_folder(i))
                end if
                new_line(1:str_len)=blank_line(1:str_len)
                read (1,10) new_line
                if (new_line(1:1).ne.'#') then
                    call parse_folder (new_line,Other_remote_folder(i))
                    n_transfers=n_transfers+1
                end if
                read (1,10,end=45) new_line
            end do
            new_line(1:str_len)=blank_line(1:str_len)
            read (1,10,end=15) new_line
            if (new_line(1:1).ne.'#') then
                call parse_folder (new_line,USB_folder)
            end if
            new_line(1:str_len)=blank_line(1:str_len)
            read (1,10,end=15) new_line
            if (new_line(1:1).ne.'#') then
                call parse_folder (new_line,Local_home_folder)
            end if
    	end do
45  	close (unit=1)
        if (USB_folder(1:1).ne.'#') USB_exist=.true.
    endif
!
	return
	end subroutine get_par
!
!==============================================================================
!
!  SUBROUTINE Verify_Ports (s_port)
!
!  This subroutine opens and trys to set the initial communication parameters
!  of the com port passed in the varible s_port.  This mostly just exercises
!  the com ports and makes sure that they are working.
!
!==============================================================================
!
	subroutine Verify_Ports (s_port)
!
	use common_vars
	use serial_vars
!
	character(len=80) tmpstring
	character(len=12) portname
	character(len=4) portnum
	integer(kind=4) i,ncoms,iHandle,nOpenTrys,ierror,nstat
	integer(kind=4) icom,ibaud
	integer(kind=1) idata,iparity,istop
!
	type (T_DCB):: dcbCom
	type (T_COMMTIMEOUTS):: ComTOS
	type (ser_port):: s_port
!
40	format (I1.1)
50	format (I2.2)
60	format (I3.3)
!
!  build the COM port names
!
	icom=s_port%kcomm
	ibaud=s_port%kbaud
	idata=s_port%kbits
	iparity=s_port%kparity
	istop=s_port%kstop
	if ((istop.ne.0).and.(istop.ne.2)) istop=0
!
	if ((icom.ge.1).and.(icom.lt.10)) then
		write (portnum(1:1),40) icom
		portname(1:9)='\\.\COM'//portnum(1:1)//char(0)
	else if ((icom.ge.10).and.(icom.lt.100)) then
		write (portnum(1:2),50) icom
		portname(1:10)='\\.\COM'//portnum(1:2)//char(0)
	else
		write (portnum(1:3),60) icom
		portname(1:11)='\\.\COM'//portnum(1:3)//char(0)
	end if
!
	open (unit=2,file='portstats.txt',position='append')
	write (2,*) '============================================================'
	write (2,*) 'Verifying serial port'
	write (2,*) comp%mon_str1(1:2)//'/'//comp%day_str(1:2)//'/'// &
			    comp%year_str(1:4)//'  '//comp%time_str(1:8)
	nOpenTrys=1
35	iHandle=CreateFile(portname,IOR(GENERIC_READ,GENERIC_WRITE),NULL, &
					   NULL_SECURITY_ATTRIBUTES,OPEN_EXISTING, &
					   FILE_ATTRIBUTE_NORMAL,NULL)
	if(iHandle.le.0) then
		if(nOpenTrys.le.5) then
			nOpenTrys=nOpenTrys+1
		goto 35
		else
			ierror=GetLastError()
			write(2,*) 'Could not open ',portname
			write(2,*) 'Returned error=',ierror
		end if
	end if
!
	nOpenTrys=1
45	nstat=GetCommState(iHandle,dcbCom)
	if(nstat.eq.0) then
		if(nOpenTrys.le.5) then
			nOpenTrys=nOpenTrys+1
			goto 45
		else
			ierror=GetLastError()
			write(2,*) 'Could not get the DCB block for ',portname
			write(2,*) 'Returned error=',ierror
		end if
	end if
!
	dcbCom%BitField=1
	dcbCom%BaudRate=ibaud
	dcbCom%ByteSize=idata
	dcbCom%Parity=iparity
	dcbCom%StopBits=istop
!
	nOpenTrys=1
55	nstat=SetCommState(iHandle,dcbCom)
	if(nstat.eq.0) then
		if(nOpenTrys.le.5) then
			nOpenTrys=nOpenTrys+1
			goto 55
		else
			ierror=GetLastError()
			write(2,*) 'Could not set the DCB block for ',portname
			write(2,*) 'Returned error=',ierror
		end if
	end if
!
	call sleepqq (100)
!
	nOpenTrys=1
65	nstat=GetCommState(iHandle,dcbCom)
	if(nstat.eq.0) then
		if(nOpenTrys.le.5) then
			nOpenTrys=nOpenTrys+1
			goto 65
		else
			ierror=GetLastError()
			write(2,*) 'Could not get final DCB block for ',portname
			write(2,*) 'Returned error=',ierror
		end if
	end if
	write(2,*) 'Final values'
	write(2,*)
	write(2,*) 'Baud rate = ',dcbCom%BaudRate
	write(2,*) 'Byte size = ',dcbCom%ByteSize
	write(2,*) 'Parity = ',dcbCom%Parity
	write(2,*) 'Stop bits = ',dcbCom%StopBits
!
	nstat=CloseHandle(iHandle)
!
	close (unit=2)
!
	return
	end subroutine Verify_Ports
!
!==============================================================================
!
!  SUBROUTINE inter_sync (chop)
!
!  This routine helps do inter-instrument synchronization.  If the LOGICAL
!  input variable "chop" is set to true, the routine chops off all but the
!  current data packet for all instruments.  This synchronizes all instruments
!  to each other and to which ever clock was used to call the routine.  If
!  "chop" is false, the shortest data queue (i.e. the slowest instrument) is
!  determined and the other instrument queues are truncated to this length.
!  This only synchronized the instruments to each other and leaves any skew
!  between the instrument and computer clocks intact.
!
!==============================================================================
!
	subroutine inter_sync (chop)
!
	use common_vars
	use file_names
!
	character(len=12) year,tim,zone
	logical chop
	integer(kind=4) itim(8),n(ninst),iLI7700cntr,nskip
!
10	format (5X,A,'/',A,'/',A,5X,A,':',A,':',A,5X,A,' UTC',5X,'DOY ')
!
	do i=1,ninst
		n(i)=0
	end do
!
	call date_and_time (year,tim,zone,itim)
!
!  get the number of packets queued up for each instrument
!
	do i=1,ninst
		nchr=nbyts(iop(i),inp(i))
		if (serial(i)%ktype.eq.indx_LI7700) then
			n(i)=0
			iLI7700cntr=iop(i)
			do j=1,nchr
				if (inbuf(i)(iLI7700cntr:iLI7700cntr).eq.'D') n(i)=n(i)+1
				call badinc(iLI7700cntr,1)
			end do
		else if ((serial(i)%ktype.eq.indx_LI7500A).or.(serial(i)%ktype.eq.indx_LI7200)) then
			n(i)=0
			k_indx=iop(i)
			k_indxP1=iop(i)
			call badinc(k_indxP1,1)
			do j=1,nchr
				if ((inbuf(i)(k_indx:k_indx).eq.char(13)).and.(inbuf(i)(k_indxP1:k_indxP1).eq.char(10))) n(i)=n(i)+1
				call badinc(k_indx,1)
				call badinc(k_indxP1,1)
			end do
		else
			n(i)=nchr/npkt(serial(i)%ktype)
		end if
	end do
!
!  If chop is false, figure out which queue is the shortest and calculate
!  how many packets to lop off the rest, else trim all to the current one
!
	if (.not.chop) then
		ntemp=minval(n)
!
		do i=1,ninst
			if (n(i).ge.ntemp) then
				n(i)=n(i)-ntemp
			else
				n(i)=0
			end if
		end do
	end if
!
	do i=1,ninst
		if (n(i).gt.0) then
			if (serial(i)%ktype.eq.indx_LI7700) then
				ncount=n(i)
				do
					call badinc(iop(i),1)
					if (inbuf(i)(iop(i):iop(i)).eq.'D') ncount=ncount-1
					if (ncount.eq.0) exit
				end do
				call badinc(iop(i),5)
			else if ((serial(i)%ktype.eq.indx_LI7500A).or.(serial(i)%ktype.eq.indx_LI7200)) then
				ncount=n(i)
				iopP1=iop(i)
				call badinc(iopP1,1)
				do
					call badinc(iop(i),1)
					call badinc(iopP1,1)
					if ((inbuf(i)(iop(i):iop(i)).eq.char(13)).and.(inbuf(i)(iopP1:iopP1).eq.char(10))) ncount=ncount-1
					if (ncount.eq.0) exit
				end do
				call badinc(iop(i),2)
            else if (serial(i)%ktype.eq.indx_LI7000) then
                ncount=n(i)
                do
                    call badinc(iop(i),1)
                    if (inbuf(i)(iop(i):iop(i)).eq.'D') ncount=ncount-1
                    if (ncount.eq.0) exit
                end do
                call badinc(iop(i),5)
			else
				nskip=n(i)*npkt(serial(i)%ktype)
				call badinc(iop(i),nskip)
			end if
			open (unit=ierr_unit,file=err_name,position='append')
			write (ierr_unit,10) year(5:6),year(7:8),year(1:4), &
				  tim(1:2),tim(3:4),tim(5:6),zone(1:5)
			write (ierr_unit,*) 'dumped ',n(i),inst_name(serial(i)%ktype),' packets'
			close (unit=ierr_unit)
		else
			cycle
		end if
	end do
!
	return
	end subroutine inter_sync
!
!==============================================================================
!
!  SUBROUTINE Build_File_Names ()
!
!  This subroutine builds the file names from the root part read from the
!  PAR file and the current date.
!
!==============================================================================
!
	subroutine Build_File_Names ()
!
	use common_vars
	use file_names
    use instrument_info
!
	integer(kind=4) iyear,interval,jticks
!
!
10	format (I3.3)
20	format (I4.4)
30	format (1X,'HuskerFlux  Version ',A)
40	format (1X,'File started on ',A,'/',A,'/',A,5X,A,5X,'UTC ',A,5X, &
            'DOY ',A)
50	format (1X,'Planned system re-boot on ',A,'/',A,'/',A,5X,A,5X,A, &
            ' UTC',5X,'after ',I3.3,' run days')
!
!  update the computer clock
!
	call Update_Comp_Time ()
!
	interval=imax_rec-irec_per
	idays=idays+1
!
!  check to see if we need to do a re-boot
!
	if ((i_reboots.gt.0).and.(idays.gt.i_reboots)) then
		write (imsg_win,*) 'idays=',idays,' i_reboots=',i_reboots
		open (unit=info_unit,file=info_name,position='append')
		write (info_unit,50) comp%mon_str1(1:2),comp%day_str(1:2),comp%year_str(1:4), &
	                     comp%time_str(1:8),comp%zone_str(1:3),i_reboots
		close (unit=info_unit)
		call ReBoot()
	end if
!
!  if the real time is less than one averaging interval before
!  midnight, this waits until midnight to proceed
!
	jticks=int((float(((comp%khr*60)+comp%kmin)*60+comp%ksec))*samp_f)
	if (jticks.gt.interval) then
		write (imsg_win,*) 'Waiting until midnight before proceeding'
		do while (jticks.ne.0)
			call Update_Comp_Time ()
			jticks=int((float(((comp%khr*60)+comp%kmin)*60+comp%ksec))*samp_f)
		end do
	end if
!
!  build the day of year for the new file name and initialize
!  instrument clock.
!
	iend=len_trim(file_root)
	raw_name(1:iend+7)=file_root(1:iend)//comp%doy_str(1:3)//'.r'//comp%year_str(3:4)
	out_name(1:iend+7)=file_root(1:iend)//comp%doy_str(1:3)//'.o'//comp%year_str(3:4)
	err_name(1:iend+7)=file_root(1:iend)//comp%doy_str(1:3)//'.e'//comp%year_str(3:4)
	info_name(1:iend+7)=file_root(1:iend)//comp%doy_str(1:3)//'.i'//comp%year_str(3:4)
	len_file_names=iend+7
!
	write (imsg_win,*) 'building output files'
	open (unit=iout_unit,file=out_name,position='append')
	write (iout_unit,*) '=======================================================', &
						'======================='
	write (iout_unit,30) prog_ver
    write (iout_unit,*) run_start(1:len_trim(run_start))
	write (iout_unit,40) comp%mon_str1(1:2),comp%day_str(1:2),comp%year_str(1:4), &
	                     comp%time_str(1:8),comp%zone_str(1:3),comp%doy_str(1:3)
	write (iout_unit,*) '=======================================================', &
						'======================='
	close (unit=iout_unit)
	open (unit=info_unit,file=info_name,position='append')
	write (info_unit,*) '=======================================================', &
						'======================='
	write (info_unit,30) prog_ver
    write (info_unit,*) run_start(1:len_trim(run_start))
	write (info_unit,40) comp%mon_str1(1:2),comp%day_str(1:2),comp%year_str(1:4), &
	                     comp%time_str(1:8),comp%zone_str(1:3),comp%doy_str(1:3)
	write (info_unit,*) '=======================================================', &
						'======================='
	close (unit=info_unit)
	open (unit=ierr_unit,file=err_name,position='append')
	write (ierr_unit,*) '=======================================================', &
						'======================='
	write (ierr_unit,30) prog_ver
    write (ierr_unit,*) run_start(1:len_trim(run_start))
	write (ierr_unit,40) comp%mon_str1(1:2),comp%day_str(1:2),comp%year_str(1:4), &
	                     comp%time_str(1:8),comp%zone_str(1:3),comp%doy_str(1:3)
	write (ierr_unit,*) '=======================================================', &
						'======================='
	close (unit=ierr_unit)
!	                     
	return
	end subroutine Build_File_Names
!
!==============================================================================
!
!  SUBROUTINE Build_Files()
!
!  This routine builds the raw data file and fills it with zeros.  This ensures
!  that something will be there if the program bombs.
!
!==============================================================================
!
	subroutine Build_Files()
!
	use common_vars
	use file_names
!
	integer(kind=2),allocatable :: idata(:),idata_rem(:)
	real(kind=4),allocatable:: adata(:),adata_rem(:)
	integer(kind=4) ialloc_err
!
	allocate (idata(nchnl*irec_div),stat=ialloc_err)
	allocate (adata(nchnl*irec_div),stat=ialloc_err)
	allocate (idata_rem(nchnl*irec_rem),stat=ialloc_err)
	allocate (adata_rem(nchnl*irec_rem),stat=ialloc_err)
!
	do i=1,nchnl*irec_div
		idata(i)=0
		adata(i)=0.0
	end do
	do i=1,nchnl*irec_rem
		idata_rem(i)=0
		adata_rem(i)=0.0
	end do
	ifstop=imax_rec/irec_div
!
	write (imsg_win,*) 'Writing raw data to: ',raw_name(1:len_file_names)
	write (imsg_win,*)
!
	open (unit=iraw_unit,file=raw_name,access='direct',form='unformatted', &
	      recl=irecl*irec_div,status='new',err=15)
	if (int_dat) then
		do i=1,ifstop
			write(iraw_unit,rec=i)idata
		end do
		do i=1,irec_rem
			write(iraw_unit,rec=ifstop+i) idata_rem
		end do
	else
		do i=1,ifstop
			write(iraw_unit,rec=i)adata
		end do
		do i=1,irec_rem
			write(iraw_unit,rec=ifstop+i) adata_rem
		end do
	end if
15	close (unit=iraw_unit)
	deallocate (idata,stat=ialloc_err)
	deallocate (adata,stat=ialloc_err)
	deallocate (idata_rem,stat=ialloc_err)
	deallocate (adata_rem,stat=ialloc_err)
!	                     
	return
	end subroutine Build_Files
!
!==============================================================================
!
!  SUBROUTINE ReBoot()
!
!  This routine will reboot the computer.  It should work in Windows NT,
!  2000, and XP
!
!==============================================================================
!
	subroutine ReBoot()
!
	use ifwin
	use dflib
	use ifwinty
	use Kernel32
	use User32
	use ADVAPI32
!
	logical res
	integer(kind=HANDLE) kill_handle
!
	type (T_TOKEN_PRIVILEGES):: kill_priv
	type (T_LUID):: LUID
!
!  open the access token associated with this process
!
	res=OpenProcessToken(GetCurrentProcess(),IOR(TOKEN_ADJUST_PRIVILEGES, &
	    TOKEN_QUERY),loc(kill_handle))
!
!  get the locally unique ID (LUID) used to represent the specified privilege namd
!
	res=LookupPrivilegeValue(NULL,SE_SHUTDOWN_NAME, &
	    loc(kill_priv%Privileges(1)%LUID))
!
!  enable the privileges 
!
	kill_priv%PrivilegeCount=1
	kill_priv%Privileges(1)%Attributes=SE_PRIVILEGE_ENABLED
!
	res=AdjustTokenPrivileges(kill_handle,.FALSE.,kill_priv,0, &
	    NULL_TOKEN_PRIVILEGES,0)
!
!  do the reboot
!
	res=ExitWindowsEX(IOR(EWX_REBOOT,EWX_FORCE),0)
!
	return
	end subroutine ReBoot
!
!==============================================================================
!
!  SUBROUTINE Get_New_Data(iraw_buf,raw_buf,data_available,indx)
!
!  This routine checks for new data in each of the serial buffers and appends
!  it to the disk write buffer (in the proper channel order)
!
!==============================================================================
!
	subroutine Get_New_Data(iraw_buf,raw_buf,data_available,indx)
!
	use common_vars
	use file_names
!
	logical data_available
	integer(kind=2) iraw(max_chan,max_ser)
	integer(kind=2) iraw_buf(nchnl,*)
	integer(kind=4) ndat(num_insts)
	real(kind=4) raw_buf(nchnl,*)
	real(kind=4) araw(max_chan,max_ser)
!
	do i=1,num_insts
		new_data(i)=.false.
	end do
!
	data_available=.false.
!
	do i=1,ninst
		ndat(i)=nbyts(iop(i),inp(i))
		if (serial(i)%ktype.eq.indx_LI7700) then
			iLI7700cntr=iop(i)
			do j=1,ndat(i)
				if (inbuf(i)(iLI7700cntr:iLI7700cntr).eq.'D') then
					new_data(i)=.true.
					exit
				end if
				call badinc(iLI7700cntr,1)
			end do
		else if ((serial(i)%ktype.eq.indx_LI7500A).or.(serial(i)%ktype.eq.indx_LI7200)) then
			k_indx=iop(i)
			k_indxP1=iop(i)
			call badinc(k_indxP1,1)
			do j=1,ndat(i)
				if ((inbuf(i)(k_indx:k_indx).eq.char(13)).and.(inbuf(i)(k_indxP1:k_indxP1).eq.char(10))) then
					new_data(i)=.true.
					exit
				end if
				call badinc(k_indx,1)
				call badinc(k_indxP1,1)
			end do
		else
			if (ndat(i).ge.npacket(serial(i)%ktype)) then
				new_data(i)=.true.
			end if
		end if
	end do
	data_available=.true.
	do i=1,ninst
		if (.not.new_data(i)) data_available=.false.
	end do
	if (.not.data_available) return
!
	do i=1,ninst
		select case (serial(i)%ktype)
			case default
				return
!
			case (indx_ATI)						! ATI sonic
				if (new_data(i)) call ATIbinCnvrt (iraw,araw,i)
!
			case (indx_R3)						! Gill R3 sonic
				if (new_data(i)) call R3binCnvrt (iraw,araw,i)
!
			case (indx_WMP)						! Gill WindMaster Pro sonic
				if (new_data(i)) call WMPbinCnvrt (iraw,araw,i)
!
			case (indx_CSAT)					! Campbell CSAT-3 sonic
				if (new_data(i)) call CSATbinCnvrt (iraw,araw,i)
!
			case (indx_Y81000)					! R.M. Young 81000 sonic
				if (new_data(i)) call Y81000binCnvrt (iraw,araw,i)
!
			case (indx_LI7500)					! LiCor LI-7500 IRGA
				if (new_data(i)) call LI7500ascCnvrt (iraw,araw,i)
!
			case (indx_LI6262)					! LiCor LI-6262 IRGA
				if (new_data(i)) call LI6262ascCnvrt (iraw,araw,i)
!
			case (indx_LI7000)					! LiCor LI-7000 IRGA
				if (new_data(i)) call LI7000ascCnvrt (iraw,araw,i)
!
			case (indx_LI820)					! LiCor LI-820 IRGA
				if (new_data(i)) call LI820ascCnvrt (iraw,araw,i)
!
			case (indx_LI840)					! LiCor LI-840 IRGA
				if (new_data(i)) call LI840ascCnvrt (iraw,araw,i)
!
			case (indx_LI7700)					! LiCor LI-7700 CH4 TDLS
				if (new_data(i)) call LI7700ascCnvrt (iraw,araw,i)
!
			case (indx_AerodyneTDL)					! Aerodyne COS TDLS
				if (new_data(i)) call AerodyneTDLascCnvrt (iraw,araw,i)
!
			case (indx_LI7500A)					! LiCor LI-7500A IRGA
				if (new_data(i)) call LI7500AascCnvrt (iraw,araw,i)
!
			case (indx_LI7200)					! LiCor LI-7200 IRGA
				if (new_data(i)) call LI7200ascCnvrt (iraw,araw,i)
!
			case (indx_PicarroTDL)				! Picarro TDLAS
				if (new_data(i)) call PicarroTDLascCnvrt (iraw,araw,i)
!
			case (indx_LGRTDL)					! Los Gatos TDLAS
				if (new_data(i)) call LGRTDLascCnvrt (iraw,araw,i)
!
			case (indx_IRGASON)					! Campbell IRGASON IRGA
				if (new_data(i)) call IRGASONascCnvrt (iraw,araw,i)
!
			case (indx_EC155)					! Campbell IRGASON IRGA
				if (new_data(i)) call EC155ascCnvrt (iraw,araw,i)
			end select
	end do
!
	do i=1,nchnl
		iraw_buf(i,indx)=iraw(chan(i)%kseq,chan(i)%kinst)
		raw_buf(i,indx)=araw(chan(i)%kseq,chan(i)%kinst)
	end do
!
	return
	end subroutine Get_New_Data
!
!==============================================================================
!
!  SUBROUTINE write_buffer (ibuf,abuf,ibufcnt)
!
!  Writes the contents of the raw data buffer to disk every 10 seconds
!
!==============================================================================
!
	subroutine write_buffer (ibuf,abuf,ibufcnt)
!
	use common_vars
	use file_names
!
	integer(kind=2) ibuf(nchnl,*)
	integer(kind=4) ibufcnt
	real(kind=4) abuf(nchnl,*)
!
	open (unit=iraw_unit,file=raw_name,access='direct',form='unformatted', &
	      recl=irecl)
!
	do i=1,ibufcnt
		if ((irec+i).gt.imax_rec) exit
		if (int_dat) then
			write(iraw_unit,rec=irec+i) (ibuf(j,i),j=1,nchnl)
		else
			write(iraw_unit,rec=irec+i) (abuf(j,i),j=1,nchnl)
		end if
	end do
	close (unit=iraw_unit)
!
	if ((irec+ibufcnt).ge.imax_rec) then
		call Build_File_Names()
		call Build_Files()
        if (file_manage) call File_manager
		irec=int((float(((ireal_hr*60)+ireal_min)*60+ireal_sec))*samp_f)-ibufcnt
	end if
!
	return
	end subroutine write_buffer
!
!==============================================================================
!
!  SUBROUTINE Init_Statistics()
!
!  This routine initializes the statistical sums at the beginning of each
!  averaging period
!
!==============================================================================
!
	subroutine Init_Statistics()
!
	use common_vars
!
	do i=1,max_ser
		nBadSync(i)=0
	end do
!
	do i=1,nchnl
		n_count=0
		run_avg(i)=0.0D0
		run_var_n(i)=0.0D0
	end do
!
	do i=1,ncov
		do j=i,ncov
			urot_covars(i,j)=0.0D0
			urot_covars(j,i)=0.0D0
		end do
	end do
!
	return
	end subroutine Init_Statistics
!
!==============================================================================
!
!  SUBROUTINE Update_int_stats (dat_buf,ameans,astds,asums,asqrs,icntr, &
!                               iterm,indx)
!
!  This routine updates the statistics used internally
!
!==============================================================================
!
	subroutine Update_int_stats (dat_buf,ameans,astds,asums,asqrs,icntr, &
								 iterm,indx)
!
	use common_vars
!
	real(kind=4) dat_buf(nchnl,*)
	real(kind=8) ameans(*),astds(*),asums(*),asqrs(*)
	integer(kind=4) icntr,iterm,indx
!
	icntr=icntr+1
	do i=1,nchnl
		asums(i)=asums(i)+dat_buf(i,indx)
		asqrs(i)=asqrs(i)+(dat_buf(i,indx)*dat_buf(i,indx))
	end do
	if (icntr.eq.iterm) then
		atim=dfloat(iterm)
		do i=1,nchnl
			ameans(i)=asums(i)/atim
			temp=(atim*asqrs(i))-(asums(i)*asums(i))
			if (temp.gt.0.0D0) then
				astds(i)=sqrt(temp/(atim*(atim-1.0D0)))
			else
				astds(i)=-9999.0D0
			end if
			asums(i)=0.0D0
			asqrs(i)=0.0D0
			icntr=0
		end do
	end if
!
	return
	end subroutine Update_int_stats
!
!==============================================================================
!
!  SUBROUTINE Update_wind_stats (dat_buf,ameans,astds,asums,asqrs,icntr, &
!                               iterm,indx)
!
!  This routine updates the statistics used for plotting horizontal wind speed
!
!==============================================================================
!
	subroutine Update_wind_stats (dat_buf,ameans,astds,asums,asqrs,icntr, &
								 iterm,indx)
!
	use common_vars
!
	real(kind=4) dat_buf(nchnl,*)
	real(kind=8) ameans,astds,asums,asqrs,atemp
	integer(kind=4) icntr,iterm,indx
!
	atemp=sqrt((dat_buf(iu,indx)**2)+(dat_buf(iv,indx)**2))
	asums=asums+atemp
	asqrs=asqrs+(atemp*atemp)
	if ((icntr+1).eq.iterm) then
		atim=dfloat(iterm)
		ameans=asums/atim
		atemp=(atim*asqrs)-(asums*asums)
		if (atemp.gt.0.0D0) then
			astds=sqrt(atemp/(atim*(atim-1.0D0)))
		else
			astds=-9999.0D0
		end if
		asums=0.0D0
		asqrs=0.0D0
	end if
!
	return
	end subroutine Update_wind_stats
!
!==============================================================================
!
!  SUBROUTINE Update_Statistics(dat_buf,dat_buf_fil,dat_buf_del,indx)
!
!  This routine updates the running statistical sums
!
!==============================================================================
!
	subroutine Update_Statistics(dat_buf,dat_buf_fil,dat_buf_del,indx)
!
	use common_vars
!
	real(kind=4) dat_buf(nchnl,*),dat_buf_fil(nchnl,*),dat_buf_del(nchnl,*)
!
	real(kind=8) prev_avg
	n_count=n_count+1
	nscan=nscan+1
	do i=1,nchnl
		prev_avg=run_avg(i)
		run_avg(i)=run_avg(i)+((dat_buf(i,indx)-run_avg(i))/n_count)
		run_var_n(i)=run_var_n(i)+((dat_buf(i,indx)-run_avg(i))*&
                     (dat_buf(i,indx)-prev_avg))
	end do
	do i=1,ncov
		do j=i,ncov
			urot_covars(i,j)=urot_covars(i,j)+(dat_buf_fil(i,indx)* &
							 dat_buf_fil(j,indx))
			urot_covars(j,i)=urot_covars(i,j)
		end do
	end do
!
	do i=1,nchnl
		do j=2,i_10sec
			dat_buf_del(i,j)=dat_buf_del(i,j-1)
		end do
		dat_buf_del(i,1)=dat_buf_fil(i,indx)
	end do
!
!	do i=1,ncov
!		do j=i,ncov
!			urot_covars(i,j)=urot_covars(i,j)+dat_buf_del(i,1+chan(j)%kdel)* &
!							dat_buf_del(j,1+chan(i)%kdel)
!			urot_covars(j,i)=urot_covars(i,j)
!		end do
!	end do
!
	return
	end subroutine Update_Statistics
!
!==============================================================================
!
!  SUBROUTINE Filter (dat_buf,dat_buf_fil,indx)
!
!  This routine filters the data
!
!==============================================================================
!
	subroutine Filter (dat_buf,dat_buf_fil,indx)
!
	use common_vars
!
	real(kind=4) dat_buf(nchnl,*),dat_buf_fil(nchnl,*)
!
	if (first_time) then
		do i=1,nchnl
			f(i)=dat_buf(i,indx)
		end do
	else
		do i=1,nchnl
			f(i)=dat_buf(i,indx)*fil_par2+f(i)*fil_par1
			dat_buf_fil(i,indx)=dat_buf(i,indx)-f(i)
		end do
	end if
!
	return
	end subroutine Filter
!
!==============================================================================
!
!  SUBROUTINE Do_Averages ()
!
!  This routine updates the averages every averaging period
!
!==============================================================================
!
	subroutine Do_Averages ()
!
	use common_vars
	use met_data
!
	real(kind=8) rec_per,temp
!
	if (first_time) return
!
	rec_per=dfloat(irec_per)
	do i=1,nchnl
		urot_means(i)=run_avg(i)
		temp=run_var_n(i)/(dfloat(n_count)-1.0D0)
		if (temp.ge.0.0D0) then
			urot_vars(i)=sqrt(temp)
		else
			urot_vars(i)=-9999.0D0
		end if
	end do
	ubar=urot_means(iu)
	vbar=urot_means(iv)
	wbar=urot_means(iw)
	ustd=urot_vars(iu)
	vstd=urot_vars(iv)
	wstd=urot_vars(iw)
!
	do i=1,ncov
		do j=i,ncov
			urot_covars(i,j)=urot_covars(i,j)/(rec_per-1.0D0)
			urot_covars(j,i)=urot_covars(i,j)
		end do
	end do
!
!  calculate a few constants for use with fluxes and corrections
!
	Tbar=urot_means(it)+273.15
	rhod=(amd/r)*(Press/Tbar)
!
	return
	end subroutine Do_Averages
!
!==============================================================================
!
!  SUBROUTINE Rotate ()
!
!  This routine does the coordinate rotations and calculates the wind direction
!
!==============================================================================
!
	subroutine Rotate ()
!
	use common_vars
!
	real(kind=8) ce,se,ct,st,ce2,se2,ct2,st2
!
	if (first_time) return
!
	if (ubar.eq.0.0) then
		if (vbar.ge.0.0) wdir=90.0
		if (vbar.lt.0.0) wdir=270.0
	end if
	if (ubar.gt.0.0) wdir=180.0-(57.3*atan(vbar/ubar))
	if ((ubar.lt.0.0).and.(vbar.ge.0.0)) wdir=-(57.3*atan(vbar/ubar))
	if ((ubar.lt.0.0).and.(vbar.lt.0.0)) wdir=360.0-(57.3*atan(vbar/ubar))
!
	wspeed=sqrt(ubar*ubar+vbar*vbar)
	ce=ubar/wspeed
	ct=wspeed/sqrt(ubar*ubar+vbar*vbar+wbar*wbar)
	se=vbar/wspeed
	st=wbar/sqrt(ubar*ubar+vbar*vbar+wbar*wbar)
	ce2=ce*ce
	ct2=ct*ct
	se2=se*se
	st2=st*st
	uu=ubar*ubar
	uv=ubar*vbar
	uw=ubar*wbar
	vv=vbar*vbar
	vw=vbar*wbar
	ww=wbar*wbar
!
!  rotate the wind components, means first
!
	do i=1,nchnl
		rot_means(i)=urot_means(i)
	end do
	if (rot(1)%krot.eq.1) rot_means(iu)=ubar*ce*ct+vbar*se*ct+wbar*st
	if (rot(2)%krot.eq.1) rot_means(iv)=-ubar*se+vbar*ce
	if (rot(3)%krot.eq.1) rot_means(iw)=-ubar*ce*st-vbar*se*st+wbar*ct
!
!  now the variances and standard deviations
!
	do i=1,nchnl
		rot_vars(i)=urot_vars(i)
	end do
	if (rot(iu)%krot.eq.1) rot_vars(iu)=uu*ct2*ce2+ &
	   2.0D0*uv*ce*se*ct2+2.0D0*uw*ce*ct*st+vv*se2*ct2+2.0D0*vw*se*ct*st+ww*st2
	if (rot(iv)%krot.eq.1) rot_vars(iv)=uu*se2-2.0D0*uv*se*ce+vv*ce2
	if (rot(iw)%krot.eq.1) rot_vars(iw)=uu*ce2*st2+ &
	   2.0D0*uv*ce*se*st2-2.0D0*uw*ce*st*ct+vv*se2*st2-2.0D0*vw*se*st*ct+ww*ct2
!
	do i=1,nchnl
		if (rot_vars(i).lt.0.0D0) then
			stddev(i)=-9999.0
			cycle
		else if (rot_vars(i).eq.0.0D0) then
			stddev(i)=0.0
			cycle
		end if
		stddev(i)=sqrt(rot_vars(i))
	end do
!
!  now do the covariances
!
	uu=urot_covars(iu,iu)
	uv=urot_covars(iu,iv)
	uw=urot_covars(iu,iw)
	vv=urot_covars(iv,iv)
	vw=urot_covars(iv,iw)
	ww=urot_covars(iw,iw)
!
	do i=1,ncov
		irotflg=rot(cov(i)%kch1)%krot+rot(cov(i)%kch2)%krot
		if (irotflg.eq.2) then
			if (((cov(i)%kch1.eq.iu).and.(cov(i)%kch2.eq.iv)).or. &
			   ((cov(i)%kch1.eq.iv).and.(cov(i)%kch2.eq.iu))) then
				rot_covars(i)=-uu*ce*se*ct+vv*ct*ce*se-uw*st*se+vw*st*ce+ &
				              uv*ct*(ce2-se2)
			else if (((cov(i)%kch1.eq.iu).and.(cov(i)%kch2.eq.iw)).or. &
			   ((cov(i)%kch1.eq.iw).and.(cov(i)%kch2.eq.iu))) then
				rot_covars(i)=-uu*ct*st*ce2-vv*ct*st*se2+ww*ct*st+ &
				              uw*ce*(ct2-st2)+vw*se*(ct2-st2)- &
							  2.0D0*uv*ct*st*ce*se
			else if (((cov(i)%kch1.eq.iv).and.(cov(i)%kch2.eq.iw)).or. &
			   ((cov(i)%kch1.eq.iw).and.(cov(i)%kch2.eq.iv))) then
				rot_covars(i)=uu*st*ce*se-vv*st*ce*se-uw*ct*se+vw*ct*ce- &
				              uv*st*(se2-ce2)
			end if
		else if (irotflg.eq.1) then
			if ((cov(i)%kch1.eq.iu).or.(cov(i)%kch2.eq.iu)) then
				if (cov(i)%kch1.eq.iu) then
					ich2=cov(i)%kch2
				else
					ich2=cov(i)%kch1
				end if
				rot_covars(i)=urot_covars(iu,ich2)*ct*ce+ &
				              urot_covars(iv,ich2)*ct*se+ &
							  urot_covars(iw,ich2)*st
			else if ((cov(i)%kch1.eq.iv).or.(cov(i)%kch2.eq.iv)) then
				if (cov(i)%kch1.eq.iv) then
					ich2=cov(i)%kch2
				else
					ich2=cov(i)%kch1
				end if
				rot_covars(i)=urot_covars(iv,ich2)*ce- &
				              urot_covars(iu,ich2)*se
			else if ((cov(i)%kch1.eq.iw).or.(cov(i)%kch2.eq.iw)) then
				if (cov(i)%kch1.eq.iw) then
					ich2=cov(i)%kch2
				else
					ich2=cov(i)%kch1
				end if
				rot_covars(i)=urot_covars(iw,ich2)*ct- &
				              urot_covars(iu,ich2)*st*ce- &
							  urot_covars(iv,ich2)*st*se
			end if
		else
			rot_covars(i)=urot_covars(cov(i)%kch1,cov(i)%kch2)
		end if
	end do
!				
	return
	end subroutine Rotate
!
!==============================================================================
!
!  SUBROUTINE Write_Averages (irec_off)
!
!  This routine writes the output file at the end of every averaging period
!
!==============================================================================
!
	subroutine Write_Averages (irec_off)
!
	use common_vars
	use met_data
	use file_names
!
!
	character(len=12) year,tim,zone
!
10	format (I2.2)
20	format (1X,'Instrument End Time is: ',A,'/',A,'/',A,5X,A,'  UTC ',A,'  DOY',A)
30	format (1X,'u = ',F7.3,2X,'v = ',F7.3,2X,'w = ',F7.3,5X,'wind speed = ', &
            F7.3,2X,'Wind Direction = ',F7.3)
40	format (4X,A,5X,F13.4,3X,F13.4,10X,F13.4,3X,F13.4)
50	format (4X,A,5X,F13.4,3X,F13.4,10X,F13.4)
60	format (4X,A,5X,F13.4)
70	format (38X,'Means and Standard Deviations',/,34X,'Unrotated', &
            29X,'Rotated')
80	format (38X,'Covariances and Fluxes',/,24X,'Unrotated',11X, &
            'Rotated',/24X,'Covariances',9X, &
            'Covariances',12X,'Fluxes')
90	format (15X,'Corrections and Other Quantities')
100	format (4X,A,5X,F13.4)
110	format (1X,'DOY ',I3.3,2X,A,' The ',A,' lost sync ',I5,' times')
120	format (1X,'Real time: ',A,'/',A,'/',A,' DOY ',A,2X,A,4X,'Inst time: ',A,'/',A,'/',A,' DOY ',A,2X,A,/,1X, &
            'Waiting for real clock to catch up')
130	format (1X,'Real time: ',A,'/',A,'/',A,' DOY ',A,2X,A,4X,'Inst time: ',A,'/',A,'/',A,' DOY ',A,2X,A,/,1X, &
            'Adjusting instrument clock to catch up')
140	format (1X,'Real End Time is: ',A,'/',A,'/',A,5X,A,'  UTC ',A,'  DOY',A)
!
	if (first_time) return
!
!  Calculate the maximum allowable clock skew in records, and the actual clock skew.
!
	ideldoy=comp%kdoy-inst%kdoy
	if (comp%kyear.ne.inst%kyear) then
		ideldoy=1
	end if
!
	irec_real=int(float(comp%khr*3600+comp%kmin*60+comp%ksec)*samp_f)
	irec_max_err=it_err*samp_f
	irec_err=irec_real-(irec+irec_off)+(ifix(samp_f*86400.0))*ideldoy
	isec_err=ifix(float(irec_err)/samp_f)
!
	open (unit=iout_unit,file=out_name,position='append')
	write (iout_unit,20) inst%mon_str1(1:2),inst%day_str(1:2),inst%year_str(1:4),inst%time_str(1:8), &
	                     inst%zone_str(1:3),inst%doy_str(1:3)
	write (iout_unit,140) comp%mon_str1(1:2),comp%day_str(1:2),comp%year_str(1:4),inst%time_str(1:8), &
	                      comp%zone_str(1:3),comp%doy_str(1:3)
	write (iout_unit,30) ubar,vbar,wbar,wspeed,wdir
!
	write (iout_unit,70)
	do i=1,nchnl
		write (iout_unit,40) chan(i)%name,urot_means(i),urot_vars(i), &
		                     rot_means(i),rot_vars(i)
	end do
!
	write (iout_unit,80)
	do i=1,ncov
		write (iout_unit,50) cov(i)%name,urot_covars(cov(i)%kch1, &
		                     cov(i)%kch2),rot_covars(i),flux(i)
	end do
!
	write (iout_unit,90)
	do i=1,ncor
		write (iout_unit,100) corr(i)%name,correction(i)
	end do
!
	close (unit=iout_unit)
	open (unit=info_unit,file=info_name,position='append')
	do i=1,ninst
		write (info_unit,110) inst%kdoy,inst%time_str(1:8),serial(i)%name, &
		      nBadSync(serial(i)%ktype)
	end do
	close (unit=info_unit)
	call Init_Statistics()
!
!  if the real clock is behind the instrument clock, wait here for the instrument
!  clock to catch up.  Note, this is ignored if it_err is zero or negative
!
	if ((it_err.gt.0).and.(isec_err.le.-it_err)) then
		open (unit=info_unit,file=info_name,position='append')
		write (info_unit,*) '=============================================================================='
		write (info_unit,120) comp%mon_str1(1:2),comp%day_str(1:2), &
		       comp%year_str(1:4),comp%doy_str(1:3),comp%time_str(1:8), &
			   inst%mon_str1(1:2),inst%day_str(1:2),inst%year_str(1:4), &
			   inst%doy_str(1:3),inst%time_str(1:8)
!
		write (info_unit,*) 'it_err=',it_err,'  isec_err',isec_err,'  irec_err=',irec_err
		write (info_unit,*) 'real TOY=',comp%toy,'  instrument TOY=',inst%toy
		write (info_unit,*) '=============================================================================='
		close (unit=info_unit)
		write (imsg_win,*) ' Waiting for Real clock to catch up'
!
		do i=1,(-isec_err)-2
			call wait (100)
			call Update_Comp_Time ()
			call UpdateClock ()
		end do
!
!  dump the accumulated data and remove any inter-instrument clock skew
!
		call inter_sync (.TRUE.)
	end if
!
!  if the real clock is ahead of the instrument clock, catch up immediately.
!  Note, this is ignored if it_err is zero or negative
!
	if ((it_err.gt.0).and.(isec_err.ge.it_err)) then
		open (unit=info_unit,file=info_name,position='append')
		write (info_unit,*) '=============================================================================='
		write (info_unit,130) comp%mon_str1(1:2),comp%day_str(1:2), &
		       comp%year_str(1:4),comp%doy_str(1:3),comp%time_str(1:8), &
			   inst%mon_str1(1:2),inst%day_str(1:2),inst%year_str(1:4), &
			   inst%doy_str(1:3),inst%time_str(1:8)
		write (info_unit,*) 'it_err=',it_err,'  isec_err=',isec_err,'  irec_err=',irec_err
		write (info_unit,*) 'real TOY=',comp%toy,'  instrument TOY=',inst%toy
		write (info_unit,*) '=============================================================================='
		close (unit=info_unit)
!
		if (ideldoy.ge.1) then
			call Build_File_Names ()
			call Build_Files ()
            if (file_manage) call File_manager
		end if
!
		call update_Comp_Time ()
		irec=int((float(((comp%khr*60)+comp%kmin)*60+comp%ksec))*samp_f)+1
		ikwrtbuf=mod(irec,i_10sec)
		irec=irec-ikwrtbuf
		call Update_inst_Time (irec)
	end if
!
!  remove any inter-instrument clock skew
!
	call inter_sync (.FALSE.)
!
	return
	end subroutine Write_Averages
!
!==============================================================================
!
!  Subroutine Units (idat,adat,indx)
!
!  This routine converts the real input data to natural units.  There are
!  several different equation types which are selected by the SETUP file.
!
!==============================================================================
!
	subroutine Units (idat,adat,dat,indx)
!
	use common_vars
	use file_names
!
	integer(kind=2) idat(nchnl,*)
	integer(kind=4) indx
	real(kind=4) adat(nchnl,*)
	real(kind=4) dat(nchnl,*)
	real(kind=4) d
!
	do i=1,nchnl
		if (int_dat) then
			dat(i,indx)=float(idat(i,indx))
		else
			dat(i,indx)=adat(i,indx)
		end if
		select case (chan(i)%keqn)
			case default
				write (imsg_win,*) 'Bad UNITS equation type in SETUP.PAR line ',i
				call stop_pgm()
!
!  polynomial up to 5th order
!
			case (1)
				d=dat(i,indx)
				d2=d*d
				d3=d2*d
				d4=d3*d
				d5=d4*d
				dat(i,indx)=chan(i)%par(1)+chan(i)%par(2)*d+chan(i)%par(3)*d2+ &
				            chan(i)%par(4)*d3+chan(i)%par(5)*d4+chan(i)%par(6)*d5
			cycle
!
!  polynomial up to 5th order on common log of data
!
			case (2)
				if (dat(i,indx).le.0.0) then
					dat(i,indx)=0.0
					cycle
				end if
				d=log(dat(i,indx))
				d2=d*d
				d3=d2*d
				d4=d3*d
				d5=d4*d
				dat(i,indx)=chan(i)%par(1)+chan(i)%par(2)*d+chan(i)%par(3)*d2+ &
				            chan(i)%par(4)*d3+chan(i)%par(5)*d4+chan(i)%par(6)*d5
			cycle
!
!  convert speed of sound to sonic temperature
!
			case (3)
				dat(i,indx)=(((dat(i,indx)/20.0293)**2)-273.15)
			cycle
!
!  missing channel
!
			case (4)
				dat(i,indx)=-9999.0
			cycle
!
!  missing channel
!
			case (9)
				dat(i,indx)=-9999.0
			cycle
		end select
	end do
!
	return
	end subroutine Units
!
!==============================================================================
!
!  SUBROUTINE Fluxes ()
!
!  This routine calculates fluxes from the covariances (both rotated and
!  unrotated).
!
!==============================================================================
!
	subroutine Fluxes ()
!
	use common_vars
	use met_data
	use file_names
!
	real(kind=8) flx_temp1
!
	do i=1,ncov
		select case (cov(i)%keqn)
			case default
				write (imsg_win,*) 'Bad FLUX equation type in SETUP.PAR line ',i
				call stop_pgm()
!
!  Sensible heat (H)  units ---> (Watts/m^2)
!
!  cov(i)%par(1) ==> pointer to rhoV.  If this is zero, met data rhoV is used
!  cov(i)%par(2) ==> pointer to w'rhoV'.  This is for the humidity correction
!                    if it is zero, no humidity correction is done.
!  cov(i)%par(3) ==> pointer to w'u'.  This is for the Gill WMP cross-wind
!                    contamination correction.  If it is zero, no correction.
!  cov(i)%par(4) ==> pointer to w'v'.  This is for the Gill WMP cross-wind
!                    contamination correction.  If it is zero, no correction.
!  cov(i)%par(5) ==> multiplicative conversion factor to put rhoV in gm/m^3.
!                    If it is zero, 1.0 is used.
!  cov(i)%par(6) ==> multiplicative scaling factor for H.  This can be used
!                    to change signs.  If it is zero, 1.0 is used.
!
		case (1)
			ipnt_rhov=ifix(cov(i)%par(1))
			ipnt_wrhov=ifix(cov(i)%par(2))
			ipnt_wu=ifix(cov(i)%par(3))
			ipnt_wv=ifix(cov(i)%par(4))
!
			if (cov(i)%par(5).eq.0.0) then
				ascale=1.0D0
			else
				ascale=dble(cov(i)%par(5))
			end if
!
			if (cov(i)%par(6).eq.0.0) then
				bscale=1.0D0
			else
				bscale=dble(cov(i)%par(6))
			end if
!
			if (ipnt_rhov.eq.0) then
				rho=rhod
			else
				rho=rhod+dble(rot_means(ipnt_rhov))*ascale
			end if
!
			if (ipnt_wrhov.eq.0) then
				ahumid=0.0D0
			else
				ahumid=-0.51D0*(Tbar/rhod)*rot_covars(ipnt_wrhov)* &
					   ascale
			end if
!
			if ((ipnt_wu.eq.0).or.(ipnt_wv.eq.0)) then
				xwind=0.0D0
			else
				xwind=4.9628D-3*(0.75D0*ubar*rot_covars(ipnt_wu)+ &
					  0.75D0*vbar*rot_covars(ipnt_wv)+ &
					  0.5D0*wbar*rot_vars(iw))
			end if
!
			rot_covars(i)=rot_covars(i)+ahumid+xwind
			flux(i)=rho*Cp*rot_covars(i)*bscale
		cycle
!			case (1)
!				rhoa=rhod
!				if (cov(i)%par(2).ne.0.0) then
!					rhov=urot_means(ifix(cov(i)%par(1)))
!					rhoa=rhod+rhov
!					rot_covars(i)=rot_covars(i)-0.51*(Tbar/rhoa)* &
!					              rot_covars(ifix(cov(i)%par(2)))
!				end if
!				if (cov(i)%par(3).ne.0.0) then
!					rot_covars(i)=rot_covars(i)+4.96D-3*rot_means(iu)* &
!					              rot_covars(ifix(cov(i)%par(3)))
!				end if
!				flux(i)=-rhoa*Cp*rot_covars(i)
!			cycle
!
!  Latent heat  (LE)  units ---> (Watts/m^2)
!
!  cov(i)%par(1) ==> pointer to rhoV.  If this is zero, met data rhoV is used
!  cov(i)%par(2) ==> pointer to w'T'.  This is for the WPL H term.
!                    if it is zero, no WPL H term is included.
!  cov(i)%par(3) ==> Determines which WPL terms are included in the final LE
!                    value.  0 = both H and LE, 1 = only LE term, 2 = no WPL
!                    terms.
!  cov(i)%par(4) ==> multiplicative conversion factor to put rhoV in gm/m^3.
!                    If it is zero, 1.0 is used.
!  cov(i)%par(5) ==> multiplicative scaling factor for LE.  This can be used
!                    to change signs.  If it is zero, 1.0 is used.
!
			case (2)
				a1=1.0
				a2=1.0
				if (cov(i)%par(4).ne.0.0) then
					a1=cov(i)%par(4)
				end if
				if (cov(i)%par(5).ne.0.0) then
					a2=cov(i)%par(5)
				end if
				rhoa=rhod
				if (cov(i)%par(1).ne.0.0) then
					rhov=a1*urot_means(ifix(cov(i)%par(1)))
					rot_covars(i)=rot_covars(i)*(1.0D0+amu*(rhov/rhod))
				end if
				if (cov(i)%par(2).ne.0.0) then
					rot_covars(i)=rot_covars(i)+(1.0D0+amu*(rhov/rhod))* &
					              (rhov/Tbar)*rot_covars(ifix(cov(i)%par(2)))
				end if
				flux(i)=a2*(al*rot_covars(i))
			cycle
!
!  Momentum   units ---> (J/m^3)
!
!  cov(i)%par(1) ==> pointer to rhoV.  If this is zero, met data rhoV is used
!  cov(i)%par(2) ==> multiplicative scaling factor for Tau.  This can be used
!                    to change signs.  If it is zero, 1.0 is used.
!
			case (3)
				a1=1.0
				if (cov(i)%par(2).ne.0.0) then
					a1=cov(i)%par(2)
				end if
				rhoa=rhod
				if (cov(i)%par(1).ne.0.0) then
					rhov=urot_means(ifix(cov(i)%par(1)))
					rhoa=rhod+rhov
				end if
				flux(i)=a1*(-rhoa*rot_covars(i))
			cycle
!
!  Third order polynomial of the covariance   units --> (??)
!
!  cov(i)%par(1) ==> constant term
!  cov(i)%par(2) ==> linear term
!  cov(i)%par(3) ==> quadratic term
!  cov(i)%par(4) ==> cubic term
!  cov(i)%par(5) ==> multiplicative flux scaling factor.  This can be used
!                    to change signs.  If it is zero, 1.0 is used.
!
			case (4)
				a1=1.0
				if (cov(i)%par(5).ne.0.0) then
					a1=cov(i)%par(5)
				end if
				covv=rot_covars(i)
				covv2=covv*covv
				covv3=covv2*covv
				flux(i)=a1*(cov(i)%par(1)+cov(i)%par(2)*covv+cov(i)%par(3)*covv2+ &
				        cov(i)%par(4)*covv3)
			cycle
!
!  Covariance of w and mixing ratio
!
!  cov(i)%par(1) ==> Pointer to Pressure (kPa)  (if negative, use the value)
!  cov(i)%par(2) ==> Pointer to Temperature (K)  (if negative use the value)
!  cov(i)%par(3) ==> Molecular weight (g mol-1)
!  cov(i)%par(4) ==> multiplicative flux scaling factor
!
			case (5)
				RR=0.008314
				ipnt_P=ifix(cov(i)%par(1))
				ipnt_T=ifix(cov(i)%par(2))
!
				if (cov(i)%par(1).lt.0.0) then
					Pa=-cov(i)%par(1)
				else
					Pa=urot_means(ipnt_P)
				end if
				if (cov(i)%par(2).lt.0.0) then
					Ta=-cov(i)%par(2)
				else
					Ta=urot_means(ipnt_T)
				end if
				if (Ta.le.100.0) Ta=Ta+273.15
				Flx_AMU=cov(i)%par(3)
				Flx_scale=cov(i)%par(4)
!
				flux(i)=rot_covars(i)*((Flx_AMU*Pa)/(RR*Ta))*flx_scale
!			ipnt_cov=ifix(cov(i)%par(5))
!			write (imsg_win,*) 'Ta = ',Ta,' Pa = ',Pa
!			write (imsg_win,*) 'cov = ',rot_covars(ipnt_cov)
!			flux(i)=rot_covars(ipnt_cov)*((Flx_AMU*Pa)/(RR*Ta))*flx_scale
			cycle
!
!  missing flux (=-9999.0)
!
			case (9)
				flux(i)=-9999.0
			cycle
		end select
	end do
!
	return
	end subroutine Fluxes
!
!==============================================================================
!
!  SUBROUTINE Corrections ()
!
!  This routine calculates the corrections as specified in the setup file
!
!==============================================================================
!
	subroutine Corrections ()
!
	use common_vars
	use met_data
	use file_names
!
	do i=1,ncor
		select case (corr(i)%keqn)
			case default
				write (imsg_win,*) 'Bad CORRECTION equation type in SETUP.PAR line ',i
				call stop_pgm()
!
!  Humidity correction for Sonic T (additive)
!
!  corr(i)%par(1) ==> pointer to T
!  corr(i)%par(2) ==> pointer to w'rhoV'
!  corr(i)%par(3) ==> multiplicative conversion factor to put rhoV in gm/m^3
!
			case (1)
				ipnt_t=ifix(corr(i)%par(1))
				ipnt_wrhov=ifix(corr(i)%par(2))
!
				if (corr(i)%par(3).eq.0.0) then
					ascale=1.0D0
				else
					ascale=dble(corr(i)%par(3))
				end if
!
				t=dble(rot_means(ipnt_t))
				if (t.lt.100.0D0) t=t+273.15D0
!
				if (ipnt_wrhov.eq.0) then
					correction(i)=0.0
				else
					correction(i)=-0.51D0*1.004*t*rot_covars(ipnt_wrhov)*ascale
				end if
!				correction(i)=0.0
			cycle
!
!  WPL term for LE only (additive)
!
!  corr(i)%par(1) ==> pointer to rhoV
!  corr(i)%par(2) ==> pointer to rhoX (density of CO2, etc.)
!  corr(i)%par(3) ==> pointer to w'rhoV'
!  corr(i)%par(4) ==> multiplicative conversion factor to put rhoV in gm/m^3
!  corr(i)%par(5) ==> multiplicative scaling factor for the WPL LE term
!
			case (2)
				a1=1.0
				if (corr(i)%par(5).ne.0.0) then
					a1=corr(i)%par(5)
				end if
				rhoX=urot_means(ifix(corr(i)%par(2)))
				correction(i)=a1*(amu*(rhoX/rhod)*rot_covars(ifix(corr(i)%par(3))))
			cycle
!
!  WPL correction for H only (additive)
!
!  corr(i)%par(1) ==> pointer to rhoV
!  corr(i)%par(2) ==> pointer to rhoX (density of CO2, etc.)
!  corr(i)%par(3) ==> pointer to T
!  corr(i)%par(4) ==> pointer to w'T'
!  corr(i)%par(5) ==> multiplicative conversion factor to put rhoV in gm/m^3
!  corr(i)%par(6) ==> multiplicative scaling factor for the WPL LE term
!
			case (3)
				a1=1.0
				if (corr(i)%par(6).ne.0.0) then
					a1=corr(i)%par(6)
				end if
				rhoX=urot_means(ifix(corr(i)%par(2)))
				rhov=urot_means(ifix(corr(i)%par(1)))
				t=rot_means(ifix(corr(i)%par(3)))
				if (t.lt.100.0D0) t=t+273.15D0
				correction(i)=a1*((1.0D0+amu*(rhov/rhod))*(rhoX/t)* &
				              rot_covars(ifix(corr(i)%par(4))))
			cycle
!
!  Frequency correction for a scalar flux from an open path instrument (multiplicative)
!
!  corr(i)%par(1) ==> pointer to mean wind speed u
!  corr(i)%par(2) ==> time constant of the scalar instrument
!  corr(i)%par(3) ==> time constant of the sonic anemometer
!  corr(i)%par(4) ==> separation between the instruments (m)
!  corr(i)%par(5) ==> path length of the scalar instrument (m)
!  corr(i)%par(6) ==> path length of the sonic anemometer (m)
!  corr(i)%par(7) ==> pointer to Monin-Obukhov stability (z/L)
!  corr(i)%par(8) ==> z-d (m)
!
			case (4)		!  Not implemented
				correction(i)=1.0
			cycle
!
!  Frequency correction for a scalar flux from a closed path instrument (multiplicative)
!
!  corr(i)%par(1) ==> pointer to mean wind speed u
!  corr(i)%par(2) ==> time constant of the scalar instrument
!  corr(i)%par(3) ==> time constant of the sonic anemometer
!  corr(i)%par(4) ==> separation between the instruments (m)
!  corr(i)%par(5) ==> path length of the sonic anemometer (m)
!  corr(i)%par(6) ==> pointer to Monin-Obukhov stability (z/L)
!  corr(i)%par(7) ==> z-d (m)
!  corr(i)%par(8) ==> tube correction factor
!
			case (5)		!  Not implemented
				correction(i)=1.0
			cycle
!
!  Frequency correction for a vector flux from an open path instrument (multiplicative)
!
!  corr(i)%par(1) ==> pointer to mean wind speed u
!  corr(i)%par(2) ==> time constant of the sonic anemometer
!  corr(i)%par(3) ==> path length of the sonic anemometer (m)
!  corr(i)%par(4) ==> pointer to Monin-Obukhov stability (z/L)
!  corr(i)%par(5) ==> z-d (m)
!
			case (6)		!  Not implemented
				correction(i)=1.0
			cycle
!
!  Calculate T*
!
!  corr(i)%par(1) ==> pointer to w'T'
!  corr(i)%par(2) ==> pointer to u*
!
			case (7)		!  Not implemented yet
				ipnt_wt=ifix(corr(i)%par(1))
				ipnt_ustar=ifix(corr(i)%par(2))
				if ((correction(ipnt_ustar).eq.0.0).or.(correction(ipnt_ustar).eq.-9999.0)) then
					correction(i)=-9999.0
					cycle
				end if
				correction(i)=-(rot_covars(ipnt_wt)/correction(ipnt_ustar))
			cycle
!
!  calculate w*
!
!  corr(i)%par(1) ==> pointer to w'T'
!  corr(i)%par(2) ==> pointer to T  If this is zero, then met T is used.
!  corr(i)%par(3) ==> value of zi (height of PBL in m)  If this is zero, 1.0 is used.
!
			case (8)		!  Not implemented yet
				correction(i)=0.0
			cycle
!
!  Calculate u*
!
!  corr(i)%par(1) ==> pointer to w'u'
!  corr(i)%par(2) ==> multiplicative scale factor for u*
!
			case (9)
				a1=1.0
				if (corr(i)%par(2).ne.0.0) then
					a1=corr(i)%par(2)
				end if
				wu=rot_covars(ifix(corr(i)%par(1)))
				if (wu.lt.0.0D0) then
					correction(i)=a1*sqrt(-wu)
				else
					correction(i)=-9999.9D0
				end if
			cycle
!
!  Calculate Monin-Obukhov stability parameter (z/L)
!
!  corr(i)%par(1) ==> pointer to T
!  corr(i)%par(2) ==> pointer to w'T'
!  corr(i)%par(3) ==> pointer to w'u'
!  corr(i)%par(4) ==> value of z-d
!  corr(i)%par(5) ==> multiplicative scale factor for z/L
!
			case (10)
				a1=1.0
				if (corr(i)%par(5).ne.0.0) then
					a1=corr(i)%par(5)
				end if
				t=rot_means(ifix(corr(i)%par(1)))
				if (t.lt.100.0D0) t=t+273.15D0
				wt=rot_covars(ifix(corr(i)%par(2)))
				wu=rot_covars(ifix(corr(i)%par(3)))
				zmd=corr(i)%par(4)
				if (wu.lt.0.0D0) then
					ustar=sqrt(-wu)
					correction(i)=a1*(((-vk*zmd*g)/(t*ustar**3))*wt)
				else
					correction(i)=-9999.0D0
				end if
			cycle
!
!  Calculate vapor pressure deficit, VPD (kPa) (from eddy covariance data)
!
!  corr(i)%par(1) ==> pointer to rhoV  If this is zero, met rhoV is used
!  corr(i)%par(2) ==> pointer to T  If this is zero, met T is used
!
			case (11)		!  Not implemented yet
				correction(i)=0.0
			cycle
!
!  Calculate the mechanical roughness length, zo (m)
!
!  corr(i)%par(1) ==> pointer to u
!  corr(i)%par(2) ==> pointer to u*
!  corr(i)%par(3) ==> value of z-d
!  corr(i)%par(4) ==> multiplicative scale factor for zo
!
			case (12)		!  Not implemented yet
				correction(i)=0.0
			cycle
!
		end select
	end do
!
	return
	end subroutine Corrections
!
!==============================================================================
!
!  SUBROUINE Update_Comp_Time ()
!
!  This routine updates and maintains the computer time structure
!
!==============================================================================
!
	subroutine Update_Comp_Time ()
!
	use common_vars
!
	character(len=12) asc_year,asc_time,asc_zone
	integer(kind=4) irtime(8),idom(12)
!
!
	data idom /0,31,59,90,120,151,181,212,243,273,304,334/
	character(len=36),parameter:: months='JanFebMarAprMayJunJulAugSepOctNovDec'
!
!
10	format (I3.3)
20	format (I2.2)
!
!  get the current system time and data and update some of the structure
!
	call date_and_time (asc_year,asc_time,asc_zone,irtime)
	ktmp=((irtime(2)-1)*3)
!
	comp%year_str(1:4)=asc_year(1:4)
	comp%mon_str1(1:2)=asc_year(5:6)
	comp%mon_str2(1:3)=months(ktmp+1:ktmp+3)
	comp%day_str(1:2)=asc_year(7:8)
	comp%hr_str(1:2)=asc_time(1:2)
	comp%min_str(1:2)=asc_time(3:4)
	comp%sec_str(1:2)=asc_time(5:6)
	comp%time_str(1:8)=asc_time(1:2)//':'//asc_time(3:4)//':'//asc_time(5:6)
!
	comp%kyear=irtime(1)
	comp%kmon=irtime(2)
	comp%kday=irtime(3)
	comp%khr=irtime(5)
	comp%kmin=irtime(6)
	comp%ksec=irtime(7)
	comp%kzone=irtime(4)/60
	write (comp%zone_str(2:3),20) iabs(comp%kzone)
	comp%zone_str(1:1)=asc_zone(1:1)
!
!  calculate the DOY and fill in more of the structure values
!
	if ((mod(irtime(1),4).eq.0).and.(irtime(2).gt.2)) then
		irdoy=idom(irtime(2))+(irtime(3))+1
	else
		irdoy=idom(irtime(2))+(irtime(3))
	end if
	comp%kdoy=irdoy
	write (comp%doy_str(1:3),10) irdoy
!
!   calculate the time of year and fill in the rest of the structure
!
	comp%toy=float(irdoy)+(3600.0*float(irtime(5))+60.0*float(irtime(6))+float(irtime(7)))/86400.0
	if ((mod(irtime(1),4).eq.0).and.(comp%toy.ge.367.0)) then
		comp%toy=comp%toy-367.0
	end if
!
	if ((mod(irtime(1),4).ne.0).and.(comp%toy.ge.366.0)) then
		comp%toy=comp%toy-366.0
	end if
!
	return
	end subroutine Update_Comp_Time
!
!==============================================================================
!
!  SUBROUINE Prime_Inst_Time ()
!
!  This routine "primes" the instrument time structure with values from the
!  current system clock.  This must be called before calling the instrument
!  clock update routine (Update_Inst_Time) because many of the values can't be
!  determined from the instrument record number such as year, doy, etc.
!
!==============================================================================
!
	subroutine Prime_Inst_Time ()
!
	use common_vars
!
	character(len=12) asc_year,asc_time,asc_zone
	integer(kind=4) irtime(8),idom(12)
!
!
	data idom /0,31,59,90,120,151,181,212,243,273,304,334/
	character(len=36),parameter:: months='JanFebMarAprMayJunJulAugSepOctNovDec'
!
!
10	format (I3.3)
20	format (I2.2)
!
!  get the current system time and data and update some of the structure
!
	call date_and_time (asc_year,asc_time,asc_zone,irtime)
	ktmp=((irtime(2)-1)*3)
!
	inst%year_str(1:4)=asc_year(1:4)
	inst%mon_str1(1:2)=asc_year(5:6)
	inst%mon_str2(1:3)=months(ktmp+1:ktmp+3)
	inst%day_str(1:2)=asc_year(7:8)
	inst%hr_str(1:2)=asc_time(1:2)
	inst%min_str(1:2)=asc_time(3:4)
	inst%sec_str(1:2)=asc_time(5:6)
	inst%time_str(1:8)=asc_time(1:2)//':'//asc_time(3:4)//':'//asc_time(5:6)
!
	inst%kyear=irtime(1)
	inst%kmon=irtime(2)
	inst%kday=irtime(3)
	inst%khr=irtime(5)
	inst%kmin=irtime(6)
	inst%ksec=irtime(7)
	inst%kzone=irtime(4)/60
	write (inst%zone_str(2:3),20) iabs(inst%kzone)
	inst%zone_str(1:1)=asc_zone(1:1)
!
!  calculate the DOY and fill in more of the structure values
!
	if ((mod(irtime(1),4).eq.0).and.(irtime(2).gt.2)) then
		irdoy=idom(irtime(2))+(irtime(3))+1
	else
		irdoy=idom(irtime(2))+(irtime(3))
	end if
	inst%kdoy=irdoy
	write (inst%doy_str(1:3),10) irdoy
!
!   calculate the time of year and fill in the rest of the structure
!
	inst%toy=float(irdoy)+(3600.0*float(irtime(5))+60.0*float(irtime(6))+float(irtime(7)))/86400.0
	if ((mod(irtime(1),4).eq.0).and.(inst%toy.ge.367.0)) then
		inst%toy=inst%toy-367.0
	end if
!
	if ((mod(irtime(1),4).ne.0).and.(inst%toy.ge.366.0)) then
		inst%toy=inst%toy-366.0
	end if
!
	return
	end subroutine Prime_Inst_Time
!
!=============================================================================
!
!  SUBROUTINE Update_Inst_Time (kticks)
!
!  This routine maintains an "instrument" clock using the instrument record
!  number (irec) and the instrument frequency (samp_f) as a time base.  It
!  must be preceded by a call to Prime_Inst_Time() to initialize some of the
!  values in the instrument time structure that can't be uniquey determined
!  from the record number such as year, doy, etc.
!
!==============================================================================
!
	subroutine Update_Inst_Time (kticks)
!
	use common_vars
!
	integer(kind=4) kticks,ktmp
	integer(kind=4) idom1(12),idom2(12)
!
!
	data idom1 /0,31,59,90,120,151,181,212,243,273,304,334/
	data idom2 /0,31,60,91,121,152,182,213,244,274,305,335/
	character(len=36),parameter:: months='JanFebMarAprMayJunJulAugSepOctNovDec'
!
!
10	format (I2.2)
20	format (I3.3)
30	format (I4.4)
!
!  first, calclate the number of seconds from the record number, then
!  calculate the hours, minutes, and seconds
!
	ktmp=ifix((float(kticks)/samp_f)+0.5)
!
	k_old_hrs=inst%khr
!
	inst%khr=ktmp/3600
	ktmp=ktmp-(inst%khr*3600)
	inst%kmin=ktmp/60
	inst%ksec=ktmp-(inst%kmin*60)
!
!  now see if the hours have rolled over, which mean that we've rolled over a
!  day.  If so, update the DOY (taking care to observe leap years).  If the
!  DOY has rolled over, also update the year.
!
	kdiff=inst%khr-k_old_hrs
!
	if (kdiff.lt.0) then
		inst%kdoy=inst%kdoy+1
		if ((mod(inst%kyear,4).eq.0).and.(inst%kdoy.ge.367)) then
			inst%kdoy=1
			inst%kyear=inst%kyear+1
		end if
		if ((mod(inst%kyear,4).ne.0).and.(inst%kdoy.ge.366)) then
			inst%kdoy=1
			inst%kyear=inst%kyear+1
		end if
	end if
!
!  now use the DOY to calculate which month this is and update the insturment time
!  structures
!
	if (mod(inst%kyear,4).eq.0) then
		inst%kmon=12
		do k=1,12
			if(inst%kdoy.gt.idom2(k)) ktmp=k
		end do
		inst%kmon=ktmp
		inst%kday=inst%kdoy-idom2(ktmp)
	else
		inst%kmon=12
		do k=1,12
			if(inst%kdoy.gt.idom1(k)) ktmp=k
		end do
		inst%kmon=ktmp
		inst%kday=inst%kdoy-idom1(ktmp)
	end if
!
!  now update the string values in the time structure
!
	write (inst%year_str(1:4),30) inst%kyear
	write (inst%doy_str(1:3),20) inst%kdoy
	write (inst%mon_str1(1:2),10) inst%kmon
	write (inst%day_str(1:2),10) inst%kday
	write (inst%hr_str(1:2),10) inst%khr
	write (inst%min_str(1:2),10) inst%kmin
	write (inst%sec_str(1:2),10) inst%ksec
	inst%time_str(1:8)=inst%hr_str(1:2)//':'//inst%min_str(1:2)//':'//inst%sec_str(1:2)
!
!  fill in the month name
!
	ktmp=(inst%kmon-1)*3
	inst%mon_str2(1:3)=months(ktmp+1:ktmp+3)
!
!  calculate the time of year and fill in the rest of the structure values
!
	inst%toy=float(inst%kdoy)+(3600.0*float(inst%khr)+60.0*float(inst%kmin)+float(inst%ksec))/86400.0
	if ((mod(inst%kyear,4).eq.0).and.(inst%toy.ge.367.0)) then
		inst%toy=inst%toy-367.0
	end if
!
	if ((mod(inst%kyear,4).ne.0).and.(inst%toy.ge.366.0)) then
		inst%toy=inst%toy-366.0
	end if
!
	return
	end subroutine Update_Inst_Time
!
!==============================================================================
!
!  SUBROUTINE BuildWindows ()
!
!  This routine builds and scales the three windows for the screen and it
!  sets up the color palette.
!
!==============================================================================
!
	subroutine BuildWindows ()
!
	use common_vars
	use file_names
	use graph_params
!
	type (qwinfo) frame_winfo,msg_winfo,dat_winfo,graf_winfo
	type (windowconfig) msg_conf,dat_conf,graf_conf,frame_conf
	type (rccoord) cur
!
	if (.not.do_graphs) then
		idat_win=8
		imsg_win=8
		open (imsg_win,file='user',title='HuskerFlux Ver. '//prog_ver// &
		      '     MESSAGES')
		return
	end if
!
	n=InitializeFonts ()
	frame_winfo%type = QWIN$MAX
	i=SETWSIZEQQ (QWIN$FRAMEWINDOW,frame_winfo)
!
	istat=GETWSIZEQQ (QWIN$FRAMEWINDOW,QWIN$SIZECURR,frame_winfo)
	istat=GETWINDOWCONFIG(frame_conf)
!
!  Open the data, message, and graphic child windows
!
	open (idat_win,file='user',title='HuskerFlux Ver. '//prog_ver// &
	      '     One-Second Averages')
	open (imsg_win,file='user',title='HuskerFlux Ver. '//prog_ver// &
	      '     MESSAGES')
	open (igrf_win,file='user',title='HuskerFlux Ver. '//Prog_ver// &
	      '     Live Time Data Display')
!
!  Get and adjust the properties of the open windows
!
	istat=SETACTIVEQQ (igrf_win)
	istat=GETWINDOWCONFIG (graf_conf)
	istat=GETWSIZEQQ (idat_win,QWIN$SIZECURR,dat_winfo)
	istat=GETWSIZEQQ (imsg_win,QWIN$SIZECURR,msg_winfo)
	istat=GETWSIZEQQ (igrf_win,QWIN$SIZECURR,graf_winfo)
!
	icharhw=graf_conf%fontsize
	icharh=iand(icharhw,131071)
	icharw=iand(ishft(icharhw,-16),131071)
!
	msg_winfo%type=QWIN$SET
	msg_winfo%x=0														! 0
	msg_winfo%y=int(0.52084*float(frame_conf%numtextrows))				! 25
	msg_winfo%h=int(0.3125*float(frame_conf%numtextrows))				! 15
	msg_winfo%w=int(0.46875*float(frame_conf%numtextcols))				! 60
!
	dat_winfo%type=QWIN$SET
	dat_winfo%x=int(0.5*float(frame_conf%numtextcols))-1				! 64
	dat_winfo%y=int(0.52084*float(frame_conf%numtextrows))				! 25
	dat_winfo%h=int(0.3125*float(frame_conf%numtextrows))				! 15
	dat_winfo%w=int(0.484375*float(frame_conf%numtextcols))				! 62
!
	graf_winfo%type=QWIN$SET
	graf_winfo%x=0														! 0
	graf_winfo%y=0														! 0
	graf_winfo%h=int(0.4584*float(frame_conf%numtextrows))				! 22
	graf_winfo%w=int(0.9765625*float(frame_conf%numtextcols))			! 125
!
	graf_conf%numxpixels=int(0.99121*float(frame_conf%numxpixels))+1	! 1015
	graf_conf%numypixels=int(0.48177*float(frame_conf%numypixels))+1	! 370
	graf_conf%numtextrows=-1
	graf_conf%numtextcols=-1
	graf_conf%mode=QWIN$SCROLLDOWN
!
	nxpix=graf_conf%numxpixels
	nypix=graf_conf%numypixels
	ngrafcols=graf_winfo%w
	ngrafrows=graf_winfo%h
	iplen=ixbottom-ixtop
	isecs=int(float(iplen)/samp_f)
!	itks=isecs/10
	itks=7
	xmin=0.0
	xmax=float(isecs)
!
	call sleepqq(500)
!
!  The preceding statement is needed to prevent an initialization error
!  with QuickWin
!
	istat=SETWINDOWCONFIG (graf_conf)
	istat=SETWSIZEQQ (imsg_win,msg_winfo)
	istat=SETWSIZEQQ (idat_win,dat_winfo)
	istat=SETWSIZEQQ (igrf_win,graf_winfo)
!
!  These statements are needed to make the window scrolling consistant between
!  "release" and "debug" versions.
!
	istat=SETACTIVEQQ (imsg_win)
	istat=GETWINDOWCONFIG (msg_conf)
	msg_conf%mode=QWIN$SCROLLDOWN
	istat=SETWINDOWCONFIG (msg_conf)
	istat=SETACTIVEQQ (idat_win)
	istat=GETWINDOWCONFIG (dat_conf)
	dat_conf%mode=QWIN$SCROLLDOWN
	istat=SETWINDOWCONFIG (dat_conf)
!
	istat=SETACTIVEQQ (igrf_win)
	istat=SETBKCOLORRGB(#FFFFFF)
	call CLEARSCREEN ($GCLEARSCREEN)
!
	i=remappalettergb (0,$HIWHITE)				! Bright White
	i=remappalettergb (1,$HIRED)				! Bright Red
	i=remappalettergb (2,$HIBLUE)				! Bright Blue
	i=remappalettergb (3,$HIGREEN)				! Bright Green
	i=remappalettergb (4,$HIMAGENTA)			! Bright Magenta
	i=remappalettergb (5,$HICYAN)				! Bright Cyan
	i=remappalettergb (6,$HIYELLOW)				! Bright Yellow
	i=remappalettergb (7,$LOBLACK)				! Black
	i=remappalettergb (8,$LOWHITE)				! Dim White
	i=remappalettergb (9,$LORED)				! Dim Red
	i=remappalettergb (10,$LOBLUE)				! Dim Blue
	i=remappalettergb (11,$LOGREEN)				! Dim Green
	i=remappalettergb (12,$LOMAGENTA)			! Dim Magenta
	i=remappalettergb (13,$LOCYAN)				! Dim Cyan
	i=remappalettergb (14,$LOBROWN)				! Brown
	i=remappalettergb (15,$LOGRAY)				! Gray
!	i=remappalettergb (10,#00800080)			! violet
!	i=remappalettergb (10,#00808000)			! Teal
!
	icolors(1,1)=5
	icolors(1,2)=3
	icolors(2,1)=3
	icolors(2,2)=0
	icolors(3,1)=0
	icolors(3,2)=3
	icolors(4,1)=1
	icolors(4,2)=6
	icolors(5,1)=4
	icolors(5,2)=8
	icolors(6,1)=6
	icolors(6,2)=3
	icolors(7,1)=5
	icolors(7,2)=3
	icolors(8,1)=3
	icolors(8,2)=0
	icolors(9,1)=5
	icolors(9,2)=3
	icolors(10,1)=3
	icolors(10,2)=0
	icolors(11,1)=5
	icolors(11,2)=3
	icolors(12,1)=3
	icolors(12,2)=0
!
	call Axes ()
!
	return
	end subroutine BuildWindows
!
!==============================================================================
!
!  SUBROUTINE Axes ()
!
!  This routine builds the three graph zones, draws and labels the x-axis tic
!  marks, and draws the initial wind direction indicator.
!
!==============================================================================
!
	subroutine Axes ()
!
	use common_vars
	use file_names
	use graph_params
!
	integer(kind=4) istat,icol(8),iymid,ix
	character(len=32) lbl_string
!
	character(len=32), parameter :: lbl_string1='00.005.010.015.020.025.030.035.0'	! 20 Hz
	character(len=32), parameter :: lbl_string2='00.010.020.030.040.050.060.070.0'	! 10 Hz
	character(len=32), parameter :: lbl_string3='00.020.040.060.080.0100 120 140 '	! 5 Hz
	character(len=32), parameter :: lbl_string4='00.025.050.075.0100 125 150 175 '	! 4 Hz
	character(len=32), parameter :: lbl_string5='00.033.366.7100 133 167 200 233 '	! 3 Hz 
	character(len=32), parameter :: lbl_string6='00.050.0100 150 200 250 300 350 '	! 2 Hz
	character(len=32), parameter :: lbl_string7='00.0100 200 300 400 500 600 700 '	! 1 Hz
!
	itic=iplen/14
	istat=setcolor (int2(7))
	istat=settextcolor (int2(7))
!
	ifrq=int(samp_f)
	select case (ifrq)
		case default
			lbl_string=lbl_string2
		case (20)
			lbl_string=lbl_string1
		case (10)
			lbl_string=lbl_string2
		case (5)
			lbl_string=lbl_string3
		case (4)
			lbl_string=lbl_string4
		case (3)
			lbl_string=lbl_string5
		case (2)
			lbl_string=lbl_string6
		case (1)
			lbl_string=lbl_string7
	end select
!
!  draw the three graph windows
!
	do i=1,n_plots
		istat=rectangle ($gfillinterior,int2(ixtop),int2(iytop(i)),int2(ixbottom),int2(iybottom(i)))
	end do
!
!  draw and label the y-axis tic marks
!
	do i=1,n_plots
		iymid=((iybottom(i)-iytop(i))/2)+iytop(i)
		call moveto (ixtop,iytop(i),ixy)
		istat=lineto (int2(ixtop-7),int2(iytop(i)))
		call moveto (ixtop,iymid,ixy)
		istat=lineto (int2(ixtop-7),int2(iymid))
		call moveto (ixtop,iybottom(i),ixy)
		istat=lineto (int2(ixtop-7),int2(iybottom(i)))
		call moveto (ixbottom,iytop(i),ixy)
		istat=lineto (int2(ixbottom+7),int2(iytop(i)))
		call moveto (ixbottom,iymid,ixy)
		istat=lineto (int2(ixbottom+7),int2(iymid))
		call moveto (ixbottom,iybottom(i),ixy)
		istat=lineto (int2(ixbottom+7),int2(iybottom(i)))
	end do
!
	iitop=5
	iibotom=2
	do i=1,n_plots
		call settextposition (iitop,icolyl,curpos)
		if (wind.and.((ichn(2*i-1)).eq.iu)) then
			call outtext ('wind ')
		else
            if (ichn(2*i-1).lt.1) cycle
			call outtext (chan(ichn(2*i-1))%name(1:5))
		end if
		call settextposition (iibotom,icolyl,curpos)
		call outtext (color_name(1+icolors(2*i-1,1))(1:6))
!
		call settextposition (iitop,icolyr,curpos)
		if (wind.and.((2*i).eq.iu)) then
			call outtext ('wind ')
		else
            if (ichn(2*i).lt.1) cycle
			call outtext (chan(ichn(2*i))%name(1:5))
		end if
		call settextposition (iibotom,icolyr,curpos)
		call outtext (color_name(1+icolors(2*i,1))(1:6))
		iitop=iitop+7
		iibotom=iibotom+7
	end do
!
!  draw and label the x-axis tic marks
!
	ix=ixtop
	do i=1,itks
		indx=(i-1)*4+1
		call moveto (ix,iybottom(3),ixy)
		istat=lineto (int2(ix),int2(iybottom(3)+5))
!
		jcol=ix/icharw
		call settextposition (22,jcol,curpos)
		call outtext (lbl_string(indx:indx+3))
!
		ix=ix+itic
		call moveto (ix,iybottom(3),ixy)
		istat=lineto (int2(ix),int2(iybottom(3)+5))
		ix=ix+itic
	end do
	call moveto (ixbottom,iybottom(3),ixy)
	istat=lineto (int2(ixbottom),int2(iybottom(3)+5))
	jcol=ixbottom/icharw
	call settextposition (22,jcol,curpos)
	call outtext (lbl_string(itks*4+1:itks*4+4))
!
!  draw the wind direction meter
!
	call moveto (icomp_x,icomp_y,ixy)
	i=setcolor (int2(3))
	itemp=ellipse ($GFILLINTERIOR,int2(icomp_x-icomp_R),int2(icomp_y-icomp_R), &
                  int2(icomp_x+icomp_R),int2(icomp_y+icomp_R))
	call moveto (icomp_x,icomp_y,ixy)
	i=setcolor (int2(1))
	itemp=ellipse ($GFILLINTERIOR,int2(icomp_x-2),int2(icomp_y-2), &
                  int2(icomp_x+2),int2(icomp_y+2))
	call settextposition (3,13,curpos)
	call outtext ('N')
	call settextposition (7,21,curpos)
	call outtext ('E')
	call settextposition (11,13,curpos)
	call outtext ('S')
	call settextposition (7,5,curpos)
	call outtext ('W')
!
	return
	end subroutine Axes
!
!==============================================================================
!
!  SUBROUTINE Ylabels ()
!
!  This routine writes the Y-axis labels on the graphics screen
!
!==============================================================================
!
	subroutine Ylabels ()
!
	use common_vars
	use file_names
	use graph_params
!
	character(len=7) maxy(nchnl+1),miny(nchnl+1),midy(nchnl+1)
!
	istat=focusqq (igrf_win)
	istat=settextcolor(7)
!
	do i=1,nchnl+1
		write (maxy(i)(1:6),'(F6.1)') graf_rnge(i,1,1)
		write (midy(i)(1:6),'(F6.1)') graf_rnge(i,2,1)
		write (miny(i)(1:6),'(F6.1)') graf_rnge(i,3,1)
	end do
!
	do i=2,n_graphs,2
		if ((wind).and.(ichn(i).eq.1)) then
			itmp=verify(maxy(ichn(i))(1:6),' ')
			if (itmp.gt.1) then
				maxy(nchnl+1)(1:7-itmp)=maxy(nchnl+1)(itmp:6)
				do j=8-itmp,6
					maxy(nchnl+1)(j:j)=' '
				end do
			end if
			itmp=verify(midy(nchnl+1)(1:6),' ')
			if (itmp.gt.1) then
				midy(nchnl+1)(1:7-itmp)=midy(nchnl+1)(itmp:6)
				do j=8-itmp,6
					midy(nchnl+1)(j:j)=' '
				end do
			end if
			itmp=verify(miny(nchnl+1)(1:6),' ')
			if (itmp.gt.1) then
				miny(nchnl+1)(1:7-itmp)=miny(nchnl+1)(itmp:6)
				do j=8-itmp,6
					miny(nchnl+1)(j:j)=' '
				end do
			end if
		else
            if (ichn(i).lt.1) cycle
			itmp=verify(maxy(ichn(i))(1:6),' ')
			if (itmp.gt.1) then
				maxy(ichn(i))(1:7-itmp)=maxy(ichn(i))(itmp:6)
				do j=8-itmp,6
					maxy(ichn(i))(j:j)=' '
				end do
			end if
			itmp=verify(midy(ichn(i))(1:6),' ')
			if (itmp.gt.1) then
				midy(ichn(i))(1:7-itmp)=midy(ichn(i))(itmp:6)
				do j=8-itmp,6
					midy(ichn(i))(j:j)=' '
				end do
			end if
			itmp=verify(miny(ichn(i))(1:6),' ')
			if (itmp.gt.1) then
				miny(ichn(i))(1:7-itmp)=miny(ichn(i))(itmp:6)
				do j=8-itmp,6
					miny(ichn(i))(j:j)=' '
				end do
			end if
		end if
	end do
!
	istat=settextcolor(7)
!
	jmax=1
	jmid=4
	jmin=7
	do i=1,n_graphs,2
        if (ichn(i).lt.1) cycle
		call settextposition (jmax,icolyl,curpos)
		call outtext (maxy(ichn(i))(1:6))
		call settextposition (jmid,icolyl,curpos)
		call outtext (midy(ichn(i))(1:6))
		call settextposition (jmin,icolyl,curpos)
		call outtext (miny(ichn(i))(1:6))
        if (ichn(i+1).lt.1) cycle
		call settextposition (jmax,icolyr,curpos)
		call outtext (maxy(ichn(i+1))(1:6))
		call settextposition (jmid,icolyr,curpos)
		call outtext (midy(ichn(i+1))(1:6))
		call settextposition (jmin,icolyr,curpos)
		call outtext (miny(ichn(i+1))(1:6))
		jmax=jmax+7
		jmid=jmid+7
		jmin=jmin+7
	end do
!
	istat=focusqq (idat_win)
	return
	end subroutine Ylabels
!
!==============================================================================
!
!  SUBROUTINE UpdateClock ()
!
!  This routine updates the on-screen clock and current file information.
!  It also updates the wind direction indicator and the wind speed.  The
!  data output by this routine are 1-second averages.  The routine also keeps
!  up to date "real time" and "instrument time" clock/calendars
!
!==============================================================================
!
	subroutine UpdateClock ()
!
	use common_vars
	use file_names
	use graph_params
!
	character(len=60) Comp_out_string,Inst_out_string
	character(len=12) vel_string
    integer(kind=2) istat,ix,iy
!
	type (windowconfig) wc
!
	character(len=36), parameter :: months='JanFebMarAprMayJunJulAugSepOctNovDec'
!
10	format (F5.1)
!
!  build the time strings
!
	Comp_out_string(1:20)=comp%mon_str2(1:3)//' '//comp%day_str(1:2)//', '// &
	                     comp%year_str(1:4)//' DOY '//comp%doy_str(1:3)
	Inst_out_string(1:20)=inst%mon_str2(1:3)//' '//inst%day_str(1:2)//', '// &
	                     inst%year_str(1:4)//' DOY '//inst%doy_str(1:3)
!
	if (.not.do_graphs) then
		istat=GetWindowConfig (wc)
		wc%title=Comp_out_string(1:21)//char(0)
		istat=SetWindowConfig (wc)
		return
	end if
!
	istat=focusqq (igrf_win)
	call setviewport (0,0,int2(nxpix),int2(nypix))
	istat=settextcolor(int2(7))
	call settextposition (18,2,curpos)
	call outtext ('Real Time '//comp%time_str(1:8))
	call settextposition (19,2,curpos)
	call outtext (Comp_out_string(1:20))
	call settextposition (21,2,curpos)
	call outtext ('Inst Time '//inst%time_str(1:8))
	call settextposition (22,2,curpos)
	call outtext (Inst_out_string(1:20))
!
	iu=rot(1)%ktype
	iv=rot(2)%ktype
	iw=rot(3)%ktype
!
!  update the wind direction meter and the wind speed indicator
!
	rad=float(icomp_R)
	u=ameans_1sec(iu)
	v=ameans_1sec(iv)
	spd=sqrt(u*u+v*v)
	ix=int(rad*v/spd)+icomp_x
	iy=int(rad*u/spd)+icomp_y
!
	call moveto (icomp_x,icompy,ixy)
	i=setcolor (int2(3))
	itemp=ellipse ($GFILLINTERIOR,int2(icomp_x-icomp_R),int2(icomp_y-icomp_R), &
	              int2(icomp_x+icomp_R),int2(icomp_y+icomp_R))
	call moveto (icomp_x,icomp_y,ixy)
	i=setcolor (int2(1))
	itemp=ellipse ($GFILLINTERIOR,int2(icomp_x-2),int2(icomp_y-2), &
                  int2(icomp_x+2),int2(icomp_y+2))
	call moveto (icomp_x,icomp_y,ixy)
	istat=lineto (ix,iy)
	call settextposition (13,7,curpos)
	write (vel_string(1:5),10) spd
	vel_string(6:11)=' m/sec'
	call outtext (vel_string(1:11))
!
	return
	end subroutine UpdateClock
!
!==============================================================================
!
!  SUBROUTINE Graph_Range (ameans,stds,first_block)
!
!  This routine calculates the upper/lower limits for plotting each parameter
!
!==============================================================================
!
	subroutine Graph_Range (ameans,stds,first_block)
!
	use common_vars
	use file_names
	use graph_params
!
	logical first_block
	real(kind=8) ameans(*),stds(*)
	real(kind=8) tmp,scale,aspan,amul,bmul,bspan
!
	do i=1,max_chan+1
		graf_rnge(i,1,2)=graf_rnge(i,1,1)
		graf_rnge(i,2,2)=graf_rnge(i,2,1)
		graf_rnge(i,3,2)=graf_rnge(i,3,1)
		if (stds(i).le.1.0E-9) then
			graf_rnge(i,1,1)=1.0
			graf_rnge(i,2,1)=0.0
			graf_rnge(i,3,1)=-1.0
			cycle
		end if
		aspan=10.0*stds(i)
		ipow=-int(log10(dabs(aspan)))+1
		if (dabs(aspan).ge.1.0) ipow=ipow-1
		amul=10.0**ipow
		bmul=1.0/amul
		bspan=dabs(aspan*amul)
		if ((bspan.ge.0.0).and.(bspan.le.3.0)) scale=3.0*bmul
		if ((bspan.gt.3.0).and.(bspan.le.5.0)) scale=5.0*bmul
		if (bspan.gt.5.0) scale=10.0*bmul
		graf_rnge(i,2,1)=float(int(ameans(i)*amul))*bmul
		graf_rnge(i,1,1)=graf_rnge(i,2,1)+(scale/2.0)
		graf_rnge(i,3,1)=graf_rnge(i,2,1)-(scale/2.0)
	end do
!
!  if this is the first time, make the old the same as the new scales
!
	if (first_block) then
		do i=1,nchnl+1
			graf_rnge(i,1,2)=graf_rnge(i,1,1)
			graf_rnge(i,2,2)=graf_rnge(i,2,1)
			graf_rnge(i,3,2)=graf_rnge(i,3,1)
		end do
		first_block=.false.
	end if
!
	call Ylabels ()
!
	return
	end subroutine Graph_Range
!
!==============================================================================
!
!  SUBROUTINE Plot_Data (y,k,itrig)
!
!  This routine does the actual "worm" plotting.
!
!==============================================================================
!
	subroutine Plot_Data (y,k,itrig)
!
	use common_vars
	use file_names
	use graph_params
!
	real(kind=4) y(*)
	integer k,itrig
!
	ii=k+iworm
	if (ii.ge.iplen) ii=iplen
	do i=1,n_graphs
		istat=SETACTIVEQQ (igrf_win)
!
		call setviewport (int2(ixtop),int2(iytop(((i+1)/2))),int2(ixbottom),int2(iybottom((i+1)/2)))
!
		if ((wind).and.(ichn(i).eq.iu)) then
			i1=max_chan+1
		else
			i1=ichn(i)
		end if
!
		if(i1.lt.1) cycle
        istat=setwindow (.true.,xmin,graf_rnge(i1,3,2),xmax,graf_rnge(i1,1,2))
!
		istat=setcolor(int2(7))
		istat=setpixel_w(dble(xbuf(k)),dble(ybuf_old(i1,k)))
		if (itrig.lt.iplen-iworm) then
			istat=setcolor(int2(icolors(i,2)))
			istat=setpixel_w (dble(xbuf(ii)),dble(ybuf_old(i1,ii)))
		end if
!
		istat=setwindow (.true.,xmin,graf_rnge(i1,3,1),xmax,graf_rnge(i1,1,1))
		if (itrig.ge.iplen-iworm) then
			istat=setcolor(int2(icolors(i,2)))
			istat=setpixel_w(dble(xbuf(ii)),dble(ybuf_old(i1,ii)))
		end if
		istat=setcolor(int2(icolors(i,1)))
		istat=setpixel_w(dble(xbuf(k)),dble(y(i1)))
		if (ii.eq.iplen) then
			istat=setcolor(int2(7))
			istat=setpixel_w(dble(xbuf(k)),dble(ybuf_old(i1,k)))
		end if
		ybuf_old(i1,k)=y(i1)
	end do
!
	return
	end subroutine Plot_Data
!
!******************************************************************************
!
!  SUBROUTINE: File_manager
!
!  This routine does comprehensive file managerment, including transfer command
!  file generation according to the setup information in File-manager-setup.par
!  and records its actions in File-manager.log.
!
!******************************************************************************
!
!
!  Main routine
!
	subroutine File_manager
!
	use ifwin
	use dflib
!
	use File_manager_Vars
!
10	format (A)
20	format (1X,A)
30	format (1X,'File manager log ',A2,'/',A2,'/',A4,4X,A2,':',A2,':',A2,2X,'UCT ',A3)
40	format (1X,'Begin processing fast data ',A2,'/',A2,'/',A4,4X,A2,':',A2,':',A2,2X,'UCT ',A3)
50	format (1X,'Finished processing fast data ',A2,'/',A2,'/',A4,4X,A2,':',A2,':',A2,2X,'UCT ',A3)
60	format (1X,'Begin processing slow data ',A2,'/',A2,'/',A4,4X,A2,':',A2,':',A2,2X,'UCT ',A3)
70	format (1X,'Finished processing slow data ',A2,'/',A2,'/',A4,4X,A2,':',A2,':',A2,2X,'UCT ',A3)
80	format (1X,'Begin processing other data ',A2,'/',A2,'/',A4,4X,A2,':',A2,':',A2,2X,'UCT ',A3)
90	format (1X,'Finished processing other data ',A2,'/',A2,'/',A4,4X,A2,':',A2,':',A2,2X,'UCT ',A3)
100	format (I)
!
!  initialize the blank string and some other stuff
!
!
!  open the log files and write a header, also build the trans.cmd file
!
	open (unit=4,file=log_file(1:len_trim(log_file)),position='append')
	call date_and_time (date_str,time_str,zone_str,date_time)
	write (4,*)
    write (4,*) '==============================================================================='
	write (4,30) date_str(5:6),date_str(7:8),date_str(1:4),time_str(1:2),time_str(3:4),time_str(5:6),zone_str(1:3)
    write (4,*) '==============================================================================='
    write (4,*)
    if (USB_exist) then
        write (4,*) 'There is a removable USB device at ',USB_folder(1:len_trim(USB_folder))
    else
        write (4,*) 'There is no removable USB device present'
    end if
    write (4,*)
    write (4,*) ' There were ',n_EC,' eddy covariance data systems'
    if (n_EC.gt.0) then
        do i=1,n_EC
            write (4,*) ' EC system ',i,' :'
	        write (4,*) '      The input folder is ',EC_in_folder(i)(1:len_trim(EC_in_folder(i)))
	        write (4,*) '      The archive folder is ',EC_archive_folder(i)(1:len_trim(EC_archive_folder(i)))
            if (EC_transfer_folder(i)(1:1).ne.'#') then
                write (4,*) '      The transfer folder is ',EC_transfer_folder(i)(1:len_trim(EC_transfer_folder(i)))
            end if
            if (EC_remote_folder(i)(1:1).ne.'#') then
                write (4,*) '      The remote receiving folder is ',EC_remote_folder(i)(1:len_trim(EC_remote_folder(i)))
            end if
            write (4,*)
        end do
    end if
    write (4,*)
    write (4,*) 'There were ',n_slow,' slow response systems'
    if (n_slow.gt.0) then
        do i=1,n_slow
            write (4,*) ' slow response system ',i,' :'
		    write (4,*) '      The input file is ',Slow_in_folder(i)(1:len_trim(Slow_in_folder(i)))//Slow_file(i)(1:len_trim(slow_file(i)))
            write (4,*) '      The archive folder is ',Slow_archive_folder(i)(1:len_trim(Slow_archive_folder(i)))
            if (Slow_transfer_folder(i)(1:1).ne.'#') then
                write (4,*) '      The transfer folder is ',Slow_transfer_folder(i)(1:len_trim(Slow_transfer_folder(i)))
            end if
            if (Slow_remote_folder(i)(1:1).ne.'#') then
                write (4,*) '      The remote receiving folder is ',Slow_remote_folder(i)(1:len_trim(Slow_remote_folder(i)))
            end if
            if (Slow_cumul_folder(i)(1:1).ne.'#') then
                write (4,*) '      The cumulative file is ',Slow_cumul_folder(i)(1:len_trim(slow_cumul_folder(i)))//Slow_file(i)(1:len_trim(slow_file(i)))
            end if
            write (4,*)
        end do
    end if
    write (4,*)
    write (4,*) 'There were ',n_other,' other data streams'
    if (n_other.gt.0) then
        do i=1,n_other
            write (4,*) ' other data stream ',i,' :'
		    write (4,*) '      The input folder is ',Other_in_folder(i)(1:len_trim(Other_in_folder(i)))
            write (4,*) '      The archive folder is ',Other_archive_folder(i)(1:len_trim(Other_archive_folder(i)))
            if (Other_transfer_folder(i)(1:1).ne.'#') then
                write (4,*) '      The transfer folder is ',Other_transfer_folder(i)(1:len_trim(Other_transfer_folder(i)))
            end if
            if (Other_remote_folder(i)(1:1).ne.'#') then
                write (4,*) '      The remote receiving folder is ',Other_remote_folder(i)(1:len_trim(Other_remote_folder(i)))
            end if
            write (4,*)
        end do
    end if
!
!  Process the E.C. files
!
    if (n_EC.gt.0) then
        do i=1,n_EC
        	cmd_line(1:str_len)=blank_line(1:str_len)
	        cmd_line='dir '//EC_in_folder(i)(1:len_trim(EC_in_folder(i)))//'/B > .\files.txt'
	        iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
	        call DOY_Year (doy_chr,year_chr)
	        open (unit=1,file='.\files.txt')
	        new_line(1:str_len)=blank_line(1:str_len)
	        cmd_line(1:str_len)=blank_line(1:str_len)
	        call date_and_time (date_str,time_str,zone_str,date_time)
	        write (4,40) date_str(5:6),date_str(7:8),date_str(1:4),time_str(1:2),time_str(3:4),time_str(5:6),zone_str(1:3)
	        do
		        read (1,10,end=25) new_line
		        itemp=scan(new_line(1:len_trim(new_line)),'.',back=.true.)
		        if (itemp.gt.0) then
                    if (new_line(itemp-3:itemp-1).ne.doy_chr(1:3)) then
                        cmd_line='mkdir '//EC_archive_folder(i)(1:len_trim(EC_archive_folder(i)))
                        iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
    			        cmd_line(1:str_len)=blank_line(1:str_len)
    			        cmd_line='move/Y '//EC_in_folder(i)(1:len_trim(EC_in_folder(i)))// &
    			                new_line(1:len_trim(new_line))//' '// &
    					        EC_archive_folder(i)(1:len_trim(EC_archive_folder(i)))
    			        iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
    			        cmd_line(1:str_len)=blank_line(1:str_len)
    		        end if
                end if
	        end do
25	        close (unit=1)
!
!  copy the archive to the USB drive is it is present
!
            if (USB_exist) then
                itemp=scan(EC_archive_folder(i)(1:len_trim(EC_archive_folder(i))),'\',back=.false.)
                cmd_line='xcopy/y/e '//EC_archive_folder(i)(1:len_trim(EC_archive_folder(i))-1)//' '//USB_folder(1:len_trim(USB_folder))//EC_archive_folder(i)(itemp:len_trim(EC_archive_folder(i)))
                iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
                cmd_line(1:str_len)=blank_line(1:str_len)
            end if
!
!  copy the archive to the transfer folder if selected
!
            if (EC_transfer_folder(i)(1:1).ne.'#') then
                cmd_line='mkdir '//EC_transfer_folder(i)(1:len_trim(EC_transfer_folder(i)))
                iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
                cmd_line(1:str_len)=blank_line(1:str_len)
                cmd_line='copy '//EC_archive_folder(i)(1:len_trim(EC_archive_folder(i)))//'*.* '//EC_transfer_folder(i)(itemp:len_trim(EC_transfer_folder(i)))
                iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
                cmd_line(1:str_len)=blank_line(1:str_len)
            end if
!
	        call date_and_time (date_str,time_str,zone_str,date_time)
	        write (4,50) date_str(5:6),date_str(7:8),date_str(1:4),time_str(1:2),time_str(3:4),time_str(5:6),zone_str(1:3)
	        write (4,*)
	        cmd_line='del/Q .\files.txt'
	        iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
	        cmd_line(1:str_len)=blank_line(1:str_len)
        end do
!
    end if
!
!  Now do the slow response data.
!
	if (n_slow.gt.0) then
		call date_and_time (date_str,time_str,zone_str,date_time)
		write (4,60) date_str(5:6),date_str(7:8),date_str(1:4),time_str(1:2),time_str(3:4),time_str(5:6),zone_str(1:3)
        do i=1,n_slow
            cmd_line(1:str_len)=blank_line(1:str_len)
            cmd_line='mkdir '//Slow_archive_folder(i)(1:len_trim(Slow_archive_folder(i)))
            iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
            cmd_line(1:str_len)=blank_line(1:str_len)
            cmd_line='mkdir '//Slow_cumul_folder(i)(1:len_trim(Slow_cumul_folder(i)))
            iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
    		cmd_line(1:str_len)=blank_line(1:str_len)
            if (Slow_cumul_folder(i)(1:1).ne.'#') then
    		    open (unit=1,file=Slow_in_folder(i)(1:len_trim(Slow_in_folder(i)))//Slow_file(i)(1:len_trim(Slow_file(i))))
    		    open (unit=2,file=Slow_cumul_folder(i)(1:len_trim(Slow_cumul_folder(i)))//Slow_file(i)(1:len_trim(Slow_file(i))),Position='APPEND')
    		    do
    			    read (1,10,end=35) new_line
    			    write (2,20) new_line(1:len_trim(new_line))
    			    new_line(1:str_len)=blank_line(1:str_len)
    		    end do
35        	    close (unit=1)
    		    close (unit=2)
            end if
            cmd_line='copy/Y '//Slow_in_folder(i)(1:len_trim(Slow_in_folder(i)))//Slow_file(i)(1:len_trim(slow_file(i)))//' '//Slow_archive_folder(i)(1:len_trim(Slow_archive_folder(i)))//'*.*'
            iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
    		cmd_line(1:str_len)=blank_line(1:str_len)
            cmd_line='del/Q '//Slow_in_folder(i)(1:len_trim(Slow_in_folder(i)))//Slow_file(i)(1:len_trim(slow_file(i)))
            iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
    		cmd_line(1:str_len)=blank_line(1:str_len)
!
!  copy the archive to the USB drive is it is present
!
            if (USB_exist) then
                itemp=scan(Slow_archive_folder(i)(1:len_trim(Slow_archive_folder(i))),'\',back=.false.)
                cmd_line='xcopy/y/e '//Slow_archive_folder(i)(1:len_trim(Slow_archive_folder(i))-1)//' '//USB_folder(1:len_trim(USB_folder))//Slow_archive_folder(i)(itemp:len_trim(Slow_archive_folder(i)))
                iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
                cmd_line(1:str_len)=blank_line(1:str_len)
            end if
!
!  copy the archive to the transfer folder if selected
!
            if (Slow_transfer_folder(i)(1:1).ne.'#') then
                cmd_line='mkdir '//Slow_transfer_folder(i)(1:len_trim(Slow_transfer_folder(i)))
                iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
                cmd_line(1:str_len)=blank_line(1:str_len)
                cmd_line='copy '//Slow_archive_folder(i)(1:len_trim(Slow_archive_folder(i)))//'*.* '//Slow_transfer_folder(i)(itemp:len_trim(Slow_transfer_folder(i)))
                iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
                cmd_line(1:str_len)=blank_line(1:str_len)
            end if
!
        end do
		call date_and_time (date_str,time_str,zone_str,date_time)
		write (4,70) date_str(5:6),date_str(7:8),date_str(1:4),time_str(1:2),time_str(3:4),time_str(5:6),zone_str(1:3)
		write (4,*)
	end if
!
!  Now do the "other" data.
!
    if (n_other.gt.0) then
        do i=1,n_other
        	cmd_line(1:str_len)=blank_line(1:str_len)
	        cmd_line='dir '//Other_in_folder(i)(1:len_trim(Other_in_folder(i)))//'/B > .\files.txt'
	        iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
	        call DOY_Year (doy_chr,year_chr)
	        open (unit=1,file='.\files.txt')
	        new_line(1:str_len)=blank_line(1:str_len)
	        cmd_line(1:str_len)=blank_line(1:str_len)
	        call date_and_time (date_str,time_str,zone_str,date_time)
	        write (4,80) date_str(5:6),date_str(7:8),date_str(1:4),time_str(1:2),time_str(3:4),time_str(5:6),zone_str(1:3)
	        do
		        read (1,10,end=45) new_line
		        itemp=scan(new_line(1:len_trim(new_line)),'.',back=.true.)
		        if (itemp.gt.0) then
                    cmd_line='mkdir '//Other_archive_folder(i)(1:len_trim(Other_archive_folder(i)))
                    iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
    			    cmd_line(1:str_len)=blank_line(1:str_len)
    			    cmd_line='copy/Y '//Other_in_folder(i)(1:len_trim(Other_in_folder(i)))// &
    			              new_line(1:len_trim(new_line))//' '// &
    				          Other_archive_folder(i)(1:len_trim(Other_archive_folder(i)))//'*.*'
    			    iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
    			    cmd_line(1:str_len)=blank_line(1:str_len)
                end if
	        end do
45	        close (unit=1)
            cmd_line='del/Q '//Other_in_folder(i)(1:len_trim(Other_in_folder(i)))//'*.*'
            iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
	        cmd_line(1:str_len)=blank_line(1:str_len)
!
!  copy the archive to the USB drive is it is present
!
            if (USB_exist) then
                itemp=scan(Other_archive_folder(i)(1:len_trim(Other_archive_folder(i))),'\',back=.false.)
                cmd_line='xcopy/y/e '//Other_archive_folder(i)(1:len_trim(Other_archive_folder(i))-1)//' '//USB_folder(1:len_trim(USB_folder))//Other_archive_folder(i)(itemp:len_trim(Other_archive_folder(i)))
                iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
                cmd_line(1:str_len)=blank_line(1:str_len)
            end if
!
!  copy the archive to the transfer folder if selected
!
            if (Other_transfer_folder(i)(1:1).ne.'#') then
                cmd_line='mkdir '//Other_transfer_folder(i)(1:len_trim(Other_transfer_folder(i)))
                iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
                cmd_line(1:str_len)=blank_line(1:str_len)
                cmd_line='copy '//Other_archive_folder(i)(1:len_trim(Other_archive_folder(i)))//'*.* '//Other_transfer_folder(i)(itemp:len_trim(Other_transfer_folder(i)))
                iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
                cmd_line(1:str_len)=blank_line(1:str_len)
            end if
	        call date_and_time (date_str,time_str,zone_str,date_time)
	        write (4,90) date_str(5:6),date_str(7:8),date_str(1:4),time_str(1:2),time_str(3:4),time_str(5:6),zone_str(1:3)
	        write (4,*)
	        cmd_line='del/Q .\files.txt'
	        iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
	        cmd_line(1:str_len)=blank_line(1:str_len)
        end do
!
    end if
!
!
105	close (unit=4)
    if (USB_exist) then
        cmd_line='Copy /Y '//log_file(1:len_trim(log_file))//' '//USB_folder(1:len_trim(USB_folder))
        iresult=systemQQ(cmd_line(1:len_trim(cmd_line)))
        cmd_line(1:str_len)=blank_line(1:str_len)
    end if
!
!  Build the FTP transfer command file if needed
!
    if (n_transfers.ne.0) then
        open (unit=1,file=transfer_command_file(1:len_trim(transfer_command_file)))
        do i=1,n_EC
            if (EC_transfer_folder(i)(1:1).ne.'#') then
                write (1,*) 'mkdir '//EC_remote_folder(i)(1:len_trim(EC_remote_folder(i)))
                write (1,*) 'cd '//EC_remote_folder(i)(1:len_trim(EC_remote_folder(i)))
                write (1,*) 'lcd '//EC_transfer_folder(i)(1:len_trim(EC_transfer_folder(i)))
                write (1,*) 'mput *.*'
                write (1,*) 'lcd '//Local_home_folder(1:len_trim(Local_home_folder))
                write (1,*) '!rmdir/S/Q '//EC_transfer_folder(i)(1:len_trim(EC_transfer_folder(i)))
                write (1,*) 'cd'
            end if
        end do
        do i=1,n_slow
            if (Slow_transfer_folder(i)(1:1).ne.'#') then
                write (1,*) 'mkdir '//Slow_remote_folder(i)(1:len_trim(Slow_remote_folder(i)))
                write (1,*) 'cd '//Slow_remote_folder(i)(1:len_trim(Slow_remote_folder(i)))
                write (1,*) 'lcd '//Slow_transfer_folder(i)(1:len_trim(Slow_transfer_folder(i)))
                write (1,*) 'mput *.*'
                write (1,*) 'lcd '//Local_home_folder(1:len_trim(Local_home_folder))
                write (1,*) '!rmdir/S/Q '//Slow_transfer_folder(i)(1:len_trim(Slow_transfer_folder(i)))
                write (1,*) 'cd'
            end if
        end do
        do i=1,n_other
            if (Other_transfer_folder(i)(1:1).ne.'#') then
                write (1,*) 'mkdir '//Other_remote_folder(i)(1:len_trim(Other_remote_folder(i)))
                write (1,*) 'cd '//Other_remote_folder(i)(1:len_trim(Other_remote_folder(i)))
                write (1,*) 'lcd '//Other_transfer_folder(i)(1:len_trim(Other_transfer_folder(i)))
                write (1,*) 'mput *.*'
                write (1,*) 'lcd '//Local_home_folder(1:len_trim(Local_home_folder))
                write (1,*) '!rmdir/S/Q '//Other_transfer_folder(i)(1:len_trim(Other_transfer_folder(i)))
                write (1,*) 'cd'
            end if
        end do
        close (unit=1)
    end if
!
!  Clean up and exit
!
	if (n_EC.gt.0) then
		deallocate (EC_in_folder,stat=ialloc_err)
		deallocate (EC_archive_folder,stat=ialloc_err)
        deallocate (EC_transfer_folder,stat=ialloc_err)
        deallocate (EC_remote_folder,stat=ialloc_err)
	end if
!
	if (n_slow.gt.0) then
		deallocate (Slow_in_folder,stat=ialloc_err)
		deallocate (Slow_archive_folder,stat=ialloc_err)
		deallocate (Slow_cumul_folder,stat=ialloc_err)
        deallocate (Slow_transfer_folder,stat=ialloc_err)
        deallocate (Slow_remote_folder,stat=ialloc_err)
		deallocate (Slow_file,stat=ialloc_err)
	end if
!
	if (n_other.gt.0) then
		deallocate (Other_in_folder,stat=ialloc_err)
		deallocate (Other_archive_folder,stat=ialloc_err)
        deallocate (Other_remote_folder,stat=ialloc_err)
        deallocate (Other_transfer_folder,stat=ialloc_err)
	end if
!
	return
    end subroutine File_manager
!
!
!******************************************************************************
!
!
	subroutine DOY_Year (iyest_doy,iyest_year)
!
!
	use ifwin
	use dflib
!
	character(len=12) year,tim,zone
	character(len=4) iyest_doy,iyest_year
	integer(kind=4) itoday_doy,itim(8),idom(12)
!
	data idom /0,31,59,90,120,151,181,212,243,273,304,334/
!
10	format (I3.3)
20	format (I4.4)
!
!
	call date_and_time (year,tim,zone,itim)
!
	itoday_doy=idom(itim(2))+itim(3)
	if ((mod(itim(1),4).eq.0).and.(itim(2).gt.2)) then
		itoday_doy=itoday_doy+1
	end if
!
	write (iyest_doy(1:3),10) itoday_doy
	write (iyest_year(1:4),20) itim(1)
!
	return
	end subroutine DOY_Year
!
!
!******************************************************************************
!
!
	subroutine parse_folder (in_string, out_string)
!
!
	character(len=*) in_string,out_string
	character(len=12) date_str,time_str,zone_str
	character(len=4) DOY,Year
	integer(kind=4) date_time(8)
!
	call date_and_time (date_str,time_str,zone_str,date_time)
!
	call DOY_Year(DOY,Year)
	in_len=len_trim(in_string)
	i_out=1
	i_in=1
	do
		if (in_string(i_in:i_in).ne.'%') then
			out_string(i_out:i_out)=in_string(i_in:i_in)
			i_out=i_out+1
		else
			i_in=i_in+1
			if (in_string(i_in:i_in).eq.'d') then
				out_string(i_out:i_out+3)=DOY(1:3)
				i_out=i_out+3
			else if (in_string(i_in:i_in).eq.'D') then
				out_string(i_out:i_out+10)=date_str(1:4)//'-'//date_str(5:6)//'-'//date_str(7:8)
				i_out=i_out+10
			else if (in_string(i_in:i_in).eq.'Y') then
				out_string(i_out:i_out+4)=date_str(1:4)
				i_out=i_out+4
			else
				out_string(i_out:i_out+1)=in_string(i_in-1:i_in)
				i_out=i_out+2
			end if
		end if
		i_in=i_in+1
		if (i_in.gt.in_len) exit
	end do
!
	return
	end subroutine parse_folder
