# Makefile created by mkmf $Id: mkmf,v 18.0.18.4.2.1.2.3 2013/08/21 15:04:06 Seth.Underwood Exp $ 

include ./intel.mk
SRCROOT = ../src/
CPPFLAGS = $(INCLUDE) -Duse_libMPI -Duse_netCDF -DINTERNAL_FILE_NML -DOVERLOAD_C8 -Duse_LARGEFILE 
FPPFLAGS += -Duse_libMPI -Duse_netCDF -DINTERNAL_FILE_NML -DOVERLOAD_C8 -Duse_LARGEFILE
CFLAGS += -openmp
FFALGS += -openmp

.DEFAULT:
	-echo $@ does not exist.
all: shallow_water.x
affinity.o: $(SRCROOT)shared/mpp/affinity.c
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) $(OTHER_CFLAGS) -c	$(SRCROOT)shared/mpp/affinity.c
atmos_model.o: $(SRCROOT)atmos_solo/atmos_model.F90 mpp.o fms.o atmosphere.o time_manager.o fms_io.o mpp_domains.o mpp_io.o diag_manager.o field_manager.o tracer_manager.o memutils.o constants.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_solo/atmos_model.F90
atmosphere.o: $(SRCROOT)atmos_spectral_shallow/atmosphere.F90 fms.o mpp.o constants.o transforms.o time_manager.o shallow_dynamics.o shallow_physics.o shallow_diagnostics.o mass_pulse.o wave_break.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral_shallow/atmosphere.F90
constants.o: $(SRCROOT)shared/constants/constants.F90 fms.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/constants/constants.F90
create_xgrid.o: $(SRCROOT)shared/mosaic/create_xgrid.c $(SRCROOT)shared/mosaic/mosaic_util.h $(SRCROOT)shared/mosaic/create_xgrid.h $(SRCROOT)shared/mosaic/constant.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) $(OTHER_CFLAGS) -c -I$(SRCROOT)shared/mosaic	$(SRCROOT)shared/mosaic/create_xgrid.c
diag_axis.o: $(SRCROOT)shared/diag_manager/diag_axis.F90 mpp_domains.o fms.o diag_data.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/diag_manager/diag_axis.F90
diag_data.o: $(SRCROOT)shared/diag_manager/diag_data.F90 $(SRCROOT)shared/include/fms_platform.h time_manager.o mpp_domains.o mpp_io.o fms.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include	$(SRCROOT)shared/diag_manager/diag_data.F90
diag_grid.o: $(SRCROOT)shared/diag_manager/diag_grid.F90 $(SRCROOT)shared/include/fms_platform.h constants.o fms.o mpp.o mpp_domains.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include	$(SRCROOT)shared/diag_manager/diag_grid.F90
diag_manager.o: $(SRCROOT)shared/diag_manager/diag_manager.F90 $(SRCROOT)shared/include/fms_platform.h time_manager.o mpp_io.o mpp.o fms.o diag_axis.o diag_util.o diag_data.o diag_table.o diag_output.o diag_grid.o constants.o mpp_domains.o fms_io.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include	$(SRCROOT)shared/diag_manager/diag_manager.F90
diag_output.o: $(SRCROOT)shared/diag_manager/diag_output.F90 mpp_io.o mpp_domains.o mpp.o diag_axis.o diag_data.o time_manager.o fms.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/diag_manager/diag_output.F90
diag_table.o: $(SRCROOT)shared/diag_manager/diag_table.F90 mpp_io.o mpp.o fms.o time_manager.o constants.o diag_data.o diag_util.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/diag_manager/diag_table.F90
diag_util.o: $(SRCROOT)shared/diag_manager/diag_util.F90 diag_data.o diag_axis.o diag_output.o diag_grid.o fms.o fms_io.o mpp_domains.o time_manager.o mpp_io.o mpp.o constants.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/diag_manager/diag_util.F90
fft.o: $(SRCROOT)shared/fft/fft.F90 platform.o fms.o fft99.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/fft/fft.F90
fft99.o: $(SRCROOT)shared/fft/fft99.F90 constants.o mpp.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/fft/fft99.F90
field_manager.o: $(SRCROOT)shared/field_manager/field_manager.F90 $(SRCROOT)shared/field_manager/parse.inc mpp.o mpp_io.o fms.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/field_manager	$(SRCROOT)shared/field_manager/field_manager.F90
fm_util.o: $(SRCROOT)shared/field_manager/fm_util.F90 field_manager.o fms.o mpp.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/field_manager/fm_util.F90
fms.o: $(SRCROOT)shared/fms/fms.F90 mpp.o mpp_domains.o mpp_io.o fms_io.o memutils.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/fms/fms.F90
fms_io.o: $(SRCROOT)shared/fms/fms_io.F90 $(SRCROOT)shared/include/fms_platform.h $(SRCROOT)shared/fms/read_data_2d.inc $(SRCROOT)shared/fms/read_data_3d.inc $(SRCROOT)shared/fms/read_data_4d.inc $(SRCROOT)shared/fms/write_data.inc mpp_io.o mpp_domains.o mpp.o platform.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include -I$(SRCROOT)shared/fms	$(SRCROOT)shared/fms/fms_io.F90
fv_advection.o: $(SRCROOT)atmos_spectral/model/fv_advection.F90 fms.o constants.o mpp_domains.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral/model/fv_advection.F90
gauss_and_legendre.o: $(SRCROOT)atmos_spectral/tools/gauss_and_legendre.F90 fms.o constants.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral/tools/gauss_and_legendre.F90
get_cal_time.o: $(SRCROOT)shared/time_manager/get_cal_time.F90 fms.o time_manager.o mpp.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/time_manager/get_cal_time.F90
gradient.o: $(SRCROOT)shared/mosaic/gradient.F90 mpp.o constants.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/mosaic/gradient.F90
gradient_c2l.o: $(SRCROOT)shared/mosaic/gradient_c2l.c $(SRCROOT)shared/mosaic/constant.h $(SRCROOT)shared/mosaic/mosaic_util.h $(SRCROOT)shared/mosaic/gradient_c2l.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) $(OTHER_CFLAGS) -c -I$(SRCROOT)shared/mosaic	$(SRCROOT)shared/mosaic/gradient_c2l.c
grid.o: $(SRCROOT)shared/mosaic/grid.F90 mpp.o constants.o fms.o fms_io.o mosaic.o mpp_domains.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/mosaic/grid.F90
grid_fourier.o: $(SRCROOT)atmos_spectral/tools/grid_fourier.F90 fms.o constants.o fft.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral/tools/grid_fourier.F90
interp.o: $(SRCROOT)shared/mosaic/interp.c $(SRCROOT)shared/mosaic/mosaic_util.h $(SRCROOT)shared/mosaic/interp.h $(SRCROOT)shared/mosaic/create_xgrid.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) $(OTHER_CFLAGS) -c -I$(SRCROOT)shared/mosaic	$(SRCROOT)shared/mosaic/interp.c
leapfrog.o: $(SRCROOT)atmos_spectral/model/leapfrog.F90 fms.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral/model/leapfrog.F90
memuse.o: $(SRCROOT)shared/memutils/memuse.c
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) $(OTHER_CFLAGS) -c	$(SRCROOT)shared/memutils/memuse.c
memutils.o: $(SRCROOT)shared/memutils/memutils.F90 mpp.o mpp_io.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/memutils/memutils.F90
mosaic.o: $(SRCROOT)shared/mosaic/mosaic.F90 mpp.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/mosaic/mosaic.F90
mosaic_util.o: $(SRCROOT)shared/mosaic/mosaic_util.c $(SRCROOT)shared/mosaic/mosaic_util.h $(SRCROOT)shared/mosaic/constant.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) $(OTHER_CFLAGS) -c -I$(SRCROOT)shared/mosaic	$(SRCROOT)shared/mosaic/mosaic_util.c
mpp.o: $(SRCROOT)shared/mpp/mpp.F90 $(SRCROOT)shared/include/fms_platform.h $(SRCROOT)shared/mpp/include/system_clock.h $(SRCROOT)shared/mpp/include/mpp_util.inc $(SRCROOT)shared/mpp/include/mpp_util_sma.inc $(SRCROOT)shared/mpp/include/mpp_util_mpi.inc $(SRCROOT)shared/mpp/include/mpp_util_nocomm.inc $(SRCROOT)shared/mpp/include/mpp_error_a_a.h $(SRCROOT)shared/mpp/include/mpp_error_a_s.h $(SRCROOT)shared/mpp/include/mpp_error_s_a.h $(SRCROOT)shared/mpp/include/mpp_error_s_s.h $(SRCROOT)shared/mpp/include/mpp_comm.inc $(SRCROOT)shared/mpp/include/mpp_comm_sma.inc $(SRCROOT)shared/mpp/include/mpp_transmit_sma.h $(SRCROOT)shared/mpp/include/mpp_transmit.inc $(SRCROOT)shared/mpp/include/mpp_reduce_sma.h $(SRCROOT)shared/mpp/include/mpp_sum_sma.h $(SRCROOT)shared/mpp/include/mpp_sum.inc $(SRCROOT)shared/mpp/include/mpp_comm_mpi.inc $(SRCROOT)shared/mpp/include/mpp_transmit_mpi.h $(SRCROOT)shared/mpp/include/mpp_reduce_mpi.h $(SRCROOT)shared/mpp/include/mpp_sum_mpi.h $(SRCROOT)shared/mpp/include/mpp_comm_nocomm.inc $(SRCROOT)shared/mpp/include/mpp_transmit_nocomm.h $(SRCROOT)shared/mpp/include/mpp_reduce_nocomm.h $(SRCROOT)shared/mpp/include/mpp_sum_nocomm.h $(SRCROOT)shared/mpp/include/mpp_chksum_int.h $(SRCROOT)shared/mpp/include/mpp_chksum_scalar.h $(SRCROOT)shared/mpp/include/mpp_chksum.h $(SRCROOT)shared/mpp/include/mpp_gather.h mpp_parameter.o mpp_data.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include -I$(SRCROOT)shared/mpp/include	$(SRCROOT)shared/mpp/mpp.F90
mpp_data.o: $(SRCROOT)shared/mpp/mpp_data.F90 $(SRCROOT)shared/include/fms_platform.h $(SRCROOT)shared/mpp/include/mpp_data_sma.inc $(SRCROOT)shared/mpp/include/mpp_data_mpi.inc $(SRCROOT)shared/mpp/include/mpp_data_nocomm.inc mpp_parameter.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include -I$(SRCROOT)shared/mpp/include	$(SRCROOT)shared/mpp/mpp_data.F90
mpp_domains.o: $(SRCROOT)shared/mpp/mpp_domains.F90 $(SRCROOT)shared/include/fms_platform.h $(SRCROOT)shared/mpp/include/mpp_define_nest_domains.inc $(SRCROOT)shared/mpp/include/mpp_domains_util.inc $(SRCROOT)shared/mpp/include/mpp_domains_comm.inc $(SRCROOT)shared/mpp/include/mpp_domains_define.inc $(SRCROOT)shared/mpp/include/mpp_domains_misc.inc $(SRCROOT)shared/mpp/include/mpp_update_domains2D.h $(SRCROOT)shared/mpp/include/mpp_update_domains2D_nonblock.h $(SRCROOT)shared/mpp/include/mpp_do_update_nonblock.h $(SRCROOT)shared/mpp/include/mpp_do_updateV_nonblock.h $(SRCROOT)shared/mpp/include/mpp_do_update.h $(SRCROOT)shared/mpp/include/mpp_do_updateV.h $(SRCROOT)shared/mpp/include/mpp_do_check.h $(SRCROOT)shared/mpp/include/mpp_do_checkV.h $(SRCROOT)shared/mpp/include/mpp_update_nest_domains.h $(SRCROOT)shared/mpp/include/mpp_do_update_nest.h $(SRCROOT)shared/mpp/include/mpp_do_redistribute.h $(SRCROOT)shared/mpp/include/mpp_get_boundary.h $(SRCROOT)shared/mpp/include/mpp_do_get_boundary.h $(SRCROOT)shared/mpp/include/mpp_domains_reduce.inc $(SRCROOT)shared/mpp/include/mpp_global_reduce.h $(SRCROOT)shared/mpp/include/mpp_global_sum.h $(SRCROOT)shared/mpp/include/mpp_global_sum_tl.h $(SRCROOT)shared/mpp/include/mpp_global_field.h $(SRCROOT)shared/mpp/include/mpp_do_global_field.h mpp_parameter.o mpp_data.o mpp.o mpp_memutils.o mpp_pset.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include -I$(SRCROOT)shared/mpp/include	$(SRCROOT)shared/mpp/mpp_domains.F90
mpp_io.o: $(SRCROOT)shared/mpp/mpp_io.F90 $(SRCROOT)shared/include/fms_platform.h $(SRCROOT)shared/mpp/include/mpp_io_util.inc $(SRCROOT)shared/mpp/include/mpp_io_misc.inc $(SRCROOT)shared/mpp/include/mpp_io_connect.inc $(SRCROOT)shared/mpp/include/mpp_io_read.inc $(SRCROOT)shared/mpp/include/mpp_read_2Ddecomp.h $(SRCROOT)shared/mpp/include/mpp_io_write.inc $(SRCROOT)shared/mpp/include/mpp_write_2Ddecomp.h $(SRCROOT)shared/mpp/include/mpp_write.h mpp_parameter.o mpp.o mpp_domains.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include -I$(SRCROOT)shared/mpp/include	$(SRCROOT)shared/mpp/mpp_io.F90
mpp_memutils.o: $(SRCROOT)shared/mpp/mpp_memutils.F90 mpp.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/mpp/mpp_memutils.F90
mpp_parameter.o: $(SRCROOT)shared/mpp/mpp_parameter.F90 $(SRCROOT)shared/include/fms_platform.h
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include	$(SRCROOT)shared/mpp/mpp_parameter.F90
mpp_pset.o: $(SRCROOT)shared/mpp/mpp_pset.F90 $(SRCROOT)shared/include/fms_platform.h mpp.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include	$(SRCROOT)shared/mpp/mpp_pset.F90
mpp_utilities.o: $(SRCROOT)shared/mpp/mpp_utilities.F90 mpp.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/mpp/mpp_utilities.F90
nsclock.o: $(SRCROOT)shared/mpp/nsclock.c
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) $(OTHER_CFLAGS) -c	$(SRCROOT)shared/mpp/nsclock.c
platform.o: $(SRCROOT)shared/platform/platform.F90 $(SRCROOT)shared/include/fms_platform.h
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include	$(SRCROOT)shared/platform/platform.F90
read_mosaic.o: $(SRCROOT)shared/mosaic/read_mosaic.c $(SRCROOT)shared/mosaic/read_mosaic.h $(SRCROOT)shared/mosaic/constant.h $(SRCROOT)shared/mosaic/mosaic_util.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) $(OTHER_CFLAGS) -c -I$(SRCROOT)shared/mosaic	$(SRCROOT)shared/mosaic/read_mosaic.c
shallow_diagnostics.o: $(SRCROOT)atmos_spectral_shallow/shallow_diagnostics.F90 fms.o transforms.o diag_manager.o time_manager.o shallow_physics.o shallow_dynamics.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral_shallow/shallow_diagnostics.F90
shallow_dynamics.o: $(SRCROOT)atmos_spectral_shallow/shallow_dynamics.F90 fms.o time_manager.o constants.o transforms.o spectral_damping.o leapfrog.o fv_advection.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral_shallow/shallow_dynamics.F90
shallow_physics.o: $(SRCROOT)atmos_spectral_shallow/shallow_physics.F90 fms.o transforms.o time_manager.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral_shallow/shallow_physics.F90
mass_pulse.o : $(SRCROOT)atmos_spectral_shallow/mass_pulse.F90 rand_generator.o transforms.o fms.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c     $(SRCROOT)atmos_spectral_shallow/mass_pulse.F90
rand_generator.o : $(SRCROOT)atmos_spectral_shallow/rand_generator.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c     $(SRCROOT)atmos_spectral_shallow/rand_generator.F90
spec_mpp.o: $(SRCROOT)atmos_spectral/tools/spec_mpp.F90 fms.o mpp_domains.o
wave_break.o : $(SRCROOT)atmos_spectral_shallow/wave_break.F90 transforms.o fms.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c     $(SRCROOT)atmos_spectral_shallow/wave_break.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral/tools/spec_mpp.F90
spectral_damping.o: $(SRCROOT)atmos_spectral/model/spectral_damping.F90 fms.o transforms.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral/model/spectral_damping.F90
spherical.o: $(SRCROOT)atmos_spectral/tools/spherical.F90 fms.o spec_mpp.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral/tools/spherical.F90
spherical_fourier.o: $(SRCROOT)atmos_spectral/tools/spherical_fourier.F90 fms.o mpp.o mpp_domains.o constants.o spec_mpp.o spherical.o gauss_and_legendre.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral/tools/spherical_fourier.F90
test_fms_io.o: $(SRCROOT)shared/fms/test_fms_io.F90 $(SRCROOT)shared/include/fms_platform.h mpp.o mpp_domains.o mpp_io.o fms_io.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include	$(SRCROOT)shared/fms/test_fms_io.F90
test_mpp.o: $(SRCROOT)shared/mpp/test_mpp.F90 $(SRCROOT)shared/include/fms_platform.h mpp.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include	$(SRCROOT)shared/mpp/test_mpp.F90
test_mpp_domains.o: $(SRCROOT)shared/mpp/test_mpp_domains.F90 $(SRCROOT)shared/include/fms_platform.h mpp.o mpp_domains.o mpp_memutils.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include	$(SRCROOT)shared/mpp/test_mpp_domains.F90
test_mpp_io.o: $(SRCROOT)shared/mpp/test_mpp_io.F90 $(SRCROOT)shared/include/fms_platform.h mpp.o mpp_domains.o mpp_io.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include	$(SRCROOT)shared/mpp/test_mpp_io.F90
test_mpp_pset.o: $(SRCROOT)shared/mpp/test_mpp_pset.F90 $(SRCROOT)shared/include/fms_platform.h mpp.o mpp_pset.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c -I$(SRCROOT)shared/include	$(SRCROOT)shared/mpp/test_mpp_pset.F90
threadloc.o: $(SRCROOT)shared/mpp/threadloc.c
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) $(OTHER_CFLAGS) -c	$(SRCROOT)shared/mpp/threadloc.c
time_manager.o: $(SRCROOT)shared/time_manager/time_manager.F90 constants.o fms.o mpp.o fms_io.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/time_manager/time_manager.F90
tracer_manager.o: $(SRCROOT)shared/tracer_manager/tracer_manager.F90 mpp.o mpp_io.o fms.o field_manager.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)shared/tracer_manager/tracer_manager.F90
transforms.o: $(SRCROOT)atmos_spectral/tools/transforms.F90 mpp.o fms.o mpp_domains.o spec_mpp.o constants.o spherical_fourier.o grid_fourier.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) -c	$(SRCROOT)atmos_spectral/tools/transforms.F90
./memuse.c: $(SRCROOT)shared/memutils/memuse.c
	cp $(SRCROOT)shared/memutils/memuse.c .
./spec_mpp.F90: $(SRCROOT)atmos_spectral/tools/spec_mpp.F90
	cp $(SRCROOT)atmos_spectral/tools/spec_mpp.F90 .
./mpp_data_mpi.inc: $(SRCROOT)shared/mpp/include/mpp_data_mpi.inc
	cp $(SRCROOT)shared/mpp/include/mpp_data_mpi.inc .
./spherical_fourier.F90: $(SRCROOT)atmos_spectral/tools/spherical_fourier.F90
	cp $(SRCROOT)atmos_spectral/tools/spherical_fourier.F90 .
./mpp_domains.F90: $(SRCROOT)shared/mpp/mpp_domains.F90
	cp $(SRCROOT)shared/mpp/mpp_domains.F90 .
./test_mpp_io.F90: $(SRCROOT)shared/mpp/test_mpp_io.F90
	cp $(SRCROOT)shared/mpp/test_mpp_io.F90 .
./mpp_transmit.inc: $(SRCROOT)shared/mpp/include/mpp_transmit.inc
	cp $(SRCROOT)shared/mpp/include/mpp_transmit.inc .
./mpp_memutils.F90: $(SRCROOT)shared/mpp/mpp_memutils.F90
	cp $(SRCROOT)shared/mpp/mpp_memutils.F90 .
./mpp_define_nest_domains.inc: $(SRCROOT)shared/mpp/include/mpp_define_nest_domains.inc
	cp $(SRCROOT)shared/mpp/include/mpp_define_nest_domains.inc .
./gradient_c2l.c: $(SRCROOT)shared/mosaic/gradient_c2l.c
	cp $(SRCROOT)shared/mosaic/gradient_c2l.c .
./constants.F90: $(SRCROOT)shared/constants/constants.F90
	cp $(SRCROOT)shared/constants/constants.F90 .
./grid.F90: $(SRCROOT)shared/mosaic/grid.F90
	cp $(SRCROOT)shared/mosaic/grid.F90 .
./nsclock.c: $(SRCROOT)shared/mpp/nsclock.c
	cp $(SRCROOT)shared/mpp/nsclock.c .
./time_manager.F90: $(SRCROOT)shared/time_manager/time_manager.F90
	cp $(SRCROOT)shared/time_manager/time_manager.F90 .
./mpp_parameter.F90: $(SRCROOT)shared/mpp/mpp_parameter.F90
	cp $(SRCROOT)shared/mpp/mpp_parameter.F90 .
./mpp_data_sma.inc: $(SRCROOT)shared/mpp/include/mpp_data_sma.inc
	cp $(SRCROOT)shared/mpp/include/mpp_data_sma.inc .
./mpp.F90: $(SRCROOT)shared/mpp/mpp.F90
	cp $(SRCROOT)shared/mpp/mpp.F90 .
./fft99.F90: $(SRCROOT)shared/fft/fft99.F90
	cp $(SRCROOT)shared/fft/fft99.F90 .
./atmos_model.F90: $(SRCROOT)atmos_solo/atmos_model.F90
	cp $(SRCROOT)atmos_solo/atmos_model.F90 .
./mpp_io_write.inc: $(SRCROOT)shared/mpp/include/mpp_io_write.inc
	cp $(SRCROOT)shared/mpp/include/mpp_io_write.inc .
./write_data.inc: $(SRCROOT)shared/fms/write_data.inc
	cp $(SRCROOT)shared/fms/write_data.inc .
./create_xgrid.c: $(SRCROOT)shared/mosaic/create_xgrid.c
	cp $(SRCROOT)shared/mosaic/create_xgrid.c .
./transforms.F90: $(SRCROOT)atmos_spectral/tools/transforms.F90
	cp $(SRCROOT)atmos_spectral/tools/transforms.F90 .
./mpp_sum.inc: $(SRCROOT)shared/mpp/include/mpp_sum.inc
	cp $(SRCROOT)shared/mpp/include/mpp_sum.inc .
./mpp_comm_nocomm.inc: $(SRCROOT)shared/mpp/include/mpp_comm_nocomm.inc
	cp $(SRCROOT)shared/mpp/include/mpp_comm_nocomm.inc .
./mpp_chksum_int.h: $(SRCROOT)shared/mpp/include/mpp_chksum_int.h
	cp $(SRCROOT)shared/mpp/include/mpp_chksum_int.h .
./mpp_reduce_nocomm.h: $(SRCROOT)shared/mpp/include/mpp_reduce_nocomm.h
	cp $(SRCROOT)shared/mpp/include/mpp_reduce_nocomm.h .
./platform.F90: $(SRCROOT)shared/platform/platform.F90
	cp $(SRCROOT)shared/platform/platform.F90 .
./mpp_write_2Ddecomp.h: $(SRCROOT)shared/mpp/include/mpp_write_2Ddecomp.h
	cp $(SRCROOT)shared/mpp/include/mpp_write_2Ddecomp.h .
./mpp_do_redistribute.h: $(SRCROOT)shared/mpp/include/mpp_do_redistribute.h
	cp $(SRCROOT)shared/mpp/include/mpp_do_redistribute.h .
./diag_manager.F90: $(SRCROOT)shared/diag_manager/diag_manager.F90
	cp $(SRCROOT)shared/diag_manager/diag_manager.F90 .
./mpp_util_nocomm.inc: $(SRCROOT)shared/mpp/include/mpp_util_nocomm.inc
	cp $(SRCROOT)shared/mpp/include/mpp_util_nocomm.inc .
./fv_advection.F90: $(SRCROOT)atmos_spectral/model/fv_advection.F90
	cp $(SRCROOT)atmos_spectral/model/fv_advection.F90 .
./mpp_global_sum.h: $(SRCROOT)shared/mpp/include/mpp_global_sum.h
	cp $(SRCROOT)shared/mpp/include/mpp_global_sum.h .
./mpp_reduce_mpi.h: $(SRCROOT)shared/mpp/include/mpp_reduce_mpi.h
	cp $(SRCROOT)shared/mpp/include/mpp_reduce_mpi.h .
./diag_table.F90: $(SRCROOT)shared/diag_manager/diag_table.F90
	cp $(SRCROOT)shared/diag_manager/diag_table.F90 .
./mpp_transmit_sma.h: $(SRCROOT)shared/mpp/include/mpp_transmit_sma.h
	cp $(SRCROOT)shared/mpp/include/mpp_transmit_sma.h .
./field_manager.F90: $(SRCROOT)shared/field_manager/field_manager.F90
	cp $(SRCROOT)shared/field_manager/field_manager.F90 .
./read_mosaic.h: $(SRCROOT)shared/mosaic/read_mosaic.h
	cp $(SRCROOT)shared/mosaic/read_mosaic.h .
./test_fms_io.F90: $(SRCROOT)shared/fms/test_fms_io.F90
	cp $(SRCROOT)shared/fms/test_fms_io.F90 .
./mpp_domains_util.inc: $(SRCROOT)shared/mpp/include/mpp_domains_util.inc
	cp $(SRCROOT)shared/mpp/include/mpp_domains_util.inc .
./get_cal_time.F90: $(SRCROOT)shared/time_manager/get_cal_time.F90
	cp $(SRCROOT)shared/time_manager/get_cal_time.F90 .
./shallow_physics.F90: $(SRCROOT)atmos_spectral_shallow/shallow_physics.F90
	cp $(SRCROOT)atmos_spectral_shallow/shallow_physics.F90 .
./mpp_io_util.inc: $(SRCROOT)shared/mpp/include/mpp_io_util.inc
	cp $(SRCROOT)shared/mpp/include/mpp_io_util.inc .
./mpp_transmit_mpi.h: $(SRCROOT)shared/mpp/include/mpp_transmit_mpi.h
	cp $(SRCROOT)shared/mpp/include/mpp_transmit_mpi.h .
./read_mosaic.c: $(SRCROOT)shared/mosaic/read_mosaic.c
	cp $(SRCROOT)shared/mosaic/read_mosaic.c .
./mpp_error_a_a.h: $(SRCROOT)shared/mpp/include/mpp_error_a_a.h
	cp $(SRCROOT)shared/mpp/include/mpp_error_a_a.h .
./shallow_dynamics.F90: $(SRCROOT)atmos_spectral_shallow/shallow_dynamics.F90
	cp $(SRCROOT)atmos_spectral_shallow/shallow_dynamics.F90 .
./mpp_io.F90: $(SRCROOT)shared/mpp/mpp_io.F90
	cp $(SRCROOT)shared/mpp/mpp_io.F90 .
./mosaic_util.h: $(SRCROOT)shared/mosaic/mosaic_util.h
	cp $(SRCROOT)shared/mosaic/mosaic_util.h .
./spherical.F90: $(SRCROOT)atmos_spectral/tools/spherical.F90
	cp $(SRCROOT)atmos_spectral/tools/spherical.F90 .
./grid_fourier.F90: $(SRCROOT)atmos_spectral/tools/grid_fourier.F90
	cp $(SRCROOT)atmos_spectral/tools/grid_fourier.F90 .
./interp.h: $(SRCROOT)shared/mosaic/interp.h
	cp $(SRCROOT)shared/mosaic/interp.h .
./mpp_do_get_boundary.h: $(SRCROOT)shared/mpp/include/mpp_do_get_boundary.h
	cp $(SRCROOT)shared/mpp/include/mpp_do_get_boundary.h .
./mpp_comm.inc: $(SRCROOT)shared/mpp/include/mpp_comm.inc
	cp $(SRCROOT)shared/mpp/include/mpp_comm.inc .
./spectral_damping.F90: $(SRCROOT)atmos_spectral/model/spectral_damping.F90
	cp $(SRCROOT)atmos_spectral/model/spectral_damping.F90 .
./mosaic.F90: $(SRCROOT)shared/mosaic/mosaic.F90
	cp $(SRCROOT)shared/mosaic/mosaic.F90 .
./mpp_sum_sma.h: $(SRCROOT)shared/mpp/include/mpp_sum_sma.h
	cp $(SRCROOT)shared/mpp/include/mpp_sum_sma.h .
./mpp_do_updateV.h: $(SRCROOT)shared/mpp/include/mpp_do_updateV.h
	cp $(SRCROOT)shared/mpp/include/mpp_do_updateV.h .
./mpp_gather.h: $(SRCROOT)shared/mpp/include/mpp_gather.h
	cp $(SRCROOT)shared/mpp/include/mpp_gather.h .
./mpp_chksum.h: $(SRCROOT)shared/mpp/include/mpp_chksum.h
	cp $(SRCROOT)shared/mpp/include/mpp_chksum.h .
./test_mpp.F90: $(SRCROOT)shared/mpp/test_mpp.F90
	cp $(SRCROOT)shared/mpp/test_mpp.F90 .
./mpp_read_2Ddecomp.h: $(SRCROOT)shared/mpp/include/mpp_read_2Ddecomp.h
	cp $(SRCROOT)shared/mpp/include/mpp_read_2Ddecomp.h .
./mpp_io_read.inc: $(SRCROOT)shared/mpp/include/mpp_io_read.inc
	cp $(SRCROOT)shared/mpp/include/mpp_io_read.inc .
./mpp_do_global_field.h: $(SRCROOT)shared/mpp/include/mpp_do_global_field.h
	cp $(SRCROOT)shared/mpp/include/mpp_do_global_field.h .
./mpp_update_domains2D.h: $(SRCROOT)shared/mpp/include/mpp_update_domains2D.h
	cp $(SRCROOT)shared/mpp/include/mpp_update_domains2D.h .
./mpp_domains_reduce.inc: $(SRCROOT)shared/mpp/include/mpp_domains_reduce.inc
	cp $(SRCROOT)shared/mpp/include/mpp_domains_reduce.inc .
./mpp_comm_sma.inc: $(SRCROOT)shared/mpp/include/mpp_comm_sma.inc
	cp $(SRCROOT)shared/mpp/include/mpp_comm_sma.inc .
./constant.h: $(SRCROOT)shared/mosaic/constant.h
	cp $(SRCROOT)shared/mosaic/constant.h .
./mpp_global_field.h: $(SRCROOT)shared/mpp/include/mpp_global_field.h
	cp $(SRCROOT)shared/mpp/include/mpp_global_field.h .
./mpp_do_updateV_nonblock.h: $(SRCROOT)shared/mpp/include/mpp_do_updateV_nonblock.h
	cp $(SRCROOT)shared/mpp/include/mpp_do_updateV_nonblock.h .
./mpp_domains_comm.inc: $(SRCROOT)shared/mpp/include/mpp_domains_comm.inc
	cp $(SRCROOT)shared/mpp/include/mpp_domains_comm.inc .
./memutils.F90: $(SRCROOT)shared/memutils/memutils.F90
	cp $(SRCROOT)shared/memutils/memutils.F90 .
./mpp_sum_nocomm.h: $(SRCROOT)shared/mpp/include/mpp_sum_nocomm.h
	cp $(SRCROOT)shared/mpp/include/mpp_sum_nocomm.h .
./mpp_get_boundary.h: $(SRCROOT)shared/mpp/include/mpp_get_boundary.h
	cp $(SRCROOT)shared/mpp/include/mpp_get_boundary.h .
./mpp_update_nest_domains.h: $(SRCROOT)shared/mpp/include/mpp_update_nest_domains.h
	cp $(SRCROOT)shared/mpp/include/mpp_update_nest_domains.h .
./mpp_chksum_scalar.h: $(SRCROOT)shared/mpp/include/mpp_chksum_scalar.h
	cp $(SRCROOT)shared/mpp/include/mpp_chksum_scalar.h .
./mpp_error_s_s.h: $(SRCROOT)shared/mpp/include/mpp_error_s_s.h
	cp $(SRCROOT)shared/mpp/include/mpp_error_s_s.h .
./mpp_io_connect.inc: $(SRCROOT)shared/mpp/include/mpp_io_connect.inc
	cp $(SRCROOT)shared/mpp/include/mpp_io_connect.inc .
./mpp_io_misc.inc: $(SRCROOT)shared/mpp/include/mpp_io_misc.inc
	cp $(SRCROOT)shared/mpp/include/mpp_io_misc.inc .
./mpp_utilities.F90: $(SRCROOT)shared/mpp/mpp_utilities.F90
	cp $(SRCROOT)shared/mpp/mpp_utilities.F90 .
./diag_grid.F90: $(SRCROOT)shared/diag_manager/diag_grid.F90
	cp $(SRCROOT)shared/diag_manager/diag_grid.F90 .
./mpp_global_reduce.h: $(SRCROOT)shared/mpp/include/mpp_global_reduce.h
	cp $(SRCROOT)shared/mpp/include/mpp_global_reduce.h .
./mpp_transmit_nocomm.h: $(SRCROOT)shared/mpp/include/mpp_transmit_nocomm.h
	cp $(SRCROOT)shared/mpp/include/mpp_transmit_nocomm.h .
./fms_platform.h: $(SRCROOT)shared/include/fms_platform.h
	cp $(SRCROOT)shared/include/fms_platform.h .
./fft.F90: $(SRCROOT)shared/fft/fft.F90
	cp $(SRCROOT)shared/fft/fft.F90 .
./mpp_error_s_a.h: $(SRCROOT)shared/mpp/include/mpp_error_s_a.h
	cp $(SRCROOT)shared/mpp/include/mpp_error_s_a.h .
./mpp_global_sum_tl.h: $(SRCROOT)shared/mpp/include/mpp_global_sum_tl.h
	cp $(SRCROOT)shared/mpp/include/mpp_global_sum_tl.h .
./create_xgrid.h: $(SRCROOT)shared/mosaic/create_xgrid.h
	cp $(SRCROOT)shared/mosaic/create_xgrid.h .
./mpp_do_update_nonblock.h: $(SRCROOT)shared/mpp/include/mpp_do_update_nonblock.h
	cp $(SRCROOT)shared/mpp/include/mpp_do_update_nonblock.h .
./atmosphere.F90: $(SRCROOT)atmos_spectral_shallow/atmosphere.F90
	cp $(SRCROOT)atmos_spectral_shallow/atmosphere.F90 .
./mosaic_util.c: $(SRCROOT)shared/mosaic/mosaic_util.c
	cp $(SRCROOT)shared/mosaic/mosaic_util.c .
./diag_axis.F90: $(SRCROOT)shared/diag_manager/diag_axis.F90
	cp $(SRCROOT)shared/diag_manager/diag_axis.F90 .
./read_data_3d.inc: $(SRCROOT)shared/fms/read_data_3d.inc
	cp $(SRCROOT)shared/fms/read_data_3d.inc .
./gradient_c2l.h: $(SRCROOT)shared/mosaic/gradient_c2l.h
	cp $(SRCROOT)shared/mosaic/gradient_c2l.h .
./read_data_4d.inc: $(SRCROOT)shared/fms/read_data_4d.inc
	cp $(SRCROOT)shared/fms/read_data_4d.inc .
./read_data_2d.inc: $(SRCROOT)shared/fms/read_data_2d.inc
	cp $(SRCROOT)shared/fms/read_data_2d.inc .
./parse.inc: $(SRCROOT)shared/field_manager/parse.inc
	cp $(SRCROOT)shared/field_manager/parse.inc .
./fms_io.F90: $(SRCROOT)shared/fms/fms_io.F90
	cp $(SRCROOT)shared/fms/fms_io.F90 .
./mpp_do_check.h: $(SRCROOT)shared/mpp/include/mpp_do_check.h
	cp $(SRCROOT)shared/mpp/include/mpp_do_check.h .
./affinity.c: $(SRCROOT)shared/mpp/affinity.c
	cp $(SRCROOT)shared/mpp/affinity.c .
./mpp_do_checkV.h: $(SRCROOT)shared/mpp/include/mpp_do_checkV.h
	cp $(SRCROOT)shared/mpp/include/mpp_do_checkV.h .
./mpp_sum_mpi.h: $(SRCROOT)shared/mpp/include/mpp_sum_mpi.h
	cp $(SRCROOT)shared/mpp/include/mpp_sum_mpi.h .
./mpp_domains_define.inc: $(SRCROOT)shared/mpp/include/mpp_domains_define.inc
	cp $(SRCROOT)shared/mpp/include/mpp_domains_define.inc .
./mpp_do_update_nest.h: $(SRCROOT)shared/mpp/include/mpp_do_update_nest.h
	cp $(SRCROOT)shared/mpp/include/mpp_do_update_nest.h .
./system_clock.h: $(SRCROOT)shared/mpp/include/system_clock.h
	cp $(SRCROOT)shared/mpp/include/system_clock.h .
./diag_data.F90: $(SRCROOT)shared/diag_manager/diag_data.F90
	cp $(SRCROOT)shared/diag_manager/diag_data.F90 .
./mpp_reduce_sma.h: $(SRCROOT)shared/mpp/include/mpp_reduce_sma.h
	cp $(SRCROOT)shared/mpp/include/mpp_reduce_sma.h .
./mpp_do_update.h: $(SRCROOT)shared/mpp/include/mpp_do_update.h
	cp $(SRCROOT)shared/mpp/include/mpp_do_update.h .
./mpp_domains_misc.inc: $(SRCROOT)shared/mpp/include/mpp_domains_misc.inc
	cp $(SRCROOT)shared/mpp/include/mpp_domains_misc.inc .
./mpp_pset.F90: $(SRCROOT)shared/mpp/mpp_pset.F90
	cp $(SRCROOT)shared/mpp/mpp_pset.F90 .
./interp.c: $(SRCROOT)shared/mosaic/interp.c
	cp $(SRCROOT)shared/mosaic/interp.c .
./mpp_data_nocomm.inc: $(SRCROOT)shared/mpp/include/mpp_data_nocomm.inc
	cp $(SRCROOT)shared/mpp/include/mpp_data_nocomm.inc .
./test_mpp_pset.F90: $(SRCROOT)shared/mpp/test_mpp_pset.F90
	cp $(SRCROOT)shared/mpp/test_mpp_pset.F90 .
./mpp_error_a_s.h: $(SRCROOT)shared/mpp/include/mpp_error_a_s.h
	cp $(SRCROOT)shared/mpp/include/mpp_error_a_s.h .
./mpp_util.inc: $(SRCROOT)shared/mpp/include/mpp_util.inc
	cp $(SRCROOT)shared/mpp/include/mpp_util.inc .
./mpp_write.h: $(SRCROOT)shared/mpp/include/mpp_write.h
	cp $(SRCROOT)shared/mpp/include/mpp_write.h .
./shallow_diagnostics.F90: $(SRCROOT)atmos_spectral_shallow/shallow_diagnostics.F90
	cp $(SRCROOT)atmos_spectral_shallow/shallow_diagnostics.F90 .
./fm_util.F90: $(SRCROOT)shared/field_manager/fm_util.F90
	cp $(SRCROOT)shared/field_manager/fm_util.F90 .
./test_mpp_domains.F90: $(SRCROOT)shared/mpp/test_mpp_domains.F90
	cp $(SRCROOT)shared/mpp/test_mpp_domains.F90 .
./mpp_util_sma.inc: $(SRCROOT)shared/mpp/include/mpp_util_sma.inc
	cp $(SRCROOT)shared/mpp/include/mpp_util_sma.inc .
./diag_util.F90: $(SRCROOT)shared/diag_manager/diag_util.F90
	cp $(SRCROOT)shared/diag_manager/diag_util.F90 .
./gradient.F90: $(SRCROOT)shared/mosaic/gradient.F90
	cp $(SRCROOT)shared/mosaic/gradient.F90 .
./gauss_and_legendre.F90: $(SRCROOT)atmos_spectral/tools/gauss_and_legendre.F90
	cp $(SRCROOT)atmos_spectral/tools/gauss_and_legendre.F90 .
./mpp_update_domains2D_nonblock.h: $(SRCROOT)shared/mpp/include/mpp_update_domains2D_nonblock.h
	cp $(SRCROOT)shared/mpp/include/mpp_update_domains2D_nonblock.h .
./diag_output.F90: $(SRCROOT)shared/diag_manager/diag_output.F90
	cp $(SRCROOT)shared/diag_manager/diag_output.F90 .
./mpp_data.F90: $(SRCROOT)shared/mpp/mpp_data.F90
	cp $(SRCROOT)shared/mpp/mpp_data.F90 .
./mpp_comm_mpi.inc: $(SRCROOT)shared/mpp/include/mpp_comm_mpi.inc
	cp $(SRCROOT)shared/mpp/include/mpp_comm_mpi.inc .
./leapfrog.F90: $(SRCROOT)atmos_spectral/model/leapfrog.F90
	cp $(SRCROOT)atmos_spectral/model/leapfrog.F90 .
./tracer_manager.F90: $(SRCROOT)shared/tracer_manager/tracer_manager.F90
	cp $(SRCROOT)shared/tracer_manager/tracer_manager.F90 .
./threadloc.c: $(SRCROOT)shared/mpp/threadloc.c
	cp $(SRCROOT)shared/mpp/threadloc.c .
./fms.F90: $(SRCROOT)shared/fms/fms.F90
	cp $(SRCROOT)shared/fms/fms.F90 .
./mpp_util_mpi.inc: $(SRCROOT)shared/mpp/include/mpp_util_mpi.inc
	cp $(SRCROOT)shared/mpp/include/mpp_util_mpi.inc .
SRC = $(SRCROOT)shared/time_manager/time_manager.F90 $(SRCROOT)atmos_spectral/tools/spherical_fourier.F90 $(SRCROOT)shared/mpp/mpp_data.F90 $(SRCROOT)shared/mpp/affinity.c $(SRCROOT)shared/mosaic/gradient_c2l.c $(SRCROOT)atmos_solo/atmos_model.F90 $(SRCROOT)atmos_spectral_shallow/shallow_diagnostics.F90 $(SRCROOT)shared/mpp/mpp_io.F90 $(SRCROOT)atmos_spectral_shallow/atmosphere.F90 $(SRCROOT)shared/fms/fms_io.F90 $(SRCROOT)shared/tracer_manager/tracer_manager.F90 $(SRCROOT)shared/mpp/mpp_memutils.F90 $(SRCROOT)shared/diag_manager/diag_util.F90 $(SRCROOT)shared/mosaic/mosaic_util.c $(SRCROOT)atmos_spectral/model/spectral_damping.F90 $(SRCROOT)shared/mosaic/gradient.F90 $(SRCROOT)shared/fft/fft.F90 $(SRCROOT)shared/field_manager/fm_util.F90 $(SRCROOT)atmos_spectral_shallow/shallow_physics.F90 $(SRCROOT)shared/mosaic/mosaic.F90 $(SRCROOT)shared/memutils/memuse.c $(SRCROOT)shared/memutils/memutils.F90 $(SRCROOT)shared/mpp/threadloc.c $(SRCROOT)shared/diag_manager/diag_data.F90 $(SRCROOT)shared/diag_manager/diag_axis.F90 $(SRCROOT)shared/mosaic/grid.F90 $(SRCROOT)shared/diag_manager/diag_manager.F90 $(SRCROOT)shared/time_manager/get_cal_time.F90 $(SRCROOT)shared/mosaic/read_mosaic.c $(SRCROOT)atmos_spectral/tools/transforms.F90 $(SRCROOT)shared/mpp/mpp_utilities.F90 $(SRCROOT)shared/constants/constants.F90 $(SRCROOT)atmos_spectral/tools/spherical.F90 $(SRCROOT)shared/mpp/mpp_pset.F90 $(SRCROOT)shared/fms/fms.F90 $(SRCROOT)shared/mpp/test_mpp_pset.F90 $(SRCROOT)shared/mpp/test_mpp_io.F90 $(SRCROOT)atmos_spectral/tools/spec_mpp.F90 $(SRCROOT)shared/fms/test_fms_io.F90 $(SRCROOT)atmos_spectral/tools/grid_fourier.F90 $(SRCROOT)shared/field_manager/field_manager.F90 $(SRCROOT)shared/mosaic/create_xgrid.c $(SRCROOT)shared/diag_manager/diag_output.F90 $(SRCROOT)shared/mosaic/interp.c $(SRCROOT)atmos_spectral/tools/gauss_and_legendre.F90 $(SRCROOT)shared/mpp/nsclock.c $(SRCROOT)atmos_spectral/model/leapfrog.F90 $(SRCROOT)shared/mpp/mpp_parameter.F90 $(SRCROOT)shared/platform/platform.F90 $(SRCROOT)shared/mpp/mpp.F90 $(SRCROOT)atmos_spectral/model/fv_advection.F90 $(SRCROOT)shared/fft/fft99.F90 $(SRCROOT)shared/mpp/mpp_domains.F90 $(SRCROOT)shared/mpp/test_mpp_domains.F90 $(SRCROOT)shared/diag_manager/diag_table.F90 $(SRCROOT)shared/diag_manager/diag_grid.F90 $(SRCROOT)shared/mpp/test_mpp.F90 $(SRCROOT)atmos_spectral_shallow/shallow_dynamics.F90 $(SRCROOT)shared/mpp/include/mpp_update_nest_domains.h $(SRCROOT)shared/mpp/include/mpp_data_mpi.inc $(SRCROOT)shared/mpp/include/mpp_chksum_scalar.h $(SRCROOT)shared/mpp/include/mpp_error_s_s.h $(SRCROOT)shared/mpp/include/mpp_transmit.inc $(SRCROOT)shared/mpp/include/mpp_define_nest_domains.inc $(SRCROOT)shared/mpp/include/mpp_io_connect.inc $(SRCROOT)shared/mpp/include/mpp_io_misc.inc $(SRCROOT)shared/mpp/include/mpp_global_reduce.h $(SRCROOT)shared/mpp/include/mpp_transmit_nocomm.h $(SRCROOT)shared/mpp/include/mpp_data_sma.inc $(SRCROOT)shared/include/fms_platform.h $(SRCROOT)shared/mpp/include/mpp_global_sum_tl.h $(SRCROOT)shared/mpp/include/mpp_error_s_a.h $(SRCROOT)shared/mpp/include/mpp_io_write.inc $(SRCROOT)shared/mosaic/create_xgrid.h $(SRCROOT)shared/mpp/include/mpp_do_update_nonblock.h $(SRCROOT)shared/fms/write_data.inc $(SRCROOT)shared/mpp/include/mpp_sum.inc $(SRCROOT)shared/mpp/include/mpp_comm_nocomm.inc $(SRCROOT)shared/mpp/include/mpp_chksum_int.h $(SRCROOT)shared/mpp/include/mpp_reduce_nocomm.h $(SRCROOT)shared/mpp/include/mpp_write_2Ddecomp.h $(SRCROOT)shared/fms/read_data_3d.inc $(SRCROOT)shared/mpp/include/mpp_do_redistribute.h $(SRCROOT)shared/mosaic/gradient_c2l.h $(SRCROOT)shared/mpp/include/mpp_util_nocomm.inc $(SRCROOT)shared/fms/read_data_4d.inc $(SRCROOT)shared/mpp/include/mpp_global_sum.h $(SRCROOT)shared/mpp/include/mpp_reduce_mpi.h $(SRCROOT)shared/fms/read_data_2d.inc $(SRCROOT)shared/mpp/include/mpp_transmit_sma.h $(SRCROOT)shared/field_manager/parse.inc $(SRCROOT)shared/mpp/include/mpp_do_check.h $(SRCROOT)shared/mpp/include/mpp_do_checkV.h $(SRCROOT)shared/mosaic/read_mosaic.h $(SRCROOT)shared/mpp/include/mpp_do_update_nest.h $(SRCROOT)shared/mpp/include/mpp_domains_define.inc $(SRCROOT)shared/mpp/include/mpp_sum_mpi.h $(SRCROOT)shared/mpp/include/system_clock.h $(SRCROOT)shared/mpp/include/mpp_domains_util.inc $(SRCROOT)shared/mpp/include/mpp_reduce_sma.h $(SRCROOT)shared/mpp/include/mpp_do_update.h $(SRCROOT)shared/mpp/include/mpp_domains_misc.inc $(SRCROOT)shared/mpp/include/mpp_io_util.inc $(SRCROOT)shared/mpp/include/mpp_data_nocomm.inc $(SRCROOT)shared/mpp/include/mpp_transmit_mpi.h $(SRCROOT)shared/mpp/include/mpp_error_a_a.h $(SRCROOT)shared/mpp/include/mpp_error_a_s.h $(SRCROOT)shared/mpp/include/mpp_write.h $(SRCROOT)shared/mpp/include/mpp_util.inc $(SRCROOT)shared/mosaic/mosaic_util.h $(SRCROOT)shared/mosaic/interp.h $(SRCROOT)shared/mpp/include/mpp_comm.inc $(SRCROOT)shared/mpp/include/mpp_do_get_boundary.h $(SRCROOT)shared/mpp/include/mpp_sum_sma.h $(SRCROOT)shared/mpp/include/mpp_util_sma.inc $(SRCROOT)shared/mpp/include/mpp_gather.h $(SRCROOT)shared/mpp/include/mpp_do_updateV.h $(SRCROOT)shared/mpp/include/mpp_chksum.h $(SRCROOT)shared/mpp/include/mpp_read_2Ddecomp.h $(SRCROOT)shared/mpp/include/mpp_io_read.inc $(SRCROOT)shared/mpp/include/mpp_do_global_field.h $(SRCROOT)shared/mpp/include/mpp_update_domains2D.h $(SRCROOT)shared/mpp/include/mpp_update_domains2D_nonblock.h $(SRCROOT)shared/mpp/include/mpp_domains_reduce.inc $(SRCROOT)shared/mpp/include/mpp_comm_sma.inc $(SRCROOT)shared/mosaic/constant.h $(SRCROOT)shared/mpp/include/mpp_global_field.h $(SRCROOT)shared/mpp/include/mpp_do_updateV_nonblock.h $(SRCROOT)shared/mpp/include/mpp_comm_mpi.inc $(SRCROOT)shared/mpp/include/mpp_domains_comm.inc $(SRCROOT)shared/mpp/include/mpp_util_mpi.inc $(SRCROOT)shared/mpp/include/mpp_sum_nocomm.h $(SRCROOT)shared/mpp/include/mpp_get_boundary.h
OBJ = rand_generator.o mass_pulse.o wave_break.o time_manager.o spherical_fourier.o mpp_data.o affinity.o gradient_c2l.o atmos_model.o shallow_diagnostics.o mpp_io.o atmosphere.o fms_io.o tracer_manager.o mpp_memutils.o diag_util.o mosaic_util.o spectral_damping.o gradient.o fft.o fm_util.o shallow_physics.o mosaic.o memuse.o memutils.o threadloc.o diag_data.o diag_axis.o grid.o diag_manager.o get_cal_time.o read_mosaic.o transforms.o mpp_utilities.o constants.o spherical.o mpp_pset.o fms.o test_mpp_pset.o test_mpp_io.o spec_mpp.o test_fms_io.o grid_fourier.o field_manager.o create_xgrid.o diag_output.o interp.o gauss_and_legendre.o nsclock.o leapfrog.o mpp_parameter.o platform.o mpp.o fv_advection.o fft99.o mpp_domains.o test_mpp_domains.o diag_table.o diag_grid.o test_mpp.o shallow_dynamics.o
OFF = $(SRCROOT)shared/memutils/memuse.c $(SRCROOT)atmos_spectral/tools/spec_mpp.F90 $(SRCROOT)shared/mpp/include/mpp_data_mpi.inc $(SRCROOT)atmos_spectral/tools/spherical_fourier.F90 $(SRCROOT)shared/mpp/mpp_domains.F90 $(SRCROOT)shared/mpp/test_mpp_io.F90 $(SRCROOT)shared/mpp/include/mpp_transmit.inc $(SRCROOT)shared/mpp/mpp_memutils.F90 $(SRCROOT)shared/mpp/include/mpp_define_nest_domains.inc $(SRCROOT)shared/mosaic/gradient_c2l.c $(SRCROOT)shared/constants/constants.F90 $(SRCROOT)shared/mosaic/grid.F90 $(SRCROOT)shared/mpp/nsclock.c $(SRCROOT)shared/time_manager/time_manager.F90 $(SRCROOT)shared/mpp/mpp_parameter.F90 $(SRCROOT)shared/mpp/include/mpp_data_sma.inc $(SRCROOT)shared/mpp/mpp.F90 $(SRCROOT)shared/fft/fft99.F90 $(SRCROOT)atmos_solo/atmos_model.F90 $(SRCROOT)shared/mpp/include/mpp_io_write.inc $(SRCROOT)shared/fms/write_data.inc $(SRCROOT)shared/mosaic/create_xgrid.c $(SRCROOT)atmos_spectral/tools/transforms.F90 $(SRCROOT)shared/mpp/include/mpp_sum.inc $(SRCROOT)shared/mpp/include/mpp_comm_nocomm.inc $(SRCROOT)shared/mpp/include/mpp_chksum_int.h $(SRCROOT)shared/mpp/include/mpp_reduce_nocomm.h $(SRCROOT)shared/platform/platform.F90 $(SRCROOT)shared/mpp/include/mpp_write_2Ddecomp.h $(SRCROOT)shared/mpp/include/mpp_do_redistribute.h $(SRCROOT)shared/diag_manager/diag_manager.F90 $(SRCROOT)shared/mpp/include/mpp_util_nocomm.inc $(SRCROOT)atmos_spectral/model/fv_advection.F90 $(SRCROOT)shared/mpp/include/mpp_global_sum.h $(SRCROOT)shared/mpp/include/mpp_reduce_mpi.h $(SRCROOT)shared/diag_manager/diag_table.F90 $(SRCROOT)shared/mpp/include/mpp_transmit_sma.h $(SRCROOT)shared/field_manager/field_manager.F90 $(SRCROOT)shared/mosaic/read_mosaic.h $(SRCROOT)shared/fms/test_fms_io.F90 $(SRCROOT)shared/mpp/include/mpp_domains_util.inc $(SRCROOT)shared/time_manager/get_cal_time.F90 $(SRCROOT)atmos_spectral_shallow/shallow_physics.F90 $(SRCROOT)shared/mpp/include/mpp_io_util.inc $(SRCROOT)shared/mpp/include/mpp_transmit_mpi.h $(SRCROOT)shared/mosaic/read_mosaic.c $(SRCROOT)shared/mpp/include/mpp_error_a_a.h $(SRCROOT)atmos_spectral_shallow/shallow_dynamics.F90 $(SRCROOT)shared/mpp/mpp_io.F90 $(SRCROOT)shared/mosaic/mosaic_util.h $(SRCROOT)atmos_spectral/tools/spherical.F90 $(SRCROOT)atmos_spectral/tools/grid_fourier.F90 $(SRCROOT)shared/mosaic/interp.h $(SRCROOT)shared/mpp/include/mpp_do_get_boundary.h $(SRCROOT)shared/mpp/include/mpp_comm.inc $(SRCROOT)atmos_spectral/model/spectral_damping.F90 $(SRCROOT)shared/mosaic/mosaic.F90 $(SRCROOT)shared/mpp/include/mpp_sum_sma.h $(SRCROOT)shared/mpp/include/mpp_do_updateV.h $(SRCROOT)shared/mpp/include/mpp_gather.h $(SRCROOT)shared/mpp/include/mpp_chksum.h $(SRCROOT)shared/mpp/test_mpp.F90 $(SRCROOT)shared/mpp/include/mpp_read_2Ddecomp.h $(SRCROOT)shared/mpp/include/mpp_io_read.inc $(SRCROOT)shared/mpp/include/mpp_do_global_field.h $(SRCROOT)shared/mpp/include/mpp_update_domains2D.h $(SRCROOT)shared/mpp/include/mpp_domains_reduce.inc $(SRCROOT)shared/mpp/include/mpp_comm_sma.inc $(SRCROOT)shared/mosaic/constant.h $(SRCROOT)shared/mpp/include/mpp_global_field.h $(SRCROOT)shared/mpp/include/mpp_do_updateV_nonblock.h $(SRCROOT)shared/mpp/include/mpp_domains_comm.inc $(SRCROOT)shared/memutils/memutils.F90 $(SRCROOT)shared/mpp/include/mpp_sum_nocomm.h $(SRCROOT)shared/mpp/include/mpp_get_boundary.h $(SRCROOT)shared/mpp/include/mpp_update_nest_domains.h $(SRCROOT)shared/mpp/include/mpp_chksum_scalar.h $(SRCROOT)shared/mpp/include/mpp_error_s_s.h $(SRCROOT)shared/mpp/include/mpp_io_connect.inc $(SRCROOT)shared/mpp/include/mpp_io_misc.inc $(SRCROOT)shared/mpp/mpp_utilities.F90 $(SRCROOT)shared/diag_manager/diag_grid.F90 $(SRCROOT)shared/mpp/include/mpp_global_reduce.h $(SRCROOT)shared/mpp/include/mpp_transmit_nocomm.h $(SRCROOT)shared/include/fms_platform.h $(SRCROOT)shared/fft/fft.F90 $(SRCROOT)shared/mpp/include/mpp_error_s_a.h $(SRCROOT)shared/mpp/include/mpp_global_sum_tl.h $(SRCROOT)shared/mosaic/create_xgrid.h $(SRCROOT)shared/mpp/include/mpp_do_update_nonblock.h $(SRCROOT)atmos_spectral_shallow/atmosphere.F90 $(SRCROOT)shared/mosaic/mosaic_util.c $(SRCROOT)shared/diag_manager/diag_axis.F90 $(SRCROOT)shared/fms/read_data_3d.inc $(SRCROOT)shared/mosaic/gradient_c2l.h $(SRCROOT)shared/fms/read_data_4d.inc $(SRCROOT)shared/fms/read_data_2d.inc $(SRCROOT)shared/field_manager/parse.inc $(SRCROOT)shared/fms/fms_io.F90 $(SRCROOT)shared/mpp/include/mpp_do_check.h $(SRCROOT)shared/mpp/affinity.c $(SRCROOT)shared/mpp/include/mpp_do_checkV.h $(SRCROOT)shared/mpp/include/mpp_sum_mpi.h $(SRCROOT)shared/mpp/include/mpp_domains_define.inc $(SRCROOT)shared/mpp/include/mpp_do_update_nest.h $(SRCROOT)shared/mpp/include/system_clock.h $(SRCROOT)shared/diag_manager/diag_data.F90 $(SRCROOT)shared/mpp/include/mpp_reduce_sma.h $(SRCROOT)shared/mpp/include/mpp_do_update.h $(SRCROOT)shared/mpp/include/mpp_domains_misc.inc $(SRCROOT)shared/mpp/mpp_pset.F90 $(SRCROOT)shared/mosaic/interp.c $(SRCROOT)shared/mpp/include/mpp_data_nocomm.inc $(SRCROOT)shared/mpp/test_mpp_pset.F90 $(SRCROOT)shared/mpp/include/mpp_error_a_s.h $(SRCROOT)shared/mpp/include/mpp_util.inc $(SRCROOT)shared/mpp/include/mpp_write.h $(SRCROOT)atmos_spectral_shallow/shallow_diagnostics.F90 $(SRCROOT)shared/field_manager/fm_util.F90 $(SRCROOT)shared/mpp/test_mpp_domains.F90 $(SRCROOT)shared/mpp/include/mpp_util_sma.inc $(SRCROOT)shared/diag_manager/diag_util.F90 $(SRCROOT)shared/mosaic/gradient.F90 $(SRCROOT)atmos_spectral/tools/gauss_and_legendre.F90 $(SRCROOT)shared/mpp/include/mpp_update_domains2D_nonblock.h $(SRCROOT)shared/diag_manager/diag_output.F90 $(SRCROOT)shared/mpp/mpp_data.F90 $(SRCROOT)shared/mpp/include/mpp_comm_mpi.inc $(SRCROOT)atmos_spectral/model/leapfrog.F90 $(SRCROOT)shared/tracer_manager/tracer_manager.F90 $(SRCROOT)shared/mpp/threadloc.c $(SRCROOT)shared/fms/fms.F90 $(SRCROOT)shared/mpp/include/mpp_util_mpi.inc
clean: neat
	-rm -f .shallow_water.x.cppdefs $(OBJ) shallow_water.x
neat:
	-rm -f $(TMPFILES)
localize: $(OFF)
	cp $(OFF) .
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
shallow_water.x: $(OBJ) 
	$(LD) $(OBJ) -o shallow_water.x  $(LDFLAGS)
