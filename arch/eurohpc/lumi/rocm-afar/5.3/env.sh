# (C) Copyright 1988- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# Source me to get the correct configure/build/run environment

# Store tracing and disable (module is *way* too verbose)
{ tracing_=${-//[^x]/}; set +x; } 2>/dev/null

module_load() {
  echo "+ module load $1"
  module load $1
}
module_unload() {
  echo "+ module unload $1"
  module unload $1
}

# Unload to be certain
module reset

# Load modules
module_load LUMI/24.03
module_load partition/G
module_load PrgEnv-cray/8.4.0
module_load buildtools/24.03

# Set environment variables for flang
export LD_LIBRARY_PATH=/users/nawabahm/rocm-afar-7110-drop-5.3.0/lib:/users/nawabahm/rocm-afar-7110-drop-5.3.0/lib/llvm/lib:$LD_LIBRARY_PATH
export PATH=/users/nawabahm/rocm-afar-7110-drop-5.3.0/bin:$PATH
export hipfort_ROOT=/users/nawabahm/hipfort/install

# Export environment variable3s
export CC=amdclang CXX=amdclang++ FC=amdflang

set -x

# Restore tracing to stored setting
{ if [[ -n "$tracing_" ]]; then set -x; else set +x; fi } 2>/dev/null

path=$BASH_SOURCE
DIR_PATH=$(dirname $path)
export ECBUILD_TOOLCHAIN=$DIR_PATH/toolchain.cmake
