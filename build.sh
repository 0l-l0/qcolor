#!/bin/sh

# qcolor - graph quasi-coloring utility
# Copyright (C) 2019  0l-l0
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

C_RED='\033[1;31m'
C_GREEN='\033[1;32m'
C_OFF='\033[0m'

BASENAME=$(ls -d $0)
cd $(dirname $BASENAME)

echo -e "$C_GREEN #$C_OFF Building qcolor binary..."

TEST_ARGS=""
if [ $# -eq 1 ] && [ "$1" = "test" ]
then
	TEST_ARGS="-f -cargs -fprofile-arcs -ftest-coverage -largs -fprofile-arcs"
	rm -rf ./obj
	echo -e "$C_GREEN #$C_OFF (Building for testing)"
fi

WIN_SYS=windows
UNAME=$(uname)
if [ "$UNAME" = "Linux" ]
then
	WIN_SYS=x11
elif [ "$UNAME" = "Darwin" ]
then
	WIN_SYS=quartz
fi

export GPR_PROJECT_PATH=${GPR_PROJECT_PATH:-'./ext/OpenCLAda'}

gprbuild -p -Pquasi_coloring.gpr \
	-XWindowing_System=$WIN_SYS -Xmode=release $TEST_ARGS

if [ $? -ne 0 ]
then
	echo -e "$C_RED #$C_OFF qcolor build failed. :("
else
	echo -e "$C_GREEN #$C_OFF qcolor binary built successfully. (Check bin directory.)"
fi
