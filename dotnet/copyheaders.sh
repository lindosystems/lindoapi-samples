#/bin/bash
#Copy headers from lindoapi/include folder
#
find . -type f -name lindo.cs -exec cp -pv ../../include/lindo.cs {} \;
find . -type f -name lindo.vb -exec cp -pv ../../include/lindo.vb {} \;
