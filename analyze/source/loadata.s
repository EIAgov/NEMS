#!/bin/csh
foreach i (`dosdir | grep -v "free space"`)
  set nm = $i
  dosread -a $i $nm
end
