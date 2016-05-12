#! /bin/sh

# port forward some ports for random use (Note: Must stop vm first)
 docker-machine stop dock2
 for i in {2000..2019}; do
   echo $i
   VBoxManage modifyvm "dock2" --natpf1 "tcp-port$i,tcp,,$i,,$i";
   VBoxManage modifyvm "dock2" --natpf1 "udp-port$i,udp,,$i,,$i";
 done
