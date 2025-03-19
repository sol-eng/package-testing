#!/bin/bash

sudo -u rstudio-pm /opt/rstudio-pm/bin/rstudio-pm --config /etc/rstudio-pm/rstudio-pm.gcfg & 
sleep 10
/opt/rstudio-pm/bin/license-manager activate $RSPM_LICENSE
#rm -rf /var/run/rstudio-pm/rstudio-pm.pid
kill `pgrep rstudio-pm`

/opt/rstudio-pm/bin/rstudio-pm --config /etc/rstudio-pm/rstudio-pm.gcfg & 

sleep 20

if [[ `rspm list repos | grep repos | awk '{print $1}'` -eq 0 ]]; then 
    bash -x /ppm-setup.sh > /var/log/pm-setup.log
fi

while true 
do
sleep 60
done 
