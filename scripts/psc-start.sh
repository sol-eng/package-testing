#!/bin/bash

/opt/rstudio-connect/bin/connect --config /etc/rstudio-connect/rstudio-connect.gcfg &

sleep 10

/opt/rstudio-connect/bin/license-manager activate $RSC_LICENSE 

kill `pgrep connect`

/opt/rstudio-connect/bin/connect --config /etc/rstudio-connect/rstudio-connect.gcfg &

sleep 10

if [ ! -f /var/lib/token/connect-bootstrap.key ]; then 
sudo -u packager bash -l -c  "/opt/python/3.9.16/bin/python -m venv ~/.venv && \
    source ~/.venv/bin/activate && \
    pip install rsconnect-python >& /dev/null && \
    rsconnect bootstrap --raw --server http://connect:3939 --jwt-keypath /var/lib/rstudio-connect/secret.key" \
    > /var/lib/token/connect-bootstrap.key

API_KEY=`cat /var/lib/token/connect-bootstrap.key`

DATA='{
  "email": "michael.mayer@posit.co",
  "first_name": "Michael",
  "last_name": "Mayer",
  "user_must_set_password": false,
  "password": "Testme1234",
  "user_role": "administrator",
  "username": "michael"
}'

curl --silent --show-error -L --max-redirs 0 --fail \
    -X POST \
    -H "Authorization: Key ${API_KEY}" \
    --data-raw "${DATA}" \
    "http://connect:3939/__api__/v1/users" > /tmp/curl.log

kill `pgrep connect`  

/opt/rstudio-connect/bin/usermanager transfer --source-username __bootstrap_admin__  --target-username michael --api-keys --delete --yes

/opt/rstudio-connect/bin/connect --config /etc/rstudio-connect/rstudio-connect.gcfg 
fi

while true
do
sleep 120
done

