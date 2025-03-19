# package-testing

Build containers

```
docker-compose build 
```

Launch containers

```
docker-compose up -d 
```

Activate account on Connect (username/pass = michael/Testme1234) via logging into Connect and send verification eMail. Once done, run `docker-compose logs connect` to see the activation link. Copy & paste this link into your browser. 

Run testing

```
docker-compose exec rserver bash -l -c "cd /code && Rscript testapi.R"
```

