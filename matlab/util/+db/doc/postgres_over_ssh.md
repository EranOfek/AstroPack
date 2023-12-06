# Using Postgres Client over SSH Tunnel


https://www.postgresql.org/docs/current/ssh-tunnels.html


ssh -L 63331:localhost:5432 ocs@10.23.1.25

psql -h localhost -p 63331 -d last_operational -W

